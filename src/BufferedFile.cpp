#include <fcntl.h>
#include <string.h>
#include <time.h>
#include "naryn.h"
#include "BufferedFile.h"
#include "TGLException.h"
#include "FileUtils.h"

int64_t BufferedFile::file_size(const char *path)
{
	struct stat st;

	if (::stat(path, &st))
		TGLError("Cannot stat file %s: %s\n", path, strerror(errno));
	return (int64_t)st.st_size;
}

int BufferedFile::open(const char *path, const char *mode, bool lock) {
    close();
    m_filename = (string)path;
    m_fp = fopen(path, mode);
    vdebug(7, "Opening file %s\n", path);

    if (m_fp) {
        if (lock) {
            struct flock fl;
            time_t start_time = time(NULL);
            bool lock_acquired = false;

            vdebug(7, "Attempting to acquire %s lock for %s\n", strcmp(mode, "r") ? "write" : "read", path);

            memset(&fl, 0, sizeof(fl));
            fl.l_type = strcmp(mode, "r") ? F_WRLCK : F_RDLCK;

            while (time(NULL) - start_time < 5) {
                if (fcntl(fileno(m_fp), F_SETLK, &fl) != -1) {
                    lock_acquired = true;
                    vdebug(7, "Lock acquired for %s\n", path);
                    break;
                }
                if (errno != EACCES && errno != EAGAIN) {
                    vdebug(7, "Unexpected error while trying to acquire lock: %s\n", strerror(errno));
                    close();
                    return -1;
                }
                check_interrupt();
                usleep(100000);  // Sleep for 100ms before retrying
            }

            /*
             * DISCLAIMER: This implementation includes an ugly workaround for dealing with stale locks on NFS systems.
             * The cp-mv strategy employed here is not an ideal solution and may lead to race conditions or data inconsistency.
             *
             * The real solution for concurrent access and locking issues is to use a proper database system,
             * which provides robust transaction management and locking mechanisms.
            */

            if (!lock_acquired) {
                vdebug(7, "Failed to acquire lock after 5 seconds. Attempting cp-mv strategy.\n");
                
                // Generate a random suffix for the temporary file
                char temp_suffix[16];
                snprintf(temp_suffix, sizeof(temp_suffix), "-%06d", rand() % 1000000);
                
                string temp_filename = m_filename + temp_suffix;
                vdebug(7, "Generated temporary filename: %s\n", temp_filename.c_str());

                // Close the original file
                fclose(m_fp);
                m_fp = NULL;

                // Perform the cp-mv strategy
                try {
                    vdebug(7, "Copying %s to %s\n", path, temp_filename.c_str());
                    FileUtils::copy_file(path, temp_filename.c_str());
                    
                    vdebug(7, "Moving %s back to %s\n", temp_filename.c_str(), path);
                    FileUtils::move_file(temp_filename.c_str(), path);
                } catch (const TGLException& e) {
                    vdebug(7, "Error during cp-mv strategy: %s\n", e.msg());
                    return -1;
                }

                // Reopen the file
                vdebug(7, "Reopening file %s after cp-mv strategy\n", path);
                m_fp = fopen(path, mode);
                if (!m_fp) {
                    vdebug(7, "Failed to reopen file %s: %s\n", path, strerror(errno));
                    return -1;
                }

                // Try to acquire the lock again
                if (fcntl(fileno(m_fp), F_SETLK, &fl) == -1) {
                    vdebug(7, "Failed to acquire lock even after cp-mv strategy: %s\n", strerror(errno));
                    close();
                    return -1;
                }
                vdebug(7, "Successfully acquired lock after cp-mv strategy\n");
            }
        }

        m_eof = false;
        m_virt_pos = m_phys_pos = 0;
        m_sbuf_pos = m_ebuf_pos = 0;

        fseeko(m_fp, 0, SEEK_END);
        m_file_size = ftello(m_fp);
        fseeko(m_fp, 0, SEEK_SET);
        vdebug(7, "File %s opened successfully, size: %lld bytes\n", path, (long long)m_file_size);
        return 0;
    }
    vdebug(7, "Failed to open file %s: %s\n", path, strerror(errno));
    return -1;
}

int BufferedFile::close()
{
	if (m_fp) {
		int retv = fclose(m_fp);
		m_fp = NULL;
		m_eof = true;
		m_phys_pos = -1;
		return retv;
	}
	return 0;
}

int BufferedFile::truncate()
{
    if (m_fp) {
        int retv = ftruncate(fileno(m_fp), m_virt_pos);

        if (!retv) {
            m_phys_pos = -1;
            m_file_size = m_virt_pos;
        }
        return retv;
    }
    return 0;
}
