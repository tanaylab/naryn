#include <fcntl.h>
#include <string.h>
#include <limits.h>
#include "BufferedFile.h"
#include "TGLException.h"

int64_t BufferedFile::file_size(const char *path)
{
	struct stat st;

	if (::stat(path, &st))
		TGLError("Cannot stat file %s: %s\n", path, strerror(errno));
	return (int64_t)st.st_size;
}

int BufferedFile::open(const char *path, const char *mode, bool lock, bool atomic) {
    close(); // Close existing
    m_real_filename = (string)path;
    m_is_atomic = atomic;

    if (m_is_atomic && (strcmp(mode, "w") == 0 || strcmp(mode, "wb") == 0)) {
        // Create a unique temp file: filename.tmp.PID
        char buf[PATH_MAX];
        snprintf(buf, sizeof(buf), "%s.tmp.%d", path, getpid());
        m_temp_filename = buf;
        m_filename = m_temp_filename; // Parent class uses m_filename
    } else {
        m_filename = m_real_filename;
        m_temp_filename.clear();
    }

    m_fp = fopen(m_filename.c_str(), mode);

	if (m_fp) {
        if (lock && !m_is_atomic) {
            struct flock fl;

            // according to fcntl() manual, lock is automatically released when the file description is closed
            memset(&fl, 0, sizeof(fl));
            fl.l_type = strcmp(mode, "r") ? F_WRLCK : F_RDLCK;
        	while (fcntl(fileno(m_fp), F_SETLKW, &fl) == -1) {
        		if (errno != EINTR) {
        			close();
                    return -1;
                }
        	}
        }

		m_eof = false;
		m_virt_pos = m_phys_pos = 0;
		m_sbuf_pos = m_ebuf_pos = 0;

		fseeko(m_fp, 0, SEEK_END);
		m_file_size = ftello(m_fp);
		fseeko(m_fp, 0, SEEK_SET);
		return 0;
	}
	return -1;
}

int BufferedFile::close()
{
	if (m_fp) {
		int retv = fclose(m_fp);
		m_fp = NULL;
        
        // ATOMIC COMMIT
        if (m_is_atomic && !m_temp_filename.empty() && retv == 0) {
            if (rename(m_temp_filename.c_str(), m_real_filename.c_str()) != 0) {
                // Log error or handle failure
                unlink(m_temp_filename.c_str()); // Cleanup
                return -1;
            }
        }

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
