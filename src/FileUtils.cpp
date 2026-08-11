#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <string>
#if defined(__APPLE__)
    #include <copyfile.h>
#else
    #include <sys/sendfile.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>

#include "FileUtils.h"
#include "TGLException.h"

struct FD {
    int fd{-1};
    ~FD() {
        if (fd != -1)
            close(fd);
    }
};

void FileUtils::copy_file(const char *src, const char *tgt) {
    FD srcfd;
    FD tgtfd;
    struct stat srcstat;

    if ((srcfd.fd = open(src, O_RDONLY, 0)) == -1)
        TGLError(errno, "Error opening file %s for reading: %s", src, strerror(errno));
    if (fstat(srcfd.fd, &srcstat) == -1)
        TGLError(errno, "Error trying to stat file %s: %s", src, strerror(errno));
    if ((tgtfd.fd = creat(tgt, srcstat.st_mode)) == -1)
        TGLError(errno, "Error opening file %s for writing: %s", tgt, strerror(errno));
#if defined(__APPLE__)
    copyfile_state_t s;
    s = copyfile_state_alloc();
    if (fcopyfile(tgtfd.fd, srcfd.fd, s, COPYFILE_ALL) == -1)
        TGLError(errno, "Error copying file %s to %s: %s", src, tgt, strerror(errno));
    copyfile_state_free(s);
#else
    if (sendfile(tgtfd.fd, srcfd.fd, NULL, srcstat.st_size) == -1)
        TGLError(errno, "Error copying file %s to %s: %s\n", src, tgt, strerror(errno));
#endif
}

void FileUtils::move_file(const char *src, const char *tgt) {
    if (rename(src, tgt) == -1) {
        if (errno == EXDEV) {
            FileUtils::copy_file(src, tgt);
            if (unlink(src) == -1) {
                auto olderrno = errno;
                unlink(tgt);
                TGLError(olderrno, "Error removing file %s: %s", src, strerror(olderrno));
            }
        } else
            TGLError(errno, "Error moving file %s to %s: %s\n", src, tgt);
    }
}

void FileUtils::atomic_write(const char *tgt, const std::function<void(const char *)> &writer) {
    // The staging file sits next to the target, so the rename below stays inside one filesystem.
    std::string stage = std::string(tgt) + ".tmp.XXXXXX";
    int fd = mkstemp(&stage[0]);

    if (fd == -1)
        TGLError(errno, "Error creating a staging file for %s: %s", tgt, strerror(errno));
    close(fd);

    // mkstemp creates the file 0600 and writer() only truncates it, so the mode has to be set
    // here: the target's own mode if it has one, and whatever the umask calls for otherwise.
    struct stat tgtstat;
    mode_t mode;

    if (stat(tgt, &tgtstat) != -1)
        mode = tgtstat.st_mode & 07777;
    else {
        mode_t mask = umask(0);
        umask(mask);
        mode = 0666 & ~mask;
    }

    if (chmod(stage.c_str(), mode) == -1) {
        auto olderrno = errno;
        unlink(stage.c_str());
        TGLError(olderrno, "Error setting permissions of %s: %s", stage.c_str(), strerror(olderrno));
    }

    try {
        writer(stage.c_str());
        FileUtils::move_file(stage.c_str(), tgt);
    } catch (...) {
        unlink(stage.c_str());
        throw;
    }
}
