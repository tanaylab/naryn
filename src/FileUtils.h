#ifndef FILEUTILS_H_INCLUDED
#define FILEUTILS_H_INCLUDED

#include <functional>

namespace FileUtils {
    // Makes a fast copy of a file while preserving permissions.
    // Throws TGLException on error, TGLException::code contains errno.
    void copy_file(const char *src, const char *tgt);

    // Renames a file or copies it, if the target is located in a different file system.
    // Throws TGLException on error, TGLException::code contains errno.
    void move_file(const char *src, const char *tgt);

    // Replaces tgt in a single step: calls writer() with a staging path in tgt's own directory,
    // then renames that file into place. A concurrent reader therefore sees either the complete
    // previous file or the complete new one, never a partial or absent one.
    // The staging name comes from mkstemp, so no other writer can pick it, including one on
    // another host sharing the directory over NFS. The staging file is removed if writer() or
    // the rename throws, and it inherits tgt's permissions when tgt already exists - rename
    // replaces the target inode, so without this an overwrite would silently reset its mode.
    // Throws TGLException on error, TGLException::code contains errno.
    void atomic_write(const char *tgt, const std::function<void(const char *)> &writer);
}

#endif
