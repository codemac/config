This package teaches Magit to use functions provided by the
`libegit2' module to perform certain tasks.  That module used the
Libgit2 implementation of the Git core methods and is implemented
in the `libgit' package.

The hope is that using a C module instead of calling out to `git'
all the time increases performance; especially on Windows where
starting a process is unreasonably slow.

This package is still experimental and not many functions have been
reimplemented to use `libgit' yet.
