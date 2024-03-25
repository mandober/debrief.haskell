# INCLUDE pragma

- name: INCLUDE
- type: file-header pragma
- tag: `{-# INCLUDE file [, file] #-}`
- status: __DEPRECATED__

The `INCLUDE` used to be necessary for specifying header files to be included when using the FFI and compiling via C.

It is no longer required for GHC, but it is accepted (and ignored) for compatibility with other compilers.
