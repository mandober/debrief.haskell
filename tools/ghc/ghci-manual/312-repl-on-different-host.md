# 3.12. Running the interpreter on a different host

312-repl-on-different-host.md

Running the interpreter on a different host {#external-interpreter-proxy}
-------------------------------------------

When using the flag `-fexternal-interpreter` 
role="  GHC will spawn and communicate with the separate
process using pipes. There are scenarios (e.g. when cross compiling)
where it is favourable to have the communication happen over the
network. GHC provides two utilities for this, which can be found in the
`utils` directory.

-   `remote-iserv` needs to be built with the cross compiler to be
    executed on the remote host. Or in the case of using it on the same
    host the stage2 compiler will do as well.
-   `iserv-proxy` needs to be built on the build machine by the build
    compiler.

After starting `remote-iserv ⟨tmp_dir⟩ ⟨port⟩` on the target and
providing it with a temporary folder (where it will copy the necessary
libraries to load to) and port it will listen for the proxy to connect.

Providing `-pgmi ⟨/path/to/iserv-proxy⟩ <-pgmi ⟨cmd⟩>` 
role="  and
`-opti ⟨slave-ip⟩ -opti ⟨slave-port⟩ [-opti -v] <-opti ⟨option⟩>` 
role="  in addition to
`-fexternal-interpreter`  will then
make ghc go through the proxy instead.

There are some limitations when using this. File and process IO will be
executed on the target. As such packages like `git-embed`, `file-embed`
and others might not behave as expected if the target and host do not
share the same filesystem.
