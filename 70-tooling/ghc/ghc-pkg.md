# ghc-pkg

- cmd: `ghc-pkg list`
- packages directory: /usr/lib/ghc/package.conf.d/
- enable package from GHCi: `:set package array`

## Query

```bash
# list regd packages in the global db
ghc-pkg list
# also include user db
ghc-pkg list --user
# list all regd versions (ASC) for [pkg]
ghc-pkg list array
# simplify listing
ghc-pkg list --simple-output

# highest regd version of {pkg-id}
ghc-pkg latest {pkg-id}

# regd desc for {pkg}, reusable as input to `ghc-pkg register`
ghc-pkg describe {pkg}

# extract field {field} of {pkg} desc. Sep multiple fields w comma
ghc-pkg field {pkg} {field}

# list ASC regd pkgs exposing module {module} in the global db
# also accepts: --user, --simple-output
ghc-pkg find-module {module}
```

## Toggle

```bash
# Register pkg using this installed pkg desc. input file as UTF-8
ghc-pkg register {filename | -}

# Unregister the specified package
ghc-pkg unregister {pkg-id}

# Expose the specified package
ghc-pkg expose {pkg-id}

# Hide the specified package
ghc-pkg hide {pkg-id}

# Trust the specified package
ghc-pkg trust {pkg-id}

# Distrust the specified package
ghc-pkg distrust {pkg-id}
```


## Other


`ghc-pkg update {filename | -}`
  Register the package, overwriting any other package with the same name.
  The input file should be encoded in UTF-8.

`ghc-pkg init {path}`
  Create and initialise a package db at {path} dir
  To register pkg in the new db: `register --package-db={path}`
  To use the new db with GHC, use GHC's `-package-db` flag

`ghc-pkg dot`
  Generate a graph of the package dependencies in a form suitable for input for
  the graphviz tools. For example, to generate a PDF of the dependency graph:
  `ghc-pkg dot | tred | dot -Tpdf >pkgs.pdf`

`ghc-pkg dump`
  Dump the registered description for every package, 
  like `ghc-pkg describe '*'`except the output is for tools, not humans.
  The output is always encoded in UTF-8, regardless of the current locale.

`ghc-pkg check`
  Check the consistency of package dependencies and list broken packages.
  Accepts the `--simple-output` flag.

`ghc-pkg recache`
  Regenerate the package db cache.
  Only necessary if pkg added to db by dropping a file into db dir.
  By default, global db is recached; to recache `--user`, `--package-db`.


## Details

Substring matching is supported for {module} in *find-module* and for {pkg} in *list*, *describe*, and *field*, where a `*` indicates open substring ends (`prefix*`, `*suffix`, `*infix*`). Use `--ipid` to match against the installed package ID instead.

When asked to modify a db, when using commands
- register, unregister
- hide, expose
- update
- check
ghc-pkg modifies the global db by default.
Specify `--user` to act on user db.
Specify `--package-db` to act on another db entirely.
When multiple of these options are given, the rightmost one is used as the db to act upon.

Commands that query the package db
- list
- latest
- describe
- field
- tree
operate on the list of dbs specified by the flags:
* --user
* --global
* --package-db

If none of these flags are given, the default is `--global --user`


## Optional flags

`--user`    use the current user's package db
`--global`  use the global package db

--package-db=FILE/DIR, -f FILE/DIR    use the specified pkgdb
--global-package-db=DIR               dir to global pkgdb
--no-user-package-db                  never read the user pkgdb
--user-package-db=DIR                 dir to user pkgdb (instead of default)

`--enable-multi-instance`
  allow registering multiple instances of the same package version
`--expand-env-vars`  
expand environment variables (${name}-style) in input package descriptions
`--expand-pkgroot`
  expand ${pkgroot}-relative paths to absolute in output package descriptions
`--no-expand-pkgroot`
  preserve ${pkgroot}-relative paths in output package descriptions
`--ipid, --unit-id`
  interpret package arguments as unit IDs (e.g. installed package IDs)

--force           ignore missing dependencies, directories, and libraries
--force-files     ignore missing directories and libraries only
--simple-output   print output in easy-to-parse format for some commands
--names-only      print names not versions; only with `list --simple-output`
--ignore-case     ignore case for substring matching

--verbose[=LEVEL], -v[LEVEL]   Verbosity level (0-2, default 1)

`-?`, `--help`        display this help and exit
`-V`, `--version`     output version information and exit


DEPRECATED:
--no-user-package-conf    never read the user package db
--package-conf=FILE/DIR   use the specified package db
