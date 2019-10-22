stylechecker: A tool to check the formatting style of Curry programs
====================================================================

This package contains the implementation of a tool to check
the formatting style of a *valid* Curry program according
to the [Curry Style Guide](http://www.informatik.uni-kiel.de/~pakcs/CurryStyleGuide.html).
The style aspects checked by this tool contain formatting (indentation) rules,
avoiding long lines, tailing spaces, etc.

Installation
------------

If you have downloaded this package, execute

    > cypm install

to install the executable `curry-stylecheck` in the package binary
directory (`HOME/.cpm/bin`).

Usage
-----

The style checker uses a default configuration, but you can define
your own configuration by putting standard values in a file
`.currystylecheckrc` (see below for its format).
This file must be in the current working directory
or in the user's home directory. A file found in the current working
directory will be preferred.

To check a program, go into the directory containing the program
and execute

    > curry-stylecheck <module_names>

If you want to check source programs which are part of a package,
execute

    > cypm exec curry-stylecheck <module_names>

The possible options for this command are:


Set the verbosity level:

    -v <level>

with the following values:

* 0: quiet (show only style warnings with their positions)
* 1: default (show module name, source code regions, and hints)
* 2: verbose (show also info and warning messages)
* 3: debug (show also all options)

Set the output format:

    -o <type>

Possible value are

* TEXT (default): human-readable messages
* JSON: to be used by a linter

Ignore or add default checks:

    -i <check>
    -a <check>

With this option one can ignore or add individual checks independent
of the default configuration.

On can set these options also in the file `.currystylecheckrc`.
There, a check is turned on if its values is `1` and it is turned
off if its values is `0`.
The following code shows a possible configuration (the default
configuration can be found in `config/currystylecheckrc`):

    -- configuration for style-check tool

    -- maximal length allowed for a line
    maxLineLength = 80

    -- output type
    oType = JSON

    -- level of verbosity,
    -- 0 quiet (warning only)
    -- 1 (default, show: modul, hints, code)
    -- 2 (verbose, show infos and warnings)
    -- 3 show all options
    verbosity = 1

    -- output
    hints = 1
    code  = 1

    -- src checks
    lineLength          = 0
    tabs                = 1
    trailingSpaces      = 1
    whiteSpaces         = 1

    -- indent and alignment
    ifThenElse    = 1
    case          = 1
    do            = 1
    let           = 1
    guard         = 1
    functionRhs   = 1
    moduleHeader  = 1
    imports       = 1
    data          = 1
    list          = 1
    deriving      = 1
    class         = 1
    instance      = 1

    -- top level declarations
    signatures    = 0
    blankLines    = 0

    -- superfluous/bad code

    ----boolean
    equalstrue        = 1
    thentrueelsefalse = 1

    ----ord
    notEqual          = 1
    notOrd            = 1

    ----list
    equalsEmptyList   = 1
    andOr             = 1

    ----func
    identFunc         = 1
    constFunc         = 1
