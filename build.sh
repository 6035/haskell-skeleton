#!/bin/bash -eu
# build.sh -- Athena build script
# Copyright (C) 2013, 2014  Benjamin Barenblat <bbaren@mit.edu>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the MIT (X11) License as described in the LICENSE file.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

# IMPORTANT:
# if you have changed a file in the base directory or in the `alex/` directory,
# running this build script will not guarantee its recompilation. run the
# command `rm -r dist` before running this script to guarantee that everything
# is recompiled.

declare -r GHC_VERSION=7.8.2
declare -r TOP="$(git rev-parse --show-toplevel)"

declare -r GREP="grep --quiet --no-messages"

function have {
  type "$1" &>/dev/null
}

cd "$TOP"
#eval $(attach -Padd -b -f ghc)
./setup.sh
export PATH="$(realpath .cabal-exec):$PATH"

PATH="$TOP"/.cabal-sandbox/bin:"$PATH"

for package in alex happy; do
  if ! [[ -f "$HOME/.cabal-install/$package" ]]; then
    cabal install $package \
        -j \
        --enable-library-profiling \
        --disable-executable-profiling \
        --enable-optimization \
        --installdir="$HOME/.cabal-install"
  fi
done

cabal install \
    --enable-library-profiling \
    --enable-executable-profiling \
    --alex-options="--ghc --template=\"$TOP/alex\""
#   --happy-options="-i -a -d"
# uncomment the trailing '\' on line 42 and the entirety of line 43 to add
# debug info to happy. the '-i' flag outputs a state diagram of the compiled
# parser into the file `src/Parser.info`, and the '-a -d' flag combo tells
# happy to print state transitions and actions taken during a parse to stderr.
# They are most useful when used together.
