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

declare -r GHC_VERSION=7.8.2
declare -r TOP="$(git rev-parse --show-toplevel)"

declare -r GREP="grep --quiet --no-messages"

function have {
    type "$1" &>/dev/null
}

cd "$TOP"
eval $(attach -Padd -b -f ghc)

if [[ ! -d .cabal-sandbox ]]; then
    # No Cabal sandbox yet.  Set one up.
    cabal sandbox init
fi

PATH="$TOP"/.cabal-sandbox/bin:"$PATH"

for package in alex happy; do
    if ! have $package; then
	cabal install $package \
	    -j \
	    --enable-library-profiling --disable-executable-profiling \
	    --enable-optimization
    fi
done

cabal install \
    --enable-library-profiling --enable-executable-profiling \
    --alex-options="--ghc --template=\"$TOP/alex\""
