#!/bin/bash -eu
# run.sh -- Athena run script
# Copyright (C) 2013, 2014  Benjamin Barenblat <bbaren@mit.edu>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the MIT (X11) License as described in the LICENSE file.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

GITROOT=$(git rev-parse --show-toplevel)
DECAFC="$GITROOT/.cabal-sandbox/bin/decafc"

if [ ! -f $DECAFC ]; then
    $GITROOT/build.sh
fi
$DECAFC $@
