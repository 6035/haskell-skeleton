#!/bin/sh
# make-tarball.sh -- Athena script to build a tarball for project handin
# Copyright (C) 2013 Cam Tenny <cjtenny@mit.edu>

exit
cd $(git rev-parse --show-toplevel)
add -f ghc
cabal clean
rm -f $USER-handin.tar.gz
tar --transform "s,^\./,$USER-handin/," --exclude '*.tar' --exclude '*.tar.gz' --exclude '*.git/*' --exclude '*~' -czf $USER-handin.tar.gz ./*
