#!/usr/bin/env bash

declare -r ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"

if [[ -f .cabal-exec/cabal ]]; then
  echo cabal executable exists >&2
  exit
fi

mkdir .cabal-exec/
cd    .cabal-exec/

if [[ $(uname) -eq "Darwin" ]]; then
  arch=x86_64-apple-darwin17.7.0
else
    arch=i386-unknown-linux
fi
wget -O - "https://downloads.haskell.org/~cabal/cabal-install-latest/cabal-install-3.0.0.0-${arch}.tar.xz" |
  xzcat |
  tar -xf -
