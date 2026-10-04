#!/bin/bash -l
set -eo pipefail

# Rebuilds $SOURCEPKG (a source tarball produced by 'R CMD build') so that
# commonly-requested files come first: top-level files, extra/, inst/doc/.
# This lets the frontend extract them by only reading the start of large
# tarballs, see:
# https://github.com/r-universe-org/frontend/commit/4d82d75ea338f29ea828e3c0132449b79986b827
#
# Usage: assemble-tarball.sh <PACKAGE> <SOURCEPKG>
# Expects "outputs/$PACKAGE" (generated readme/manual/citation/metadata files
# under extra/) to exist in the current directory, and merges it in.

PACKAGE="$1"
SOURCEPKG="$2"

gunzip "$SOURCEPKG"
TARFILE="${SOURCEPKG%.gz}"
rm -Rf pkgstage
mkdir pkgstage
tar xf "$TARFILE" -C pkgstage
cp -R "outputs/$PACKAGE/." "pkgstage/$PACKAGE/"
(
  cd pkgstage
  find "$PACKAGE" -maxdepth 1 \( -type f -o -type l \) | sort > ../priority.txt
  { find "$PACKAGE/extra" \( -type f -o -type l \) 2>/dev/null || true
    find "$PACKAGE/inst/doc" \( -type f -o -type l \) 2>/dev/null || true
  } | sort >> ../priority.txt
  find "$PACKAGE" \( -type f -o -type l \) | sort > ../allfiles.txt
  grep -vFxf ../priority.txt ../allfiles.txt > ../rest.txt || true
  cat ../priority.txt ../rest.txt > ../filelist.txt
  tar -cvf "../$TARFILE" -T ../filelist.txt
)
rm -Rf pkgstage priority.txt allfiles.txt rest.txt filelist.txt
gzip "$TARFILE"
