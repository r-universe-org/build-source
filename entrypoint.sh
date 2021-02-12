#!/bin/bash -l
set -e
echo "Building ${1} in ${PWD}"
echo "CommitRef: ${2}"
echo "Subdir: ${3}"

# Setup build environment
if [ "${R_LIBS_USER}" ]; then mkdir -p $R_LIBS_USER; fi

# Get the package dir
REPO=$(basename $1)

# Clone, and checkout the revision if any
# Removed --depth 1 because we want to read vignette c/m times
git clone "$1" "${REPO}"
if [ "${2}" ]; then
( cd ${REPO}; git fetch origin "$2"; git reset --hard "$2" )
fi

# Subdirectory containing the R package
PKGDIR="${REPO}"
if [ "${3}" ]; then
PKGDIR="${PKGDIR}/${3}"
fi

COMMIT_TIMESTAMP="$(git --git-dir=${REPO}/.git log -1 --format=%ct)"
DISTRO="$(lsb_release -sc)"
PACKAGE=$(grep '^Package:' "${PKGDIR}/DESCRIPTION" | sed 's/^Package://')
VERSION=$(grep '^Version:' "${PKGDIR}/DESCRIPTION" | sed 's/^Version://')
PACKAGE=$(echo -n "${PACKAGE//[[:space:]]/}")
VERSION=$(echo -n "${VERSION//[[:space:]]/}")
PKG_VERSION="${PACKAGE}_${VERSION}"
SOURCEPKG="${PKG_VERSION}.tar.gz"
BINARYPKG="${PKG_VERSION}_R_x86_64-pc-linux-gnu.tar.gz"

# Get dependencies
Rscript --no-init-file -e "setwd('$PKGDIR'); install.packages(remotes::local_package_deps(dependencies=TRUE))"

# Delete latex vignettes for now (latex is to heavy for github actions)
rm -f ${PKGDIR}/vignettes/*.Rnw

# Build source package. Try vignettes, but build without otherwise.
# R is weird like that, it should be possible to build the package even if there is a documentation bug.
#mv ${REPO}/.git tmpgit
R --no-init-file CMD build ${PKGDIR} --no-manual ${BUILD_ARGS} || R --no-init-file CMD build ${PKGDIR} --no-manual --no-build-vignettes ${BUILD_ARGS}
#mv tmpgit ${REPO}/.git

# Set output values
echo ::set-output name=DISTRO::$DISTRO
echo ::set-output name=PACKAGE::$PACKAGE
echo ::set-output name=VERSION::$VERSION
echo ::set-output name=SOURCEPKG::$SOURCEPKG
echo ::set-output name=COMMIT_TIMESTAMP::$COMMIT_TIMESTAMP

# Confirm that package can be installed on Linux
# For now we don't do a full build to speed up building of subsequent Win/Mac binaries
test -f "$SOURCEPKG"
R CMD INSTALL "$SOURCEPKG"
SYSDEPS=$(Rscript -e "cat(maketools::package_sysdeps_string('$PACKAGE'))")
echo ::set-output name=SYSDEPS::$SYSDEPS

# Check for vignettes
VIGNETTES=$(Rscript -e "cat(buildtools::vignettes_base64('$PKGDIR','$PACKAGE'))")
echo ::set-output name=VIGNETTES::$VIGNETTES

# Check for a package logo
PKGLOGO=$(Rscript -e "cat(buildtools::find_logo('$PKGDIR'))")
if [ "$PKGLOGO" ]; then
echo ::set-output name=PKGLOGO::$PKGLOGO
fi

### Test if Java is needed to load
### TODO: this seems to fail builds?
#RJAVA=$(R -e "library('$PACKAGE'); sessionInfo()" | grep "rJava")
#if [ "$RJAVA" ]; then
#echo ::set-output name=NEED_RJAVA::true
#fi

echo "Build complete!"
