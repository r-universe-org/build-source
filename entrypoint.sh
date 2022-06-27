#!/bin/bash -l
set -e
echo "Building ${1} in ${PWD}"
echo "CommitRef: ${2}"
echo "Subdir: ${3}"
echo "Branch: ${4}"
echo "Articles: ${5}"

# Setup build environment
if [ "${R_LIBS_USER}" ]; then mkdir -p $R_LIBS_USER; fi

# Get the package dir
URL="$1"
REPO=$(basename $URL)

# Clone, and checkout the revision if any
# Removed --depth 1 because we want to read vignette c/m times
echo "::group::Cloning R package repository"
git clone --recurse-submodules "$URL" "${REPO}"
if [ "${2}" ]; then
( cd ${REPO}; git fetch origin "$2"; git reset --hard "$2" )
fi
echo "::endgroup::"

# Subdirectory containing the R package
PKGDIR="${REPO}"
if [ "${3}" ]; then
SUBDIR="$3"
PKGDIR="${PKGDIR}/${SUBDIR}"
fi

# Get system dependencies
echo "::group::Installing system dependencies"
Rscript --no-init-file -e "buildtools::install_sysdeps('$PKGDIR')"
echo "::endgroup::"

# Experimental: support pkgs like rJava
if test -f "$PKGDIR/.prepare"; then
  echo "Trying to run $PKGDIR/.prepare"
  (cd $PKGDIR; sh .prepare) || true
fi
if [ "$REPO" = "arrow" ]; then
  DATE=$(date -d yesterday +%Y%m%d)
  (cd $PKGDIR; sed -i "s/.9000$/.$DATE/" DESCRIPTION)
  (cd $PKGDIR; make sync-cpp; cp -f ../dev/tasks/homebrew-formulae/autobrew/apache-arrow.rb tools/apache-arrow.rb)
fi

DISTRO="$(lsb_release -sc)"
PACKAGE=$(grep '^Package:' "${PKGDIR}/DESCRIPTION" | sed 's/^Package://')
VERSION=$(grep '^Version:' "${PKGDIR}/DESCRIPTION" | sed 's/^Version://')
PACKAGE=$(echo -n "${PACKAGE//[[:space:]]/}")
VERSION=$(echo -n "${VERSION//[[:space:]]/}")
PKG_VERSION="${PACKAGE}_${VERSION}"
SOURCEPKG="${PKG_VERSION}.tar.gz"
BINARYPKG="${PKG_VERSION}_R_x86_64-pc-linux-gnu.tar.gz"

# Export some outputs (even upon failure)
COMMIT_TIMESTAMP="$(git --git-dir=${REPO}/.git log -1 --format=%ct)"
echo ::set-output name=DISTRO::$DISTRO
echo ::set-output name=PACKAGE::$PACKAGE
echo ::set-output name=VERSION::$VERSION
echo ::set-output name=COMMIT_TIMESTAMP::$COMMIT_TIMESTAMP

# Get maintainer details
MAINTAINERINFO=$(Rscript -e "cat(buildtools::maintainer_info_base64('${PKGDIR}'))")
echo ::set-output name=MAINTAINERINFO::$MAINTAINERINFO

# Get commit metadata
COMMITINFO=$(Rscript -e "cat(buildtools::commit_info_base64('$REPO'))")
echo ::set-output name=COMMITINFO::$COMMITINFO

# Get commit metadata
GITSTATS=$(Rscript -e "cat(buildtools::get_gitstats_base64('$REPO','$PKGDIR','$URL'))")
echo ::set-output name=GITSTATS::$GITSTATS

# Look for a package logo
PKGLOGO=$(Rscript -e "cat(buildtools::find_logo('$PKGDIR', '$URL', '$SUBDIR'))")
if [ "$PKGLOGO" ]; then
echo ::set-output name=PKGLOGO::$PKGLOGO
fi

# DEBUGGING
echo "::group::Show contents of $MY_UNIVERSE"
R -e "try(available.packages(repos = '${MY_UNIVERSE}')[,'Version',drop=FALSE])"
echo "::endgroup::"

# Get dependencies
echo "::group::Installing R dependencies"
Rscript --no-init-file -e "buildtools::install_dependencies('$PKGDIR')"
echo "::endgroup::"

# Delete latex vignettes for now (latex is to heavy for github actions)
#rm -f ${PKGDIR}/vignettes/*.Rnw

# Do not build articles (vignettes) for remotes
BUILD_ARGS="--no-resave-data"
if [ "${5}" == "false" ]; then
  BUILD_ARGS="${BUILD_ARGS} --no-build-vignettes"
  rm -Rf ${PKGDIR}/vignettes
fi

# Override rmarkdown engine
#if ls ${PKGDIR}/vignettes/*; then
#echo "Found vignettes..."
echo "buildtools::replace_rmarkdown_engine()" > /tmp/vignettehack.R
#fi

# Replace or add "Repository:" in DESCRIPTION
if [ "${MY_UNIVERSE}" ]; then
sed '/^[[:space:]]*$/d' -i "${PKGDIR}/DESCRIPTION" # deletes empty lines at end of DESCRIPTION
sed -n -e '/^Repository:/!p' -e "\$aRepository: ${MY_UNIVERSE}" -i "${PKGDIR}/DESCRIPTION"
echo "RemoteUrl: ${1}" >> "${PKGDIR}/DESCRIPTION"
echo "RemoteRef: ${4:-HEAD}" >> "${PKGDIR}/DESCRIPTION"
echo "RemoteSha: ${2}" >> "${PKGDIR}/DESCRIPTION"
fi

# Build source package. Try vignettes, but build without otherwise.
# R is weird like that, it should be possible to build the package even if there is a documentation bug.
#mv ${REPO}/.git tmpgit
echo "::group::R CMD build"
if ! R_TEXI2DVICMD=emulation PDFLATEX=pdftinytex R_TESTS="/tmp/vignettehack.R" R --no-init-file CMD build ${PKGDIR} --no-manual ${BUILD_ARGS} 1> >(tee stderr_build.log); then
VIGNETTE_FAILURE=1
echo "::endgroup::"
echo "::group::R CMD build (trying without vignettes)"
echo "---- ERROR: failed to run: R CMD build -----"
echo "Trying to build source package without vignettes...."
R --no-init-file CMD build ${PKGDIR} --no-manual --no-build-vignettes ${BUILD_ARGS}
fi
echo "::endgroup::"
#mv tmpgit ${REPO}/.git

# Test for filesize enforced by pkg server
test -f "$SOURCEPKG"
FILESIZE=$(stat -c%s "$SOURCEPKG")
if (( FILESIZE >  104857600 )); then
echo "File $SOURCEPKG is more than 100MB. This is currently not allowed."
exit 1
fi

# Support Windows-only packages (but not articles which require installation)
OSTYPE=$(Rscript -e "cat(buildtools::get_ostype('$PKGDIR'))")
if [ "$OSTYPE" = "windows" ]; then
echo "Skipping install/runtime checks for windows-only package"
echo ::set-output name=SOURCEPKG::$SOURCEPKG
exit 0
fi

# Confirm that package can be installed on Linux
# For now we don't do a full check to speed up building of subsequent Win/Mac binaries
echo "::group::Test if package can be installed"
R CMD INSTALL "$SOURCEPKG"
echo "::endgroup::"

# Upon successful install, set output value
echo ::set-output name=SOURCEPKG::$SOURCEPKG

# Lookup system dependencies
SYSDEPS=$(Rscript -e "cat(buildtools::sysdeps_base64('$PACKAGE'))")
echo ::set-output name=SYSDEPS::$SYSDEPS

# Get vignette metadata
VIGNETTES=$(Rscript -e "cat(buildtools::vignettes_base64('$REPO','$PACKAGE','$SUBDIR'))")
echo ::set-output name=VIGNETTES::$VIGNETTES

# Build and insert pdf manual into the tar.gz
echo "::group::Build readme and/or pdf reference manual"
mkdir -p outputs/$PACKAGE
R CMD Rd2pdf --no-preview --title="Package: $PACKAGE (via r-universe)" --output=outputs/$PACKAGE/manual.pdf "$PKGDIR" 2> stderr_manual.txt || MANUAL_FAILURE=1
if [ "$MANUAL_FAILURE" ]; then
cat stderr_manual.txt
fi

# Find readme URL
README=$(Rscript -e "cat(buildtools::find_readme_url('$URL', '$SUBDIR'))")
if [ "$README" ]; then
echo ::set-output name=README::$README
Rscript -e "cat(buildtools::render_readme('$README', 'outputs/$PACKAGE'))" 2> stderr_readme.txt || README_FAILURE=1
else
echo "No readme file found"
fi
echo "::endgroup::"

# Generate CITATION.cff
# NB: CITATION file can contain a script and fail, for example:
# https://github.com/girke-lab/fmcsR/blob/master/inst/CITATION
echo "::group::Generate citation files"
Rscript -e "buildtools::generate_citation_files('$PKGDIR', 'outputs/$PACKAGE')" || CITATION_FAILURE=1
echo "::endgroup::"

# if outputs has any files, add them to tarball
echo "::group::Adding extra files to tarball"
gunzip "$SOURCEPKG"
tar rfv ${SOURCEPKG%.gz} -C outputs "$PACKAGE"
gzip ${SOURCEPKG%.gz}
Rscript -e "buildtools::list_assets('outputs/$PACKAGE')"
echo "::endgroup::"

# TODO: can we explicitly set action status/outcome in GHA?
echo "Build complete!"
if [ "$VIGNETTE_FAILURE" ]; then
echo "Installation OK but failed to build vignettes:"
cat stderr_build.log
exit 1
elif [ "$MANUAL_FAILURE" ]; then
echo "Installation OK but failed to build PDF manual:"
cat stderr_manual.txt
exit 1
elif [ "$README_FAILURE" ]; then
echo "Installation OK but failed to render README:"
cat stderr_readme.txt
exit 1
elif [ "$CITATION_FAILURE" ]; then
echo "Package OK but problem generating citation files (see above)"
exit 1
else
exit 0
fi
