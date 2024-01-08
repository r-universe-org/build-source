#!/bin/bash -l
set -e
echo "Building ${1} in ${PWD}"
echo "CommitRef: ${2}"
echo "Subdir: ${3}"
echo "Branch: ${4}"
echo "Articles: ${5}"

# Setup build environment (expand ~ to $HOME)
export R_LIBS_USER="${PWD}/pkglib"
mkdir -p $R_LIBS_USER
R -e ".libPaths()"

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
elif [ "$REPO" = "arrow" ]; then
  export ARROW_R_DEV=1=TRUE
#  DATE=$(date -d yesterday +%Y%m%d)
#  (cd $PKGDIR; sed -i "s/.9000$/.$DATE/" DESCRIPTION)
  (cd $PKGDIR; make sync-cpp || true)
#elif [ "$REPO" = "duckdb" ]; then
#  (cd $PKGDIR; ./configure || true)
fi

# Normalize DESCRIPTION file
DESCRIPTION="${PKGDIR}/DESCRIPTION"
R -e "buildtools:::normalize_description('${DESCRIPTION}')"

DISTRO="$(lsb_release -sc)"
PACKAGE=$(Rscript -e "cat(as.data.frame(read.dcf('${DESCRIPTION}'))\$Package)")
VERSION=$(Rscript -e "cat(as.data.frame(read.dcf('${DESCRIPTION}'))\$Version)")
OSTYPE=$(Rscript -e "cat(as.data.frame(read.dcf('${DESCRIPTION}'))\$OS_type)")
PKG_VERSION="${PACKAGE}_${VERSION}"
SOURCEPKG="${PKG_VERSION}.tar.gz"

# Export some outputs (even upon failure)
echo "DISTRO=$DISTRO" >> $GITHUB_OUTPUT
echo "PACKAGE=$PACKAGE" >> $GITHUB_OUTPUT
echo "VERSION=$VERSION" >> $GITHUB_OUTPUT
echo "OSTYPE=$OSTYPE" >> $GITHUB_OUTPUT

# Get maintainer details
MAINTAINERINFO=$(Rscript -e "cat(buildtools::maintainer_info_base64('${PKGDIR}'))")
echo "MAINTAINERINFO=$MAINTAINERINFO" >> $GITHUB_OUTPUT

# Get commit metadata
COMMITINFO=$(Rscript -e "cat(buildtools::commit_info_base64('$REPO'))")
echo "COMMITINFO=$COMMITINFO" >> $GITHUB_OUTPUT

# DEBUGGING
if [ "${MY_UNIVERSE}" ]; then
echo "::group::Show contents of $MY_UNIVERSE"
R -e "try(available.packages(repos = '${MY_UNIVERSE}')[,'Version',drop=FALSE])"
echo "::endgroup::"
else
echo "MY_UNIVERSE is not set"
fi

# Get dependencies
echo "::group::Installing R dependencies"
Rscript --no-init-file -e "buildtools::install_dependencies('$PKGDIR')"
echo "::endgroup::"

# These are set in install_dependencies() above
if [ -f "/NEED_RJAVA" ]; then
  echo "NEED_RJAVA=true" >> $GITHUB_OUTPUT
fi
if [ -f "/NEED_JAGS" ]; then
  echo "NEED_JAGS=true" >> $GITHUB_OUTPUT
fi
if [ -f "/NEED_CMDSTAN" ]; then
  echo "NEED_CMDSTAN=true" >> $GITHUB_OUTPUT
fi

# Delete latex vignettes for now (latex is to heavy for github actions)
#rm -f ${PKGDIR}/vignettes/*.Rnw

# Do not build articles (vignettes) for remotes
#BUILD_ARGS="--resave-data"
if [ "${5}" == "false" ]; then
  BUILD_ARGS="${BUILD_ARGS} --no-build-vignettes"
  rm -Rf ${PKGDIR}/vignettes
fi

# Default LazyData to true
#if [ -d "${PKGDIR}/data" ]; then
  #TODO: maybe also check for BuildResaveData=no (e.g. diptest/VLMC)
  #if ! grep '^LazyData:' "${PKGDIR}/DESCRIPTION"; then
  #echo "NOTE: setting LazyData: true in DESCRIPTION"
  #echo "LazyData: true" >> "${PKGDIR}/DESCRIPTION"
  #fi
  # Preinstall a copy to support --resave-data
#fi

if ls ${PKGDIR}/data/*.R 2>/dev/null; then
  echo "Found R files under data. Preinstalling..."
  R CMD INSTALL ${PKGDIR} --clean
fi

# Hack for NEWS.md parser in base R
if [ -f "${PKGDIR}/NEWS.md" ]; then
sed -i "1,50s/(development version)/${VERSION}/" "${PKGDIR}/NEWS.md" || true
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
# We set a timeout such that the workflow can post a 'failure' instead of timing out in CI.
#mv ${REPO}/.git tmpgit
echo "::group::R CMD build"
if ! R_TEXI2DVICMD=emulation PDFLATEX=pdftinytex R_TESTS="/tmp/vignettehack.R" timeout 3600 R --no-init-file CMD build ${PKGDIR} --no-manual ${BUILD_ARGS} 1> >(tee stderr_build.log); then
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
echo "Fake install for windows-only package"
INSTALLARGS="--fake"
else
INSTALLARGS="--build"
LINUXBINARY="${PKG_VERSION}_R_x86_64-pc-linux-gnu.tar.gz"
fi

# Confirm that package can be installed on Linux
# For now we don't do a full check to speed up building of subsequent Win/Mac binaries
echo "::group::install package and generate html docs"
#export _R_HELP_LINKS_TO_TOPICS_=FALSE
PATH="/shims:$PATH" R CMD INSTALL "$SOURCEPKG" --html $INSTALLARGS
echo "::endgroup::"

# Build and insert pdf manual into the tar.gz
echo "::group::Build readme and manuals"
mkdir -p outputs/$PACKAGE

# Generate PDF manual
R CMD Rd2pdf --no-preview --title="Package: $PACKAGE (via r-universe)" --output=outputs/$PACKAGE/manual.pdf "$PKGDIR" 2> stderr_manual.txt || MANUAL_FAILURE=1
if [ "$MANUAL_FAILURE" ]; then
cat stderr_manual.txt
fi

# Test if package needs compilation (R simply checks if there is a 'src' dir)
NEEDS_COMPILATION=$(Rscript -e "cat(buildtools::needs_compilation('${PACKAGE}'))")
if [ "$NEEDS_COMPILATION" == "yes" ]; then
echo "NEEDS_COMPILATION=yes" >> $GITHUB_OUTPUT
fi

# Find readme URL
export README_URL=$(Rscript -e "cat(buildtools::find_readme_url('$URL', '$SUBDIR'))")
if [ "$README_URL" ]; then
Rscript -e "cat(buildtools::render_readme('$README_URL', 'outputs/$PACKAGE'))" 2> stderr_readme.txt || README_FAILURE=1
else
echo "No readme file found"
fi
echo "::endgroup::"

# Generate CITATION.cff
# NB: CITATION file can contain a script and fail, for example:
# https://github.com/girke-lab/fmcsR/blob/master/inst/CITATION
echo "::group::Render NEWS, citation files, html-manual, metadata"
Rscript -e "buildtools::generate_citation_files('$PKGDIR', 'outputs/$PACKAGE')" || CITATION_FAILURE=1
Rscript -e "buildtools::render_news_files('$PACKAGE', 'outputs/$PACKAGE', '$URL')" || NEWS_FAILURE=1
Rscript -e "buildtools::render_html_manual('$PACKAGE', 'outputs/$PACKAGE/extra')"
Rscript -e "buildtools::generate_metadata_files('$PACKAGE', '$REPO', '$SUBDIR', 'outputs/$PACKAGE', '$PKGDIR', '$URL')"
echo "::endgroup::"

# if outputs has any files, add them to tarball
echo "::group::Adding extra files to tarball"
gunzip "$SOURCEPKG"
tar rfv ${SOURCEPKG%.gz} -C outputs "$PACKAGE"
gzip ${SOURCEPKG%.gz}
echo "::endgroup::"

# Upon successful install, set output value
echo "SOURCEPKG=$SOURCEPKG" >> $GITHUB_OUTPUT

# If a binary package was created rename and output
if [ "$LINUXBINARY" ] && [ -f "$LINUXBINARY" ]; then
BINARYPKG="${PKG_VERSION}-${DISTRO}.tar.gz"
mv "$LINUXBINARY" "$BINARYPKG"
echo "BINARYPKG=$BINARYPKG" >> $GITHUB_OUTPUT
fi

# Check for some traps at install time
if [ -f "/NEED_FORTRAN" ]; then
  echo "NEED_FORTRAN=true" >> $GITHUB_OUTPUT
fi
if [ -f "/NEED_CARGO" ]; then
  echo "NEED_CARGO=true" >> $GITHUB_OUTPUT
fi

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
