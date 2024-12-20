FROM ghcr.io/r-universe-org/base-image

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh
COPY pdftinytex /root/bin/pdftinytex
COPY tinyxelatex /root/bin/tinyxelatex
COPY shims /shims

COPY dummykey/key.zip /key.zip

# Workaround to allow building some old pkgs.
# This flag is added by Debian via 'dpkg-buildflags' but not standard
RUN sed -i 's/-Werror=format-security//g' $(R RHOME)/etc/Makeconf

RUN unzip -P dummy /key.zip -d /root/.ssh/ &&\
  chmod 400 /root/.ssh/id_rsa &&\
  ssh-keyscan github.com >> ~/.ssh/known_hosts

# Use devel-pak
RUN R -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/devel/source/linux-gnu/x86_64")'
RUN R -e 'install.packages("remotes");remotes::install_local("/pkg");library(buildtools)'

RUN echo "options(error = rlang::entrace)" >> /etc/R/Rprofile.site

ENTRYPOINT ["/entrypoint.sh"]
