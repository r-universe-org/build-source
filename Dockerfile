FROM runiverse/base

COPY buildtools /pkg
COPY entrypoint.sh /entrypoint.sh
COPY pdftinytex /root/bin/pdftinytex

RUN R -e 'install.packages("remotes");remotes::install_local("/pkg")'

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
