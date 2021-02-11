FROM runiverse/base

COPY pkg /pkg
COPY entrypoint.sh /entrypoint.sh

RUN R -e 'install.packages("remotes");remotes::install_local("/pkg")'

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
