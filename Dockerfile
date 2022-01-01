FROM runiverse/base

COPY buildtools /pkg
COPY entrypoint.sh /entrypoint.sh
COPY pdftinytex /root/bin/pdftinytex
COPY dummykey/id_rsa /root/.ssh/id_rsa
COPY dummykey/id_rsa.pub /root/.ssh/id_rsa.pub

RUN chmod 400 /root/.ssh/id_rsa
RUN R -e 'install.packages("remotes");remotes::install_local("/pkg")'

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
