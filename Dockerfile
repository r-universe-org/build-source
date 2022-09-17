FROM runiverse/base

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh
COPY pdftinytex /root/bin/pdftinytex
COPY dummykey/id_rsa /root/.ssh/id_rsa
COPY dummykey/id_rsa.pub /root/.ssh/id_rsa.pub

RUN chmod 400 /root/.ssh/id_rsa && ssh-keyscan github.com >> ~/.ssh/known_hosts
RUN R -e 'install.packages("remotes");remotes::install_local("/pkg");library(buildtools)'

ENTRYPOINT ["/entrypoint.sh"]
