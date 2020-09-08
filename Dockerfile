FROM runiverse/base

RUN \
	R -e 'install.packages(c("remotes", "maketools"))'

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
