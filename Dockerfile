FROM runiverse/base

RUN \
	R -e 'install.packages("remotes");remotes::install_github("jeroen/maketools")'

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
