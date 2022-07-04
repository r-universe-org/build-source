# Action: build-source

This action builds an R source package tarball from a git url and collects metadata about the package. It part of the [package build workflow](https://github.com/r-universe-org/workflows/blob/master/build.yml) that runs for each package update. The action automatically takes care of dependencies.

You can test it locally like this:

```
docker run -it ghcr.io/r-universe-org/build-source "https://github.com/jeroen/jsonlite"
```

You can also pass a custom branch/ref and subdir where to find an R package:

```
docker run -it ghcr.io/r-universe-org/build-source "https://github.com/duckdb/duckdb" "master" "tools/rpkg"
```

