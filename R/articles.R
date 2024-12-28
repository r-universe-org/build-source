#' Render rmarkdown article
#'
#' @export
#' @rdname articles
#' @param input path to Rmd file
#' @param ... passed to render
render_article <- function(input, ...){
  rmarkdown::render(input, output_format = r_universe_format(), ...)
}

#' @export
#' @rdname articles
render_quarto <- function(input, ...){
  quarto::quarto_render(input, metadata = quarto_html_meta())
}

template_file <- function(path){
  system.file('rmd-template', path, package = 'buildtools', mustWork = TRUE)
}

# this replaces: quarto:::get_meta_for_html()
# See html format parameters: https://quarto.org/docs/reference/formats/html.html
quarto_html_meta <- function(){
  list(
    template = template_file('template.html'),
    theme = 'none',
    minimal = TRUE,
    toc = TRUE,
    'toc-depth' = 2,
    'embed-resources' = TRUE,
    'highlight-style' = 'pygments'
  )
}

r_universe_format <- function(){
  rmarkdown::html_document(
    toc = TRUE,
    toc_depth = 2,
    theme = NULL,
    mathjax = NULL,
    highlight = 'pygments',
    template = template_file('template.html'),
    includes = rmarkdown::includes(
      in_header = template_file('header.html'),
      after_body = template_file('footer.html')
    )
  )
}

#  Hack to replace knitr::rmarkdown engine
#' @export
#' @rdname articles
replace_rmarkdown_engine <- function(){
  message("Setting r-universe rmarkdown theme...")
  rmd_engine <- tools::vignetteEngine('rmarkdown', package = 'knitr')
  tools::vignetteEngine('rmarkdown', package = 'knitr', tangle = rmd_engine$tangle,
    pattern = rmd_engine$pattern, weave = function(file,..., output_format = NULL){
      load_custom_output_package(file)
      rmd_engine$weave(file,..., output_format = r_universe_format())
    }
  )

  notangle_rmd_engine <- tools::vignetteEngine('rmarkdown_notangle', package = 'knitr')
  tools::vignetteEngine('rmarkdown_notangle', package = 'knitr', tangle = notangle_rmd_engine$tangle,
    pattern = notangle_rmd_engine$pattern, weave = function(..., output_format = NULL){
      notangle_rmd_engine$weave(..., output_format = r_universe_format())
    }
  )

  # For backward compatibility with old vignettes that use legacy knitr::knitr engine
  old_engine <- tools::vignetteEngine('knitr', package = 'knitr')
  tools::vignetteEngine('knitr', package = 'knitr', tangle = old_engine$tangle,
    pattern = old_engine$pattern, weave = function(file, ..., output_format = NULL){
      if (grepl("\\.[Rr]md$", file)){
        rmd_engine$weave(file, ..., output_format = r_universe_format())
      } else {
        old_engine$weave(file, ...)
      }
    })

  # For Hendrik's rsp vignettes
  setHook(packageEvent("R.rsp", "onLoad"), function(...) {
    message("Found R.rsp! Replacing R.rsp markdown theme...")
    rsp_engine <- tools::vignetteEngine('rsp', package='R.rsp')
    tools::vignetteEngine('rsp', package = 'R.rsp', weave = function(file, ..., postprocess = TRUE){
      if(isTRUE(grepl("\\.md\\.rsp$", file))){
        mdfile <- rsp_engine$weave(file, ..., postprocess = FALSE)
        render_article(mdfile)
      } else {
        rsp_engine$weave(file, ..., postprocess = postprocess)
      }
    }, tangle = rsp_engine$tangle, pattern = rsp_engine$pattern)
  })

  # Mainly Dirk's vignettes
  setHook(packageEvent("simplermarkdown", "onLoad"), function(...) {
    message("Found simplermarkdown! Replacing mdweave_to_html theme...")
    old_engine <- tools::vignetteEngine('mdweave_to_html', package='simplermarkdown')
    tools::vignetteEngine('mdweave_to_html', package = 'simplermarkdown', weave = function(file, ...){
      mdfile <- file.path(tempdir(), paste0(tools::file_path_sans_ext(file), '.md'))
      simplermarkdown:::mdweave(file, mdfile, ...)
      htmlfile <- render_article(mdfile)
      file.copy(htmlfile, '.', overwrite = TRUE)
    }, tangle = old_engine$tangle, pattern = old_engine$pattern)
  })

  # Litedown (TODO: do not override slides)
  setHook(packageEvent("litedown", "onLoad"), function(...) {
    message("Found litedown! Enabling r-universe template")
    old_engine <- tools::vignetteEngine('vignette', package='litedown')
    tools::vignetteEngine('vignette', package = 'litedown', weave = function(file, encoding, ...){
      meta <- buildtools:::read_yaml_font_matter(file)$options$meta
      has_plugins <- setdiff(c(meta$css ,meta$js), c('@default'))
      template <- if(length(has_plugins)){
        template_file('litedown-default.html')
      } else {
        template_file('litedown-custom.html')
      }
      mdfile <- file.path(tempdir(), paste0(tools::file_path_sans_ext(file), '.md'))
      htmlfile <- file.path(tempdir(), paste0(tools::file_path_sans_ext(file), '.html'))
      load_custom_output_package(file)
      litedown::fuse(file, mdfile, ...)
      options(litedown.html.template = template)
      on.exit(options(litedown.html.template = NULL))
      litedown::mark(mdfile, htmlfile)
      file.copy(htmlfile, '.', overwrite = TRUE)
    }, tangle = old_engine$tangle, pattern = old_engine$pattern)
  })

  # Experimental quarto override
  setHook(packageEvent("quarto", "onLoad"), function(...) {
    message("Found quarto! Replacing html engine...")
    quarto_engine <- tools::vignetteEngine('html', package='quarto')
    environment(quarto_engine$weave)$meta$format$html <- quarto_html_meta()
  })
}

read_yaml_font_matter <- function(rmd_file){
  rmarkdown:::output_format_from_yaml_front_matter(readLines(rmd_file, n = 100))
}

# If a package uses a custom 'output' it may also assume functions from this package
load_custom_output_package <- function(rmd_file){
  try({
    name <- read_yaml_font_matter(rmd_file)$name
    if(length(name) && grepl("::", name, fixed = TRUE)){
      pkg <- strsplit(name, '::', fixed = TRUE)[[1]][1]
      require(pkg, character.only = TRUE)
    }
  }, silent = TRUE)
}
