#' Fixup HTML things
#'
#' Post edit HTML pages for help files
#'
#' @export
#' @rdname buildtools
#' @param path directory with html files
tweak_help_files <- function(path){
  files <- list.files(path, pattern = '.html$', full.names = TRUE)
  lapply(files, inject_help_css_js)
  invisible()
}

inject_help_css_js <- function(path){
  cat("Tweaking", path, "\n")
  doc <- xml2::read_html(path)

  # Inject CSS sheet
  head <- xml2::xml_find_first(doc, "head")
  xml2::xml_add_child(head, xml2::read_xml('<link rel="stylesheet" href="https://r-universe.dev/static/help.css"/>'))

  # Inject JavaScript
  body <- xml2::xml_find_first(doc, "body")
  xml2::xml_add_child(body, xml2::read_xml('<script src="https://r-universe.dev/static/help.js"></script>'))

  # Remove the default R.css include
  xml2::xml_remove(xml2::xml_find_first(head, "//link[@href = 'R.css']"))

  # These are only for 00Index.html
  xml2::xml_remove(xml2::xml_find_all(doc, "//img[@class = 'toplogo']"))
  xml2::xml_remove(xml2::xml_find_all(doc, "//a[img[@class = 'arrow']]"))

  # Remove .html extension for hyperlinks under /help
  links <- xml2::xml_find_all(doc, "//a")
  xml2::xml_set_attr(links, 'href', sub("(/help/.*)\\.html$", '\\1', xml2::xml_attr(links, 'href')))
  xml2::xml_set_attr(links, 'href', sub("00Index.html$", './', xml2::xml_attr(links, 'href')))

  # Write output
  xml2::write_html(doc, path)
}
