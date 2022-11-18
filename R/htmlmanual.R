#' Generate HTML reference manual
#'
#' Renders HTML version of the full package manual
#'
#' @rdname html_manual
#' @param package name of the package
#' @param outdir where to put the html file
#' @export
render_html_manual <- function(package, outdir = '.'){
  message("Rendering HTML reference manual for package: ", package)
  dir.create(outdir, showWarnings = FALSE)
  installdir <- system.file(package = package, mustWork = TRUE)
  manfiles <- load_rd_env(package)
  desc <- package_desc(installdir)
  links <- tools::findHTMLlinks(installdir)
  aliases <- readRDS(file.path(installdir, "help", "aliases.rds"))
  doc <- xml2::read_html(system.file(package = 'buildtools', 'help-template/manual.html'), options = c("RECOVER", "NOERROR"))
  body <- xml2::xml_find_first(doc, '//body')
  xml2::xml_set_attr(body, 'class', 'macintosh')
  xml2::xml_set_text(xml2::xml_find_first(doc, '//title'), sprintf("Package '%s' reference manual", desc$package))
  xml2::xml_set_text(xml2::xml_find_first(body, '//h1'), sprintf("Package '%s'", desc$package))
  lapply(xml2::xml_find_all(doc, "//td[starts-with(@class,'description')]"), function(node){
    field <- substring(xml2::xml_attr(node, 'class'), 13)
    if(length(desc[[field]])){
      xml2::xml_set_text(node, desc[[field]])
    }
  })
  nodes <- lapply(ls(manfiles), function(page_id){
    render_one_page(page_id, rd = manfiles[[page_id]], package = package, links = links)
  })
  lapply(nodes, xml2::xml_add_child, .x = body)
  fix_links(doc, package, aliases)
  fix_images(doc, package)
  prismjs::prism_process_xmldoc(doc)
  render_math(doc)
  outfile <- file.path(outdir, paste0(package, '.html'))
  xml2::write_html(doc, outfile)
  return(outfile)
}

#' @rdname html_manual
#' @export
render_base_manuals <- function(outdir = '.'){
  lapply(basepkgs, render_html_manual, outdir = outdir)
}

#TODO: maybe use tools::Rd_db() instead ?
load_rd_env <- function(package){
  manfiles <- new.env(parent = emptyenv())
  installdir <- system.file(package = package, mustWork = TRUE)
  lazyLoad(file.path(installdir, 'help', package), env = manfiles)
  return(manfiles)
}

render_one_page <- function(page_id, rd, package, links){
  #rd <- tools:::parse_Rd(path)
  out <- tempfile(fileext = '.html')
  #Sys.setenv('_R_HELP_LINKS_TO_TOPICS_' = FALSE)
  #Sys.setenv('_R_HELP_ENABLE_ENHANCED_HTML_' = FALSE)
  html <- tools::Rd2HTML(rd, package = package, out = out, stages=c("build", "install", "render"),
                         Links = links, stylesheet="", dynamic = FALSE)
  doc <- xml2::read_html(html)
  container <- xml2::xml_find_first(doc, "//div[@class = 'container']")
  xml2::xml_set_attr(container, 'id', page_id)
  xml2::xml_set_attr(container, 'class', "container manual-page")
  xml2::xml_remove(xml2::xml_find_first(doc, "//div[a[@href = '00Index.html']]")) # Remove footer
  headertable <- xml2::xml_find_first(doc, "//table[.//td[text() = 'R Documentation']]")
  xml2::xml_remove(headertable)
  titlenode <- xml2::xml_find_first(doc, '//h2')
  titlelink <- xml2::xml_replace(titlenode, 'a')
  xml2::xml_set_attr(titlelink, 'href', paste0("#", page_id))
  xml2::xml_set_attr(titlelink, 'style', 'text-decoration: none; color:black;')
  xml2::xml_add_child(titlelink, titlenode)
  return(container)
}

fix_links <- function(doc, package, aliases){
  links <- xml2::xml_find_all(doc, "//a[starts-with(@href,'../../')]")
  xml2::xml_set_attr(links, 'href', sub("00Index.html$", './', xml2::xml_attr(links, 'href')))
  linkvalues <- substring(xml2::xml_attr(links, 'href'), 7)
  matches <- gregexec("^([^/]+)/(html|help)/([^/]+)\\.html", linkvalues, perl = TRUE)
  parsedlinks <- regmatches(linkvalues, matches)
  newlinks <- vapply(parsedlinks, function(x){
    if(length(x) == 4){
      linkpkg <- x[2]
      topic <- x[4]
      if(linkpkg == package){
        if(length(aliases[topic]))
          topic <- aliases[topic]
        return(paste0("#", topic))
      } else if(all(c(linkpkg,package) %in% basepkgs)){
        # Remove this clause when manuals are published and link to full r-universe URL
        # This way e.g. Matrix links both correct from its own universe and base-manual
        return(sprintf('%s.html#%s', linkpkg, topic))
      } else {
        pkguniv <- find_package_universe(linkpkg)
        if(length(pkguniv)){
          return(sprintf('%s/%s.html#%s', pkguniv, linkpkg, topic))
        }
      }
    }
    return("#")
  }, character(1))
  xml2::xml_set_attr(links, 'href', newlinks)

  # Open external links in a new page
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[starts-with(@href,'http://')]"), 'target', '_blank')
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[starts-with(@href,'https://')]"), 'target', '_blank')

  # Remove dead links produced above
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[@href = '#']"), 'href', NULL)
}

fix_images <- function(doc, package){
  images <- xml2::xml_find_all(doc, "//img[starts-with(@src,'../help/')]")
  lapply(images, function(x){
    helpdir <- system.file(package = package, 'help', mustWork = TRUE)
    img <- file.path(helpdir, xml2::xml_attr(x, 'src'))
    if(!file.exists(img)){
      stop("Document references non-existing image: ", xml2::xml_attr(x, 'src'))
    }
    # TODO: maybe better just remove these images, because they seem mostly
    # intended for pkgdown, and don't show up in the PDF manual either...
    xml2::xml_set_attr(x, 'src', image_base64(img))
  })
}

image_base64 <- function(path){
  ext <- tolower(utils::tail(strsplit(path, '.', fixed = TRUE)[[1]], 1))
  type <- switch(ext,
                 svg = 'image/svg+xml',
                 png = 'image/png',
                 jpeg = 'image/jpeg',
                 jpg = 'image/jpeg',
                 stop("Unknown image extension: ", path))
  content <- readBin(path, raw(), file.info(path)$size)
  b64 <- gsub('\n', '', jsonlite::base64_enc(content), fixed = TRUE)
  sprintf('data:%s;base64,%s', type, b64)
}

# Simulate what happens in R katex-config.js script
# https://github.com/r-devel/r-svn/blob/HEAD/doc/html/katex-config.js
render_math <- function(doc){
  macros = list("\\R"= "\\textsf{R}", "\\mbox"= "\\text", "\\code"= "\\texttt")
  lapply(xml2::xml_find_all(doc, "//code[@class = 'reqn']"), function(x){
    input <- trimws(xml2::xml_text(x))
    output <- katex::katex_html(input, preview = FALSE, macros = macros, throwOnError = FALSE)
    newnode <- xml2::read_xml(paste0('<code class="reqn">', trimws(output), '</code>'))
    xml2::xml_replace(x, xml2::xml_root(newnode))
  })
}

package_desc <- function(path){
  desc <- as.list(tools:::.read_description(file.path(path, 'DESCRIPTION')))
  names(desc) <- tolower(names(desc))
  desc$date <- trimws(strsplit(desc$built, ';')[[1]][3])
  desc
}

pkg_news <- function(path){
  for(x in file.path(path, c("NEWS", "NEWS.md", "NEWS.Rd"))){
    if(file.exists(x)){
      return(x)
    }
  }
}

find_package_url_internal <- function(package){
  message("Looking up universe for: ", package)
  url <- sprintf('https://r-universe.dev/stats/powersearch?limit=50&all=true&q=package:%s', package)
  out <- jsonlite::fromJSON(url)
  universe <- if(length(out$results)){
    sprintf("https://%s.r-universe.dev/%s", out$results[['_user']][1], package)
  } else if(package %in% universe_list(Sys.getenv("MY_UNIVERSE"))){
    sprintf('%s/%s', Sys.getenv("MY_UNIVERSE"), package)
  } else if(package %in% basepkgs){
    'https://r-universe.dev/manuals'
  }
}

list_universe_packages_internal <- function(universe){
  message("Listing packages in: ", universe)
  if(nchar(universe)){
    jsonlite::fromJSON(sprintf('%s/packages', universe))
  }
}

universe_list <- memoise::memoise(list_universe_packages_internal)
find_package_universe <- memoise::memoise(find_package_url_internal)


basepkgs <- c("base", "boot", "class", "cluster", "codetools", "compiler",
              "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
              "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
              "parallel", "rpart", "spatial", "splines", "stats",
              "stats4", "survival", "tcltk", "tools", "utils")

#res = html_manual("~/workspace/V8")