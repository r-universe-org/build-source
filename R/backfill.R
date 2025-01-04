#' Backfill fields
#'
#' Some functions to manually update stats without rebuilding
#'
#' @export
backfill_search_results <- function(){
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  df <- jsonlite::stream_in(url(paste0('https://r-universe.dev/stats/files?type=src&fields=_searchresults,_score&nocache=', rnorm(1))))
  df <- df[is.na(df[['_searchresults']]),]
  df <- df[order(df[['_score']], decreasing = TRUE),]
  for(i in seq_len(nrow(df))){
    info <- as.list(df[i,])
    tryCatch({
      count <- get_blackbird_count(info$package)
      if(!is.numeric(count) || length(count) != 1) stop("Unexpected blackbird count for: ", info$package)
      json <- jsonlite::toJSON(list('$set' = list('_searchresults' = count)), auto_unbox = TRUE, verbose = TRUE)
      h <- curl::new_handle(userpwd = userpwd, copypostfields = json, httpheader = "Content-Type: application/json")
      url <- sprintf("https://%s.r-universe.dev/api/packages/%s/%s/update", info$user, info$package, info$version)
      req <- curl::curl_fetch_memory(url, handle = h)
      if(req$status > 300){
        stop(sprintf("Failure at %s:\n%s", url, rawToChar(req$content)))
      }
      message("OK: ", url)
    }, error = function(e){
      message(e)
      Sys.sleep(900)
    })
  }
}

#' @export
backfill_bioc_downloads <- function(){
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  df <- jsonlite::stream_in(url('https://bioc.r-universe.dev/stats/files?type=src&fields=_downloads.count&nocache=123'))
  df <- df[is.na(df[['_downloads']]$count),]
  for(i in seq_len(nrow(df))){
    info <- as.list(df[i,])
    tryCatch({
      downloads <- bioc_monthly_downloads(info$package)
      if(!is.numeric(downloads$count) || length(downloads$count) != 1) stop("Unexpected download count for: ", info$package)
      json <- jsonlite::toJSON(list('$set' = list('_downloads' = downloads)), auto_unbox = TRUE, verbose = TRUE)
      h <- curl::new_handle(userpwd = userpwd, copypostfields = json, httpheader = "Content-Type: application/json")
      url <- sprintf("https://%s.r-universe.dev/api/packages/%s/%s/update", info$user, info$package, info$version)
      req <- curl::curl_fetch_memory(url, handle = h)
      if(req$status > 300){
        stop(sprintf("Failure at %s:\n%s", url, rawToChar(req$content)))
      }
      message("OK: ", url)
    }, error = function(e){
      message(e)
    })
  }
}

#' @export
backfill_indexurls <- function(){
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  df <- jsonlite::stream_in(url('https://r-universe.dev/stats/files?type=src&fields=_indexed,_indexurl&nocache=123'))
  indexed <- df[df[['_indexed']],]
  noindex <- df[!df[['_indexed']] & is.na(df[['_indexurl']]),]
  noindex$indexowner <- indexed[match(noindex$package, indexed$package), 'user']
  for(i in seq_len(nrow(noindex))){
    info <- as.list(noindex[i,])
    if(!length(info$indexowner) || is.na(info$indexowner)){
      next;
    }
    tryCatch({
      info$newurl <- sprintf('https://%s.r-universe.dev/%s', info$indexowner, info$package)
      stopifnot(is.character(info$newurl) && length(info$newurl))
      json <- jsonlite::toJSON(list('$set' = list('_indexurl' = info$newurl)), auto_unbox = TRUE, verbose = TRUE)
      h <- curl::new_handle(userpwd = userpwd, copypostfields = json, httpheader = "Content-Type: application/json")
      url <- sprintf("https://%s.r-universe.dev/api/packages/%s/%s/update", info$user, info$package, info$version)
      req <- curl::curl_fetch_memory(url, handle = h)
      if(req$status > 300){
        stop(sprintf("Failure at %s:\n%s", url, rawToChar(req$content)))
      }
      message("OK: ", url)
    }, error = function(e){
      message(e)
    })
  }
}


