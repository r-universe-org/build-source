# This is experimental. The GH blackbird API seemingly may only be used with a
# webui user_session cookie, not a token. So we hack it.
get_blackbird_count <- function(package){
  tryCatch({
    # Todo: we could also include (path:*.R OR path:*.*md) to get only R results
    url <- paste0('https://github.com/search/blackbird_count?saved_searches=&q=%22library%28', package, '%29%22')
    handle <- make_session_handle()
    warmup <- curl::curl_fetch_memory('https://github.com/issues', handle = curl::handle_setheaders(handle, accept = 'text/html'))
    req <- curl::curl_fetch_memory(url, handle = curl::handle_setheaders(handle, accept = 'application/json'))
    if(req$status_code != 200){
      stop(paste('HTTP', req$status_code))
    }
    data <- jsonlite::fromJSON(rawToChar(req$content))
    if(data$failed == TRUE){
      stop("Server says blackbird count failed")
    }
    return(data$count)
  }, error = function(e){
    message("Failed to get blackbird count: ", e$message)
  })
}

make_session_handle <- function(){
  agent <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36'
  curl::new_handle(useragent = agent, cookie = sprintf("user_session=%s;", Sys.getenv("DUMMY_SESSION")))
}

#out <- get_blackbird_count('dplyr')
