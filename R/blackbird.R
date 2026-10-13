# This is experimental. The GH blackbird API seemingly may only be used with a
# webui user_session cookie, not a token. So we hack it.
get_script_count <- function(package){
  if(nchar(Sys.getenv('BLACKBIRD_COUNT'))){
    message("Found BLACKBIRD_COUNT")
    return(as.numeric(Sys.getenv('BLACKBIRD_COUNT')))
  }

  # Note: codesearch has very low rate limit and slighly lower count as it only
  # includes default branches, and skips forks and large files.
  tryCatch({
    query = sprintf("library(%s)", package)
    message("Using codesearch for scripts with: ", query)
    apidata <- gh::gh('/search/code', q = query)
    if(length(apidata$total_count)){
      return(apidata$total_count)
    }
  }, error = function(e){
    message("Failed to call codesearch: ", e$message)
  })

  tryCatch({
    current_info(package)[['_searchresults']]
  }, error = function(e){
    message("Failed to get script count: ", e$message)
  })
}

make_session_handle <- function(){
  agent <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36'
  session <- Sys.getenv("DUMMY_SESSION")
  if(!nchar(session))
    stop("No dummy session, skipping script count")
  curl::new_handle(useragent = agent, cookie = sprintf("user_session=%s;", session))
}
