#' @rdname getInsights
#' @export
#'
#' @title 
#' Extract Insights metric from a Facebook page (admin role required)
#'
#' @description
#' \code{getInsights} retrieves information from an owned Facebook page. Note 
#' that you must specify wich metric from insights you need. 
#'
#' @author
#' Danilo Silva \email{silvadaniloc@@gmail.com}
#' Eduardo Carvalho \email{eduardooc.86@@gmail.com}
#'
#' @param object_id An object (page, post, domain) ID.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param period Time intervals to return
#' 
#' @param metric The metric wich you want to get values (All metrics are listed
#' in https://developers.facebook.com/docs/graph-api/reference/v2.1/insights)
#' 
#' @param n Number of time intervals of metric values to return. Note that all
#' metrics returned will be multiple of 3, except for lifetime period. Default
#' n is 5
#'
#'
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting page impressions for Facebook's Facebook Page
#' ## (only owner or admin of page)
#'  load("fb_oauth")
#'	insights <- getInsights(object_id="20531316728", token=fb_oauth, metric='page_impressions')
#' ## Getting post impressions for a random Facebook`s page post
#' ## (only owner or admin of page)
#'  insights <- getInsights(object_id='221568044327801_754789777921289', 
#'      token=fb_oauth, metric='post_impressions', period='days_28')
#' }
#'

getInsights <- function(object_id, token, metric, period='day', n=5){
  url <- paste0('https://graph.facebook.com/', object_id,
                '/insights/', metric, '?period=', period)
  
  # making query
  content <- callAPI(url=url, token=token)
  l <- length(content$data[[1]]$values)
  if (l==0){ 
    stop("No public posts mentioning the string were found")
  }
  
  ## retrying 3 times if error was found
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    print(url)
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content$data)==0){ 
    stop("No public posts mentioning the string were found")
  }
  df <- insightsDataToDF(content$data, content$data[[1]]$values)
  
  if (n>nrow(df)){
    df.list <- list(df)
    while (l<n & l>0 & 
             !is.null(content$paging$`next`)&
             period != 'lifetime'){
      # waiting one second before making next API call...
      Sys.sleep(0.5)
      url <- content$paging$`next`
      content <- callAPI(url=url, token=token)
      l <- l + nrow(df)
      
      ## retrying 3 times if error was found
      error <- 0
      while (length(content$error_code)>0){
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url=url, token=token)		
        if (error==3){ stop(content$error_msg) }
      }
      
      df.list <- c(df.list, list(insightsDataToDF(content$data, content$data[[1]]$values)))
    }
    df <- do.call(rbind, df.list)
  }
  cat(l, "objects found ")
  return(df)
}
