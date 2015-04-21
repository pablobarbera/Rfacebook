#' @rdname getInsights
#' @export
#'
#' @title 
#' Extract Insights metric from a Facebook page (admin role required)
#'
#' @description
#' \code{getInsights} retrieves information from an owned Facebook page. Note 
#' that you must specify wich metric from insights you need and the period.
#'
#' @details
#' The current list of supported metrics and periods is: page_fan_adds, page_fan_removes, 
#' page_views_login, page_views_login, page_views_logout, page_views, page_story_adds, 
#' page_impressions, page_posts_impressions, page_consumptions, post_consumptions_by_type, 
#' page_consumptions, and page_fans_country.
#'
#' For more information, see: \url{https://developers.facebook.com/docs/graph-api/reference/v2.1/insights}
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
#' @param metric The metric which you want to get values for.
#'
#' @param period Time intervals to return
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
#' ## Getting post impressions for a random Facebook's page post
#' ## (only owner or admin of page)
#'  insights <- getInsights(object_id='221568044327801_754789777921289', 
#'      token=fb_oauth, metric='post_impressions', period='days_28')
#' ## Count of fans by country
#'   insights <- getInsights(object_id='221568044327801_754789777921289', 
#'      token=fb_oauth, metric='page_fans_country', period='lifetime')
#' }
#'

getInsights <- function(object_id, token, metric, period='day', n=5){
  url <- paste0('https://graph.facebook.com/', object_id,
                '/insights/', metric, '?period=', period)
  
  # making query
  content <- callAPI(url=url, token=token)
  if (length(content$data)==0){ 
    stop("No data available. Are you the owner of this page? See ?getInsights.")
  }

  l <- length(content$data[[1]]$values)
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

  df <- insightsDataToDF(content$data, content$data[[1]]$values, metric)
  
  if (n>nrow(df)){
    df.list <- list(df)
    while (l<n & l>0 & 
             !is.null(content$paging$`previous`)&
             period != 'lifetime'){
      # waiting one second before making next API call...
      Sys.sleep(0.5)
      url <- content$paging$`previous`
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
      
      df.list <- c(df.list, list(insightsDataToDF(content$data, content$data[[1]]$values, metric)))
    }
    df <- do.call(rbind, df.list)
  }
  return(df)
}
