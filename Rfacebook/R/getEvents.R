#' @rdname getEvents
#' @export
#'
#' @title 
#' Extract list of events from a public Facebook page or group
#'
#' @description
#' \code{getEvents} retrieves event information from a public Facebook group or page.
#'
#' @author
#' Pablo Barbera \email{pbarbera@@usc.edu}
#' @seealso \code{\link{getPage}}, \code{\link{fbOAuth}}
#'
#' @param page Facebook ID for the group or page.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Number of posts of page to return. Note that number can be sometimes higher or lower, depending on status of API.
#'
#' @param since A strtotime data value that points to the start of the time range to be searched. For more information on the accepted values, see: http://php.net/manual/en/function.strtotime.php. Note that the function uses the start time of the event to determine whether the event occurred after the date entered here.
#' 
#' @param verbose If TRUE, will report a number of the posts retrieved. Note that this indicates the total number of posts called from the API, which may be different than the number of posts in the output data frame. For example, if n is set to 20, the function retrieves and reports 25 events, and then drops the bottom 5 events to give the desired outpout of 20.
#' 
#' @param api API version. e.g. "v2.8". \code{NULL} is the default.
#'
#' @examples \dontrun{
#'	load("fb_oauth")
#' ## Downloading events from Playa Vista Farmers' Market
#'  events <- getEvents(page="playavistaFM", token=fb_oauth)
#' }

function (page, token, n = 25, since = NULL, verbose = TRUE, api = NULL) 
{
  url <- paste0("https://graph.facebook.com/", page, "?fields=events{description,name,start_time,end_time,place,id,attending_count,", 
                "declined_count,maybe_count,noreply_count}")
  content <- callAPI(url = url, token = token, api = api)
  l <- length(content$events$data)
  cat(l, "events ")
  error <- 0
  while (length(content$error_code) > 0) {
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url = url, token = token, api = api)
    if (error == 3) {
      stop(content$error_msg)
    }
  }
  if (length(content$events$data) == 0) {
    message("No public events were found")
    return(data.frame())
  }
  df <- eventDataToDF(content$events$data)
  if (!is.null(since)) {
    dates <- as.Date(df$start_time)
    mindate <- min(dates)
    sincedate <- as.Date(since)
  }
  if (is.null(since)) {
    sincedate <- as.Date("1970/01/01")
    mindate <- as.Date(Sys.time())
  }
  if (n > 25) {
    df.list <- list(df)
    if (l < n & length(content$events$data) > 0 & !is.null(content$events$paging$`next`) & 
        sincedate <= mindate) {
      Sys.sleep(0.5)
      url <- content$events$paging$`next`
      content <- callAPI(url = url, token = token, api = api)
      l <- l + length(content$data)
      if (length(content$data) > 0) {
        if (verbose) 
          cat(l, "events ")
      }
      error <- 0
      while (length(content$error_code) > 0) {
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url = url, token = token, 
                           api = api)
        if (error == 3) {
          stop(content$events$error_msg)
        }
      }
      new.df <- eventDataToDF(content$data)
      df.list <- c(df.list, list(new.df))
      if (!is.null(since) & nrow(new.df) > 0) {
        dates <- as.Date(new.df$start_time)
        mindate <- min(dates)
      }
    }
    while (l < n & length(content$data) > 0 & !is.null(content$paging$`next`) & 
           sincedate <= mindate) {
      Sys.sleep(0.5)
      url <- content$paging$`next`
      content <- callAPI(url = url, token = token, api = api)
      l <- l + length(content$data)
      if (length(content$data) > 0) {
        if (verbose) 
          cat(l, "events ")
      }
      error <- 0
      while (length(content$error_code) > 0) {
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url = url, token = token, 
                           api = api)
        if (error == 3) {
          stop(content$events$error_msg)
        }
      }
      new.df <- eventDataToDF(content$data)
      df.list <- c(df.list, list(new.df))
      if (!is.null(since) & nrow(new.df) > 0) {
        dates <- as.Date(new.df$start_time)
        mindate <- min(dates)
      }
    }
    df <- do.call(rbind, df.list)
  }
  if (nrow(df) > n) {
    df <- df[1:n, ]
  }
  if (!is.null(since)) {
    dates <- as.Date(df$start_time)
    df <- df[dates >= sincedate, ]
  }
  return(df)
}
