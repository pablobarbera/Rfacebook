#' @rdname searchPages
#' @export
#'
#' @title 
#' Search pages that mention a string
#'
#' @description
#' \code{searchPages} retrieves public pages that mention a given keyword
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}, Joel Gombin \email{joel.gombin@@gmail.com}
#' @seealso \code{\link{fbOAuth}}, \code{\link{searchFacebook}}
#'
#' @param string string or string vector containing keywords to search.
#' Note that the returned results will contain any of the keywords. 
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of pages to return.
#' 
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Searching 100 public pages that mention "facebook"
#'  load("fb_oauth")
#'	pages <- searchPages( string="facebook", token=fb_oauth, n=100 )
#' }
#'

searchPages <- function(string, token, n=200)
{
  if (length(string)>1){ string <- paste(string, collapse=" ") }
  
  url <- paste("https://graph.facebook.com/search?q=", string,
               "&type=page&limit=", sep="")
  if (n<=200){
    url <- paste(url, n, sep="")
  }
  if (n>200){
    url <- paste(url, "200", sep="")
  }
  url <- paste(url, 
               "&fields=id,about,category,category_list,description,general_info,likes,link,location,name,talking_about_count,username,website",
               sep="")
  
  url <- utils::URLencode(url)
  
  ## making query
  content <- callAPI(url=url, token=token)
  l <- length(content$data); cat(l, "pages ")
  
  ## retrying 3 times if error was found
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content$data)==0){ 
    stop("No public page mentioning the string were found")
  }
  df <- searchPageDataToDF(content$data)

  ## paging if n>200
  if (n>200){
    df.list <- list(df)
    while (l<n & length(content$data)>0 &
             !is.null(content$paging$`next`)){
      url <- content$paging$`next`
      content <- callAPI(url=url, token=token)
      l <- l + length(content$data)
      if (length(content$data)>0){ cat(l, " ") }
      
      ## retrying 3 times if error was found
      error <- 0
      while (length(content$error_code)>0){
        cat("Error!\n")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url=url, token=token)		
        if (error==3){ stop(content$error_msg) }
      }
      
      df.list <- c(df.list, list(searchPageDataToDF(content$data)))
    }
    df <- do.call(rbind, df.list)
  }
  return(df)
}
