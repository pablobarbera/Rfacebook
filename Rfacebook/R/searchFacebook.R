#' @rdname searchFacebook
#' @export
#'
#' @title 
#' Search public posts that mention a string
#'
#' @description
#' \code{searchFacebook} retrieves public status updates that mention a given keyword
#'
#' @details
#'
#' Note: Public post search was deprecated with version 2.0 of the Facebook Graph API,
#' and therefore this function will no longer work. For more information about these
#' changes, go to: \url{https://developers.facebook.com/docs/apps/changelog}
#'
#' The function will only work for OAuth tokens generated with version 1.0 of the API

#' The search is performed also on the text of the comments too, which explains
#' why some of the returned messages do not mention the string that is being
#' searched.
#'
#' Note that only messages up to around two weeks old or less can be returned.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{fbOAuth}}
#'
#' @param string string or string vector containing keywords to search.
#' Note that the returned results will contain any of the keywords. It is not
#' possible to search for status updates that include all of the keywords.
#' 
#' @param token An OAuth token created with \code{fbOAuth}. Only tokens 
#' created for version 1.0 of the Facebook Graph API will return results.
#'
#' @param n Maximum number of posts to return.
#' 
#' @param since A UNIX timestamp or strtotime data value that points to
#' the start of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#' @param until A UNIX timestamp or strtotime data value that points to
#' the end of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#' @examples \dontrun{
#' ## Searching 100 public posts that mention "facebook"
#'	posts <- searchFacebook( string="facebook", token=fb_oauth, n=100 )
#' ## Searching 100 public posts that mention "facebook" from yesterday
#'	posts <- searchFacebook( string="facebook", token=fb_oauth, n=100,
#'    since = "yesterday 00:00", until = "yesterday 23:59" )
#' }
#'

searchFacebook <- function(string, token, n=200, since=NULL, until=NULL)
{

	tkversion <- getTokenVersion(token)

	if (tkversion=="v2"){
		stop("Searching for posts was deprecated with version 2.0 of",
		" the Facebook Graph API.\nFor more details see ?searchFacebook")
	}

	if (length(string)>1){ string <- paste(string, collapse=" ") }

	url <- paste("https://graph.facebook.com/search?q=", string,
		"&type=post&limit=", sep="")
	if (n<=200){
		url <- paste(url, n, sep="")
	}
	if (n>200){
		url <- paste(url, "200", sep="")
	}
	url <- paste(url, 
		"&fields=from.fields(name,id),message,created_time,type,link,likes.summary(true),comments.summary(true),shares",
		sep="")
	## add since/until
	if (!is.null(since)){
		url <- paste(url, "&since=", since, sep="")
	}
	if (!is.null(until)){
		url <- paste(url, "&until=", until, sep="")
	}
	
	url <- utils::URLencode(url)
	
	## making query
	content <- callAPI(url=url, token=token)
	l <- length(content$data); cat(l, "posts ")
	
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
		message("No public posts mentioning the string were found")
		return(data.frame())
	}
	df <- searchDataToDF(content$data)

	## paging if n>200
	if (n>200){
		df.list <- list(df)
		while (l<n & length(content$data)>0 &
			!is.null(content$paging$`next`)){
			url <- content$paging$`next`
			if (!is.null(since)){
				url <- paste(url, "&since=", since, sep="")
			}
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

			df.list <- c(df.list, list(searchDataToDF(content$data)))
		}
		df <- do.call(rbind, df.list)
	}
	return(df)
}



