#' @rdname getNewsfeed
#' @export
#'
#' @title 
#' Download recent posts from the authenticated user's newsfeed
#'
#' @description
#' \code{getNewsfeed} retrieves status updates from the authenticated user's 
#' News Feed
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{fbOAuth}}, \code{\link{getPost}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of posts to return.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Capture 100 most recent posts on my newsfeed
#'	load("fb_oauth")
#'	my_newsfeed <- getNewsfeed(token=fb_oauth, n=100)
#' }
#'

getNewsfeed <- function(token, n=200)
{
	

	url <- "https://graph.facebook.com/me/home?limit="
	if (n<=200){
		url <- paste(url, n, sep="")
	}
	if (n>200){
		url <- paste(url, "200", sep="")
	}
	url <- paste(url, 
		"&fields=from.fields(name,id),to,message,created_time,type,link,likes.summary(true).limit(1),comments.summary(true).limit(1),shares",
		sep="")

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
		stop("No more posts were found on News Feed.")
	}
	df <- newsDataToDF(content$data)

	## paging if n>200
	if (n>200){
		df.list <- list(df)
		while (l<n & length(content$data)>0 &
			!is.null(content$paging$`next`)){
			url <- content$paging$`next`
			content <- callAPI(url=url, token=token)
			l <- l + length(content$data)
			if (length(content$data)>0){ cat(ifelse(l<n,l,n), " ") }

			## retrying 3 times if error was found
			error <- 0
			while (length(content$error_code)>0){
				cat("Error!\n")
				Sys.sleep(0.5)
				error <- error + 1
				content <- callAPI(url=url, token=token)		
				if (error==3){ stop(content$error_msg) }
			}

			df.list <- c(df.list, list(newsDataToDF(content$data)))
		}
		df <- do.call(rbind, df.list)
		if (nrow(df)>n){ df <- df[1:n,]}
	}
	return(df)
}



