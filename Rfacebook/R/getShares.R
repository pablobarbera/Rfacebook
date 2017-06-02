#' @rdname getShares
#' @export
#'
#' @title 
#' Extract list of users who publicly shared a public Facebook post
#'
#' @description
#' \code{getShares} retrieves a list of posts that correspond to shares
#' of a post on a public Facebook page. Only public posts by users who have
#' granted authorization to the app used to authenticate.
#'
#' @details 
#' \code{getShares} returns a data frame with four variables: \code{from_name}
#' (the name of the user who shared the post), \code{from_id} (the ID of the
#' user who shared the post), \code{shared_time} (the time at which the post
#' was shared), and \code{id} (the ID of the new post).
#'
#' For more information on why not all shared posts are returned, see here:
#' \url{https://developers.facebook.com/bugs/1404733043148335/}
#'
#' @author
#' Pablo Barbera \email{pbarbera@@usc.edu}
#' @seealso \code{\link{getPage}}, \code{\link{getPost}}
#'
#' @param post A post ID
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of shares to return.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting shares of most recent post
#'	shares <- getShares(post=fb_page$id[1], n=2000, token=fb_oauth)
#' }
#'

getShares <- function(post, token, n=100){

	url <- paste0("https://graph.facebook.com/", post,
				"?fields=sharedposts{from}")
	if (n>100){
		url <- gsub("\\{from\\}", ".limit(100)\\{from\\}", url)
	}
	# making query
	content <- callAPI(url=url, token=token)

	# error traps: retry 3 times if error
	error <- 0
	while (length(content$error_code)>0){
		cat("Error!\n")
		Sys.sleep(0.5)
		error <- error + 1
		content <- callAPI(url=url, token=token)		
		if (error==3){ stop(content$error_msg) }
	}
	if (length(content)==0){ 
		stop("No public shares were found.")
	}

    df <- sharesToDF(content$sharedposts$data)
    next_url <- content$sharedposts$paging$`next`
    while (!is.null(next_url)){
    	content <- callAPI(next_url, token)
    	df <- rbind(df, sharesToDF(content$sharedposts$data))
    	next_url <- content$sharedposts$paging$`next`
    }
	return(df)
}
