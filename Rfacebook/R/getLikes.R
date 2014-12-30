#' @rdname getLikes
#' @export
#'
#' @title 
#' Extract list of likes of a Facebook friend
#'
#' @description
#' \code{getLikes} retrieves information about a friend's likes
#'
#' @details
#' 
#' This function requires the use of an OAuth token with the following
#' permissions: user_likes, friends_likes
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}, \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param user A user ID or screen name.
#'
#' @param n Maximum number of likes to return for each user.
#'
#' @examples \dontrun{
#'  token <- 'XXXXX'
#'  my_likes <- getLikes(user="me", token=token)
#' }
#'

getLikes <- function(user, n=500, token){
	query <- paste0('https://graph.facebook.com/', user, 
		'?fields=likes.limit(', n, ').fields(id,name,website)')
	content <- callAPI(query, token)
    df <- userLikesToDF(content$likes$data)
    next_url <- content$likes$paging$`next`
    while (!is.null(next_url)){
    	content <- callAPI(next_url, token)
    	df <- rbind(df, userLikesToDF(content$data))
    	next_url <- content$paging$`next`
    }
	return(df)
}




