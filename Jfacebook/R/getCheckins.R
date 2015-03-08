#' @rdname getCheckins
#' @export
#'
#' @title 
#' Extract list of checkins of a Facebook friend
#'
#' @description
#' \code{getCheckins} retrieves information about a friend's checkins
#'
#' @details
#' 
#' This function requires the use of an OAuth token with the following
#' permissions: user_status, user_checkins, friends_status, friends_checkins
#'
#' Check-in search was deprecated with version 2.0 of the Facebook Graph API.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param user A user ID or screen name.
#'
#' @param n Maximum number of checkins to return for each user.
#'
#' @param tags If \code{TRUE}, output of function will be a list of two
#' elements: a data frame with user's checkins and a list of data frames,
#' where each element contains information about users tagged in each checkin.
#'
#' @examples \dontrun{
#'  token <- 'XXXXX'
#'  my_checkins <- getCheckins(user="me", token=token)
#' }
#'

getCheckins <- function(user, n=10, token, tags=FALSE){
    
    tkversion <- getTokenVersion(token)

    if (tkversion=="v2"){
        stop("Searching for posts was deprecated with version 2.0 of",
        " the Facebook Graph API.\nFor more details see ?getCheckins")
    }

    query <- paste0('https://graph.facebook.com/', user, 
		'?fields=checkins.limit(', n, ').fields(tags,created_time,',
			'place.fields(id,name,location))')
	content <- callAPI(query, token)
    if (length(content$checkins)>0){
	    df <- checkinDataToDF(content$checkins$data)
        if (tags) {tags.df <- tagsDataToDF(content$checkins$data)}
    }
    if (length(content$checkins)==0){
        df <- NULL
        if (tags){tags.df <- rep(NULL, n)}
    }
    if (tags) out <- list(checkins=df, tagged=tags.df)
    if (!tags) out <- df
	return(out)
}





