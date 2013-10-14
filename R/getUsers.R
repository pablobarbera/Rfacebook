#' @rdname getUsers
#' @export
#'
#' @title 
#' Extract information about one or more Facebook users
#'
#' @description
#' \code{getUsers} retrieves public information about one or more Facebook users.
#'
#' @details
#' When applied to a large number of users (500 or less), it's probably better
#' to split the vector of user IDs in batches of 500 to reduce probability
#' of errors in the API calls.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}, \code{\link{getPosts}}, \code{\link{searchFacebook}}
#'
#' @param users A vector of either user IDs or screen names or a mix of both.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param private_info If \code{FALSE}, will return only information that is 
#' publicly available for all users (name, gender, locale, profile picture). 
#' If \code{TRUE}, will return additional information for users who are friends 
#' with the authenticated user: birthday, location, hometown, and relationship 
#' status. Note that these fields will only be returned for friends. For other 
#' users, they will be \code{NA}, even if they are visible on Facebook via web.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about me
#'	load("fb_oauth")
#'	me <- getUsers("me", token=fb_oauth, private_info=FALSE)
#'	me$username
#' ## Getting information about my friends
#'	my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
#'	my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)
#'	table(my_friends_info$relationship_status)
#' ## Searching public posts about facebook and looking at gender of users
#'  fb.posts <- searchFacebook( "facebook", token=fb_oauth, n=100)
#'	fb.users <- getUsers( fb.posts$from_id, token=fb_oauth )
#' 	table(fb.users$gender)
#' }
#'

getUsers <- function(users, token, private_info=FALSE)
{
	## probably need to add here error trap when length(users)>X	

	users.query <- paste(users, collapse=",")
	## first query: checking what users are actual users vs pages
	query <- paste('https://graph.facebook.com/?ids=', users.query, sep="")
	## making query
	content <- callAPI(query, token)

	actual.users <- which(unlist(lapply(content, function(x) is.null(x$category))))
	pages <- which(unlist(lapply(content, function(x) !is.null(x$category))))

	## getting data for users

	if (private_info==TRUE){
	query <- paste('https://graph.facebook.com/?ids=', 
		paste(names(actual.users), collapse=","), 
		"&fields=id,name,username,first_name,last_name,gender,locale,birthday,",
		"location,hometown,relationship_status,picture.type(large)", sep="")
	}
	if (private_info==FALSE){
	query <- paste('https://graph.facebook.com/?ids=', 
		paste(names(actual.users), collapse=","),
		"&fields=id,name,username,first_name,last_name,gender,locale,",
		"picture.type(large)", sep="")
	}		
	## making query
	content <- callAPI(query, token)
	df.users <- userDataToDF(content, private_info=private_info)

	## getting data for pages
	query <- paste('https://graph.facebook.com/?ids=', 
		paste(names(pages), collapse=","), 
		"&fields=id,name,username,category,likes,picture.type(large)", sep="")
	## making query
	content <- callAPI(query, token)
	df.pages <- userDataToDF(content, private_info=private_info)

	df <- rbind(df.users, df.pages)
	df <- df[!is.na(df$id),]
	
	return(df)
}




