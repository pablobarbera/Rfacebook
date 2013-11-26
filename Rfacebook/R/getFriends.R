#' @rdname getFriends
#' @export
#'
#' @title 
#' Extract list of friends with their information
#'
#' @description
#' \code{getFriends} retrieves information about the user's friends.
#'
#' @details
#' 
#' This function requires the use of an OAuth token with extended permissions.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param simplify If \code{TRUE}, function will return only name and id for each friend.
#' If \code{FALSE}, it will return additional information from their profiles: gender, birthday,
#' location, hometown, relationship status and profile picture. 
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about my friends
#'	load("fb_oauth")
#'	my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
#' ## Since users are ordered by ID, this will return 10 oldest user accounts
#'	head(my_friends, n=10)
#' }
#'



getFriends <- function(token, simplify=FALSE){
	if (simplify==TRUE){
		query <- 'https://graph.facebook.com/me/friends?limit=5000'
		content <- callAPI(query, token)
		friends <- matrix(unlist(content$data), ncol=2, byrow=TRUE)
		while (length(content$data)>0){
			query <- content$paging$`next`
			content <- callAPI(query, token)
			if (length(content$data)>0){
				friends <- rbind(friends, matrix(unlist(content$data), ncol=2, byrow=TRUE))
			}
		}
		friends <- data.frame(friends, stringsAsFactors=F)
		names(friends) <- c("name", "id")
	}	

	if (simplify==FALSE){
		query <- paste("https://graph.facebook.com/me/friends?",
			"fields=id,name,first_name,last_name,gender,locale,birthday,location,",
			"hometown,relationship_status,picture.type(large)&limit=100", sep="")
		content <- callAPI(query, token)
		friends <- userDataToDF(content$data, private_info=TRUE)
		while (length(content$data)>0){
			query <- content$paging$`next`
			content <- callAPI(query, token)
			if (length(content$data)>0){
				friends <- rbind(friends, userDataToDF(content$data, private_info=TRUE))
			}
		}
	}	
	return(friends)
}
