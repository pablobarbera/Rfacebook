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
#' This function requires the use of a OAuth token with extended 
#' permissions. After the introduction of version 2.0 of the Graph API,
#' only friends who are using the application that you used to generate the 
#' token to query the API will be returned.
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
#' ## Copy and paste token created at FB Graph API Explorer
#'  token <- "XXXXXX"
#'	my_friends <- getFriends(token=token, simplify=TRUE)
#' ## Since users are ordered by ID, this will return 10 oldest user accounts
#'	head(my_friends, n=10)
#' }
#'

getFriends <- function(token, simplify=FALSE){
	
	tkversion <- getTokenVersion(token)

	if (tkversion=="v2"){
		message("Only friends who use the application will be returned\n",
			"See ?getFriends for more details")
		base_url <- 'https://graph.facebook.com/v2.0'
	}

	if (tkversion=="v1"){
		base_url <- 'https://graph.facebook.com/'
	}

	if (simplify==TRUE){
		query <- paste0(base_url, '/me/friends?limit=5000')
		content <- callAPI(query, token)
		if (length(content$data)==0){
			stop("No friend information is available. See ?getFriends for more details.")
		}
		error <- tryCatch(friends <- matrix(unlist(content$data), ncol=2, byrow=TRUE),
			error=function(e) e)
		if (inherits(error, 'error')){
			stop("No friend information is available. See ?getFriends for more details.")
		}
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
		query <- paste(base_url, "/me/friends?",
			"fields=id,name,first_name,last_name,gender,locale,birthday,location,",
			"hometown,relationship_status,picture.type(large)&limit=100", sep="")
		content <- callAPI(query, token)
		if (length(content$data)==0){
			stop("No friend information is available.")
		}
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
