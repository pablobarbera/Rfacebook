#' @rdname getUsers
#' @export
#'
#' @title 
#' Extract information about one or more Facebook users
#'
#' @description
#' \code{getUsers} retrieves public information about one or more Facebook users.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}, \code{\link{getPost}}, \code{\link{searchFacebook}}
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
#'  load("fb_oauth")
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

	n.users <- length(users)
	first.n <- ifelse(n.users > 500, 500, n.users)
	
	users.query <- paste(users[1:first.n], collapse=",")
	## first query: checking what users are actual users vs pages
	query <- paste0('https://graph.facebook.com/?ids=', users.query)
	## making query
	content <- callAPI(query, token)
	if (length(content$error_code)>0){
		stop(content$error_msg, ". Querying too many users?")
	}	
	actual.users <- which(unlist(lapply(content, function(x) is.null(x$category))))
	pages <- which(unlist(lapply(content, function(x) !is.null(x$category))))	
	## getting data for users	
	if (length(actual.users)>0){
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
	}
	if (length(actual.users)==0){
		df.users <- NULL
	}
	if (length(pages)>0){
		## getting data for pages
		query <- paste('https://graph.facebook.com/?ids=', 
			paste(names(pages), collapse=","), 
			"&fields=id,name,username,category,likes,picture.type(large)", sep="")
		## making query
		content <- callAPI(query, token)
		df.pages <- userDataToDF(content, private_info=private_info)	
	}
	if (length(pages)==0){
		df.pages <- NULL
	}
	df <- rbind(df.users, df.pages)
	#df <- df[!is.na(df$id),]

	# next users, in batches of 500
	if (n.users > 500){
		n.done <- dim(df)[1]
		cat(n.done, "users -- ")
		while (n.done < n.users){
			first.n <- n.done + 1
			last.n <- ifelse(n.done + 500 > n.users, n.users, n.done + 500)
			users.query <- paste(users[first.n:last.n], collapse=",")
			## first query: checking what users are actual users vs pages
			query <- paste0('https://graph.facebook.com/?ids=', users.query)
			## making query
			content <- callAPI(query, token)
			if (length(content$error_code)>0){
				stop(content$error_msg, ". Querying too many users?")
			}	
			actual.users <- which(unlist(lapply(content, function(x) is.null(x$category))))
			pages <- which(unlist(lapply(content, function(x) !is.null(x$category))))	
			## getting data for users	
			if (length(actual.users)>0){
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
			}
			if (length(actual.users)==0){
				df.users <- NULL
			}
			if (length(pages)>0){
				## getting data for pages
				query <- paste('https://graph.facebook.com/?ids=', 
					paste(names(pages), collapse=","), 
					"&fields=id,name,username,category,likes,picture.type(large)", sep="")
				## making query
				content <- callAPI(query, token)
				df.pages <- userDataToDF(content, private_info=private_info)	
			}
			if (length(pages)==0){
				df.pages <- NULL
			}
			df <- rbind(df, df.users, df.pages)
			#df.new <- df[!is.na(df$id),]
			n.done <- dim(df)[1]
			cat(n.done, "users -- ")
		}
	}
	# returning in original order of users
	df <- df[order(match(df$id, users)),]
	return(df)
}




