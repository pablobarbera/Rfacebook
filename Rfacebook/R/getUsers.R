#' @rdname getUsers
#' @export
#'
#' @title 
#' Extract information about one or more Facebook users
#'
#' @description
#' \code{getUsers} retrieves public information about one or more Facebook users.
#'
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API. All the remaining fields will be missing.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}, \code{\link{getPost}}, \code{\link{searchFacebook}}
#'
#' @param users A vector of user IDs.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param private_info If \code{FALSE}, will return only information that is 
#' publicly available for all users (name, gender, locale, profile picture). 
#' If \code{TRUE}, will return additional information for users who are friends 
#' with the authenticated user: birthday, location, hometown, and relationship 
#' status. Note that these fields will ONLY be returned for friends and when
#' the version of the token that is used to query the API is 1.0. For other 
#' users, they will be \code{NA}, even if they are visible on Facebook via web.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about the authenticated user
#'  load("fb_oauth")
#'	fb <- getUsers("me", token=fb_oauth)
#'	fb$username
#' }
#'

getUsers <- function(users, token, private_info=FALSE)
{

	tkversion <- getTokenVersion(token)
	api.max <- ifelse(tkversion=='v2', 50, 500)

	if (length(users)==1 && users=='me'){
		query <- paste0('https://graph.facebook.com/',
			ifelse(tkversion=='v2', 'v2.0/', ''), 'me?')
		content <- callAPI(query, token)
		df <- userDataToDF(list(content), private_info=private_info)
		return(df)
	}

	n.users <- length(users)
	first.n <- ifelse(n.users > api.max, api.max, n.users)
	
	users.query <- paste(users[1:first.n], collapse=",")
	## first query: checking what users are actual users vs pages
	query <- paste0('https://graph.facebook.com/',
		ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', users.query)
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
		query <- paste('https://graph.facebook.com/',
			ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
			paste(names(actual.users), collapse=","), 
			"&fields=id,name,first_name,middle_name,last_name,gender,locale,birthday,",
			"location,hometown,relationship_status,picture.type(large)", sep="")
		}
		if (private_info==FALSE){
		query <- paste('https://graph.facebook.com/',
			ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
			paste(names(actual.users), collapse=","),
			"&fields=id,name,first_name,middle_name,last_name,gender,locale,",
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
		query <- paste('https://graph.facebook.com/',
			ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
			paste(names(pages), collapse=","), 
			"&fields=id,name,category,likes,picture.type(large)", sep="")
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
	if (n.users > api.max){
		n.done <- dim(df)[1]
		cat(n.done, "users -- ")
		while (n.done < n.users){
			first.n <- n.done + 1
			last.n <- ifelse(n.done + api.max > n.users, n.users, n.done + api.max)
			users.query <- paste(users[first.n:last.n], collapse=",")
			## first query: checking what users are actual users vs pages
			query <- paste0('https://graph.facebook.com/',
				ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', users.query)
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
				query <- paste('https://graph.facebook.com/', 
					ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
					paste(names(actual.users), collapse=","), 
					"&fields=id,name,first_name,middle_name,last_name,gender,locale,birthday,",
					"location,hometown,relationship_status,picture.type(large)", sep="")
				}
				if (private_info==FALSE){
				query <- paste('https://graph.facebook.com/', 
					ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
					paste(names(actual.users), collapse=","),
					"&fields=id,name,first_name,middle_name,last_name,gender,locale,",
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
				query <- paste('https://graph.facebook.com/', 
					ifelse(tkversion=='v2', 'v2.0/', ''), '?ids=', 
					paste(names(pages), collapse=","), 
					"&fields=id,name,category,likes,picture.type(large)", sep="")
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




