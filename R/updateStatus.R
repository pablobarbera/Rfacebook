#' @rdname updateStatus
#' @export
#'
#' @title 
#' Update Facebook status from R
#'
#' @description
#' \code{updateStatus} sends a status update that will be displayed
#' on the Facebook profile of the authenticated user.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{getPost}}
#'
#' @param text string, text of the status update
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}. It needs to have extended permissions in order 
#' to successfully post to the Facebook profile.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  updateStatus("this is just a test", token=fb_oauth)
#' }
#'

updateStatus <- function(text, token)
{
	## query including text
	query <- paste('https://graph.facebook.com/me/feed?message=', text, sep="")
	## making query
	if (class(token)=="config"){
		url.data <- POST(query, config=token)
	}
	if (class(token)=="character"){
		url <- paste0(query, "&access_token=", token)
		url.data <- GET(url)
	}
	if (class(token)!="character" & class(token)!="config"){
		stop("Error in access token. See help for details.")
	}
	## output
	if (url.data$status_code==200){
		id <- fromJSON(rawToChar(url.data$content))$id
		message("Success! Link to status update:", paste("http://www.facebook.com/", id, sep=""))
	}
	if (url.data$status_code==400){
		error <- fromJSON(rawToChar(url.data$content))$error$code
		message <- fromJSON(rawToChar(url.data$content))$error$message
		if (error==2500){
			message("Failed update. OAuth token does not have permission to update status. ",
				"See ?fbOAuth for more details.")
		}
		if (error!=2500){
			message("Failed update.", message)
		}
	}
}




