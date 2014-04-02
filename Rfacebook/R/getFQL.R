#' @rdname getFQL
#' @export
#'
#' @title 
#' Executes a FQL query to the Facebook Graph API
#'
#' @description
#' \code{getFQL} connects to Facebook's Graph API and executes a FQL query.
#' See \url{https://developers.facebook.com/docs/technical-guides/fql/} for
#' an overview of the Facebook Query Language.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param query Text of query
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting list of friends of authenticated user
#'	load("fb_oauth")
#'	d <- getFQL("SELECT uid2 FROM friend WHERE uid1=me()", token=fb_oauth)
#' }
#'

getFQL <- function(query, token){

	# cleaning text of query
	query <- gsub(" ", "+", query)
	url <- paste0('https://graph.facebook.com/fql?q=', query)
	
	# making query
	content <- callAPI(url=url, token=token)
	
	return(content$data)
}
