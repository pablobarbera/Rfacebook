#' @rdname getReactions
#' @export
#'
#' @title 
#' Extract total count of reactions to one or more Facebook posts
#'
#' @description
#' \code{getReactions} retrieves information from a single or multiple posts,
#' returning the total count of reactions of each type (like, love, haha,
#' wow, sad, angry).
#'
#' @details
#' The solution implemented here is based on this Stack Overflow response:
#' \url{http://stackoverflow.com/questions/36930414/how-can-i-get-facebook-graph-api-reaction-summary-count-separately}
#'
#' @author
#' Pablo Barbera \email{pbarbera@@usc.edu}
#' @seealso \code{\link{getPage}}, \code{\link{getPost}}
#'
#' @param post A post ID, or a vector of post IDs
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param verbose logical, default is \code{TRUE}, which will print additional
#' information on the console.
#'
#' @param api API version. e.g. "v2.8". \code{NULL} is the default.
#' 
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting reactions for most recent post
#'	post <- getReactions(post=fb_page$id[1], token=fb_oauth)
#' }
#'


getReactions <- function(post, token, verbose=TRUE, api=NULL){

	add_to_url <- paste0("?fields=reactions.type(LIKE).limit(0).summary(true).as(like),",
		"reactions.type(LOVE).limit(0).summary(true).as(love),",
		"reactions.type(HAHA).limit(0).summary(true).as(haha),",
		"reactions.type(WOW).limit(0).summary(true).as(wow),",
		"reactions.type(SAD).limit(0).summary(true).as(sad),",
		"reactions.type(ANGRY).limit(0).summary(true).as(angry)")
	reactions <- data.frame()

	if (verbose==TRUE){ pb <- utils::txtProgressBar(min=0,max=length(post), style=3) }
	i = 0
	for (p in as.character(post)){
		url <- paste0('https://graph.facebook.com/', p, add_to_url)
		# making query
		content <- callAPI(url=url, token=token, api=api)
		# DF with results
		new.df <- data.frame(
			id = p,
			likes_count = ifelse(!is.null(content$like$summary$total_count),
				content$like$summary$total_count, 0),
			love_count = ifelse(!is.null(content$love$summary$total_count),
				content$love$summary$total_count, 0),
			haha_count = ifelse(!is.null(content$haha$summary$total_count),
				content$haha$summary$total_count, 0),
			wow_count = ifelse(!is.null(content$wow$summary$total_count),
				content$wow$summary$total_count, 0),
			sad_count = ifelse(!is.null(content$sad$summary$total_count),
				content$sad$summary$total_count, 0),
			angry_count = ifelse(!is.null(content$angry$summary$total_count),
				content$angry$summary$total_count, 0),
			stringsAsFactors=FALSE)
		reactions <- rbind(reactions, new.df)
		if (verbose==TRUE){ i <- i + 1; utils::setTxtProgressBar(pb, i) }
	}

	return(reactions)

}






