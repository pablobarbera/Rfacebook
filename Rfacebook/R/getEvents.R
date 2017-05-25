#' @rdname getEvents
#' @export
#'
#' @title 
#' Extract list of events from a public Facebook page or group
#'
#' @description
#' \code{getEvents} retrieves event information from a public Facebook group or page.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getPage}}, \code{\link{fbOAuth}}
#'
#' @param group_id Facebook ID for the group or page.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param api API version. e.g. "v2.8". \code{NULL} is the default.
#'
#' @examples \dontrun{
#'	load("fb_oauth")
#' ## Downloading events from Playa Vista Farmers' Market
#'  events <- getEvents(page="playavistaFM", token=fb_oauth)
#' }

getEvents <- function(page, token, api="v2.9"){

	url <- paste0('https://graph.facebook.com/', page,
		'?fields=events{description,name,start_time,end_time,place,id,attending_count,',
		'declined_count,maybe_count,noreply_count}')

	# making query
	content <- callAPI(url=url, token=token, api=api)
	l <- length(content$events$data); cat(l, "events ")
	
	## retrying 3 times if error was found
	error <- 0
	while (length(content$error_code)>0){
		cat("Error!\n")
		Sys.sleep(0.5)
		error <- error + 1
		content <- callAPI(url=url, token=token, api=api)		
		if (error==3){ stop(content$error_msg) }
	}
	if (length(content$events$data)==0){ 
		message("No public events were found")
		return(data.frame())
	}
	df <- eventDataToDF(content$events$data)

	return(df)
}


