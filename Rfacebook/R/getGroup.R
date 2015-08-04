#' @rdname getGroup
#' @export
#'
#' @title 
#' Extract list of posts from a public Facebook group
#'
#' @description
#' \code{getGroup} retrieves information from a public Facebook group.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{getPost}}, \code{\link{fbOAuth}}
#'
#' @param group_id Facebook ID for the group. Note that this is different from
#' the name on the URL. You can use \code{searchGroup} to find the ID.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Number of posts of page to return. Note that number can be sometimes
#' higher or lower, depending on status of API.
#'
#' @param since A UNIX timestamp or strtotime data value that points to
#' the start of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#' @param until A UNIX timestamp or strtotime data value that points to
#' the end of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#'
#' @examples \dontrun{
#' ## Find Facebook ID for R-Users Facebook group
#'	load("fb_oauth")
#'	ids <- searchGroup(name="rusers", token=fb_oauth)
#'  ids[1,] # id = 18533493739
#' ## Downloading posts from R-Users Facebook group
#'  group <- getGroup(group_id=18533493739, token=fb_oauth)
#' ## Downloading posts from R-Users Facebook group in January 2013
#'  group <- getGroup(group_id=18533493739, token=fb_oauth,
#'		since='2013/01/01', until='2013/01/31')
#' }

getGroup <- function(group_id, token, n=100, since=NULL, until=NULL){

	url <- paste0('https://graph.facebook.com/', group_id,
		'/feed?fields=from,message,created_time,type,link,comments.summary(true)',
		',likes.summary(true),shares')
	if (!is.null(until)){
		url <- paste0(url, '&until=', until)
	}
	if (!is.null(since)){
		url <- paste0(url, '&since=', since)
	}
	if (n<=100){
		url <- paste0(url, "&limit=", n)
	}
	if (n>100){
		url <- paste0(url, "&limit=100")
	}
	# making query
	content <- callAPI(url=url, token=token)
	l <- length(content$data); cat(l, "posts ")
	
	## retrying 3 times if error was found
	error <- 0
	while (length(content$error_code)>0){
		cat("Error!\n")
		Sys.sleep(0.5)
		error <- error + 1
		content <- callAPI(url=url, token=token)		
		if (error==3){ stop(content$error_msg) }
	}
	if (length(content$data)==0){ 
		stop("No public posts were found")
	}
	df <- pageDataToDF(content$data)

	# sometimes posts older than 'until' are returned, so here
	# I'm making sure the function stops when that happens
	if (!is.null(since)){
		dates <- formatFbDate(df$created_time, 'date')
		mindate <- min(dates)
		sincedate <- as.Date(since)
	}
	if (is.null(since)){
		sincedate <- as.Date('1970/01/01')
		mindate <- as.Date(Sys.time())
	}

	## paging if n>100
	if (n>100){
		df.list <- list(df)
		while (l<n & length(content$data)>0 & 
			!is.null(content$paging$`next`) & sincedate <= mindate){
			# waiting one second before making next API call...
			Sys.sleep(0.5)
			url <- content$paging$`next`
			content <- callAPI(url=url, token=token)
			l <- l + length(content$data)
			if (length(content$data)>0){ cat(l, "posts ") }

			## retrying 3 times if error was found
			error <- 0
			while (length(content$error_code)>0){
				cat("Error!\n")
				Sys.sleep(0.5)
				error <- error + 1
				content <- callAPI(url=url, token=token)		
				if (error==3){ stop(content$error_msg) }
			}
			new.df <- pageDataToDF(content$data)
			df.list <- c(df.list, list(new.df))

			if (!is.null(since)){
				dates <- formatFbDate(new.df$created_time, 'date')
				mindate <- min(dates)
			}
		}
		df <- do.call(rbind, df.list)
	}
	# deleting posts after specified date
	if (!is.null(since)){
		dates <- formatFbDate(df$created_time, 'date')
		df <- df[dates>=sincedate,]
	}
	return(df)
}


#' @rdname searchGroup
#' @export
#' @title 
#' Find Facebook ID of a group
#'
#' @description
#' Use \code{searchGroup} in combination with \code{getGroup} to scrape
#' public posts on Facebook groups.
#'
#' @param name Name of Facebook group (in URL)
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.

#' @examples \dontrun{
#' ## Find Facebook ID for R-Users Facebook group
#'	load("fb_oauth")
#'	ids <- searchGroup(name="rusers", token=fb_oauth)
#'  ids[1,] # id = 18533493739
#' ## Downloading posts from R-Users Facebook group
#'  group <- getGroup(group_id=18533493739, token=fb_oauth)
#' ## Downloading posts from R-Users Facebook group in January 2013
#'  group <- getGroup(group_id=18533493739, token=fb_oauth,
#'		since='2013/01/01', until='2013/01/31')
#' }

searchGroup <- function(name, token){
	url <- paste0('https://graph.facebook.com/search?q=',
		name, '&type=group')
	# making query
	content <- callAPI(url=url, token=token)

	# if no data, return error message
	if (length(content$data)==0){ 
		stop("No groups with this name were found.")
	}

	# if data, return a data frame
	df <- data.frame(
		name = unlist(lapply(content$data, '[[', 'name')),
		privacy = unlist(lapply(content$data, '[[', 'privacy')),
		id = unlist(lapply(content$data, '[[', 'id')),
		stringsAsFactors=F)

	return(df)
}
