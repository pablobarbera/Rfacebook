#' @rdname getPost
#' @export
#'
#' @title 
#' Extract information about a public Facebook post
#'
#' @description
#' \code{getPost} retrieves information about a public Facebook post, including
#' list of comments and likes.
#'
#' @details 
#' \code{getPost} returns a list with three components: \code{post}, 
#' \code{likes}, and \code{comments}. First, \code{post} contains information
#' about the post: author, creation date, id, counts of likes, comments, and 
#' shares, etc. Second, \code{likes} is a data frame that contains names and
#' Facebook IDs of all the users that liked the post. Finally, \code{comments}
#' is a data frame with information about the comments to the post (author, 
#' message, creation time, id).
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{getPage}}, \code{\link{fbOAuth}}
#'
#' @param post A post ID
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of comments and likes to return.
#'
#' @param comments logical, default is \code{TRUE}, which will return data frame
#' with comments to the post.
#'
#' @param likes logical, default is \code{TRUE}, which will return data frame
#' with likes for the post.
#'
#' @param n.likes numeric, maximum number of likes to return. Default is \code{n}.
#'
#' @param n.comments numeric, maximum number of likes to return. Default is 
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting information and likes/comments about most recent post
#'	post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
#' }
#'

getPost <- function(post, token, n=500, comments=TRUE, likes=TRUE, n.likes=n,
	n.comments=n){

	url <- paste0("https://graph.facebook.com/", post,
				"?fields=from,message,created_time,type,link,name,shares")

	if (comments==TRUE){
		url <- paste0(url, ",comments.summary(true).",
			"fields(id,from,message,created_time,like_count)")
		if (n.comments>=500){
			url <- paste0(url, ".limit(500)")
		}
		if (n.comments<500){
			url <- paste0(url, ".limit(", n.comments, ")")
		}
	}
	if (comments==FALSE){
		url <- paste0(url, ",comments.summary(true)")
	}
	if (likes==TRUE){
		url <- paste0(url, ",likes.summary(true).",
			"fields(id,name)")
		if (n.likes>=2000){
			url <- paste0(url, ".limit(2000)")
		}
		if (n.likes<2000){
			url <- paste0(url, ".limit(", n.likes, ")")
		}
	}
	if (likes==FALSE){
		url <- paste0(url, ",likes.summary(true)")
	}

	# making query
	content <- callAPI(url=url, token=token)

	# error traps: retry 3 times if error
	error <- 0
	while (length(content$error_code)>0){
		cat("Error!\n")
		Sys.sleep(0.5)
		error <- error + 1
		content <- callAPI(url=url, token=token)		
		if (error==3){ stop(content$error_msg) }
	}
	if (length(content)==0){ 
		stop("Post could not be found")
	}

	# putting it together
	out <- list()
	out[["post"]] <- postDataToDF(content)
	if (likes && n.likes > 0) out[["likes"]] <- likesDataToDF(content$likes$data)
	if (likes && n.likes > 0) n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
	if (n.likes == 0) n.l <- 0
	if (!likes) n.l <- Inf
	if (comments && n.likes > 0) out[["comments"]] <- commentsDataToDF(content$comments$data)
	if (comments && n.likes > 0) n.c <- ifelse(!is.null(out$comments), dim(out$comments)[1], 0)
	if (n.comments == 0) n.c <- 0
	if (!comments) n.c <- Inf
	
	# paging if we n.comments OR n.likes haven't been downloaded
	if (n.likes > n.l || n.comments > n.c){
		# saving URLs for next batch of likes and comments
		if (likes) url.likes <- content$likes$paging$`next`
		if (!likes) url.likes <- NULL
		if (comments) url.comments <- content$comments$paging$`next`
		if (!comments) url.comments <- NULL

		if (!is.null(url.likes) && likes && n.likes > n.l){
			# retrieving next batch of likes
			url <- content$likes$paging$`next`
			content <- callAPI(url=url.likes, token=token)
			out[["likes"]] <- rbind(out[["likes"]],
					likesDataToDF(content$data))
			n.l <- dim(out$likes)[1]
			# next likes, in batches of 500
			while (n.l < n.likes & length(content$data)>0 &
				!is.null(url <- content$paging$`next`)){
				url <- content$paging$`next`
				content <- callAPI(url=url, token=token)
				out[["likes"]] <- rbind(out[["likes"]],
					likesDataToDF(content$data))
				n.l <- dim(out$likes)[1]
			}
		}
		if (!is.null(url.comments) && comments && n.comments > n.c){
			# retriving next batch of comments
			content <- callAPI(url=url.comments, token=token)
			out[["comments"]] <- rbind(out[["comments"]],
					commentsDataToDF(content$data))
			n.c <- dim(out$comments)[1]
			# next comments, in batches of 500
			while (n.c < n.comments & length(content$data)>0 &
				!is.null(content$paging$`next`)){
				url <- content$paging$`next`
				content <- callAPI(url=url, token=token)
				out[["comments"]] <- rbind(out[["comments"]],
					commentsDataToDF(content$data))
				n.c <- dim(out$comments)[1]
			}
		}
	}

	return(out)
}
