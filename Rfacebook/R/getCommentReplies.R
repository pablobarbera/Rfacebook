#' @rdname getCommentReplies
#' @export
#'
#' @title 
#' Extract replies to comments on page post
#'
#' @description
#' \code{getCommentReplies} retrieves the list of comments replying to 
#' an individual comment on a post by a public Facebook page.
#'
#' @details 
#' \code{getCommentReplies} returns a list with three components: \code{comment}, 
#' \code{replies}, and \code{likes}. First, \code{comment} contains information
#' about the original comment: author, creation date, id, counts of likes and comments,
#' etc. Second, \code{replies} is a data frame with information about the replies
#' to the comment (author, message, creation time, id). Finally, \code{likes} is 
#' data frame that contains names and Facebook IDs of all the users that liked the comment.
#'
#' @author
#' Yan Turgeon
#' @seealso \code{\link{getPage}}, \code{\link{getPost}}
#'
#' @param comment_id A comment ID
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of replies and likes to return.
#'
#' @param replies logical, default is \code{TRUE}, which will return data frame
#' with replies to the post.
#'
#' @param likes logical, default is \code{TRUE}, which will return data frame
#' with likes for the post.
#'
#' @param n.likes numeric, maximum number of likes to return. Default is \code{n}.
#'
#' @param n.replies numeric, maximum number of replies to return. Default is 
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'  load("fb_oauth")
#'  fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting information and likes/comments about most recent post
#'  post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
#' ## Downloading list of replies to first comment
#'  replies <- getCommentReplies(comment_id=post$id[1], token=fb_oauth)
#' }
#'

getCommentReplies <- function(comment_id, token, n=500, replies=TRUE, likes=FALSE, n.likes=n,
                     n.replies=n){
  
  url <- paste0("https://graph.facebook.com/", comment_id,
                "?fields=from,message,created_time,like_count,comment_count") #return initial comments
  
  if (replies==TRUE){
    url <- paste0(url, ",comments.summary(true).",
                  "fields(from,id,message,created_time,like_count)") #return reply
    if (n.replies>=500){
      url <- paste0(url, ".limit(500)")
    }
    if (n.replies<500){
      url <- paste0(url, ".limit(", n.replies, ")")
    }
  }
  if (replies==FALSE){
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
    message("Comment could not be found")
    return(data.frame())
  }
  
  # putting it together
  out <- list()
  out[["comment"]] <- replyDataToDF(content)
  if (likes && n.likes > 0) out[["likes"]] <- likesDataToDF(content$likes$data)
  if (likes && n.likes > 0) n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  if (n.likes == 0) n.l <- 0
  if (!likes) n.l <- Inf
  if (replies && n.likes > 0) out[["replies"]] <- repliesDataToDF(content$comments$data)
  if (replies && n.likes > 0) n.c <- ifelse(!is.null(out$replies), dim(out$replies)[1], 0)
  if (n.replies == 0) n.c <- 0
  if (!replies) n.c <- Inf
  
  # paging if we n.comments OR n.likes haven't been downloaded
  if (n.likes > n.l || n.replies > n.c){
    # saving URLs for next batch of likes and comments
    if (likes) url.likes <- content$likes$paging$`next`
    if (!likes) url.likes <- NULL
    if (replies) url.comments <- content$comments$paging$`next`
    if (!replies) url.comments <- NULL
    
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
    if (!is.null(url.comments) && replies && n.replies > n.c){
      # retriving next batch of comments
      content <- callAPI(url=url.comments, token=token)
      out[["replies"]] <- rbind(out[["replies"]],
                                 repliesDataToDF(content$data))
      n.c <- dim(out$replies)[1]
      # next comments, in batches of 500
      while (n.c < n.replies & length(content$data)>0 &
             !is.null(content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token)
        out[["replies"]] <- rbind(out[["replies"]],
                                   repliesDataToDF(content$data))
        n.c <- dim(out$replies)[1]
      }
    }
  }
  
  return(out)
}