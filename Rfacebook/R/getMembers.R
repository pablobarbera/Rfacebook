#' @rdname getMembers
#' @export
#' @title
#' Retrieve members from a public group
#'
#' @description
#' \code{getMembers} retrieves members from a public group, up to 5000 members.
#'
#' @param page A group ID
#'
#' @author
#' Yan Turgeon
#' @seealso \code{\link{getPage}}, \code{\link{getPost}}, \code{\link{getCommentReplies}}

#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#' 
#' @examples \dontrun{
#' ## Find Facebook ID for R-Users Facebook group
#'	load("fb_oauth")
#'	ids <- searchGroup(name="rusers", token=fb_oauth)
#'  ids[1,] # id = 18533493739
#' ## Retrieves members ID for R-Users Facebook group
#'  members <- getMembers(group_id="18533493739", token=fb_oauth)
#'}

getMembers <- function(group_id, token, n=5000, api=NULL){
    url <- paste0('https://graph.facebook.com/', group_id,
                  '/members?fields=id,name,first_name,last_name,administrator&limit=', n)

    # making query
  content <- callAPI(url=url, token=token)
 
 #if no data, return error message
  if (length(content$data)==0){ 
    message("No groups with this name were found.")
    return(data.frame())
  }
  
  # if data, return a data frame
  df <- data.frame(
    name = unlist(lapply(content$data, '[[', 'name')),
    id = unlist(lapply(content$data, '[[', 'id')),
    administrator = unlist(lapply(content$data, '[[', 'administrator')),
    stringsAsFactors=F)
  
  return(df)
}
