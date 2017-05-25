#' @rdname getPageToken
#' @export
#'
#' @title 
#' Get a page access token
#' 
#' @description
#' Gets a page access token that can be used to e.g. get insights for a page.
#' 
#' @param page A page ID or page name.
#' 
#' @param token the token (with scope 'manage_pages') of a user that has admin access to the page
#'
#' @return the page access token string
#'
#' @examples
#' \dontrun{
#' ## Get a normal access token with manage_pages scope
#' token = fbOAuth(app_id, app_secret, scope="manage_pages")
#' ## Get a page access token for a page
#' page_token = getPageToken(page, token)
#' ## Get page insights
#' getInsights(page, token=page_token, metric = "page_impressions")
#' }
getPageToken <- function(page, token)
{
  url = paste0("https://graph.facebook.com/", page, "?fields=access_token")
  callAPI(url, token=token)$access_token
}