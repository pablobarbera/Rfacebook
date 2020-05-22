#' @rdname getPageInfo
#' @export
#'
#' @title 
#' Extract information about a public Facebook page
#'
#' @description
#' \code{getPage} retrieves information from a public Facebook page.
#'
#'
#' @details
#' This function will only return information from public pages, not users
#' with public profiles. It makes a query per page so it might be slow if you have a lot of pages. 
#'
#' @author
#' Wouter van Atteveldt \email{wouter@@vanatteveldt.com}
#'
#' @param pages A vector of page IDs or page names.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param fields Character vector of fields to extract, e.g. name, about, category, fan_count
#' 
#' @return a data frame with one row per page with the requested fields as columns and the page name/id as row.name.
#' 
#' @examples \dontrun{
#'   getPageInfo(pages=c("cnn", "nytimes"), token = fb_token, 
#'     fields = c("id", "name", "category", "fan_count"))
#' }
#'
getPageInfo <- function(pages, token, fields="name", api=NULL){
  getSinglePage = function(page) {
	  url <- paste0('https://graph.facebook.com/', page, '?fields=', paste(fields, collapse = ","))
	  callAPI(url=url, token=token, api=api)
  }
  info = lapply(pages, getSinglePage)
  as.data.frame(do.call(rbind, info), row.names=pages)
}

