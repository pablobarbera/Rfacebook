#' @rdname getNetwork
#' @export
#'
#' @title 
#' Extract network of friends of authenticated user
#'
#' @description
#' \code{getNetwork} retrieves list of mutual friendships and returns the
#' adjacency matrix or edge list for the network representing the neighborhood
#' of the authenticated user.
#'
#' @details
#' 
#' This function requires the use of an OAuth token with extended permissions.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getFriends}}, \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param format Either "edgelist" for a list of edges in the network or 
#' "adj.matrix" for an adjacenty matrix of dimensions (n x n), with n being
#' the number of friends, and 0 or 1 indicating whether friend i is also friends
#' with friend j.
#'
#' @param verbose logical, default is \code{TRUE}, which will print additional
#' information on the console.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting my network of friends
#'  load("fb_oauth")
#'  mat <- getNetwork(token=fb_oauth, format="adj.matrix")
#'  library(igraph)
#'  network <- graph.adjacency(mat, mode="undirected")
#'  pdf("network_plot.pdf")
#'  plot(network)
#'  dev.off()
#' }
#'

getNetwork <- function(token, format='edgelist', verbose=TRUE){
	if (format %in% c("edgelist", "adj.matrix") == FALSE){
		stop("format not recognized. Choose either 'edgelist' or 'adj.matrix'.")
	}
	friends <- getFriends(token=token, simplify=TRUE)
	edge.list <- NULL
	n <- length(friends$id)
	if (verbose==TRUE){ pb <- txtProgressBar(min=1,max=n, style=3) }
	for (i in 1:n){
		query <- paste0("https://graph.facebook.com/me/mutualfriends/", friends$id[i], "?")
		content <- callAPI(query, token)
		mutual.friends <- unlist(lapply(content[[1]], '[[', 'name'))
		for (friend in mutual.friends){
			edge.list <- rbind(edge.list, c(friends$name[i], friend))
		}
		if (verbose==TRUE){ setTxtProgressBar(pb, i) }
	}
	if (format=="adj.matrix"){
		adj.matrix <- matrix(
			unlist(lapply(friends$name, function(x) 
			friends$name %in% edge.list[edge.list[,1]==x,2])),
			ncol=n, nrow=n)
		dimnames(adj.matrix) <- list(friends$name, friends$name)
		
	}
	if (format=="edgelist"){ return(edge.list) }
	if (format=="adj.matrix") { return(adj.matrix) }
}





