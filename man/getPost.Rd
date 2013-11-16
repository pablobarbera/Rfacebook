\name{getPost}
\alias{getPost}
\title{Extract information about a public Facebook post}
\usage{
  getPost(post, token, n = 500, comments = TRUE,
    likes = TRUE)
}
\arguments{
  \item{post}{A post ID}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{n}{Maximum number of comments and likes to return.}

  \item{comments}{logical, default is \code{TRUE}, which
  will return data frame with comments to the post.}

  \item{likes}{logical, default is \code{TRUE}, which will
  return data frame with likes for the post.}
}
\description{
  \code{getPost} retrieves information about a public
  Facebook post, including list of comments and likes.
}
\details{
  \code{getPost} returns a list with three components:
  \code{post}, \code{likes}, and \code{comments}. First,
  \code{post} contains information about the post: author,
  creation date, id, counts of likes, comments, and shares,
  etc. Second, \code{likes} is a data frame that contains
  names and Facebook IDs of all the users that liked the
  post. Finally, \code{comments} is a data frame with
  information about the comments to the post (author,
  message, creation time, id).
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Getting information about Facebook's Facebook Page
	load("fb_oauth")
	fb_page <- getPage(page="facebook", token=fb_oauth)
## Getting information and likes/comments about most recent post
	post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{getUsers}}, \code{\link{getPage}},
  \code{\link{fbOAuth}}
}

