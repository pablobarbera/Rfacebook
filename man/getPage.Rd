\name{getPage}
\alias{getPage}
\title{Extract list of posts from a public Facebook page}
\usage{
  getPage(page, token, n = 200, simplify = TRUE)
}
\arguments{
  \item{page}{A page ID or page name.}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}
}
\description{
  \code{getPage} retrives information from a public
  Facebook page.
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Getting information about Facebook's Facebook Page
	load("fb_oauth")
	fb_page <- getPage(users="facebook", token=fb_oauth)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{getUsers}}, \code{\link{getPosts}},
  \code{\link{fbOAuth}}
}

