\name{getPage}
\alias{getPage}
\title{Extract list of posts from a public Facebook page}
\usage{
  getPage(page, token, n = 100)
}
\arguments{
  \item{page}{A page ID or page name.}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{n}{Number of posts of page to return. Note that
  number can be sometimes higher or lower, depending on
  status of API.}
}
\description{
  \code{getPage} retrieves information from a public
  Facebook page. Note that information about users that
  have turned on the "follow" option on their profile can
  also be retrieved with this function.
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
  \code{\link{getUsers}}, \code{\link{getPost}},
  \code{\link{fbOAuth}}
}

