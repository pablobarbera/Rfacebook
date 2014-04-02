\name{getPage}
\alias{getPage}
\title{Extract list of posts from a public Facebook page}
\usage{
getPage(page, token, n = 100, feed = FALSE)
}
\arguments{
  \item{page}{A page ID or page name.}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{n}{Number of posts of page to return. Note that
  number can be sometimes higher or lower, depending on
  status of API.}

  \item{feed}{If \code{TRUE}, the function will also return
  posts on the page that were made by others (not only the
  admin of the page).}
}
\description{
\code{getPage} retrieves information from a public Facebook
page. Note that information about users that have turned on
the "follow" option on their profile can also be retrieved
with this function.
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Getting information about Facebook's Facebook Page
	load("fb_oauth")
	fb_page <- getPage(page="facebook", token=fb_oauth)
## Getting posts on Humans of New York page, including posts by others users
## (not only owner of page)
 page <- getPage(page="humansofnewyork", token=fb_oauth, feed=TRUE)
}
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
\code{\link{getUsers}}, \code{\link{getPost}},
\code{\link{fbOAuth}}
}

