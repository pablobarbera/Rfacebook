\name{searchFacebook}
\alias{searchFacebook}
\title{Search public posts that mention a string}
\usage{
  searchFacebook(string, token, n = 200, since = NULL,
    until = NULL)
}
\arguments{
  \item{string}{string or string vector containing keywords
  to search. Note that the returned results will contain
  any of the keywords. It is not possible to search for
  status updates that include all of the keywords.}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{n}{Maximum number of posts to return.}

  \item{since}{A UNIX timestamp or strtotime data value
  that points to the start of the time range to be
  searched. For more information on the accepted values,
  see:
  \url{http://php.net/manual/en/function.strtotime.php}}

  \item{until}{A UNIX timestamp or strtotime data value
  that points to the end of the time range to be searched.
  For more information on the accepted values, see:
  \url{http://php.net/manual/en/function.strtotime.php}}
}
\description{
  \code{searchFacebook} retrives public status updates that
  mention a given keyword
}
\details{
  For now, \code{searchFacebook} only accepts a single
  string due to limitations of the Facebook API.

  The search is performed also on the text of the comments
  too, which explains why some of the returned messages do
  not mention the string that is being searched.

  Note that messages only around two weeks old or less can
  be returned.
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Searching 100 public posts that mention "facebook"
	load("fb_oauth")
	posts <- searchFacebook( string="facebook", token=fb_oauth, n=100 )
## Searching 100 public posts that mention "facebook" from yesterday
	posts <- searchFacebook( string="facebook", token=fb_oauth, n=100 ,
		since = "yesterday 00:00", until = "yesterday 23:59")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{fbOAuth}}
}

