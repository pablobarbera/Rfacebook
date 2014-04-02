\name{getFriends}
\alias{getFriends}
\title{Extract list of friends with their information}
\usage{
getFriends(token, simplify = FALSE)
}
\arguments{
  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{simplify}{If \code{TRUE}, function will return only
  name and id for each friend. If \code{FALSE}, it will
  return additional information from their profiles:
  gender, birthday, location, hometown, relationship status
  and profile picture.}
}
\description{
\code{getFriends} retrieves information about the user's
friends.
}
\details{
This function requires the use of an OAuth token with
extended permissions.
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Getting information about my friends
	load("fb_oauth")
	my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
## Since users are ordered by ID, this will return 10 oldest user accounts
	head(my_friends, n=10)
}
}
\author{
Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
\code{\link{getUsers}}, \code{\link{fbOAuth}}
}

