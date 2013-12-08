\name{getCheckins}
\alias{getCheckins}
\title{Extract list of checkins of a Facebook friend}
\usage{
  getCheckins(user, n = 10, token)
}
\arguments{
  \item{token}{A temporary access token created at
  \url{https://developers.facebook.com/tools/explorer}}

  \item{user}{A user ID or screen name.}

  \item{n}{Maximum number of checkins to return for each
  user.}
}
\description{
  \code{getCheckins} retrieves information about a friend's
  checkins
}
\details{
  This function requires the use of an OAuth token with the
  following permissions: user_status, user_checkins,
  friends_status, friends_checkins
}
\examples{
\dontrun{
 token <- 'XXXXX'
 my_checkins <- getCheckins(user="me", token=token)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{getFriends}}
}

