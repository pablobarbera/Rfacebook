\name{getCheckins}
\alias{getCheckins}
\title{Extract list of checkins of a Facebook friend}
\usage{
getCheckins(user, n = 10, token, tags = FALSE)
}
\arguments{
  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{user}{A user ID or screen name.}

  \item{n}{Maximum number of checkins to return for each
  user.}

  \item{tags}{If \code{TRUE}, output of function will be a
  list of two elements: a data frame with user's checkins
  and a list of data frames, where each element contains
  information about users tagged in each checkin.}
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

