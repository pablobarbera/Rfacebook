\name{getUsers}
\alias{getUsers}
\title{Extract information about one or more Facebook users}
\usage{
  getUsers(users, token, private_info = FALSE)
}
\arguments{
  \item{users}{A vector of either user IDs or screen names
  or a mix of both.}

  \item{token}{Either a temporary access token created at
  \url{https://developers.facebook.com/tools/explorer} or
  the OAuth token created with \code{fbOAuth}.}

  \item{private_info}{If \code{FALSE}, will return only
  information that is publicly available for all users
  (name, gender, locale, profile picture). If \code{TRUE},
  will return additional information for users who are
  friends with the authenticated user: birthday, location,
  hometown, and relationship status. Note that these fields
  will only be returned for friends. For other users, they
  will be \code{NA}, even if they are visible on Facebook
  via web.}
}
\description{
  \code{getUsers} retrieves public information about one or
  more Facebook users.
}
\examples{
\dontrun{
## See examples for fbOAuth to know how token was created.
## Getting information about me
 load("fb_oauth")
	me <- getUsers("me", token=fb_oauth, private_info=FALSE)
	me$username
## Getting information about my friends
	my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
	my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)
	table(my_friends_info$relationship_status)
## Searching public posts about facebook and looking at gender of users
 fb.posts <- searchFacebook( "facebook", token=fb_oauth, n=100)
	fb.users <- getUsers( fb.posts$from_id, token=fb_oauth )
	table(fb.users$gender)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{getFriends}}, \code{\link{getPost}},
  \code{\link{searchFacebook}}
}

