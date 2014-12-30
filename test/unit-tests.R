library(devtools)
install_github("pablobarbera/Rfacebook", subdir="Rfacebook")

library(devtools)
setwd("~/Dropbox/git/Rfacebook")
document("Rfacebook")
build("Rfacebook")
install.packages("Rfacebook_0.5.tar.gz", repo=NULL, type="source")


library(Rfacebook)
load('test/credentials.dta')
source('test/creating-credentials.r')


##########################################
## fbOauth
##########################################

## test of Facebook app, v1.0

app_id = credentials[['v1']][['app_id']]
app_secret = credentials[['v1']][['app_secret']]

fb_oauth <- fbOAuth(app_id, app_secret)
class(fb_oauth)

save(fb_oauth, file="~/Dropbox/credentials/facebook/tokenv1")
load("~/Dropbox/credentials/facebook/tokenv1")

me <- getUsers("me", token=fb_oauth)
me$username
me$id
me$gender
class(fb_oauth)

me <- getUsers("pablobarbera", token=fb_oauth)

## test of Facebook app, v2.x

app_id = credentials[['v2']][['app_id']]
app_secret = credentials[['v2']][['app_secret']]

fb_oauth <- fbOAuth(app_id, app_secret)

save(fb_oauth, file="~/Dropbox/credentials/facebook/tokenv2")
load("~/Dropbox/credentials/facebook/tokenv2")

me <- getUsers("me", token=fb_oauth)
me$username
me$id
me$gender
class(fb_oauth)

me <- getUsers("pablobarbera", token=fb_oauth)

callAPI('https://graph.facebook.com/v2.2/me', fb_oauth)

##########################################
## getUsers
##########################################

load("~/Dropbox/credentials/facebook/tokenv1")
friends <- getFriends(fb_oauth, simplify=TRUE)
obama <- getPage("barackobama", fb_oauth, n=1)
post <- getPost(obama$id, fb_oauth, comments=TRUE, n.comments=120, likes=FALSE)
testusers <- post$comments$from_id

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
getUsers('me', fb_oauth)
getUsers('me', fb_oauth, private_info=TRUE)
users <- getUsers(friends$id, fb_oauth)
str(users)
users <- getUsers(testusers, fb_oauth)
str(users)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
getUsers('me', fb_oauth)
getUsers('me', fb_oauth, private_info=TRUE)
users <- getUsers(friends$id, fb_oauth)
str(users)
users <- getUsers(testusers, fb_oauth)
str(users)

## test of Facebook token, Unversioned
token <- tokenv1
getUsers('me', token)
getUsers('me', token, private_info=TRUE)
users <- getUsers(friends$id, token)
str(users)
users <- getUsers(testusers, token)
str(users)

## test of Facebook token, v2.x

getUsers('me', token)
getUsers('me', token, private_info=TRUE)
users <- getUsers(friends$id, token)
str(users)
users <- getUsers(testusers, token)
str(users)


##########################################
## getFriends
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
friends <- getFriends(fb_oauth, simplify=TRUE)
str(friends)
friends <- getFriends(fb_oauth, simplify=FALSE)
str(friends)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
friends <- getFriends(fb_oauth, simplify=TRUE)
str(friends)
friends <- getFriends(fb_oauth, simplify=FALSE)
str(friends)

## test of Facebook token, Unversioned
token <- tokenv1
friends <- getFriends(token, simplify=TRUE)
str(friends)
friends <- getFriends(token, simplify=FALSE)
str(friends)

## test of Facebook token, v2.x
token <- tokenv2
friends <- getFriends(token, simplify=TRUE)
str(friends)
friends <- getFriends(token, simplify=FALSE)
str(friends)


##########################################
## getPage
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
fb_page <- getPage(page="facebook", token=fb_oauth)
str(fb_page)
page <- getPage(page="humansofnewyork", token=fb_oauth, feed=TRUE)
str(page)
page <- getPage(page="humansofnewyork", token=fb_oauth, n=250, 
	since='2012/12/15', until='2013/01/31')
str(page)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
fb_page <- getPage(page="facebook", token=fb_oauth)
page <- getPage(page="humansofnewyork", token=fb_oauth, feed=TRUE)
page <- getPage(page="humansofnewyork", token=fb_oauth, n=250, 
	since='2012/12/15', until='2013/01/31')


## test of Facebook token, Unversioned
token <- tokenv1
fb_page <- getPage(page="facebook", token=token)
page <- getPage(page="humansofnewyork", token=token, feed=TRUE)
page <- getPage(page="humansofnewyork", token=token, n=1000, 
	since='2012/12/15', until='2013/01/31')


## test of Facebook token, v2.x
token <- tokenv2
fb_page <- getPage(page="facebook", token=token)
page <- getPage(page="humansofnewyork", token=token, feed=TRUE)
page <- getPage(page="humansofnewyork", token=token, n=1000, 
	since='2012/12/15', until='2013/01/31')

##########################################
## searchFacebook
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
posts <- searchFacebook( string="facebook", token=fb_oauth, n=300)
str(posts)

posts <- searchFacebook( string="pablobarbera", token=fb_oauth, n=100)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
posts <- searchFacebook( string="facebook", token=fb_oauth, n=100)


##########################################
## getLikes
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
likes <- getLikes(user="me", token=fb_oauth)
str(likes)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
likes <- getLikes(user="me", token=fb_oauth)
str(likes)

## test of Facebook token, Unversioned
token <- tokenv1
likes <- getLikes(user="me", token=token)
str(likes)

## test of Facebook token, v2.x
token <- tokenv2
likes <- getLikes(user="me", token=token)
str(likes)


##########################################
## getPost
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
fb_page <- getPage(page="barackobama", token=fb_oauth)
post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
str(post)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
fb_page <- getPage(page="barackobama", token=fb_oauth)
post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
str(post)


# TODO
# nested comments?

##########################################
## getNetwork
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
mat <- getNetwork(token=fb_oauth, format="adj.matrix")
str(mat)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
mat <- getNetwork(token=fb_oauth, format="adj.matrix")
str(mat)


##########################################
## getNewsfeed
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
my_newsfeed <- getNewsfeed(token=fb_oauth, n=100)
str(my_newsfeed)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
my_newsfeed <- getNewsfeed(token=fb_oauth, n=100)
str(my_newsfeed)


##########################################
## searchPage
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
pages <- searchPages( string="facebook", token=fb_oauth, n=100 )
str(pages)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
pages <- searchPages( string="facebook", token=fb_oauth, n=100 )
str(pages)


##########################################
## updateStatus
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
updateStatus("test", token=fb_oauth)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
updateStatus("test", token=fb_oauth)



##########################################
## getCheckins
##########################################

## test of Facebook app, v1.0
load("~/Dropbox/credentials/facebook/tokenv1")
getCheckins(user="me", token=fb_oauth)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
getCheckins(user="me", token=fb_oauth)


