library(devtools)
setwd("~/Dropbox/git/Rfacebook")
document("Rfacebook")
build("Rfacebook")
install.packages("Rfacebook_0.5.tar.gz", repo=NULL, type="source")

library(Rfacebook)
load('test/credentials.dta')

tokenv1 <- paste0(
	'CAACEdEose0cBABUajrGv8mcZBnKeHXsaSVodjlfrCDD5Ywt2WHCXPcVrNzQB',
	'wndEmYaVUhZBtghNTDHf6oLyiAzAo0HJh9ThRSDrQc2JkZCsGYfPWZCqllfVonI',
	'BeHtIh37yf1KumMma3AnMXB3mRT5MPpTNJA4TmDAjQZCcmamP0JeqbOC4y6AXYfZ',
	'Ba57J0XLQ7Kjl2zoRS6IKSfVZBPN')
tokenv2 <- paste0('CAACEdEose0cBAGnuypoH4kxrjSoTD2avuiYLEjRxAWv3KGZBis',
	'ufSDHUnsZARFa19mpnkZCUzKPp94SeEjsqQAN24UTERMbalgIFpca3SemdL7T18kK7',
	'W5Ul9kiL12eH5TAbZCMFxD9ax4bL2Hx5njlOZBpKhDZBJYTyavyDGbJoe0Hoe1hrC9',
	'GHgBFq0wB45FzNZBBIn6lVzs7q2SCBN9E')

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
	since=as.Date('2013/01/01'), until=as.Date('2013/01/31'))
str(page)

## test of Facebook app, v2.0
load("~/Dropbox/credentials/facebook/tokenv2")
fb_page <- getPage(page="facebook", token=fb_oauth)
page <- getPage(page="humansofnewyork", token=fb_oauth, feed=TRUE)
page <- getPage(page="humansofnewyork", token=fb_oauth, n=1000, since='2013/01/01', until='2013/01/31')


## test of Facebook token, Unversioned
token <- tokenv1
fb_page <- getPage(page="facebook", token=token)
page <- getPage(page="humansofnewyork", token=token, feed=TRUE)
page <- getPage(page="humansofnewyork", token=token, n=1000, since='2013/01/01', until='2013/01/31')


## test of Facebook token, v2.x
token <- tokenv2
fb_page <- getPage(page="facebook", token=token)
page <- getPage(page="humansofnewyork", token=token, feed=TRUE)
page <- getPage(page="humansofnewyork", token=token, n=1000, since='2013/01/01', until='2013/01/31')







## to-do

# fix extended permissions in fbOAuth to comply with new Facebook API



get_domain_url <- function(){
	require(httr)
	full_url <- oauth_callback()
	message("Copy and paste into Site URL: ", 
		gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, repl="\\1"))
}

get_domain_url()

get_fb_oauth <- function(app_id, app_secret){
	require(httr)
	facebook <- oauth_endpoint(
	  authorize = "https://www.facebook.com/dialog/oauth",
	  access = "https://graph.facebook.com/oauth/access_token")	
	myapp <- oauth_app("facebook", key, secret)

	facebook_token <- oauth2.0_token(facebook, myapp,
	  type = "application/x-www-form-urlencoded")
	facebook_sig <- sign_oauth2.0(facebook_token$access_token)
	return(facebook_sig)
}

get_domain_url <- function(){
	require(httr)
	full_url <- oauth_callback()
	message("Copy and paste into Site URL: ", 
		gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, repl="\\1"))
}

getFBOAuth <- function(app_id, app_secret){
	require(httr); require(rjson)
	## getting callback URL
	full_url <- oauth_callback()
	full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, repl="\\1")
	message <- paste("Copy and paste into Site URL on Facebook App Settings:", full_url, "\nWhen done, press any key to continue...")
	invisible(readline(message))
	facebook <- oauth_endpoint(
	  authorize = "https://www.facebook.com/dialog/oauth",
	  access = "https://graph.facebook.com/oauth/access_token")	
	myapp <- oauth_app("facebook", app_id, app_secret)

	facebook_token <- oauth2.0_token(facebook, myapp,
	  type = "application/x-www-form-urlencoded")
	fb_oauth <- sign_oauth2.0(facebook_token$access_token)
	## testing that authentication successful. 
	if (GET("https://graph.facebook.com/me", config=fb_oauth)$status==200){
		message("Authentication successful.")
	}
	return(fb_oauth)
}

fb_oauth <- getFBOAuth(app_id="136359959907305", app_secret="d9179adc873190147ec03de7b939e2b7")







my_oauth <- get_fb_oauth(key="136359959907305", secret="d9179adc873190147ec03de7b939e2b7")

save(my_oauth, file="~/oauth_fb")

load("~/oauth_fb")

library(httr)

## get one user (test)
getUser <- function(id, df=TRUE)

GET("https://graph.facebook.com/barackobama", config=my_oauth)


fromJSON(rawToChar(GET("https://graph.facebook.com/me", config=my_oauth)$content))$username

## multiple users in one query
getUsers <- function(ids, df=TRUE)

GET("https://graph.facebook.com/?ids=100003357890267,100000809628582", config=my_oauth)

## search posts
GET("https://graph.facebook.com/search?q=rajoy&type=post", config=my_oauth)

## search for first 500 (note that it will search on comments too!) ## it lets you go back in time around 2 weeks
searchFacebook <- function(q, limit, likes, comments, df=TRUE)
url.data <- GET(URLencode("https://graph.facebook.com/search?q=barathon&type=post&limit=500&fields=from.fields(name,id,category),message,created_time,type,link,shares,likes.fields(id,name),comments"), config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))

lapply(lapply(json.data[[1]], '[[', "likes"), length)

url.data <- GET(URLencode("https://graph.facebook.com/search?q=barathon&type=post&limit=500&since=2013-07-20&until=2013-07-25"), config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))

## get an individual post
getPost

url.data <- GET("https://graph.facebook.com/6815841748_10151732588336749?fields=from.fields(name,id,category),message,created_time,type,link,shares,comments.limit(10).summary(true),likes.limit(10).summary(true)", config=my_oauth)
library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))




url.data <- GET(URLencode("https://graph.facebook.com/search?q=rajoy&type=post&limit=500"), config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))

lapply(json.data[[1]], '[[', "message")

url.data <- GET("https://graph.facebook.com/search?fields=from,message,created_time,type&q=rajoy&limit=10000&type=post&access_token=CAAB8BMb4yZBkBAEhZBqBmpjJxtzONLf60nsUCRzOklRsZASuMKCxZBJylx2dIT976dE8hV3ZAgSdkrGdGJrPj4KAmxxpBIvXUs56lC0FWo1HURtUfuaKhAizYV5qE0yhKJjQzG4g5GRuNEE4Icn7uYDRZCpmKLqUcZD&until=1375612932", config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))


url.data <- GET("https://graph.facebook.com/search?fields=from,message,created_time,type&q=rajoy&limit=10000&type=post&access_token=CAAB8BMb4yZBkBAEhZBqBmpjJxtzONLf60nsUCRzOklRsZASuMKCxZBJylx2dIT976dE8hV3ZAgSdkrGdGJrPj4KAmxxpBIvXUs56lC0FWo1HURtUfuaKhAizYV5qE0yhKJjQzG4g5GRuNEE4Icn7uYDRZCpmKLqUcZD&until=1375595086", config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))



GET("https://graph.facebook.com/me", config=my_oauth)

url.data <- GET("https://graph.facebook.com/search?q=rajoy&type=post&limit=10&fields=from,message,created_time,type", 
	config=my_oauth)

library(rjson)
json.data <- fromJSON(rawToChar(url.data$content))

cbind(
lapply(json.data[[1]], '[[', "id"),
lapply(json.data[[1]], '[[', "message"),
unlist(lapply(lapply(json.data[[1]], '[[', "from"), function(x) x[['name']])),
unlist(lapply(json.data[[1]], '[[', "created_time")),
unlist(lapply(json.data[[1]], '[[', "type"))
)



## this is inspired by: http://applyr.blogspot.com/2012/01/mining-facebook-data-most-liked-status.html?spref=tw

library(rjson)
url.data <- GET("https://graph.facebook.com/me/friends", config=my_oauth)
json.data <- fromJSON(rawToChar(url.data$content))

friends.ids <- unlist(lapply(json.data[[1]], '[[', 'id'))
friends.names <- unlist(lapply(json.data[[1]], '[[', 'name'))

edge.list <- NULL

for (i in 1:length(friends.ids)){
	url.data <- GET(paste0("https://graph.facebook.com/me/mutualfriends/", friends.ids[i]), config=my_oauth)
	json.data <- fromJSON(rawToChar(url.data$content))
	mutual.friends <- unlist(lapply(json.data[[1]], '[[', 'name'))
	for (friend in mutual.friends){
		edge.list <- rbind(edge.list, c(friends.names[i], friend))
	}
#	edge.list[[friends.names[i]]] <- 
	cat(i, " ")
}

library(igraph)
network <- graph.edgelist(edge.list, directed=FALSE)
plot(network)


colnames(friendship.matrix) <- rownames(friendship.matrix) <- friends.names

library(igraph)
network <- graph.adjacency(friendship.matrix)
plot(network)



