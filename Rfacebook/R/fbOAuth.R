#' @rdname fbOAuth
#' @export
#'
#' @title 
#' Create OAuth token to Facebook R session
#'
#' @description
#' \code{fbOAuth} creates a long-lived OAuth access token that enables R to make
#' authenticated calls to the Facebook API. The token can be saved as a
#' file in disk to be re-used in future sessions. This function relies on the
#' \code{httr} package to create the OAuth token, and is a simplified version
#' of one of its examples.
#'
#' This function will automatically detect the API version for the token you create. 
#'
#' @details
#' There are two different ways of making authenticated requests. One is to obtain
#' a temporary access token from \url{https://developers.facebook.com/tools/explorer/},
#' which can be used as argument in any of the functions in Rfacebook. An example is 
#' shown below.
#'
#' However, this token has a 2-hour lifetime by default and after it expires, it
#' needs to be renewed. The second alternative is to create an OAuth token. The 
#' process to create it is a bit more tedious. It is divided in three steps.
#' 
#' First, go to \url{https://developers.facebook.com/apps}, register as a developer
#' and create a new app. You will also need a verified Facebook account.
#' After that, click in "Show" under "App Secret" to find your 'App ID' and 'App Secret'.
#'
#' Second, run the \code{fbOAuth} function with your "App ID" and "App Secret" as 
#' arguments. It will return a URL, which you will need to paste into the "Website with
#' Facebook login" field in your App Settings on Facebook. Once you've done so, press Enter.
#'
#' Third, after pressing Enter, R will try to open a browser window to sign the token. If 
#' everything works well, you will get a message that says you can return to R. If not,
#' try again in a few minutes to make sure your app had its settings updated properly.
#' 
#' To ensure proper functioning of the "getInsights" function-family you will need to 
#' specify the exact permissions granted to your app. As this is (to our knowledge) currently not
#' possible through the R based authentication process, please follow these steps:
#' 
#' -> Create App as mentioned above.
#' 1. Open the "Graph API Explorer": \url{https://developers.facebook.com/tools/explorer/}
#' 2. Select your app in the upper right corner
#' 3. Click "Get Token" -> "Get Access Token"
#' 4. In the popup navigate to "Extended Permissions" and select "Insights"
#' 5. Confirm
#' 6. Ignore the following warning message ("Submit for Login Review...") and confirm again.
#' 6. Go back to R and run fbOAuth with extended_permissions (still) set to FALSE. 
#' -> See third step for possible messages concerning token creation.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{getUsers}}, \code{\link{getPost}}, \code{\link{searchFacebook}}
#'
#' @param app_id numeric, App ID of application to be used to create OAUth token. Available
#' at \url{https://developers.facebook.com/apps}
#' 
#' @param app_secret string, App Secret of application to be used to create OAUth token.
#' Available at \url{https://developers.facebook.com/apps}, in Basic Settings panel.
#'
#' @param extended_permissions If \code{TRUE}, the token will give access to some of
#' the authenticated user's private information (birthday, hometown, location,
#' relationships) and that of his/her friends, and permissions to post
#' status updates as well as to access checkins, likes, and the user's newsfeed
#' If \code{FALSE}, token will give access only to public information. Note 
#' that \code{updateStatus} will only work for tokens with extended permissions.
#' After version 2.0 of the Graph API, creating an application with these permissions
#' requires passing App Review (\url{https://developers.facebook.com/docs/facebook-login/review})
#'
#' @param legacy_permissions For tokens created with old versions of the API, this option
#' adds the "read_stream" permission
#'
#'
#' @examples \dontrun{
#' ## an example of an authenticated request after creating the OAuth token
#' ## where app_id and app_secret are fictitious, and token is saved for
#' ## future sessions
#'	fb_oauth <- fbOAuth(app_id="123456789", app_secret="1A2B3C4D")
#'	save(fb_oauth, file="fb_oauth")
#'	load("fb_oauth")
#'	me <- getUsers("me", token=fb_oauth)
#'	me$username
#' 
#' ## an example of a request using a temporary access token
#' 	token <- "XXXXXXAAAAAAA1111"
#' 	me <- getUsers("me", token=token)
#' }
#'


fbOAuth <- function(app_id, app_secret, extended_permissions=FALSE, legacy_permissions=FALSE)
{
	## getting callback URL
	full_url <- oauth_callback()
	full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
	message <- paste("Copy and paste into Site URL on Facebook App Settings:", 
		full_url, "\nWhen done, press any key to continue...")
	## prompting user to introduce callback URL in app page
	invisible(readline(message))
	## a simplified version of the example in httr package
	facebook <- oauth_endpoint(
	  authorize = "https://www.facebook.com/dialog/oauth",
	  access = "https://graph.facebook.com/oauth/access_token")	
	myapp <- oauth_app("facebook", app_id, app_secret)
	if (extended_permissions==TRUE){
		scope <- paste("user_birthday,user_hometown,user_location,user_relationships,",
			"publish_actions,user_status,user_likes", collapse="")
	}
	else { scope <- "public_profile,user_friends"}
	
	if (legacy_permissions==TRUE) {
	  scope <- paste(scope, "read_stream", sep = ",")
	}

	## with early httr versions
	if (packageVersion('httr') <= "0.2"){
		facebook_token <- oauth2.0_token(facebook, myapp,
		  scope=scope, type = "application/x-www-form-urlencoded")
		fb_oauth <- sign_oauth2.0(facebook_token$access_token) 
		if (GET("https://graph.facebook.com/me", config=fb_oauth)$status==200){
			message("Authentication successful.")
		}
	}

	## less early httr versions
	if (packageVersion('httr') > "0.2" & packageVersion('httr') <= "0.6.1"){
		fb_oauth <- oauth2.0_token(facebook, myapp,
		  scope=scope, type = "application/x-www-form-urlencoded", cache=FALSE)	
		if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
	      	message("Authentication successful.")
	  	}	
	}

	## current httr version
	if (packageVersion('httr') > "0.6.1"){
		Sys.setenv("HTTR_SERVER_PORT" = "1410/")
		fb_oauth <- oauth2.0_token(facebook, myapp,
		  scope=scope, type = "application/x-www-form-urlencoded", cache=FALSE)		
		if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
	      	message("Authentication successful.")
	  	}	
	}
	## identifying API version of token
	error <- tryCatch(callAPI('https://graph.facebook.com/pablobarbera', fb_oauth),
		error = function(e) e)
	if (inherits(error, 'error')){
		class(fb_oauth)[4] <- 'v2'
	}
	if (!inherits(error, 'error')){
		class(fb_oauth)[4] <- 'v1'
	}

	return(fb_oauth)
}

