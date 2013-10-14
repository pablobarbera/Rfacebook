searchDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

userDataToDF <- function(user_data, private_info){
	df <- data.frame(
		id = unlistWithNA(user_data, 'id'),
		name = unlistWithNA(user_data, 'name'),
		username = unlistWithNA(user_data, 'username'),
		first_name = unlistWithNA(user_data, 'first_name'),
		last_name = unlistWithNA(user_data, 'last_name'),
		gender = unlistWithNA(user_data, 'gender'),
		locale = unlistWithNA(user_data, 'locale'),
		category = unlistWithNA(user_data, 'category'),
		likes = unlistWithNA(user_data, 'likes'),
		picture = unlistWithNA(user_data, c('picture', 'data', 'url')),
		stringsAsFactors=F)
	if (private_info==TRUE){
		df$birthday <- unlistWithNA(user_data, 'birthday')
		df$location <- unlistWithNA(user_data, c('location', 'name'))
		df$hometown <- unlistWithNA(user_data, c('hometown', 'name'))
		df$relationship_status <- unlistWithNA(user_data, 'relationship_status')
	}
	return(df)
}

unlistWithNA <- function(lst, field){
	if (length(field)==1){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst, '[[', field))
	}
	if (length(field)==2){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
	}
	if (field[1]=="shares"){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
		vect <- rep(0, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
	}
	if (length(field)==3){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
	}
	if (field[1] %in% c("comments", "likes") & !is.na(field[2])){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
		vect <- rep(0, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
	}
	return(vect)
}

callAPI <- function(url, token){
	if (class(token)=="config"){
		url.data <- GET(url, config=token)
	}
	if (class(token)=="character"){
		url <- paste0(url, "&access_token=", token)
		url.data <- GET(url)
	}
	if (class(token)!="character" & class(token)!="config"){
		stop("Error in access token. See help for details.")
	}
	content <- fromJSON(rawToChar(url.data$content))
	if (length(content$error)>0){
		stop(content$error$message)
	}	
	return(content)
}




