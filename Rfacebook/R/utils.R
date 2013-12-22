searchDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		link = unlistWithNA(json, 'link'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

newsDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		to_id = unlistWithNA(json, c('to', 'data', "1", 'id')),
		to_name = unlistWithNA(json, c('to', 'data', '1', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		link = unlistWithNA(json, 'link'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

pageDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		link = unlistWithNA(json, 'link'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

postDataToDF <- function(json){
	df <- data.frame(
		from_id = json$from$id,
		from_name = json$from$name,
		message = ifelse(!is.null(json$message),json$message, NA),
		created_time = json$created_time,
		type = json$type,
		link = ifelse(!is.null(json$link), json$link, NA),
		id = json$id,
		likes_count = ifelse(!is.null(json$likes$summary$total_count),
			json$likes$summary$total_count, 0),
		comments_count = ifelse(!is.null(json$comments$summary$total_count),
			json$comments$summary$total_count, 0),
		shares_count = ifelse(!is.null(json$shares$count),
			json$shares$count, 0),
		stringsAsFactors=F)
	return(df)
}

likesDataToDF <- function(json){
	mat <- matrix(unlist(json), 
		ncol=2, byrow=TRUE)
	df <- data.frame(mat, stringsAsFactors=F)
	names(df) <- c("from_name", "from_id")
	return(df)
}

commentsDataToDF <- function(json){
	if (length(json)>1){
		df <- data.frame(
			from_id = unlistWithNA(json, c('from', 'id')),
			from_name = unlistWithNA(json, c('from', 'name')),
			message = unlistWithNA(json, 'message'),
			created_time = unlistWithNA(json, 'created_time'),
			likes_count = unlistWithNA(json, 'like_count'),
			id = unlistWithNA(json, 'id'),
		stringsAsFactors=F)
	}
	if (length(json)==1){
		df <- data.frame(
			from_id = json$from$id,
			from_name = json$from$name,
			message = json$message,
			created_time = json$created_time,
			likes_count = json$like_count,
			id = json$id,
		stringsAsFactors=F)
	}
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

checkinDataToDF <- function(checkin_data){
	df <- data.frame(
		checkin_time = unlistWithNA(checkin_data, 'created_time'),
		place_id = unlistWithNA(checkin_data, c('place', 'id')),
		place_name = unlistWithNA(checkin_data, c('place', 'name')),
		place_city = unlistWithNA(checkin_data, c('place', 'location','city')),
		place_state = unlistWithNA(checkin_data, c('place', 'location','state')),
		place_country = unlistWithNA(checkin_data, c('place', 'location','country')),
		place_lat = unlistWithNA(checkin_data, c('place', 'location', 'latitude')),
		place_long = unlistWithNA(checkin_data, c('place', 'location', 'longitude')),
		stringsAsFactors=F)
	return(df)
}

userLikesToDF <- function(user_likes){
	df <- data.frame(
		id = unlistWithNA(user_likes, 'id'),
		names = unlistWithNA(user_likes, 'name'),
		website = unlistWithNA(user_likes, 'website'),
		stringsAsFactors=F)
	return(df)
}


tagsDataToDF <- function(tags){
    tags <- lapply(tags[[2]]$data, '[[', "tags")
    tags <- lapply(tags, '[[', 'data')
    tagsListToDF <- function(x){
    	if (!is.null(x)){
    	    values <- data.frame(matrix(unlist(x),ncol=2,byrow=TRUE),
    	    	stringsAsFactors=F)
    		names(values) <- c("id", "name")	
    	}
    	if (is.null(x)){
    		values <- NULL
    	}
    	return(values)
    }
    tags <- lapply(tags, tagsListToDF)
    return(tags)
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
		notnulls <- unlist(lapply(lst, function(x) 
			tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]), 
				error=function(e) FALSE)))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
	}
	if (length(field)==4 & field[1]=="to"){
		notnulls <- unlist(lapply(lst, function(x) 
			tryCatch(!is.null(x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]), 
				error=function(e) FALSE)))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
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




