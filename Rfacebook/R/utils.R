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

insightsDataToDF <- function(x){
  
  values <- list()
  
  if(grepl('^post',x$data[[1]]$name)){
  
  for (i in 1:length(x$data[[1]]$values)){
    tmp <- data.frame(unlist(x$data[[1]]$values[[i]]$value), stringsAsFactors=F)
    tmp$variable <- row.names(tmp)
    row.names(tmp) <- NULL
    names(tmp) <- c('value', 'variable')
    values[[i]] <- tmp
  }  
  } else { 
    
  for (i in 1:length(x$data[[1]]$values)){
    tmp <- data.frame(unlist(x$data[[1]]$values[[i]]$value), end_time=x$data[[1]]$values[[i]]$end_time, stringsAsFactors=F)
    tmp$variable <- row.names(tmp)
    row.names(tmp) <- NULL
    names(tmp) <- c('value', 'end_time', 'variable')
    values[[i]] <- tmp
  }
  }
  
  values <- do.call('rbind',values)
  
  df <- data.frame(
    id=x$data[[1]]$id,
    name=x$data[[1]]$name,
    period=x$data[[1]]$period,
    title=x$data[[1]]$title,
    description=x$data[[1]]$description,
    values,
    stringsAsFactors=FALSE
  )

  if(length(unique(df$variable))==1 & df$variable[1]==1){
    df$variable <- NULL
  } else {
    df <- df
  }

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
	if (!is.null(json)){
		df <- data.frame(
			from_name = unlistWithNA(json, "name"),
			from_id = unlistWithNA(json, "id"),
			stringsAsFactors=F
			)
	}
	if (length(json)==0){
		df <- NULL
	}
	return(df)
}

commentsDataToDF <- function(json){
	if (!is.null(json)){
		df <- data.frame(
			from_id = unlistWithNA(json, c('from', 'id')),
			from_name = unlistWithNA(json, c('from', 'name')),
			message = unlistWithNA(json, 'message'),
			created_time = unlistWithNA(json, 'created_time'),
			likes_count = unlistWithNA(json, 'like_count'),
			id = unlistWithNA(json, 'id'),
		stringsAsFactors=F)
	}
	if (is.null(json)){
		df <- NULL
	}
	return(df)
}

userDataToDF <- function(user_data, private_info){
	df <- data.frame(
		id = unlistWithNA(user_data, 'id'),
		name = unlistWithNA(user_data, 'name'),
		username = unlistWithNA(user_data, 'username'),
		first_name = unlistWithNA(user_data, 'first_name'),
		middle_name = unlistWithNA(user_data, 'middle_name'),
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
    tags <- lapply(tags, '[[', "tags")
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
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field]]))
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

searchPageDataToDF <- function(json){
  df <- data.frame(
    id = unlistWithNA(json, 'id'),
    about = unlistWithNA(json, 'about'),
    category = unlistWithNA(json, 'category'),
    description = unlistWithNA(json, 'description'),
    general_info = unlistWithNA(json, 'general_info'),
    likes = unlistWithNA(json, 'likes'),
    link = unlistWithNA(json, 'link'),
    city = unlistWithNA(json, c('location', 'city')),
    state = unlistWithNA(json, c('location', 'state')),
    country = unlistWithNA(json, c('location', 'country')),
    latitude = unlistWithNA(json, c('location', 'latitude')),
    longitude = unlistWithNA(json, c('location', 'longitude')),
    name = unlistWithNA(json, 'name'),
    talking_about_count = unlistWithNA(json, 'talking_about_count'),
    username = unlistWithNA(json, 'username'),
    website = unlistWithNA(json, 'website'),
    stringsAsFactors=F)
  return(df)
}

#' @rdname callAPI
#' @export
#'
#' @title 
#' Make an API request
#'
#' @description
#' \code{callAPI} is an internal function to run an API request.
#'
#' @param url URL of API request
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}. It needs to have extended permissions in order 
#' to successfully post to the Facebook profile.
#'
#'

callAPI <- function(url, token){
	if (class(token)[1]=="config"){
		url.data <- GET(url, config=token)
	}
	if (class(token)[1]=="Token2.0"){
		url.data <- GET(url, config(token=token))
	}	
	if (class(token)[1]=="character"){
		url <- paste0(url, "&access_token=", token)
		url <- gsub(" ", "%20", url)
		url.data <- GET(url)
	}
	if (class(token)[1]!="character" & class(token)[1]!="config" & class(token)[1]!="Token2.0"){
		stop("Error in access token. See help for details.")
	}
	content <- rjson::fromJSON(rawToChar(url.data$content))
	if (length(content$error)>0){
		stop(content$error$message)
	}	
	return(content)
}

getTokenVersion <- function(token){

	if (!is.na(class(token)[4])){
		tkversion <- class(token)[4]
	}
	if (is.na(class(token)[4])){
		error <- tryCatch(callAPI('https://graph.facebook.com/pablobarbera', token),
			error = function(e) e)
		if (inherits(error, 'error')){
			tkversion <- 'v2'
		}
		if (!inherits(error, 'error')){
			tkversion <- 'v1'
		}
	}
	return(tkversion)

}


formatFbDate <- function(datestring, format="datetime") {
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")    
    }
    if (format=="date"){
        date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")   
    }
    return(date)
}



