install.packages("Rfacebook")
//install.packages("contrib.url")
library(devtools)
install_github("Rfacebook", "pablobarbera", subdir = "Rfacebook")

library(Rfacebook)
token <- "XXXXXXXXXXXXXX"
me <- getUsers("pablobarbera", token, private_info = TRUE)
me$name

getPage <- function(page, token, n=25, since=NULL, until=NULL, feed=FALSE, reactions=FALSE) {
	url <- paste0('https://graph.facebook.com/', page, 'posts?fields=from,message,created_time,type,link,story,comments.summary(true)','likes.summary(true),shares')
	if(feed) {
		url <- paste0('https://graph.facebook.com/', page, '/feed?fields=from,message,created_time,type,link,story,comments.summary(true)', 'likes.summary(true),shares')
		if(!is.null(until)) {
			url <- paste0(url, '&until=', until)
		}
		if(!is.null(since)) {
			url <- paste0(url, '&since=', since)
		}
		if(n<=25) {
			url <- paste0(url, "&limit=", n)
		}
		
		content <- callAPI(url=url, token=token)
		l <- length(content$data); cat(l, "posts")
		
		error <- 0
		while(length(content$error_code)>0) {
			cat("Error!\n")
			Sys.sleep(0.5)
			error <- error + 1
			content <- callAPI(url=url, token=token)
			if(error==3) {
				stop(content$error_msg) }
			}
			if(length(content$data)==0) {
				message("No public posts were found")
				return(data.frame())
			}
			df <- pageDataToDF(content$data)
			
			if(!is.null(since)) {
				dates <- formatFbDate(df$created_time, 'date')
				mindate <- min(dates)
				sincedate <- as.Date(since)
			}
			if(is.null(since)) {
				sincedate <- as.Date('1970/01/01')
				mindate <- as.Date(Sys.time())
			}
			if(n > 25) {
				df.list <- list(df)
				while(l < n & length(content$data) > 0) & !is.null(content$paging$`next`) & sincedate <= mindate) {
					Sys.sleep(0.5)
					url <- content$paging$`next`
					content <- callAPI(url=url, token=token)
					l <- l + length(content$data)
					if(length(content$data) > 0) { cat(l, "posts") }
					
					error <- 0
					while(length(content$error_code) > 0) {
						cat("Error!\n")
						Sys.sleep(0.5)
						error <- error + 1
						content <- callAPI(url=url, token=token)
						if(error==3) { stop(content$error_msg) }
					}
					new.df <- pageDataToDF(content$data)
					df.list <- c(df.list, list(new.df))
					
					if(!is.null(since) & nrow(new.df) > 0) {
						dates <- formatFbDate(new.df$created_time, 'date')
						mindate <- min(dates)
					}
					df <- do.call(rbind, df.list)
				}
				if(nrow(df) > n) {
					df <- df[1:n,]
				}
				
				if(!is.null(since)) {
					dates <- formatFbDate(df$created_time, 'date')
					df <- df[dates>=sincedate,]
				}
				if(reactions == TRUE) {
					re = getReactions(df$id, token=token, verbose=FALSE)
					df <- merge(df, re, all.x=TRUE)
					df <- df[order(df$created_time),]
				
				}
				return (df)
                
                format.facebook.date <- function(datestring) {
                    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
                }
                
                aggregate.metric <- function(metric) {
                    m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
                    m$month <- as.Date(paste0(m$month, "-15"))
                    m$metric <- metric
                    return(m)
                }
                
                page$datetime <- format.facebook.date(page$created_time)
                page$month <- format(page$datetime, "%Y-%m")
                df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
                df <- do.call(rbind, df.list)
                library(ggplot2)
                library(scales)
                ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) +
                scale_x_date(breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post",
                breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())
                
                page <- getPage("DonaldTrump", token, n = 5000, since='2015/01/01', until='2015/12/31')
                
                post_id <- head(page$id, n = 1)
                post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = FALSE)
            }
