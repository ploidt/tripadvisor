library(rvest)

# set working directory
setwd("/Users/ploid/Github/tripadvisor/")

d = NULL

for(i in seq(0, 0, 0)){

  urlReview <- paste0("https://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-or",i,"-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html#REVIEWS")
  
  reviews <- urlReview %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id") %>%
    gsub("rn", "", .) %>%
    as.integer()
  
  review_link <- reviews %>%
    html_node(".quote a") %>%
    html_attr("href") 
  review_link <- trimws(review_link,which="both")
  
  review_link <- paste("https://www.tripadvisor.com",review_link,sep="")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  rating <- reviews %>%
    html_node(".rating .rating_s_fill") %>%
    html_attr("alt") %>%
    gsub(" of 5 stars", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%b %d, %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  
  member <- urlReview %>% 
    read_html() %>% 
    html_nodes("#REVIEWS .col1of2")
  
  location <- member %>%
    html_node(".location") %>%
    html_text()
  

  data <- data.frame(i, id, quote, rating, date, location,review_link, stringsAsFactors = FALSE)
  d = rbind(d,data)
}

get.full.review = function(review_link,id){
  fullreview <- review_link %>%
    read_html() %>%
    html_node(paste(paste("#review_", id, sep = ""),"p", sep = " ")) %>%
    html_text()
  
  return(fullreview)
}

fullrev <- rep(NA,nrow(d))

for(i in 1:nrow(d)){
  
  fullrev[i] <- get.full.review(d$review_link[i],d$id[i])
  if(i %% 20 == 0) print(i)
  
}

d$fullreview <- fullrev
d %>% View()

