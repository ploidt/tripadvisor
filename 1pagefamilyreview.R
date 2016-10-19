require(RSelenium)
library(rvest)

remDr <- remoteDriver(browserName = "phantomjs")

remDr$open()
remDr$navigate("http://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html")
remDr$deleteAllCookies()

#family
family.boolean <- remDr$executeScript(script = "return document.getElementById('taplc_prodp13n_hr_sur_review_filter_controls_0_filterSegment_Family').checked", args = vector())
family.boolean

remDr$executeScript(script = "document.getElementById('taplc_prodp13n_hr_sur_review_filter_controls_0_filterSegment_Family').checked = true", args = list())
remDr$executeScript(script = "ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.toggleFilter(); ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.trackCheckBoxClick(this, 'Reviews_Controls', 'traveler_filter', 'Families');",args = list())

#reload bf change?
Sys.sleep(10)

web.html <- read_html(remDr$getPageSource()[[1]])

d = NULL

get.all.info <- function(web.html){
  reviews <- web.html %>%
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
  
  review_link <- paste("https://www.tripadvisor.com", review_link ,sep="")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  rating <- reviews %>%
    html_node(".sprite-rating_s .rating_s_fill") %>%  
    html_attr("alt") %>%
    gsub(" of 5 bubbles", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%b %d, %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  
  member <- web.html %>% 
    html_nodes("#REVIEWS .col1of2")
  
  memberid <- member %>%
    html_node(".memberOverlayLink") %>%
    html_attr("id")
  memberid <- substr(memberid,5,36)
  
  location <- member %>%
    html_node(".location") %>%
    html_text()
  
  
  data <- data.frame(i, id, quote, rating, date, memberid, location, review_link, stringsAsFactors = FALSE)
  
}

for(i in seq(0, 50, 10)){
  
  # urlReview <- paste0("https://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-or",i,"-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html#REVIEWS")
  
  temp <- get.all.info(web.html)
  d = rbind(d,temp)
  if(i == 0){
    next.btn <- remDr$findElement(using = "xpath", value = "//*[@id='REVIEWS']/div[17]/div/a")
    next.btn$getElementAttribute('href')
    
    next.btn$clickElement()
    
    Sys.sleep(5)
    
    web.html <- read_html(remDr$getPageSource()[[1]])
  }
  
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

get.user.age = function(uid){
  profile.link <- paste("https://www.tripadvisor.com/MemberProfile-a_uid.", uid, sep = "")
  remDr$navigate(profile.link)
  profile.html <- read_html(remDr$getPageSource()[[1]])
  
  age.since <- profile.html %>%
    html_node(".ageSince")
  
  age <- age.since %>%
    html_nodes("p") %>%
    html_text(trim=TRUE) %>%
    gsub(" year old", "", .) 
  
  return(age[2])
}

ages <- rep(NA,nrow(d))

for(i in 1:nrow(d)){
  
  ages[i] <- get.user.age(d$memberid[i])
  
}

d$age <- ages

remDr$close()
d %>% View()
