require(RSelenium)
library(rvest)
path <- "/Users/ploid/GitHub/tripadvisor/watpho/couples"
setwd(path)

remDr <- remoteDriver(browserName = "phantomjs")

remDr$open()
remDr$navigate("http://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html#REVIEWS")
remDr$deleteAllCookies()

#family
# family.boolean <- remDr$executeScript(script = "return document.getElementById('taplc_prodp13n_hr_sur_review_filter_controls_0_filterSegment_Family').checked", args = vector())
# family.boolean
# 
# remDr$executeScript(script = "document.getElementById('taplc_prodp13n_hr_sur_review_filter_controls_0_filterSegment_Family').checked = true", args = list())
# remDr$executeScript(script = "ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.toggleFilter(); ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.trackCheckBoxClick(this, 'Reviews_Controls', 'traveler_filter', 'Families');",args = list())

#couples

remDr$executeScript(script ="document.getElementById('taplc_prodp13n_hr_sur_review_filter_controls_0_filterSegment_Couples').checked = true", args = list())
remDr$executeScript(script = "ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.toggleFilter(); ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.trackCheckBoxClick(this, 'Reviews_Controls', 'traveler_filter', 'Couples');",args = list())

#reload bf change
Sys.sleep(5)

for(i in seq(493, 600, 1)){
  
  print(i)
  wantToSkipPage <- TRUE
  file.name <- paste("date_couples_review_page_",i,".csv",sep="")
  page.link <- paste("http://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-or",(i*10)-10,"-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html",sep="")
  
  if(wantToSkipPage){
    
    remDr$navigate(page.link)
    Sys.sleep(5) 
  }
  
  web.html <- read_html(remDr$getPageSource()[[1]])
  
  d = NULL
  
  get.all.info <- function(web.html){
    reviews <- web.html %>%
      html_nodes("#REVIEWS .innerBubble")
    
    date <- reviews %>%
      # html_node(".rating .ratingDate") %>%
      html_node(".ratingDate") %>%
      html_text() %>%
      gsub("Reviewed ", "", .) %>%
      # html_attr("title") %>%
      strptime("%b %d, %Y") %>%
      as.POSIXct()
    
    data <- data.frame(i, id, date, stringsAsFactors = FALSE)
    
  }
  
  
  temp <- get.all.info(web.html)
  d = rbind(d,temp)

  
  write.csv(d, file = file.name)
  
}
remDr$close()
# d %>% View()
