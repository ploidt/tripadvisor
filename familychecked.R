require(RSelenium)
remDr <- remoteDriver(browserName = "chrome")
remDr$open()
remDr$navigate("http://www.tripadvisor.com/Attraction_Review-g293916-d311043-Reviews-Temple_of_the_Reclining_Buddha_Wat_Pho-Bangkok.html")

#family
remDr$executeScript(script = "ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.toggleFilter(); ta.plc_prodp13n_hr_sur_review_filter_controls_0_handlers.trackCheckBoxClick(this, 'Reviews_Controls', 'traveler_filter', 'Families');",args = list())

#reload bf change?