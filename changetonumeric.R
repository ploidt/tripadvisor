library(foreign)

attraction.file.name <- "BangkokHualamphongStation"
setwd(paste('/Users/ploid/GitHub/tripadvisor-watpho/cleandata/clean',attraction.file.name, sep = ""))

merge_data <-  read.csv('merged.csv', header = TRUE)
merge_data <- unique(merge_data)

file.1 <- paste("textdata_",attraction.file.name,".csv", sep = "")
file.2 <- paste("kdata_",attraction.file.name,".csv", sep = "")

attraction <- rep(NA,nrow(merge_data))
for(i in 1:nrow(merge_data)){
  attraction[i] <- 'Bangkok Hualamphong Station'
}
merge_data$attraction <- attraction

count.country <- function(country){
  country.size <- length(which(merge_data$country == country))
  return(country.size)
}

countrySize <- rep(NA,nrow(merge_data))
for(i in 1:nrow(merge_data)){
  countrySize[i] <- count.country(merge_data$country[i])
}
merge_data$countrySize <- countrySize
merge_data <- merge_data[which(merge_data$category != "category"),]

merge_data$category <- factor(merge_data$category)
# merge_data$month <- factor(merge_data$month)
# merge_data$year <- factor(merge_data$year)
merge_data$weekday <- factor(merge_data$weekday)
merge_data$weektype <- factor(merge_data$weektype)
# merge_data$rating <- factor(merge_data$rating )
merge_data$country <- factor(merge_data$country)
# merge_data$gender <- factor(merge_data$gender)
# merge_data$continent <- factor(merge_data$continent)
# merge_data$subcontinent <- factor(merge_data$subcontinent)
merge_data$age <- factor(merge_data$age)
merge_data <- merge_data[which(!is.na(merge_data$date)),]

merge_data <- merge_data[ , !(names(merge_data) %in% c('X'))]

# write.csv(dataset,file = file.name)
# write.arff(dataset, file = "test_weka.arff")
setwd('/Users/ploid/GitHub/tripadvisor-watpho/cleandata/result/textdata')
merge_data <- unique(merge_data)
write.csv(merge_data, file = file.1)

merge_data_no_country_na <- merge_data[which(merge_data$countrySize > 0),]
merge_data_no_country_na <- merge_data_no_country_na[which(!is.na(merge_data_no_country_na$continent)),]

# meanAge <- mean(na.omit(as.numeric(merge_data_no_country_na$age)))
sdAge <- sd(as.numeric(merge_data_no_country_na$age), na.rm = TRUE)
print(ggplot(merge_data_no_country_na[which(merge_data_no_country_na$age!=""),],aes(age)) + geom_bar(fill = "blue",na.rm = TRUE))

unfactor.category <- function(x){
  x <- as.character(x)
  if(x == "business") return(0)
  if(x == "couples") return(1)
  if(x == "family") return(2)
  if(x == "friend") return(3)
  if(x == "solo") return(4)
}
unfactor.weektype <- function(x){
  x <- as.character(x)
  if(x == "weekday") return(0)
  if(x == "weekend") return(1)
}
unfactor.age <- function(x){
  x <- as.character(x)
  if(!is.na(x) && !is.null(x) && x!=""){
    if(x == "13-17") return(0)
    if(x == "18-24") return(1)
    if(x == "25-34") return(2)
    if(x == "35-49") return(3)
    if(x == "50-64") return(4)
    if(x == "65+") return(5)
    # if(x == "") return(NA)
  }else{
    # meanAge <- mean(na.omit(as.numeric(merge_data_no_country_na$age)))
    # tempX <- round(rnorm(1, mean = meanAge, sd = sdAge))
    # while(tempX < 0 || tempX > 5){
    #   tempX <- round(rnorm(1, mean = meanAge, sd = sdAge))
    # }
    # return(tempX)
    return(NA)
  }
  
}
# unfactor.gender <- function(x){
#   x <- as.character(x)
#   if(!is.na(x) && !is.null(x)){
#     if(x == "M") return(0)
#     if(x == "F") return(1)
#     if(x == "U") return(2)
#   }else{
#     if(runif(1, min=0, max=1) > 0.5){
#       return(0)
#     }else{
#       return(1)
#     }
#   }
# }

unfactor.weekday <- function(x){
  x <- as.character(x)
    if(!is.na(x) && !is.null(x)){
    if(x == "Monday") return(0)
    if(x == "Tuesday") return(1)
    if(x == "Wednesday") return(2)
    if(x == "Thursday") return(3)
    if(x == "Friday") return(4)
    if(x == "Saturday") return(5)
    if(x == "Sunday") return(6)
  }else{
    return(NA)
  }
}

# merge_data_no_country_na$continent <- as.character(levels(
#   merge_data_no_country_na$continent))[merge_data_no_country_na$continent]
merge_data_no_country_na$category <- as.character(levels(
  merge_data_no_country_na$category))[merge_data_no_country_na$category]
merge_data_no_country_na$weektype <- as.character(levels(
  merge_data_no_country_na$weektype))[merge_data_no_country_na$weektype]
merge_data_no_country_na$age <- as.character(levels(
  merge_data_no_country_na$age))[merge_data_no_country_na$age]
# merge_data_no_country_na$gender <- as.character(levels(
#   merge_data_no_country_na$gender))[merge_data_no_country_na$gender]
merge_data_no_country_na$weekday <- as.character(levels(
  merge_data_no_country_na$weekday))[merge_data_no_country_na$weekday]

for(i in 1:nrow(merge_data_no_country_na)){
  # merge_data_no_country_na$continent[i] <- unfactor.continent(merge_data_no_country_na$continent[i])
  merge_data_no_country_na$category[i] <- unfactor.category(merge_data_no_country_na$category[i])
  merge_data_no_country_na$weektype[i] <- unfactor.weektype(merge_data_no_country_na$weektype[i])
  merge_data_no_country_na$age[i] <- unfactor.age(merge_data_no_country_na$age[i])
  # merge_data_no_country_na$gender[i] <- unfactor.gender(merge_data_no_country_na$gender[i])
  merge_data_no_country_na$weekday[i] <- unfactor.weekday(merge_data_no_country_na$weekday[i])
}
# merge_data_no_country_na$continent <- factor(merge_data_no_country_na$continent)
merge_data_no_country_na <- unique(merge_data_no_country_na)
k.data <- merge_data_no_country_na
# k.data <- merge_data_no_country_na[which(merge_data_no_country_na$countrySize >100),]
# k.data <- k.data[which(!is.na(k.data$age)),]
# k.data <- k.data[which(!is.na(k.data$gender)),]
k.data <- k.data[ , !(names(k.data) %in% c('quote','date','location','country','fullreview','attraction'))]
# k.data$subcontinent <- as.numeric(factor(k.data$subcontinent , levels=c("australia and new zealand" , "caribbean", "central america","central asia","eastern africa","eastern asia","eastern europe","melanesia","micronesia","middle africa","northern africa","northern america","northern europe","polynesia","south america","south-eastern asia","southern africa","southern asia","southern europe","western africa","western asia","western europe")))
k.data$rating <- as.numeric(k.data$rating)
k.data$age <- as.numeric(k.data$age) 
k.data$gender <- as.numeric(k.data$gender)
k.data$continent <- as.numeric(k.data$continent)
k.data$subcontinent <- as.numeric(k.data$subcontinent)
k.data$day <- as.numeric(k.data$day)
k.data$month <- as.numeric(k.data$month)
k.data$year <- as.numeric(k.data$year)
k.data$weekday <- as.numeric(k.data$weekday)
k.data$weektype <- as.numeric(k.data$weektype)
k.data$category <- as.numeric(k.data$category)

setwd('/Users/ploid/GitHub/tripadvisor-watpho/cleandata/result/kdata')
write.csv(k.data, file = file.2)



