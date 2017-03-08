# install.packages('Amelia')
library(ggplot2)
# library(Amelia)

attraction.file.name <- 'BangkokHualamphongStation'

# setwd(paste('/Users/ploid/GitHub/tripadvisor-watpho/data/AmphawaFloatingMarket/', category, sep = ""))
data.path <- paste('/Users/ploid/GitHub/tripadvisor-watpho/data/', attraction.file.name, sep = "")
setwd(data.path)

data.file.list <- list.files(path = data.path)

for(i in 1:length(data.file.list)){
  if(grepl('business',data.file.list[i])){
    category <- 'business'
  }else if(grepl('couple',data.file.list[i]) || grepl('couples',data.file.list[i])){
    category <- 'couples'
  }
  else if(grepl('family',data.file.list[i])){
    category <- 'family'
  }
  else if(grepl('family',data.file.list[i])){
    category <- 'friend'
  }
  else if(grepl('family',data.file.list[i])){
    category <- 'solo'
  }else{
    category <- NA
  }
}

read.file.name <- 'BangkokHualamphongStation_couple_1to6.csv'
date.format <- "%Y-%m-%d"
# date.format <- "%m/%d/%y"


dataset <- read.csv(read.file.name, header = TRUE)
dataset <- unique(dataset)
file.name <- paste(category, "_clean.csv", sep = "")


dataset$rating <- factor(dataset$rating)
dataset$fullreview <- iconv(dataset$fullreview)
dataset$quote <- iconv(dataset$quote)

#drop column i, id, member, id , review_link
drops <- c('X', 'i','id','review_link')
dataset <- dataset[ , !(names(dataset) %in% drops)]

separate.gender <- function(age) {
  gender <- NA
  if(grepl('female', age, ignore.case = TRUE)){
    gender <- 1
  }else if(grepl('male', age, ignore.case = TRUE)){
    gender <- 0
  }else if(grepl('another gender', age, ignore.case = TRUE)){
    gender <- 2
  }
  return(gender)
}
  
#separateage and gender
gender <- rep(NA,nrow(dataset))
for(i in 1:nrow(dataset)){
  gender[i] <- separate.gender(dataset$age[i])
}

#clean age columnm
dataset$gender <- gender

dataset$age <- gsub('male', '', dataset$age, ignore.case = TRUE)
dataset$age <- gsub('fe', '', dataset$age, ignore.case = TRUE)
dataset$age <- gsub('another gender identity', '', dataset$age, ignore.case = TRUE)
dataset$age <- gsub('[|]', '', dataset$age)
dataset$age <- trimws(dataset$age)
dataset$age <- factor(dataset$age)

#change location column name
colnames(dataset)[which(names(dataset) == "trimws.location.")] <- "location"

setwd('/Users/ploid/GitHub/tripadvisor-watpho')
UScities <- read.csv("Top5000Population.csv",header = FALSE)
cities <- as.vector(UScities$V1)
capitalCities <- read.csv("country-list.csv",header = TRUE)
countryCapital <- as.vector(capitalCities$capital)
countryName <- as.vector(capitalCities$country)
worldCitiesFile <- read.csv("world_cities.csv",header = TRUE, fileEncoding="latin1")
worldCities <- as.vector(worldCitiesFile$city)
continentfile <- read.csv("country-continent.csv",header = TRUE)
worldCountry <- as.vector(continentfile$name)

find.continent <- function(country){
  row.number <- which(tolower(iconv(continentfile$name)) == country)
  result.continent <- continentfile$factregion[row.number]
  # result.continent <- tolower(result.continent)
  return(result.continent)
}

find.sub.continent <- function(country){
  row.number <- which(tolower(iconv(continentfile$name)) == country)
  result.sub.continent <- continentfile$factsubregion[row.number]
  # result.sub.continent <- tolower(result.sub.continent)
  return(result.sub.continent)
}

find.num.country <- function(country){
  row.number <- which(tolower(iconv(continentfile$name)) == country)
  result.num.country <- continentfile$factcountry[row.number]
  # result.num.country <- tolower(result.num.country)
  return(result.num.country)
}

find.country <- function(capital){
  row.number <- which(tolower(capitalCities$capital) == capital)
  result.country <- capitalCities$country[row.number]
  result.country <- tolower(result.country)
  return(result.country)
}

find.country2 <- function(city){
  row.number <- which(tolower(worldCitiesFile$city) == city)
  result.country <- worldCitiesFile$country[row.number]
  result.country <- tolower(result.country)
  return(result.country)
}

separate.country <- function(location){
  if(location != "" && !is.na(location)){
    splited <- strsplit(location, ",")
    splited <- trimws(tail(unlist(splited),n=1))
    splited <- tolower(splited)
    
    if(grepl('\\=', location) || grepl("[[:digit:]]", location)){
      splited <- NA
    }
    if(grepl('\\/', location)){
      splited <- gsub("/"," ",splited , fixed = TRUE)
    }
    if(grepl('\\.', location)){
      splited <- gsub(".","",splited , fixed = TRUE)
    }
    
    splited.space <- as.vector(unlist(strsplit(as.character(splited), " ")))
    splited.space <- trimws(tail(splited.space,n=1))
    
    if(splited %in% c("london")){
      splited <- "uk"
    }
    if(splited %in% c("hk","hkg")){
      splited <- "china"
    }
    if(splited %in% c("brunei")){
      splited <- "brunei darussalam"
    }
    if(splited %in% c("lund")){
      splited <- "sweden"
    }
    if(splited %in% c("las pinas","davao city") || splited.space %in% c("davao")){
      splited <- "philippines"
    }
    if(splited %in% c("milano","ischia")){
      splited <- "italy"
    }
    if(splited %in% c("auckland")){
      splited <- "new zealand"
    }
    if(splited %in% c("cannes")){
      splited <- "france"
    }
    if(splited %in% c("ho chi minh","ho chi minh city")){
      splited <- "vietnam"
    }
    if(splited %in% c("uae","dxb")){
      splited <- "united arab emirates"
    }
    if(splited %in% c("bangkok","phuket","bkk","pattaya") ||splited.space %in% c("bangkok","phuket","thailand")){
      splited <- "thailand"
    }
    if(splited.space %in% c("ireland")){
      splited <- "ireland"
    }
    if(splited.space %in% c("finland")){
      splited <- "finland"
    }
    if(splited.space %in% c("japan")){
      splited <- "japan"
    }
    if(splited.space %in% c("pr")){
      splited <- "puerto rico"
    }
    if(splited %in% c("sg","s'pore") || splited.space %in% c("sg","s'pore")){
      splited <- "singapore"
    }
    if(splited %in% c("the netherlands","oosterhout")){
      splited <- "netherlands"
    }
    if(splited %in% c("district of columbia")){
      splited <- "columbia"
    }
    if(splited %in% c("vancouver bc","toronto","vancouver","ontario","british columbia","on","montreal") || splited.space %in% c("bc")){
      splited <- "canada"
    }
    if(splited %in% c("sp","rj","pe","ba","brasil","brasilia") || splited.space %in% c("sp","rj","pe","ba","brasil","paulo")){
      splited <- "brazil"
    }
    if(splited %in% c("melbourne","brisbane","queensland","newcastle","tullamarine","au","fremantle","gold coast city","gold coast") ||splited.space %in% c("australia","nsw","au")){
      splited <- "australia"
    }
    if(splited %in% c("kl","penang","selangor")){
      splited <- "malaysia"
    }
    if(splited %in% c("barcelona")){
      splited <- "spain"
    }
    if(splited %in% c("bangalore","chennai","noida")|| splited.space %in% c("india")){
      splited <- "india"
    }
    if(splited %in% c("bali","jakarta","ubud") || splited.space %in% c("bali","jakarta","jakarta barat")){
      splited <- "indonesia"
    }
    if(splited %in% tolower(c(trimws(worldCities)))){
      splited <- find.country2(splited)
    }
    if(splited %in% tolower(c(trimws(countryCapital)))){
      splited <- find.country(splited)
    }
    if((splited %in% tolower(c(state.name,state.abb,"united states","dc","Los Angeles","hyde park","santa clara","long beach","phoenix","new york city","united states of america","us","san fran"))) || splited.space %in% tolower(c(state.name,state.abb,"usa","nyc","brooklyn"))){
      splited <- "usa"
    }
    if(splited %in% c("united kingdom","gateshead","wells","england","sheringham","durham","devon","blackpool","darlington","surrey","highgate","milton keynes","retford","lincolnshire","stockport","hertfordshire","derbyshire","watford") || splited.space %in%  c("united kingdom","uk","england","yorkshire","scotland","lancs","manchester","aberdeen")){
      splited <- "uk"
    }
    return(splited)
  }else{
    return(NA)
  }
}

country <- rep(NA,nrow(dataset))
countrynum <- rep(NA,nrow(dataset))
for(i in 1:nrow(dataset)){
  country[i] <- separate.country(as.character(dataset$location[i]))
}
dataset$country <- factor(country)

# find continent and sub continent
continent <- rep(NA,nrow(dataset))
sub.continent <- rep(NA,nrow(dataset))
for(i in 1:nrow(dataset)){
  if(!is.na(dataset$country[i]) && dataset$country[i] %in% tolower(c(trimws(iconv(worldCountry))))){
    countrynum[i] <- find.num.country(as.character(dataset$country[i]))
    continent[i] <- find.continent(as.character(dataset$country[i]))
    sub.continent[i] <- find.sub.continent(as.character(dataset$country[i]))
  }else{
    continent[i] <- NA
  }
}
dataset$countrynum <- countrynum
dataset$continent <- continent
dataset$subcontinent <- sub.continent

# separate date data
day <- rep(NA,nrow(dataset))
month <- rep(NA,nrow(dataset))
year <- rep(NA,nrow(dataset))
weekday <- rep(NA,nrow(dataset))
weektype <- rep(NA,nrow(dataset))
for(i in 1:nrow(dataset)){
  if(!is.na(dataset$date[i])){
    date <- as.Date(dataset$date[i], format = date.format)
    day[i] <- as.numeric(format(date , format = "%d"))
    month[i] <- as.numeric(format(date , format = "%m"))
    year[i] <- as.numeric(format(date , format = "%y"))
    weekday[i] <- weekdays(date)
    if(weekday[i] %in% c("Saturday","Sunday")){
      weektype[i] <- "weekend"
    }else{
      weektype[i] <- "weekday"
    }
  }
}

dataset$day <- day
dataset$month <- month
dataset$year <- year
dataset$weekday <- factor(weekday)
dataset$weektype <- factor(weektype)

# create category data
dataset$category <- category

# delete all NA
# dataset <- dataset[complete.cases(dataset),]
# remove all data that have countrySize less than 10
# dataset <- dataset[which(dataset$countrySize >10),]
# ggplot(dataset,aes(country)) + geom_bar(show.legend = TRUE)

# remove all NA in date column
# dataset <- dataset[!(is.na(dataset$date)),]
# dataset <- dataset[!(is.na(dataset$fullreview)),]

#write a file
setwd('/Users/ploid/GitHub/tripadvisor-watpho/cleandata')
wd.clean <- paste('clean', attraction.file.name, sep = "")
if(dir.exists(paste('/Users/ploid/GitHub/tripadvisor-watpho/cleandata/', wd.clean, sep = "")) == FALSE){
  dir.create(wd.clean)
}
setwd(paste('/Users/ploid/GitHub/tripadvisor-watpho/cleandata/',wd.clean , sep = ""))

write.csv(dataset,file = file.name)
# write.arff(dataset, file = "test_weka.arff")