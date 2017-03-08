setwd("C:/Users/Administrator/Desktop/scrape-amazon-master/data")
rm(dataset)
file.list <- list.files()

for(file in file.list) {
  if(!exists("dataset")){
    dataset <- read.csv(file,header=TRUE)
  }
  
  if(exists("dataset")){
    temp.dataset <- read.csv(file,header=TRUE)
    dataset <- rbind(dataset,temp.dataset)
    rm(temp.dataset)
  }
}

write.csv(dataset, file = "amazon_final.csv")