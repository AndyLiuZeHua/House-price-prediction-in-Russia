library(dplyr)
data <- read.csv("train.csv")

for(j in 1:291) {
  test1 <- 
    data %>%
    group_by(data[,j]) %>%
    summarise(median_price = median(price_doc)) 
  test2 <-
    test1 %>%
    filter(!is.na(test1[,1]))
  
  for(i in 1:30471) {
    if(is.na(data[i,j])) {
      testnew <- abs(test2[,2]-data[i,292])
      num <- which(testnew == min(testnew))
      data[i,j] <- test2[num,1]
    }
  }
  
}

write.csv(data,"dataclean_noNA.csv")
