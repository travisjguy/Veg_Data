veg <- read.csv("~/Dropbox/Bighorn Sheep Data/Data for R class/veg.csv")

head(veg)

library("dplyr")

library("stringr")

library("magrittr")


length_veg <- sum(veg$Length)    ## finds the total length of all transects combined

total_length <- veg %>% group_by(Species) %>%
  summarize(total_length = sum(Length),
            percent_length = sum(Length)/length_veg)

head(total_length)

print(total_length, n= nrow(total_length))





