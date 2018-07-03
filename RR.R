fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileLocal <- "stormData.csv.bz2"

if(!file.exists(fileLocal)){
  download.file(fileURL, fileLocal)
}


#stormData <- read.csv(bzfile("stormData.csv.bz2"))


stormDataSmall <- stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]


stormDataSmall[toupper(stormDataSmall$PROPDMGEXP) == "H", ]$PROPDMGEXP = "H"
stormDataSmall[toupper(stormDataSmall$PROPDMGEXP) == "K", ]$PROPDMGEXP = "K"
stormDataSmall[toupper(stormDataSmall$PROPDMGEXP) == "M", ]$PROPDMGEXP = "M"
stormDataSmall[toupper(stormDataSmall$PROPDMGEXP) == "B", ]$PROPDMGEXP = "B"

stormDataSmall$PROPDMG_AMT = 0
stormDataSmall[stormDataSmall$PROPDMGEXP == "H",]$PROPDMG_AMT = stormDataSmall[stormDataSmall$PROPDMGEXP == "H",]$PROPDMG * 10 ^ 2
stormDataSmall[stormDataSmall$PROPDMGEXP == "K",]$PROPDMG_AMT = stormDataSmall[stormDataSmall$PROPDMGEXP == "K",]$PROPDMG * 10 ^ 3
stormDataSmall[stormDataSmall$PROPDMGEXP == "M",]$PROPDMG_AMT = stormDataSmall[stormDataSmall$PROPDMGEXP == "M",]$PROPDMG * 10 ^ 6
stormDataSmall[stormDataSmall$PROPDMGEXP == "B",]$PROPDMG_AMT = stormDataSmall[stormDataSmall$PROPDMGEXP == "B",]$PROPDMG * 10 ^ 9


stormDataSmall[toupper(stormDataSmall$CROPDMGEXP) == "K", ]$CROPDMGEXP = "K"
stormDataSmall[toupper(stormDataSmall$CROPDMGEXP) == "M", ]$CROPDMGEXP = "M"
stormDataSmall[toupper(stormDataSmall$CROPDMGEXP) == "B", ]$CROPDMGEXP = "B"

stormDataSmall$CROPDMG_AMT = 0
stormDataSmall[stormDataSmall$CROPDMGEXP == "H",]$CROPDMG_AMT = stormDataSmall[stormDataSmall$CROPDMGEXP == "H",]$CROPDMG * 10 ^ 2
stormDataSmall[stormDataSmall$CROPDMGEXP == "K",]$CROPDMG_AMT = stormDataSmall[stormDataSmall$CROPDMGEXP == "K",]$CROPDMG * 10 ^ 3
stormDataSmall[stormDataSmall$CROPDMGEXP == "M",]$CROPDMG_AMT = stormDataSmall[stormDataSmall$CROPDMGEXP == "M",]$CROPDMG * 10 ^ 6
stormDataSmall[stormDataSmall$CROPDMGEXP == "B",]$CROPDMG_AMT = stormDataSmall[stormDataSmall$CROPDMGEXP == "B",]$CROPDMG * 10 ^ 9


library("dplyr")
library("ggplot2")


# Top fatalities 
# sum_fatalities <- select(stormDataSmall, FATALITIES, EVTYPE) %>%
#   group_by(EVTYPE) %>%
#   summarise(Tot_fatalities = sum(FATALITIES)) %>%
#   arrange(desc(Tot_fatalities))
# 
# top_fatalities <- sum_fatalities[1:10,]
# 
# top_fatalities$EVTYPE <- factor(top_fatalities$EVTYPE ,levels = top_fatalities$EVTYPE)



# g <- ggplot(top_fatalities,(aes(EVTYPE,Tot_fatalities))) + 
#   geom_bar(stat = "identity") +
#   theme_light(base_size = 10) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold")) +
#   labs(x = "Event Category", y = "Total Fatalities", title = "Top 10 major fatalities in weather event")
# 
# print(g)


# Top Injuries 
# sum_injuries <- select(stormDataSmall, INJURIES, EVTYPE) %>%
#   group_by(EVTYPE) %>%
#   summarise(Tot_injuries = sum(INJURIES)) %>%
#   arrange(desc(Tot_injuries))
# 
# top_injuries <- sum_injuries[1:10,]
# 
# top_injuries$EVTYPE <- factor(top_injuries$EVTYPE ,levels = top_injuries$EVTYPE)


# g <- ggplot(top_injuries,(aes(EVTYPE,Tot_injuries))) + 
#   geom_bar(stat = "identity") +
#   theme_light(base_size = 10) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold")) +
#   labs(x = "Event Category", y = "Total Injuries", title = "Top 10 major injuries record in weather event")
# 
# print(g)





# Top Economics Disasters by extream weather
economis_loss <- select(stormDataSmall, EVTYPE, PROPDMG_AMT, CROPDMG_AMT) %>%
  group_by(EVTYPE) %>%
  summarise(tot_economis_loss = sum(PROPDMG_AMT + CROPDMG_AMT)) %>%
  arrange(desc(tot_economis_loss))

top_economis_loss <- economis_loss[1:10,]

top_economis_loss$EVTYPE <- factor(top_economis_loss$EVTYPE ,levels = top_economis_loss$EVTYPE)


g <- ggplot(top_economis_loss,(aes(EVTYPE,tot_economis_loss))) + 
  geom_bar(stat = "identity") +
  theme_light(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Event Category", y = "Sum of loss ($)", title = "Top 10 major economis loss in weather event")

print(g)