library(rvest)
library(lubridate)
library(dplyr)

# loop for all states:

states = c("alabama","alaska","american-samoa","arizona","arkansas","california","colorado","connecticut",
           "delaware","district-of-columbia","florida","georgia","guam","hawaii","idaho","illinois",
           "indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts",
           "michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new-hampshire",
           "new-jersey","new-mexico","new-york","north-carolina","north-dakota","northern-mariana-islands","ohio","oklahoma",
           "oregon","pennsylvania","puerto-rico","rhode-island","south-carolina","south-dakota","tennessee","texas",
           "us-virgin-islands","utah","vermont","virginia","washington","west-virginia","wisconsin","wyoming")

tail = c("cases","tests-viral","tests-antibody","outcomes") 

BaseURL = "https://covidtracking.com/data/state"
Demo = data.frame(row.names = c("States","Date","Totalcases","Newcases","Confirmedcases","Probablecases",
    "TotalPCR","TotalABT_p","TotalABT_s","Recovered","Totaldeath","Newdeath","Probabledeath","Confirmeddeath"))

for (i in states){
  JoinURL <-paste(BaseURL,i, sep="/")
  
  #first table: cases of state
  FullURL = paste(JoinURL,"cases",sep="/")
  webpage <- read_html(FullURL)
  tables <- html_nodes(webpage,"table")
  
  parse_data <- html_table(tables[1], fill=TRUE)
  parse_data2 <- parse_data[[1]]
  #parse_data2 = parse_data2[1:245,]
  colnames(parse_data2) = c("Date","Totalcases","Newcases","Confirmedcases","Probablecases")
  
  parse_data2$Date = gsub("Date","",parse_data2$Date)
  parse_data2$Date = mdy(parse_data2$Date)
  
  parse_data2$Totalcases = gsub("^.*?)","",parse_data2$Totalcases) #clear everything to the first ")"
  parse_data2$Totalcases = gsub(",","",parse_data2$Totalcases)
  parse_data2$Totalcases = as.numeric(parse_data2$Totalcases)
  
  parse_data2$Newcases = gsub("New cases","",parse_data2$Newcases)
  parse_data2$Newcases = gsub(",","",parse_data2$Newcases)
  parse_data2$Newcases = as.numeric(parse_data2$Newcases)
  
  parse_data2$Confirmedcases = gsub("Confirmed cases","",parse_data2$Confirmedcases)
  parse_data2$Confirmedcases = gsub(",","",parse_data2$Confirmedcases)
  parse_data2$Confirmedcases = as.numeric(parse_data2$Confirmedcases)
  
  parse_data2$Probablecases = gsub("Probable Cases","",parse_data2$Probablecases)
  parse_data2$Probablecases = gsub(",","",parse_data2$Probablecases)
  parse_data2$Probablecases = as.numeric(parse_data2$Probablecases)
  
  parse_data2 = filter(parse_data2,parse_data2$Date >= as.Date("2020-04-01"))
  
  # second table: Viral (PCR) tests:
  FullURL = paste(JoinURL,"tests-viral",sep="/")
  webpage <- read_html(FullURL)
  tables <- html_nodes(webpage,"table")
  
  parse_data <- html_table(tables[1], fill=TRUE)
  parse_data3 <- parse_data[[1]]
  #parse_data3 = parse_data3[1:245,]
  parse_data3 <- parse_data3[,c(1,6)]
  colnames(parse_data3) = c("Date","TotalPCR")
  
  parse_data3$Date = gsub("Date","",parse_data3$Date)
  parse_data3$Date = mdy(parse_data3$Date)
  
  parse_data3$TotalPCR = gsub("^.*?)","", parse_data3$TotalPCR) #clear everything to the first ")"
  parse_data3$TotalPCR = gsub(",","", parse_data3$TotalPCR)
  parse_data3$TotalPCR = as.numeric( parse_data3$TotalPCR)
  
  parse_data3 = filter(parse_data3,parse_data3$Date >= as.Date("2020-04-01"))
  
  # third table: antibody tests
  FullURL = paste(JoinURL,"tests-antibody",sep="/")
  webpage <- read_html(FullURL)
  tables <- html_nodes(webpage,"table")
  
  parse_data <- html_table(tables[1], fill=TRUE)
  parse_data4 <- parse_data[[1]]
  #parse_data4 = parse_data4[1:245,]
  parse_data4 <- parse_data4[,1:3]
  colnames(parse_data4) = c("Date","TotalABT_p","TotalABT_s")
  
  parse_data4$Date = gsub("Date","",parse_data4$Date)
  parse_data4$Date = mdy(parse_data4$Date)
  
  parse_data4$TotalABT_p = gsub("^.*?)","", parse_data4$TotalABT_p) #clear everything to the first ")"
  parse_data4$TotalABT_p = gsub(",","", parse_data4$TotalABT_p)
  parse_data4$TotalABT_p = as.numeric(parse_data4$TotalABT_p)
  
  parse_data4$TotalABT_s = gsub("^.*?)","", parse_data4$TotalABT_s) #clear everything to the first ")"
  parse_data4$TotalABT_s = gsub(",","", parse_data4$TotalABT_s)
  parse_data4$TotalABT_s = as.numeric(parse_data4$TotalABT_s)
  
  parse_data4 = filter(parse_data4,parse_data4$Date >= as.Date("2020-04-01"))
  
  #fourth table: outcomes of state
  FullURL = paste(JoinURL,"outcomes",sep="/")
  webpage <- read_html(FullURL)
  tables <- html_nodes(webpage,"table")
  
  parse_data <- html_table(tables[1], fill=TRUE)
  parse_data5 <- parse_data[[1]]
  #parse_data5 = parse_data5[1:245,]
  colnames(parse_data5) = c("Date","Recovered","Totaldeath","Newdeath","Probabledeath","Confirmeddeath")
  
  parse_data5$Date = gsub("Date","",parse_data5$Date)
  parse_data5$Date = mdy(parse_data5$Date)
  
  parse_data5$Recovered = gsub("Recovered","",parse_data5$Recovered)
  parse_data5$Recovered = gsub(",","",parse_data5$Recovered)
  parse_data5$Recovered = as.numeric(parse_data5$Recovered)
  
  parse_data5$Totaldeath = gsub("^.*?)","",parse_data5$Totaldeath) #clear everything to the first ")"
  parse_data5$Totaldeath = gsub(",","",parse_data5$Totaldeath)
  parse_data5$Totaldeath = as.numeric(parse_data5$Totaldeath)
  
  parse_data5$Newdeath = gsub("New deaths","",parse_data5$Newdeath)
  parse_data5$Newdeath = gsub(",","",parse_data5$Newdeath)
  parse_data5$Newdeath = as.numeric(parse_data5$Newdeath)
  
  parse_data5$Probabledeath = gsub("^.*?)","",parse_data5$Probabledeath)
  parse_data5$Probabledeath = gsub(",","",parse_data5$Probabledeath)
  parse_data5$Probabledeath = as.numeric(parse_data5$Probabledeath)
  
  parse_data5$Confirmeddeath = gsub("^.*?)","",parse_data5$Confirmeddeath)
  parse_data5$Confirmeddeath = gsub(",","",parse_data5$Confirmeddeath)
  parse_data5$Confirmeddeath = as.numeric(parse_data5$Confirmeddeath)
  
  parse_data5 = filter(parse_data5,parse_data5$Date >= as.Date("2020-04-01"))
  
  #merge two table into one single for each state and then combine them
  demo = merge(parse_data2,parse_data3,"Date")
  demo = merge(demo,parse_data4,"Date")
  demo = merge(demo,parse_data5,"Date")
  demo$States = c(i)
  
  demo <- demo[, c("States","Date","Totalcases","Newcases","Confirmedcases","Probablecases",
              "TotalPCR","TotalABT_p","TotalABT_s","Recovered","Totaldeath","Newdeath","Probabledeath","Confirmeddeath")]
  demo <- demo[order(demo$Date,decreasing = TRUE),] #sort the table based on decreasing date
  Demo = rbind(Demo,demo)
}

test1 = ifelse(is.na(Demo$TotalPCR),0,Demo$TotalPCR)
test2 = ifelse(is.na(Demo$TotalABT_p),0,Demo$TotalABT_p)
test3 = ifelse(is.na(Demo$TotalABT_s),0,Demo$TotalABT_s)

Demo$Totaltests = ifelse(is.na(Demo$TotalPCR) & is.na(Demo$TotalABT_p) & is.na(Demo$TotalABT_s),"", test1 + test2 + test3)
str(Demo$Totaltests)

Demo <- Demo[, c("States","Date","Totalcases","Newcases","Confirmedcases","Probablecases",
                 "TotalPCR","TotalABT_p","TotalABT_s","Totaltests","Recovered","Totaldeath","Newdeath","Probabledeath","Confirmeddeath")]
row.names(Demo) = 1:nrow(Demo)
Demo$States = as.factor(Demo$States)
str(Demo)
View(Demo)

#filter based on date, ex: for only november
Novemberdata = filter(Demo,Demo$Date >= as.Date("2020-11-01"))
View(Novemberdata)

#filter based on date and state: 
Novemberalabama = filter(Demo,Demo$Date >= as.Date("2020-11-01") & Demo$States == "alabama")
View(Novemberalabama)

write.csv(Demo,"realtime_covid.csv")

#data 
library(stringr)
table(is.na(Demo))
str_detect(Demo,"NA")
