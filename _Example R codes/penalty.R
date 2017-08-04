library(lubridate)
Timestamp<- parse_date_time('10/8/2016 10:49:00','%m/%d/%Y %H:%M:%S')
due<- parse_date_time('10/7/2016 17:00:00','%m/%d/%Y %H:%M:%S')
late<- as.numeric(Timestamp-due)

exp(-1*(-log(0.9)*(late/24)))*10