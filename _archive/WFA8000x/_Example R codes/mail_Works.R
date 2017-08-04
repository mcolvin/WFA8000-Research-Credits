

#install.packages("RDCOMClient")
# Outlook has to be open...
library(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "colvin.mike@gmail.com"
outMail[["subject"]] = "reply to this if you get it"
outMail[["body"]] = "this is a test message please reply if you get it. \n Thanks, \n Mike"
## send it                     
outMail$Send()