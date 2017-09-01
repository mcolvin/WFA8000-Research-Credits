

#install.packages("RDCOMClient")
# Outlook has to be open...
library(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "5155200564@vtext.com" # email a text msg to phone
outMail[["subject"]] = "Done"
outMail[["body"]] = "simulations are done"
## send it                     
outMail$Send()