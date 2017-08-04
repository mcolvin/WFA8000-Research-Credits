


library(rdrop2)

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

# SEE
# http://deanattali.com/2015/06/14/mimicking-google-form-shiny/

# the .httr-oauth was generated automatically to app folder by runging
# token <- rdrop2:drop_auth()

# CODE TO SAVE DATA LOCALLY SAVEDATA AND THEN UPLOAD TO DROPBOX

saveData <- function(data) {
 
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    token <- readRDS("droptoken.rds")
    drop_acc(dtoken = token)   
    drop_upload(filePath, dest = "stuff", dtoken = token)
}

