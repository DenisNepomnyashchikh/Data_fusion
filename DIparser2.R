library(XML)
library(RCurl)
library(RJSONIO)

#Keys for GoogleBooks API
key1 <- "AIzaSyCucN6THP0WVK9GCj2jbjX7QW8uYnzRHsk"
key2 <- "AIzaSyBn3x94wRY_Q5hf5TxrcyXqvA02k60hDFQ"
#key1 <- "AIzaSyB15GhTa5RQPNyltNeT6c1f0wPqC0za_DE"
#key2 <- "AIzaSyApjSjwgDfXGcpKIB6FsIVj5MtNAU1nbzA"

#Read csv file from Task4
csv <- read.csv('New_Book_Catalogue.csv', header=T, sep = ";", encoding="Windows-1250")

#Create subset for testing
testcsv <-  csv[324:326,]

#Remove all empty rows
testcsv <- na.omit(testcsv)

#Remove all duplicate rows from dataset
testcsv <- testcsv[!duplicated(testcsv$title), ]

#Convert title and authors in character type and remove factors
testcsv[2] <- lapply(testcsv[2], as.character)
testcsv[4] <- lapply(testcsv[4], as.character)
testcsv <- data.frame(testcsv, stringsAsFactors=FALSE)

#Create a short names for columns
a <- testcsv$title
b <- testcsv$authors

#Remove odd whitespaces in the end of every rows
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
a <- trim(a)
b <- trim(b)

#Update our dataset
testcsv$title <- a
testcsv$authors <- b

#Replace whitespaces between words on %20
remspace <- function(y){
  gsub(pattern = " ", replacement = "%20", x = y)
}
a <- remspace(a)
b <- remspace(b)

#Create new columns in out dataset
testcsv$newname <- 0
testcsv$publisher <- 0
testcsv$subtitle <- 0
testcsv$publishdate <- 0
testcsv$pagecount <- 0
testcsv$category <- 0
testcsv$language <- 0
testcsv$sale <- 0
testcsv$price <- 0
testcsv$currency <- 0
testcsv$mature <- 0
testcsv$isbn10 <- 0
testcsv$isbn13 <- 0
testcsv$matching <- 0

#Create flag about possible wrong matching
matching <- 0

#Create main function that returning list with new valiables. 
#
ppp <- function(x, y, year, key){
  #result <- 0
  #gu1 <- 0
  #First attempt to find book with title and authors
  gu1 <- getURL(paste0("https://www.googleapis.com/books/v1/volumes?q=intitle:",x,"+inauthor:",y,"&start4ndex=0&key=",key,"&maxResults=40"),ssl.verifyhost=F,ssl.verifypeer=F,followlocation=T)
  result <- fromJSON(gu1)
  matching <- 0
  
  if (result$totalItems>30){
    #result <- 0
    #gu1 <- 0
    yy <- tail(strsplit(a,split="%20")[[1]],1)
    gu4 <- getURL(paste0("https://www.googleapis.com/books/v1/volumes?q=intitle:",yy,"+inauthor:",y,"&start4ndex=0&key=",key,"&maxResults=40"),ssl.verifyhost=F,ssl.verifypeer=F,followlocation=T)
    result <- fromJSON(gu4)
    matching <- 1
    gu1 <- gu4
  }
  
  #If not, try to find book only with title
  if (result$totalItems==0){
    gu2 <- getURL(paste0("https://www.googleapis.com/books/v1/volumes?q=intitle:",x,"&start4ndex=0&key=",key,"&maxResults=40"),ssl.verifyhost=F,ssl.verifypeer=F,followlocation=T)
    result <- fromJSON(gu2)
    gu1 <- gu2
    matching <- 2
    #If not, try to find book with authors and only one word of title
    if (result$totalItems==0){
      xx <- strsplit(x,"%20")[[1]][1] #Remove all words after first word
      gu2 <- getURL(paste0("https://www.googleapis.com/books/v1/volumes?q=intitle:",xx,"+inauthor:",y,"&start4ndex=0&key=",key,"&maxResults=40"),ssl.verifyhost=F,ssl.verifypeer=F,followlocation=T)
      result <- fromJSON(gu2)
      gu1 <- gu2
      matching <- 3
      #If not, get the first book from the list of books these authors and state a flag "probably not" 
      if (result$totalItems==0){
        gu2 <- getURL(paste0("https://www.googleapis.com/books/v1/volumes?q=inauthor:",y,"&start4ndex=0&key=",key,"&maxResults=40"),ssl.verifyhost=F,ssl.verifypeer=F,followlocation=T)
        result <- fromJSON(gu2)
        gu1 <- gu2
        matching <- 4
      }
    }
  }
  
  num <- 1
  if (result$totalItems > 40){
    result$totalItems <- 40
  }
  if (result$totalItems >= 2) {
    result$totalItems <- result$totalItems - 1
  }
  date <- 0
  #next
  for (i in 1:result$totalItems){
    if (result$totalItems == 0){
      next
    }
    date1 <- result$items[[i]]$volumeInfo$publishedDate
    date <- c(date, date1)  
  }
  
  substrLeft <- function(z){
    substr(z, 1, 4)
  }
  kkk <- substrLeft(date[-1])
    
  for (i in 1:length(kkk)){
    if (result$totalItems == 0){
      next
    }  
    if (length(date) == 1){
      next
    }
   if (year == kkk[i]){
      num <- i    
    }
    
  }
  
  #Create new variables from JSON for our new columns  
  newname <- result$items[[num]]$volumeInfo$title
  publ <- result$items[[num]]$volumeInfo$publisher
  sub <- result$items[[num]]$volumeInfo$subtitle
  pram <- result$items[[num]]$saleInfo$retailPrice$amount
  prcur <- result$items[[num]]$saleInfo$retailPrice$currencyCode
  date <- result$items[[num]]$volumeInfo$publishedDate
  pc <- result$items[[num]]$volumeInfo$pageCount
  cat <-result$items[[num]]$volumeInfo$categories
  mr <- result$items[[num]]$volumeInfo$maturityRating
  lang <- result$items[[num]]$volumeInfo$language
  sale <- result$items[[num]]$saleInfo$saleability
  isbn10 <- NA
  isbn13 <- NA
  
  #Create list which consist of information about ISBN
  ll <- result$items[[num]]$volumeInfo$industryIdentifiers[[1]]
  
  #Check type of identifier
  isbncheck <- sapply(ll[[1]], `[`, 1)
  
  #Save the answer from API (JSON)
  resultjson <- gu1
  
  #If no identifiers or OTHER then just return list of variables
  if (is.null(ll)){
    return(list(newname, publ, sub, date, pc, cat, lang, sale, pram, prcur, mr, isbn10, isbn13, matching, resultjson))
  }
  else if (isbncheck == "OTHER"){
    return(list(newname, publ, sub, date, pc, cat, lang, sale, pram, prcur, mr, isbn10, isbn13, matching, resultjson))
  }
  #If find ISBN10 or ISBN13 then save their 
  else if (isbncheck == "ISBN_10"){
    isbn10 <- sapply(ll[[2]], `[`, 1)
    #isbn13 <- NA
  }
  else if (isbncheck == "ISBN_13"){
    isbn13 <- sapply(ll[[2]], `[`, 1)
    #isbn13 <- NA
  }
  
  #Create a list with second output with identifiers (only for ISBN13)
  ll2 <- result$items[[num]]$volumeInfo$industryIdentifiers[[2]]
  #If it is empty then return our list
  if (is.null(ll2)){
    list(newname, publ, sub, date, pc, cat, lang, sale, pram, prcur, mr, isbn10, isbn13, matching, resultjson)
  }
  #If find ISBN13 then save that 
  else if (sapply(ll2[[1]], `[`, 1) == "ISBN_13"){
    #isbn10 <- NA
    isbn13 <- sapply(ll2[[2]], `[`, 1)
  }
  
  #return main list of variables
  
  list(newname, publ, sub, date, pc, cat, lang, sale, pram, prcur, mr, isbn10, isbn13, matching, resultjson) 
  
  
}

#Function to convert NULL to NA because NULL has zero length
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#Create a vector for storing JSON
res<-0

#Main loop - execute function for every pair title and authors with divided by 2 loops without breaks for replacing keys
for (i in 1:nrow(testcsv)){
  if (i>600) {
    cc <- ppp(a[i], b[i], testcsv$year[i], key2)
  }
  else {
    cc <- ppp(a[i], b[i], testcsv$year[i], key1)
  }
  #Save vector without NULL in our dataset except 14-th value (JSON)
  testcsv[i, 10:23] <- unlist(nullToNA(cc[-15]))
  #Save JSON to res
  res <- c(res, cc[15])
}

#Remove first zero value in res
res <- data.frame(res[-1])
#Set names for res like ids from our dataset
colnames(res) <- testcsv$id

#Create two csv files. 
#1. Our new dataset with added variables in separate columns.
#2. Dataset with output from API (JSON)
write.csv(testcsv, row.names=FALSE, file = "testcsv.csv")
write.csv(res, row.names=FALSE, file = "resjson.csv")
