##########################################
#
#  Read in school reviews from API
#  in XML format and create dataframe
#
##########################################


#API function: 
#http://fredheir.github.io/WebScraping/Lecture2_2015/p2_out.pdf

#XML2: https://blog.rstudio.com/2015/04/21/xml2/
#https://www.rdocumentation.org/packages/xml2/versions/1.2.0/topics/xml_text


###################

#require(ggplot2)
require(lubridate)
require(plyr)
require(stringr)
require(jsonlite)
require(XML)
library(xml2)
library(gdata)

#Need to set working directory
setwd(mydir)

#Clear objects
rm(list=ls(pattern="*"))

#Number display
options(scipen=999)


###################
# Need to input 
#  API key, state, 
#  city
###################

#API link
link <- "https://api.greatschools.org"

#API key
APIKey <- 

#Set location
state <- "IL"
district <- "Chicago"

#Sample
sample <- "private"



###################

if (1) {


##-------------------------------------
#
# Get list of all schools 
#
##-------------------------------------

#Note: Excludes preK  

     url <- paste(link,"/schools/",state,"/",district,"?key=",APIKey,sep="")
  
   if (sample != "") {
     url <- paste(link,"/schools/",state,"/",district, "/", sample, "?key=",APIKey,"&limit=5000",sep="")     
   }
   
  
 #Read the data - get number of schools  
  pg <- read_xml(url)
  children = xml_children(pg)
  numschools = length(children)

  print(numschools)
  
 #Create empty data frame 
  vars <- c("gsId", "name", "type", "gradeRange","enrollment","gsRating",
            "parentRating","city","state","districtId","district",
            "districtNCESId","address","phone","fax","website","ncesId",
            "lat","lon","overviewLink","ratingsLink","reviewsLink","schoolStatsLink")
  
  schooldata <- data.frame(matrix(NA_character_,nrow=0,ncol=length(vars)))  
  colnames(schooldata) <- vars
  
  rows = 1:numschools

  for (i in rows) {
  
    c = children[[i]]  
    d = xml_children(c) 

    schooldatarow <- data.frame(matrix(NA_character_,nrow=1,ncol=length(vars)))  
    colnames(schooldatarow) <- vars    
        
    for (v in vars) {
      
      path <- paste("//schools/school[",i,"]/",v,sep="")
      xt <- xml_text(xml_find_all(d,path))
      #print(xt)
      if (length(xt)>0) {
        schooldatarow[v] <- xt
      }
    }
    
    schooldata <- rbind(schooldata,schooldatarow)
    print(i)
  }
    
  #write.csv(schooldata, file = paste("schooldata_Chicago_public.csv", sep=""),row.names=FALSE) 
   write.csv(schooldata, 
             file = paste("schooldata_",district,"_",sample,".csv", sep=""),
             row.names=FALSE)
     
}
  
  
  
  
  
  
if (1) {  
  
  ##-------------------------------------
  #
  # Get school reviews
  #
  ##-------------------------------------

  keep(schooldata,link,APIKey,state,district,sample, sure = TRUE)
  
  #School IDs 
  schoolIDs <- unlist(schooldata["gsId"])
  #schoolIDs <- schoolIDs[1:5,]  
  
  getData <- function(idtext,urltext,varsdf,pathtext,tablenametext) {
    
    pg <- read_xml(urltext)
    children = xml_children(pg)
    numentries = length(children)
    
    if (numentries>0) {
    rows = 1:numentries    
    
    gsdata <- data.frame(matrix(NA_character_,nrow=0,ncol=length(varsdf)))  
    colnames(gsdata) <- varsdf    

    for (i in rows) {
      
      c = children[[i]]  
      d = xml_children(c) 
      
      gsdatarow <- data.frame(matrix(NA_character_,nrow=1,ncol=length(varsdf)))  
      colnames(gsdatarow) <- varsdf    
      
      for (v in vars) {
        
        path <- paste(pathtext,"[",i,"]/",v,sep="")
        xt <- xml_text(xml_find_all(d,path))
        #print(xt)
        if (length(xt)>0) {
          gsdatarow[v] <- xt
        }
      }
      
      gsdata <- rbind(gsdata,gsdatarow)
    }
    
    gsdata["gsId"] = idtext
    write.csv(gsdata, file = paste(tablenametext,".csv", sep=""),row.names=FALSE)
    return(gsdata)      
    }
  }

  #Create empty data frame 
  vars <- c("schoolName", "schoolAddress", "rating", "submitter","postedDate","comments")
  
  fulldata <- data.frame(matrix(NA_character_,nrow=0,ncol=length(vars)))  
  colnames(fulldata) <- vars    
  
  pathtext <- "//reviews/review"
  
  tablenamestub <- "reviewdata"

  counter = 0
      
  for (i in schoolIDs) {

    counter = counter + 1
    print(counter)
    
    #Review link  
     url <- paste(link,"/reviews/school/",state,"/",i,"?key=",APIKey,"&limit=900",sep="") 
        
    tablename <- paste(tablenamestub,i,sep="")
    
    getData(i,url,vars,pathtext,tablename)
    
    #fulldata <- rbind(fulldata,gsdata)
  }
  
  for (i in schoolIDs) {  
    tablename <- paste(tablenamestub,i,".csv",sep="")
    if (file.exists(tablename)) {
    temp=read.csv(tablename, header=TRUE)
    fulldata <- rbind(fulldata,temp)
    file.remove(tablename)
    }
  }

  #Assign document ID (not sure how useful this will be)
  fulldata["docId"] <- seq.int(nrow(fulldata))  

  write.csv(fulldata, 
            file = paste("fulldata_",district,"_",sample,".csv", sep=""),
            row.names=FALSE)
  
  
    
  #Check for duplicates
  #sub<-fulldata[c("schoolName","schoolAddress","rating","submitter","postedDate","gsId")]
  #duplicated(sub)
  
}



  
  