######################################################
#
#  Topic modeling of school reviews
#
######################################################



# # # # # # # # # # # # 
# #  Preliminary steps
# # # # # # # # # # # # 

 setwd(mydir)
 rm(list=ls(pattern="*"))
 
 library(stm)        # Package for structural topic modeling
 library(igraph)     # Package for network analysis and visualisation
 library(stmCorrViz) # Package for hierarchical correlation view of STMs
 library(stringr)
 library(tm)
 library(plyr)
 library(taRifx)     # Package for destring() 
 library(plyr)       # Package for join()

 library(maptools)  
 
#Run diagnostics  
 diagnostics = 0
  
 
# # # # # # # # # # # # 
# #  Read data
# # # # # # # # # # # #  

#Review data  
#Need to convert to Unicode-8 when saving as csv  
 reviewdata <- read.csv("fulldata.csv") 

#School data
 schooldata <- read.csv("schooldata.csv") 
 schoolvars <- c("gsId","type","gradeRange","enrollment","gsRating","parentRating","ncesId")
 schooldata <- schooldata[schoolvars]  
 
#Merge review and school data 
 data <- join(reviewdata, schooldata, by="gsId", type="left")
 table(data$type)
 
 
 
# # # # # # # # # # # # #
# #  Prepare review data
# # # # # # # # # # # # #   
 
#Rename columns of dataframe
 colnames(data)[1]<-"school" 
#colnames(data)[colnames(data)=="comments"]<-"documents"  

#Select parent reviews
 table(data$submitter)
 data <- data[ which(data$submitter=="parent"), ] 

#Drop reviews flagged unusuable 
 table(data$unusable)
 data <- data[ which(is.na(data$unusable)), ] 
 
#Get date of review 
#Might convert to semesters
 data$month <- destring(substring(data$postedDate,4,5))
 data$year <- destring(substring(data$postedDate,7,8))+2000
 table(data$year)
 table(data$month)   
 
#Convert to lowercase 
 data$documents <- tolower(data$comments)
 
#Replace all punctuation - reduce weird vocabulary 
#Won't be used by STM anyway 
 for (p in c(",", "/", "-", "_", "(", ")", "!", "?", ".")) {
   print(p)
   data$documents <- gsub(p, " ", data$documents, fixed=TRUE)   
   print(data$documents[[1]])
 }
 
#Remove extra spaces (will matter for compound words)
 data$documents<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data$documents, perl=TRUE)  
 print(data$documents[[1]])
 
#Replace strings
 data$documents <- gsub("after school","after-school",data$documents)
 data$documents <- gsub("afterschool","after-school",data$documents) 
 data$documents <- gsub(" k "," kindergarten ",data$documents) 
 data$documents <- gsub(" pk "," pre-kindergarten ",data$documents)  
 data$documents <- gsub("pre kindergarten","pre-kindergarten ",data$documents)   
 data$documents <- gsub("prekindergarten","pre-kindergarten ",data$documents)    
 data$documents <- gsub("middle school","middle-school",data$documents) 
 data$documents <- gsub("high school","high-school",data$documents)  
 data$documents <- gsub("middleschool","middle-school",data$documents) 
 data$documents <- gsub("highschool","high-school",data$documents)   
 data$documents <- gsub("preschool","pre-school",data$documents)    
 data$documents <- gsub("pre school","pre-school",data$documents)     
 data$documents <- gsub("private school","private-school",data$documents)      
 data$documents <- gsub("privateschool","private-school",data$documents)       
 data$documents <- gsub("special need","special-need",data$documents) 
 data$documents <- gsub("special education","special-education",data$documents)    
 data$documents <- gsub("special ed","special-education",data$documents)  
 data$documents <- gsub("common core","common-core",data$documents)   
 data$documents <- gsub("gen ed","general-education",data$documents)  
 data$documents <- gsub("dual language","dual-language",data$documents)   
 data$documents <- gsub("g&t","gifted and talented",data$documents)   
 data$documents <- gsub("hard work","hard-work",data$documents)   
 data$documents <- gsub("worm","warm",data$documents)   
 data$documents <- gsub(" pe "," physical-education ",data$documents)  
 data$documents <- gsub("physical education","physical-education",data$documents)   
 data$documents <- gsub("physical ed","physical-education",data$documents)   
 data$documents <- gsub("well being","well-being",data$documents)   
 data$documents <- gsub("wellbeing","well-being",data$documents)    
 data$documents <- gsub("team work","teamwork",data$documents) 
 data$documents <- gsub("team-work","teamwork",data$documents)
 data$documents <- gsub("self-confidence","self confidence",data$documents) 
 data$documents <- gsub("open house","openhouse",data$documents) 
 data$documents <- gsub("open-house","openhouse",data$documents)  

#Remove word 'school' 
 data$documents <- gsub(" school","",data$documents)    
 
 write.csv(data, file = "school_reviews.csv")   
  
#Drop columns 
#data <- data[, !(colnames(data) %in% c("month", "year"))] 
 data[,c("unusable","submitter","postedDate")] <- list(NULL)

#Drop missing reviews
 data <- data[ which(!is.na(data$documents)), ]   
  
#Metadata 
 metacols <- data[c("school","year","type")]

 

# # # # # # # # # # # # # #
# #  Start data processing
# # # # # # # # # # # # # #     
  
 processed <- textProcessor(data$documents, metadata=metacols)
 
#'documents' contains count of each vocab word in the document
#'vocab' contains list of vocab words
 processed[["documents"]][[1]]  
 processed[["vocab"]][1:10]
 processed[["vocab"]][1000:1010]
 vocablist <- processed[["vocab"]]
 write.csv(vocablist, file = "vocablist.csv") 

if (diagnostics==1) {   
#Plot documents removed at each threshold 
#Word removed if not appearing in threshold+1 documents
#To impose no restrictions, set lower.thresh=0
 plotRemoved(processed$documents, lower.thresh = seq(1, 50, by = 1))
}

#Word removed if not appearing in threshold+1 documents
 #To impose no restrictions, set lower.thresh=0
 out <- prepDocuments(processed$documents, processed$vocab, 
                        processed$meta, lower.thresh=4)
  
#Display words removed and restricted vocab list  
 #out[["words.removed"]]
 #out[["vocab"]]
 vocablist <- out[["vocab"]]
 write.csv(vocablist, file = "restricted_vocablist.csv") 
 vocablist <- out[["words.removed"]]
 write.csv(vocablist, file = "removed_vocablist.csv")  
 

 
# # # # # # # # # # # # # #
# #  Diagnostics: select K
# # # # # # # # # # # # # #      
 
if (diagnostics==1) {   
 #https://www.rdocumentation.org/packages/stm/versions/1.3.3/topics/searchK 
 #See https://github.com/bstewart/stm/issues/121 for interpretation   
 # prevalence =~ school + s(year) + type 
  storage <- searchK(documents = out$documents, vocab = out$vocab, 
                    K = c(3,18,33,48),  
                    max.em.its = 75, data = out$meta, 
                    init.type = "Spectral")   
 
#Plot semantic coherence and exclusivity as functions of K 
#Might need to expand plot window 
 if(!is.null(dev.list())) dev.off()
 old.par <- par(mfrow=c(2, 1))
 plot(storage[["results"]][["K"]],storage[["results"]][["exclus"]], 
               xlab="", ylab="Exclusivity")
 plot(storage[["results"]][["K"]],storage[["results"]][["semcoh"]], 
               xlab="K", ylab="Semantic coherence")
 dev.copy(pdf,"Kplots.pdf", width=4, height=8) 
 par(old.par)
 dev.off() 
}
  
if (1==0) {
#NOT SURE THIS WORKS    
#Allow algorithm to determine number of topics (set K = 0)
#install.packages(c("Rtsne", "rsvd", "geometry"))
 fitdata <- stm(documents = out$documents, vocab = out$vocab, 
                K = 0, prevalence =~ school + s(year) + type,  
                max.em.its = 75, data = out$meta, 
                init.type = "Spectral")   
 
 fitdata[["settings"]][["dim"]][["K"]] 
}

 

# # # # # # # # # # # # # #
# #  Estimation
# # # # # # # # # # # # # #        
 
#Set K = number of topics  
 myK = 9
  
#Estimate the model with prespecified K   
 fitdata <- stm(documents = out$documents, vocab = out$vocab, 
                K = myK,  
                max.em.its = 75, data = out$meta, 
                init.type = "Spectral")    

#Posterior probability of topic for each document
 #fitdata[["theta"]]

#Display words associated with each topic  
 labelTopics(fitdata, topics=1:myK,n=10)


  
# # # # # # # # # # # # # #
# #  Summarize results
# # # # # # # # # # # # # #         

#Plot showing topic proportions
 if(!is.null(dev.list())) dev.off()
 plot(fitdata, type = "summary", xlim = c(0, .4))
 dev.copy(pdf,"topic_proportions.pdf", width=8, height=8) 
 par(old.par)
 dev.off()  
 
 plot(fitdata, type = "labels",topics=c(1:6), xlim = c(0, .4),text.cex=.5)
 
 plot(fitdata, type = "perspectives",topics=c(2:3),text.cex=1.3)
 plot(fitdata, type = "perspectives",topics=c(2,5),text.cex=1.3) 
 plot(fitdata, type = "perspectives",topics=c(6,5),text.cex=1)  
 plot(fitdata, type = "perspectives",topics=c(8,9),text.cex=1)   
 plot(fitdata, type = "perspectives",topics=c(9,5),text.cex=1)    
 
#Get school attached to first 300 characters of each document
#Use text of original review (in "comments" column)
 shortdoc <- paste(data$school,substring(data$comments,1,300),sep=": ")
 #Display texts strongly associated with each topic
 #n=2 texts for each topic 
 #Need to enable printing  
 for (i in 1:myK) {
   print(labelTopics(fitdata, i))   
   print(findThoughts(fitdata, texts = shortdoc, n = 2, topics = i))
 }
 
 
 
 
# # # # # # # # # # # # # # #
# #  Analysis by school type
# # # # # # # # # # # # # # #         
 
#For each topic, average theta in public vs. private vs. charter schools  
 for (i in 1:myK) {
   print(paste("Topic", i, sep = " "))
   df <- data.frame(fitdata[["theta"]][,i],data$type) 
   colnames(df) <- c("topic","type")   
   print(ddply(df,~type,summarize,mean=mean(topic)))
 } 

#Estimate effect of covariates
#Don't understand why there are 25 sets of estimates
 out$meta$type <- as.factor(out$meta$type)
 out$meta <- within(out$meta, type <- relevel(type,ref="public"))
#est <- estimateEffect(1:myK ~ type, fitdata, metadata = out$meta, uncertainty = "Global")
 prep <- estimateEffect(c(2,5,6,9) ~ type, fitdata, metadata = out$meta, uncertainty = "Global") 
 prep$parameters[[1]][[1]] 
 prep$parameters[[1]][[1]]$est["typecharter"]
 prep$parameters[[1]][[1]]$est["typeprivate"] 
 summary(prep)

#Make sure topics included here were already run in previous step 
#For public, plot the intercept
#For private, plot intercept+typeprivate
#For charter, plot intercept+typecharter
 pointlabels <- c('Topic 2: Public','Topic 5: Public',
                  'Topic 2: Private','Topic 5: Private',
                  'Topic 2: Charter','Topic 5: Charter')
  plot(prep, covariate = "type", topics = c(2,5),
      model = fitdata, method = "pointestimate",
      xlab = "Point estimate",
      main = "",
      xlim = c(.04, .15), labeltype = "custom",
      custom.labels = pointlabels)

 dev.off()    
#par(mfrow=c(4,1))   
 par(mfrow=c(1,1))
for (k in c(2,5,6,9)) { 
    
  if (k==2) {
    title <- "Parent involvement"
  }
  if (k==5) {
    title <- "Academic quality"
  }  
  if (k==6) {
    title <- "Extracurriculars and special programs"
  }  
  if (k==9) {
    title <- "Community and student support"
  }  
  
 #Note: couldn't make labels bigger, so made graph smaller  
  pointlabels <- c('Public',
                   'Private',
                   'Charter')
  plot(prep, covariate = "type", topics = k,
       model = fitdata, method = "pointestimate",
       xlab = "Point estimate",
       main = title,
       xlim = c(0, .15), labeltype = "custom",
       custom.labels = pointlabels, 
       cex.lab=1, cex=4)  
  #pointLabel(c(0,0,0), y = NULL, labels=pointlabels)
  
  out<-paste("topic",k,"bytype",".pdf",sep="")
  dev.copy(pdf,out, width=4, height=3)     
  dev.off()      
  dev.off()        
}  
   
 
