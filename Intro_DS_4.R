#QUESTION 1
#a)
keyval <- function(key, value=NULL)
{
  if(missing(value))
  {
    list(key=NULL, value=key)
  }else
  {
    list(key=key, value=value)
  }
}


values <- function(kv)
{
  kv$value
}
keys <- function(kv)
{
  kv$key
}


unlist(strsplit(x=train3$bp.clean1, split=" "))
theMapper <- function(key, value)
{
  theSplit <- unlist(strsplit(x=value, split=" "))
  values <- rep(x=1, times=NROW(theSplit))
  keyval(key=theSplit,value=values)
}
firstMap <- theMapper(value=train3$bp.clean1)
firstMap

theReducer <- function(key, value)
{
  keyval(key=key, value=sum(value))
}

theDF <- data.frame(key=keys(firstMap), value=values(firstMap))
aggregate(value~key, data=theDF, sum)

simpleFunc <- function(data, func)
{
  func(data)
}
simpleFunc(1:10,mean)

mapreduce <- function(data, map, reduce)
{
  
  mappedResults <- map(.,data)
  organizedResults <- tapply(X=values(mappedResults),
                             INDEX=keys(mappedResults),
                             FUN=function(x) x)
  theReduced <- mapply(reduce, key=names(organizedResults), value=organizedResults,
                       SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  keyval(key=sapply(theReduced, keys), value=sapply(theReduced, values))
}

trial <- mapreduce(data=train3$bp.clean1, map=theMapper, reduce=theReducer)
require(plyr)
result <- as.data.frame(t(laply(trial,matrix)))

rmr2::mapreduce(data=train3$bp.clean1, map=theMapper, reduce=theReducer)

#b)
#var1
train3$url.n <- nchar(train3$url)
#var2
train3$urlid.n <- nchar(train3$urlid)
#var3
train3$log.comra1 <- log(train3$commonlinkratio_1)
#var4
train3$log.comra2 <- log(train3$commonlinkratio_2)
#var5
train3$log.comra3 <- log(train3$commonlinkratio_3)
#var6
train3$log.comra4 <- log(train3$commonlinkratio_4)
#var7
train3$log.compra <- log(train3$compression_ratio)
#var8
train3$log.fmt <- log(train3$frameTagRatio)
#var9
train3$log.html <- log(train3$html_ratio)
#var10
train3$log.spra <- log(train3$spelling_errors_ratio)
#var11
train3$log.pmra <- log(train3$parametrizedLinkRatio)
#var12
train3$log.emra <- log(train3$embed_ratio)
#var13
train3$url.c <- gsub("^http://(?:www[.])?(.*)$", "\\1", train3$url)
train3$url.c <- gsub("\\/", " ", train3$url.c)
train3$url.c <- gsub("\\-", " ", train3$url.c)
#var14
train3$bp.words <- str_count(train3$boilerplate, '\\w+')
#var15
train3$bp.n <- nchar(train3$boilerplate)
#var16
train3$bp.clean1 <- gsub("^\\{|\\}$","", train3$boilerplate)
train3$bp.clean1 <- gsub("[[:punct:]]"," ", train3$bp.clean1)
train3$bp.clean1 <- gsub("[[:digit:]]"," ", train3$bp.clean1)

#var17
require(stringr)
train3$bp.cap <- str_count(train3$bp.clean1, '[A-Z]')
#var18 
require(SnowballC)
train3$bp.stem <- wordStem(train3$bp.clean1)
#var19
require(RTextTools)
train3$doc.matrix <- create_matrix(train3$doc.matrix, language="english", 
                          removeNumbers=TRUE, removeStopwords=TRUE,
                          stemWords=TRUE)

#var20
train3$alc.num <- as.numeric(train3$alchemy_category)


#QUESTION 2
#API inquiry URL construction 
#this one is the default first page result for Sports section
http://api.nytimes.com/svc/search/v1/article?api-key=1D6AB47D959E8B266850AC01D1F38EDE:9:68403017&query=nytd_section_facet:[Sports]&fields=url,nytd_section_facet,lead_paragraph,word_count,nytd_title
#this one is the specified firt page result for Sports section - I use this base to grab 2000 articles 
http://api.nytimes.com/svc/search/v1/article?api-key=1D6AB47D959E8B266850AC01D1F38EDE:9:68403017&query=nytd_section_facet:[Sports]&fields=url,nytd_section_facet,body,word_count,nytd_title&offset=0

#I do a test run on 10 results on the sports section to see what needs to be done to turn 2000 articles into a dataframe
require(rjson)
Sports.Query <-"http://api.nytimes.com/svc/search/v1/article?api-key=1D6AB47D959E8B266850AC01D1F38EDE:9:68403017&query=nytd_section_facet:[Sports]&fields=url,nytd_section_facet,body,word_count,nytd_title&offset=2"
Sports.Query
Sports.Results <- fromJSON(file=Sports.Query)
View(Sports.Results)
str(Sports.Results)
str(Sports.Results$results)
Sports.Results$results[1]
require(plyr)
Sports.Df <- ldply(Sports.Results$results, as.data.frame)
View(Sports.Df)

#Based on what I did above, I construct a loop to grab 2000 articles and turn into a dataframe 

get.api <- function(section, lastPage=0)
{
  results <- vector("list", lastPage+1)
  baseQuery <- "http://api.nytimes.com/svc/search/v1/article?api-key=1D6AB47D959E8B266850AC01D1F38EDE:9:68403017&query=nytd_section_facet:[%s]&fields=url,nytd_section_facet,body,nytd_title&offset=%s"
  for(i in 0:lastPage)
  {
    tempURL <- sprintf(baseQuery, section, i)
    tempResult <- fromJSON(file=tempURL)
    results[[i+1]] <- ldply(tempResult$results, as.data.frame)
  }
  return(ldply(results))
}

Arts <- get.api(section="Arts", lastPage=199)
dim(Arts)
Arts <- Arts[Arts$nytd_section_facet=="Arts",] #delete duplicate articles that appeared in two or more sections 
dim(Arts)

Business <- get.api(section="Business", lastPage=199)
dim(Business)
Business <- Business[Business$nytd_section_facet=="Business",] #delete duplicate articles that appeared in two or more sections 
dim(Business)

Obituaries <- get.api(section="Obituaries", lastPage=199)
dim(Obituaries)
Obituaries <- Obituaries[Obituaries$nytd_section_facet=="Obituaries",] #delete duplicate articles that appeared in two or more sections 
dim(Obituaries)

Sports <- get.api(section="Sports", lastPage=199)
dim(Sports)
Sports <- Sports[Sports$nytd_section_facet=="Sports",] #delete duplicate articles that appeared in two or more sections 
dim(Sports)

World <- get.api(section="World", lastPage=199)
dim(World)
World <- World[World$nytd_section_facet=="World",] #delete duplicate articles that appeared in two or more sections 
dim(World)

#Contains all articles from the api
api.all <- rbind(Arts,Business,Obituaries,Sports,World)
dim(api.all)
api.all$nytd_section_facet <- factor(api.all$nytd_section_facet)

#save api.all to a file
write.table(api.all, "apiall.csv", sep="\t")
write.table(api.all, "apiall.tsv", sep="\t")

#create test and trian data by taking random sample of 5000 
api.test <- api.all[sample(nrow(api.all),5000),]
api.train <- api.all[sample(nrow(api.all),5000),]

#create a document matrix
require(RTextTools)
library(e1071)
require(useful)

api.all$text <- paste(api.all$nytd_title, api.all$body)
doc.matrix <- create_matrix(api.all$text, language="english", 
                            removeNumbers=TRUE, removeStopwords=TRUE,
                            stemWords=TRUE)
memory.limit(size=5000)
###THIS DOESNT RUN DUE TO MEMORY LIMIT (RAN OUT OF RAM!!!!!)
doc.matrix.ch <- apply(as.matrix(doc.matrix),2,as.character)

#naiveBayes
nb1 <- naiveBayes(x=doc.matrix.ch, y=api.all$nytd_section_facet)

#predict naiveBayes
nb1.predict <- predict(nb1, doc.matrix.ch)


#and therefore I resort to testing with a small subsample
test.art <- get.api(section="Arts", lastPage=2)
test.sport <- get.api(section="Sports", lastPage=2)
test.world <- get.api(section="World", lastPage=2)
test.business <- get.api(section="Business", lastPage=2)
test.obituaries <- get.api(section="Obituaries", lastPage=2)

test.art <- subset(test.art, test.art$nytd_section_facet=="Arts")
test.sport <- subset(test.sport,test.sport$nytd_section_facet=="Sports")
test.business <- subset(test.business,test.business$nytd_section_facet=="Business")
test.obituaries <- subset(test.obituaries,test.obituaries$nytd_section_facet=="Obituaries")
test.world <- subset(test.world, test.world$nytd_section_facet=="World")
test.bayes <- rbind(test.art, test.sport,test.world, test.business, test.obituaries)

dim(test.bayes)

test.bayes$body <- as.character(test.bayes$body)
test.bayes$nytd_title <- as.character(test.bayes$nytd_title)
test.bayes$nytd_section_facet <- factor(test.bayes$nytd_section_facet)
test.bayes$text <- paste(test.bayes$nytd_title,test.bayes$body)
test.doc <- create_matrix(test.bayes$text, language="english", 
                          removeNumbers=TRUE, removeStopwords=TRUE,
                          stemWords=TRUE)
test.doc.matrix <- apply(as.matrix(test.doc),2, as.character)
test.nb1 <- naiveBayes(x=test.doc.matrix, y=test.bayes$nytd_section_facet)
head(test.nb1$tables)  
test.nb1$apriori

test.np.predict <- predict(test.nb1, test.doc.matrix)
table(test.np.predict)
test.bayes$nb.predict <- predict(test.nb1, test.doc.matrix)
xtabs(~test.bayes$nytd_section_facet+test.bayes$nb.predict)

a <- apply(doc.matrix[test.bayes$nytd_section_facet=="Arts"& test.bayes$nb.predict=="Arts"], 2, sum)
table(a[a>0])
apply(doc.matrix[test.bayes$nytd_section_facet=="Sports"& test.bayes$nb.predict=="Sports"], 2, sum)


order(table(a))