#cleaning webscraped structured dirty data 
###Calling in the dataset
require(stringr)
library(XML)
Fireball <- readHTMLTable("http://www.nuforc.org/webreports/ndxsFireball.html", 
                          header = TRUE, as.data.frame=TRUE,
                          stringAsFactor=FALSE, sep = "/t")
Fireball <- as.data.frame(Fireball)
Circle <-  readHTMLTable("http://www.nuforc.org/webreports/ndxsCircle.html", header = TRUE, as.data.frame=TRUE,
                         stringAsFactor=FALSE, sep = "/t")
Circle <- as.data.frame(Circle)
Triangle <- readHTMLTable("http://www.nuforc.org/webreports/ndxsTriangle.html", header = TRUE, as.data.frame=TRUE,
                          stringAsFactor=FALSE, sep = "/t")
Triangle <- as.data.frame(Triangle)

###combine three dataframes 
Q1 <- rbind(Circle, Fireball, Triangle) #combine into one dataframe 

###remove all oddities within NULL.Duration column 
Q1_A <- Q1[-grep("[[:punct:]]", Q1$NULL.Duration),] #drop cases with punctuation
Q1_A$NULL.Duration <- gsub(".+-", "", Q1_A$NULL.Duration) #delete before "-"
Q1_A$NULL.Duration <- gsub(".+to", "", Q1_A$NULL.Duration, ignore.case=TRUE) #delete before "to"
Q1_A$NULL.Duration <- gsub(".+or", "", Q1_A$NULL.Duration, ignore.case=TRUE) #delete before "or"
Q1_A$NULL.Duration <- gsub("unknown", "", Q1_A$NULL.Duration, ignore.case=TRUE) #remove "unknown"
Q1_A$NULL.Duration <- gsub("<", "", Q1_A$NULL.Duration) #remove "<"
Q1_A$NULL.Duration <- gsub(">", "", Q1_A$NULL.Duration) #remove ">"
Q1_A$NULL.Duration <- gsub("\\(", "", Q1_A$NULL.Duration) #remove "("
Q1_A$NULL.Duration <- gsub(")", "", Q1_A$NULL.Duration) #remove ")"
Q1_A$NULL.Duration <- gsub("\\-", "", Q1_A $NULL.Duration) #remove "-"
###reorder dataframe to drop empty cases
Q1_AA <- Q1_A[order(as.character(Q1_A$NULL.Duration)),]
Q1_AA <- Q1_AA[-1:-790,] #drop empty cases after view(Q1_AA)
###remove further oddities 
Q1_AA <- Q1_AA[-grep("less", Q1_AA$NULL.Duration, ignore.case=TRUE),] #drop cases with "less"
Q1_AA <- Q1_AA[-grep("longer", Q1_AA$NULL.Duration, ignore.case=TRUE),] #drop cases with "longer"
Q1_AA <- Q1_AA[-grep("while", Q1_AA$NULL.Duration, ignore.case=TRUE),] #drop cases with "while"
Q1_AA <- Q1_AA[-grep("about", Q1_AA$NULL.Duration, ignore.case=TRUE),] #drop cases with "about"
Q1_AA <- Q1_AA[-grep("so", Q1_AA$NULL.Duration, ignore.case=TRUE),] #drop cases with "so"

###include ID variable in Q1 dataframe 
Q1_AA$ID <- c(1:15702)

###Create dataframe for duration varaible 
dur <- data.frame(Q1_AA$ID, Q1_AA$NULL.Duration )
dur$dur_spt <- str_split(Q1_AA$NULL.Duration, pattern=" ") #split duration variable 
dur_out <- unlist(dur$dur_spt) #take dur_spt out of dataframe to unlist
#I only took the first three columns (a.k.a first three words from the original "cleaned" duration variable)

dur_out1 <- data.frame(Reduce(rbind, dur$dur_spt)) #make it into dataframe 
###include ID vairable in dur_out1 dataframe
dur_out1$ID <- c(1:15702)
dur_out1$X1 <-gsub("[^0-9]","", dur_out1$X1) #make all non-numeric value blank
dur_out1$X1 <-gsub("0","", dur_out1$X1) #delete all 0
dur_out1$X2 <-gsub("0","", dur_out1$X2) #delete all 0
dur_out1$X3 <-gsub("0","", dur_out1$X3) #delete all 0
dur_out1$X2 <-gsub("minute", "60", dur_out1$X2)
dur_out1$X2 <-gsub("min", "60", dur_out1$X2)
dur_out1$X2 <-gsub("Min", "60", dur_out1$X2)
dur_out1$X2 <-gsub("MIN", "60", dur_out1$X2)
dur_out1$X2 <-gsub("60u.", "60", dur_out1$X2)
dur_out1$X2 <-gsub("60tue", "60", dur_out1$X2)
dur_out1$X2 <-gsub("60s", "60", dur_out1$X2)
dur_out1$X2 <-gsub("hour", "3600", dur_out1$X2)
dur_out1$X2 <-gsub("hr", "3600", dur_out1$X2)
dur_out1$X2 <-gsub("3600s", "3600", dur_out1$X2)
dur_out1$X2 <-gsub("second", "1", dur_out1$X2)
dur_out1$X2 <-gsub("sec", "1", dur_out1$X2)
dur_out1$X2 <-gsub("1s", "1", dur_out1$X2)
dur_out1$X2 <-gsub("[^0-9]","", dur_out1$X2) #make all non-numeric value blank
dur_out1$X3 <-gsub("minute", "60", dur_out1$X3)
dur_out1$X3 <-gsub("min", "60", dur_out1$X3)
dur_out1$X3 <-gsub("Min", "60", dur_out1$X3)
dur_out1$X3 <-gsub("MIN", "60", dur_out1$X3)
dur_out1$X3 <-gsub("60u.", "60", dur_out1$X3)
dur_out1$X3 <-gsub("60tue", "60", dur_out1$X3)
dur_out1$X3 <-gsub("60s", "60", dur_out1$X3)
dur_out1$X3 <-gsub("hour", "3600", dur_out1$X3)
dur_out1$X3 <-gsub("Ho.", "3600", dur_out1$X3)
dur_out1$X3 <-gsub("hr", "3600", dur_out1$X3)
dur_out1$X3 <-gsub("3600s", "3600", dur_out1$X3)
dur_out1$X3 <-gsub("3600rs", "3600", dur_out1$X3)
dur_out1$X3 <-gsub("second", "1", dur_out1$X3)
dur_out1$X3 <-gsub("SECOND", "1", dur_out1$X3)
dur_out1$X3 <-gsub("se.", "1", dur_out1$X3)
dur_out1$X3 <-gsub("sec", "1", dur_out1$X3)
dur_out1$X3 <-gsub("1s", "1", dur_out1$X3)
dur_out1$X3 <-gsub("[^0-9]","", dur_out1$X3) #make all non-numeric value blank
dur_out1$X1 <-as.numeric(dur_out1$X1) #turn it into numeric variable 
dur_out1$X2 <-as.numeric(dur_out1$X2) 
dur_out1$X3 <-as.numeric(dur_out1$X3)
dur_out1[is.na(dur_out1)] <-1 #turn NA into 1

with(dur_out1, as.numeric(X1)*as.numeric(X2))
dur_mul1 <- within(dur_out1, X2 <- as.numeric(X1)*as.numeric(X2)) #multiply X1 by X2

with(dura_out1, as.numeric(X2)*as.numeric(X3))
dur_mul2 <- within(dur_out1, X3 <- as.numeric(X2)*as.numeric(X3)) #mutilply X2 (=X1*X2) * X3 (this turns all values into seconds)

###clean up NULL.Datetime
Q1_AA$NULL.Date...Time <- as.Date(Q1_AA$NULL.Date...Time, format="%m/%d/%y") #reformat column
require(lubridate)
Q1_AA$Year <- year(Q1_AA$NULL.Date...Time) #extract year 
unique(Q1_AA$Year) #look at years, years before 1970 appear very strangely (like 1963=2063 and so forth)

###I use excel to merge Q1_AA and dur_out1
write.table(Q1_AA, file="Q1_AA_JP.csv", row.names=TRUE, sep=",")
write.table(dur_out1, file="dur_out1_JP.csv", row.names=TRUE, sep=",")
Q1_AA2 <- read.csv("C:/Users/Jeeyoon/Downloads/Q1_AA_JP.csv", sep=",")
Q1_Final <- data.frame(Q1_AA2$NULL.Date...Time, Q1_AA2$NULL.City, Q1_AA2$NULL.State, 
                       Q1_AA2$NULL.Shape, Q1_AA2$Dur_sec, Q1_AA2$NULL.Summary, Q1_AA2$NULL.Posted)
###PART2 of QUESTION 1
write.table(Q1_Final, file="Q1_Final.csv", header=TRUE, row.names=FALSE, sep=",")

###How many UFO sightings in Alaska?
summary(Q1_AA2$NULL.State=="AK")
### 52 sightings 

###How many UFO sightings of durations less than 2 minutes in NY?
NY <- Q1_AA2[Q1_AA2$NULL.State=="NY",]
NY_dur <- NY[as.numeric(NY$Dur_sec) < 120,]
dim(NY_dur)
###640 sightings 

###What's the average duration of UFO sightings of Firebeball?
FB <- Q1_AA2[Q1_AA2$NULL.Shape =="Fireball",]
mean(as.numeric(FB$Dur_sec))
###108.0154 seconds

###Which year has the maximum number of sightings?
aggregate(NULL.Date...Time ~ Year, length, data=Q1_AA) #group years 
###2012

write.table(Q1_Final, file="Q1_Final.csv", sep=",")
###end of Question1, clean up work sapce
rm(Circle) ; rm(Fireball); rm(Triangle); 
rm(FB) ; rm(NY) ; rm(NY_dur)  
rm(Q1); rm(Q1_A); rm(Q1_AA); rm(Q1_AA2) 

#structured unstructured clean data 
###read in the file 
filename<-"C:/Users/Jeeyoon/Downloads/news.txt"
data<-readLines(conn)
conn<-file(filename,open='r')
text<-readLines(conn)
class(text)

Q2_raw1 <- readLines("C:/Users/Jeeyoon/Downloads/news.txt")

TTL <- Q2_raw1[seq(from=1, to=length(Q2_raw1), by=8)]
DES <- Q2_raw1[seq(from=2, to=length(Q2_raw1), by=8)]
URL <- Q2_raw1[seq(from=3, to=length(Q2_raw1), by=8)]
NUM <- Q2_raw1[seq(from=4, to=length(Q2_raw1), by=8)]
PDATE <- Q2_raw1[seq(from=5, to=length(Q2_raw1), by=8)]
PUB <- Q2_raw1[seq(from=6, to=length(Q2_raw1), by=8)]
CAT <- Q2_raw1[seq(from=7, to=length(Q2_raw1), by=8)]

Q2 <- data.frame(TTL, DES, URL, NUM, PDATE, PUB, CAT)

###reformat pdate 
d <-strsplit(as.character(Q2$PDATE),split=' ') #split the pdate
D <- unlist(d) #unlist it 
D1 <- D[seq(from=1, to=length(D), by=4)]
D2 <- D[seq(from=2, to=length(D), by=4)]  
D3 <- D[seq(from=3, to=length(D), by=4)]
D4 <- D[seq(from=4, to=length(D), by=4)]

D2_1 <- gsub("mo-1nth.abb", c(1:12), D2) #sub month.abb with number
D2_1 <- match(D2, month.abb) 
D2_1 <- paste("0", D2_1[D2_1 <10])
D2_1 <- gsub(" ", "", D2_1)
Date <- paste(D1,D2_1,D3)
Date <- gsub(" ", "", Date) 
Q2$Date <- Date

###number of commas in Q2$DES
library(stringr)
Q2$DESCOM <- str_count(Q2$DES, ',')

###number of words in Q2$TTL
Q2$TTLWD <- str_count(Q2$TTL, '\\w+')

###Q2 structured data frame
Q2_Final <- data.frame(Q2$Date, Q2$PUB, Q2$CAT, Q2$TTLWD, Q2$DESCOM)

###clean up work space
rm(URL) ; rm(TTL); rm(STTL) ; rm(PUB) ; rm(PDATE); rm(NUM); rm(CAT)
rm(d); rm(D); rm(D1); rm(D2); rm(D3); rm(D4); rm(D2_1)
                                                               

###Which category of news is most popular?
table(Q2$CAT)
#sport

###how many business events happened during April to May 2011?
table(Q2$CAT[(Q2$Date > 03312011)|(Q2$Date < 06012011)])
#5,367

###How many news events come from usatoday.com?
table(Q2$PUB)
#10,110

###Which newspaper website has the longest title in average?
Q2$PUB[max(mean(nchar(as.character(Q2$TTL))))]
#New York Times 

###Which newspaper website uses least commas (in average) in description?
library(stringr)
nyt_mean <- mean(str_count(Q2$DES[Q2$PUB=="nyt"], ','))
ut_mean <- mean(str_count(Q2$DES[Q2$PUB=="ut"], ','))
rts_mean <- mean(str_count(Q2$DES[Q2$PUB=="reuters"], ','))
#usa today 


###Joining datasets through SQL 
write.csv(Q1_Final, file="UFO.csv", sep=",", row.names=FALSE) 
Q3<- read.table("C:/Users/Jeeyoon/Desktop/CRIME.csv", sep=",")
Q3_A <- Q3[-1:-14,]
Q3_AA2 <-Q3_A[-9,] #remove empty rows
Q3_AA3 <-Q3_AA2[-53:-67,] #remove unnecessary rows
###clean up state names (V1)
###because this dataset is small and each state only has one observation, I will fix it manually
fix(Q3_AA3)
write.csv(Q3_AA3, file="CRIME4.csv")

###Dataset I downloaded
###http://www.infochimps.com/datasets/crime-rates-by-state-2004-and-2005-and-by-type-2005-cleaned-up-v

#SQL code 
select*FROM UFO;

select State, Col_4, Col_5 FROM UFO;

select 
State, 
Col_4, 
Col_5 
FROM 
UFO

WHERE
  
  State IN ('NY')

order by
Col_4 desc
;

##Circle

###How many rapes in NY between 2004 and 2005
select*FROM CRIME;

select State, ROBBERY FROM CRIME;

select 
State, 
ROBBERY
FROM 
CRIME

WHERE

State IN ('NY')

;
###white state has most abount of assults?
select*FROM CRIME_ALL;

select State, Rape, Robbery, Assult FROM CRIME_ALL;

select 
State, 
Rape, 
Robbery 
Assult,
FROM
CRIME_ALL

order by
Assult desc
;

##Distrcit of Columbia 

###Ranking of states with longest UFO sighting duration (by incident) in seconds and violet crime (rape, robbery, assult) rate?
SELECT*FROM CRIME4;
SELECT
  UFO.State,
  UFO.Col_5,
  UFO.Col_4,
  Rape,
  Robbery,
  Assult
FROM
  CRIME4 JOIN UFO ON CRIME4.State = UFO.State
ORDER BY
  UFO.Col_5 desc
;

###Pennsylvania has the longest incident of UFO citing in seconds. In 2005, PA had 154.6 assults, 28.9 robberies and 6.1 rapes.  

###R version of SQL code 
library(sqldf)
#set up interface
C <- dbDriver("SQLite")
con <- dbConnect(C, dbname = "UFO")
dbGetQuery(con, "select*FROM UFO;

select State, Col_4, Col_5 FROM UFO;

select 
State, 
Col_4, 
Col_5 
FROM 
UFO

WHERE
  
  State IN ('NY')

order by
Col_4 desc
;")

