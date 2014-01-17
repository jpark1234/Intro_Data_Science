#Question 1#
Q1 <- new_structure
#a.1
levels(Q1$shape) <- c("Circle","Circle","Triangle", "Triangle","Fireball", "Fireball")
boxplot(as.numeric(Q1$duration) ~ Q1$shape)
#a.2
Q1$date.ref <- as.Date(Q1$date, "%m/%d/%y")
require(lubridate)
Q1$year <- year(Q1$date.ref)
Q1$year <- gsub("2030", "1930", Q1$year)
Q1$year <- gsub("2035", "1935", Q1$year)
Q1$year <- gsub("2041", "1941", Q1$year)
Q1$year <- gsub("2044", "1944", Q1$year)
Q1$year <- gsub("2045", "1945", Q1$year)
Q1$year <- gsub("2046", "1946", Q1$year)
Q1$year <- gsub("2047", "1947", Q1$year)
Q1$year <- gsub("2048", "1948", Q1$year)
Q1$year <- gsub("2049", "1949", Q1$year)
Q1$year <- gsub("2050", "1950", Q1$year)
Q1$year <- gsub("2051", "1951", Q1$year)
Q1$year <- gsub("2052", "1952", Q1$year)
Q1$year <- gsub("2053", "1953", Q1$year)
Q1$year <- gsub("2054", "1954", Q1$year)
Q1$year <- gsub("2055", "1955", Q1$year)
Q1$year <- gsub("2056", "1956", Q1$year)
Q1$year <- gsub("2057", "1957", Q1$year)
Q1$year <- gsub("2058", "1958", Q1$year)
Q1$year <- gsub("2059", "1959", Q1$year)
Q1$year <- gsub("2060", "1960", Q1$year)
Q1$year <- gsub("2061", "1961", Q1$year)
Q1$year <- gsub("2062", "1962", Q1$year)
Q1$year <- gsub("2063", "1963", Q1$year)
Q1$year <- gsub("2064", "1964", Q1$year)
Q1$year <- gsub("2065", "1965", Q1$year)
Q1$year <- gsub("2066", "1966", Q1$year)
Q1$year <- gsub("2067", "1967", Q1$year)
Q1$year <- gsub("2068", "1968", Q1$year)
Q1$month <- month(Q1$date.ref)
Q1$day <- day(Q1$date.ref)

circle <- Q1[Q1$shape=="Circle",]
triangle <- Q1[Q1$shape=="Triangle",]
fireball <- Q1[Q1$shape=="Fireball",]


plot(circle$year, circle$duration, type="l", col="red", xlab="year", ylab="duration",
     main="time series of duration by year")
lines(triangle$year, triangle$duration, type="l", lty=2, col="blue")
lines(fireball$year, fireball$duration, type="l", lty=3, col="green")
legend("topleft", c("circle", "triangle", "fireball"), fill=c("red","blue", "green"), cex=0.8) 



#a.3
rcol <- rainbow(70, alpha=0.5)
barplot(table(Q1$state), las=2, col=rcol)

#a.4 custom plot
ag.dur <-aggregate(as.numeric(Q1b$duration), by=list(State=Q1b$state), function(x) mean(x, na.rm=TRUE))
plot(ag.dur, ylab="average duration", main="average duration by state")

#b.1
statepop <- read.table("C:/Users/Jeeyoon/Desktop/Statepop.csv", header=TRUE, sep=",")
state.freq<- as.data.frame(table(Q1$state))
fix(state.freq) #change variable name to match for merging 
norm.pop <- merge(statepop, state.freq, ID="state")
norm.pop$this <- norm.pop$freq/norm.pop$population

Q1b <-merge(Q1, norm.pop, ID="state")

barplot(Q1b$this, xlab= "population size", main="Noramlized")
barplot(Q1b$population, xlab = "population size", main="Not Noramlized")


#b.2
require(maps)
library(RColorBrewer)
Q1b$this.mil <- Q1b$this*1000000
colors <- brewer.pal(5, "PuOr")
pallet <- colorRampPalette(colors)

map('state', fill=TRUE, col=pallet(Q1b$this.mil))

#b.3
#what month, day and year do UFOs appear more? 
hist(Q1b$month)
hist(Q1b$day)
hist(as.numeric(Q1b$year))

#does the length of the summary show any interesting patterns? 
require(stringr)
Q1b$sum.no <- str_count(Q1b$summary, '\\w+')
hist(Q1b$sum.no)
mean(Q1b$sum.no)
plot(Q1b$duration, Q1b$sum.no)
plot(Q1b$state, Q1b$sum.no, xlab="state", ylab="number of words in description")
Q1b$sum.no.sq <- Q1b$sum.no^2

plot(Q1b$state, Q1b$freq, xlab="state", ylab="frequency of UFO sighting")
lines(Q1b$state, Q1b$sum.no.sq, type="p", col="red")

#c
Metric.1 <- lm(as.numeric(Q1b$shape)~Q1b$duration + Q1b$freq + as.numeric(Q1b$state))
require(randomForest)
Metric.2 <- Q1b ~ Q1b$shape~Q1b$duration + Q1b$freq + Q1b$year+ Q1b$state
Metric.2
require(devtools)
install_github("useful", "jaredlander")
require(useful)
Q1b.X <- build.x(Metric.2, data=Q1b)
Q1b.Y <- build.y(Metric.2, data=Q1b)
Metric.2.forest <- randomForest(x=Q1b.X, y=Q1b.Y)
Metric.2.forest 

       
###QUESTION 2
#a.
require(stringr)
library(RColorBrewer)
colors <- rainbow(5,alpha=0.5)
pal <- colorRampPalette

rcol = rainbow(75, alpha=1)
hcol <- heat.colors(50, alpha=0.75)
ccol <- cm.colors(50, alpha=0.75)
tcol <- terrain.colors(50, alpha=0.75)

train$boiler.num.words <- str_count(train$boilerplate, '\\w+')       
train$url.length<- nchar(as.character(train$url))

#info 1
# Multiple charts in one window
par(mfrow=c(2,3), las=1, bty="n")

hist(train$boiler.num.words, breaks=75, xlab="Number of Words", ylab="count", main="Boilerplate:Total", col=ccol)
hist(train$url.length, breaks=50, xlab="URL length", ylab="count", main="URL:Total", col=ccol)

hist(train$url.length[train$label==0], breaks=50, 
     ylim=c(0,600),
     xlab="URL length", ylab="count", 
     main="Non-evergreen", col=hcol)
hist(train$url.length[train$label==1], breaks=50, 
     ylim=c(0,600),
     xlab="URL length", ylab="count", 
     main="Evergreen", col=tcol)

hist(train$boiler.num.words[train$label==0], breaks=50, 
     ylim=c(0,1500),
     xlab="Number of Words", ylab="count", 
     main="Non-evergreen", col=hcol)

hist(train$boiler.num.words[train$label==1], breaks=50, 
     ylim=c(0,1500),
     xlab="Number of Words", ylab="count", 
     main="Evergreen", col=tcol)

par(mfrow=c(1,3), las=1, bty="n")

#info2 plot 1
spelling.1 <- density(train$spelling_errors_ratio[train$label==1])
spelling.2 <- density(train$spelling_errors_ratio[train$label==0])

plot(spelling.1, lwd=2, col="green",
     xlab="Ratio of Spelling Error in URL",
     ylab="Density",
     main="Spelling Error")
lines(spelling.2, type="l", lwd=2, col="red")
legend("topright", c("Evergreen", "Non-evergreen"), fill=c("green","red"), cex=0.8) 

#info 2 plot2
image.1 <- density(train$image_ratio[train$label==1])
image.2 <- density(train$image_ratio[train$label==0])
plot(image.1, lwd=2, col="green",
     xlim=c(0,4),
     xlab="Ratio of Image in URL",
     ylab="Density",
     main="Image Ratio")
lines(image.2, type="l", lwd=2, col="red")
legend("topright", c("Evergreen", "Non-evergreen"), fill=c("green","red"), cex=0.8) 

#info 2 plot3
frame.1 <- density(train$frameTagRatio[train$label==1])
frame.2 <- density(train$frameTagRatio[train$label==0])
plot(frame.1, lwd=2, col="green",
     xlab="Ratio of Frame Tag in URL",
     ylab="Density",
     main="Frame Tag Ratio")
lines(frame.2, type="l", lwd=2, col="red")
legend("topright", c("Evergreen", "Non-evergreen"), fill=c("green","red"), cex=0.8) 




#sanity check
train2 <- read.table("C:/Users/Jeeyoon/Documents/Columbia University/FALL SEMESTER CLASSES/STAT W4242/Kaggle Challenge/train.tsv", header=TRUE, sep="\t")

plot(train$spelling_errors_ratio[train$label==1], pch=19, col=hcol, 
     xlab="frequency", ylab="ratio of spelling errors",
     main="Spelling Errors")

plot(train$spelling_errors_ratio[train$label==2], pch=19, col=ccol, 
     xlab="frequency", ylab="ratio of spelling errors",
     main="Spelling Errors")


#changed label 0 to 2
train$label[train$label==2] = 0
#changed label 0 to 2 
train$lengthyLinkDomain[train$lengthyLinkDomain==0] =2
rcol = rainbow(2, alpha=0.5)
rcol2 = rainbow(2, alpha=0.5)
label.color <- rcol[train$label]
lengthy.domain.color <- rcol2[train$lengthyLinkDomain]
plot(train$url.length, train$boiler.num.words, col=label.color, pch=19)
plot(train$url.length, train$non_markup_alphanum_characters, col=label.color, pch=19)

plot(train$url.length, train$linkwordscore, col=label.color, pch=19)

plot(sqrt(train$spelling_errors_ratio), sqrt(train$image_ratio), col=label.color, pch=19)


plot(train$url.length, train$image_ratio, col=label.color, pch=19)
plot(train$numberOfLinks, train$linkwordscore, col=label.color, pch=19)
plot(train$url.length, train$numberOfLinks, col=label.color, pch=19)
plot(train$linkwordscore, train$spelling_errors_ratio, col=label.color, pch=19)
plot(train$spelling_errors_ratio, col=label.color, pch=19)
require(ggplot2)  
ggplot(train, aes(x=numberOfLinks, y=spelling_errors_ratio, color=label.color))+ geom_point()
       
       