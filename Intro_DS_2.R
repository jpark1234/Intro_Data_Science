#Question 2#
#(a)
profile <- read.table("C:/Users/DS2/profile.csv", header=TRUE, sep=",")
Monday <- read.table("C:/Users/DS2/Monday.csv", header=TRUE, sep=",")
Tuesday <- read.table("C:/Users/DS2/Tuesday.csv", header=TRUE, sep=",")
Wednesday <- read.table("C:/Users/DS2/Wednesday.csv",header=TRUE, sep=",")
Thursday <- read.table("C:/Users/DS2/Thursday.csv", header=TRUE,sep=",")
Friday <- read.table("C:/Users/DS2/Friday.csv", header=TRUE,sep=",")
Saturday <- read.table("C:/Users/DS2/Saturday.csv", header=TRUE,sep=",")
Sunday <- read.table("C:/Users/Jeeyoon/Documents/Columbia University/FALL SEMESTER CLASSES/STAT W4242/Homework/Assignment 2/Sunday.csv", header=TRUE,sep=",")
Linkage <- read.table("C:/Users/Jeeyoon/Documents/Columbia University/FALL SEMESTER CLASSES/STAT W4242/Homework/Assignment 2/Linkage.csv", header=TRUE, sep=",")

colnames(Monday) <- c("X", "mon_visit", "mon_post", "mon_spent", "mon_new_friend", "user_id")
colnames(Tuesday) <- c("X", "tue_visit", "tue_post", "tue_spent", "tue_new_friend", "user_id")
colnames(Wednesday) <- c("X", "wed_visit", "wed_post", "wed_spent", "wed_new_friend", "user_id")
colnames(Thursday) <- c("X", "thur_visit", "thur_post", "thur_spent", "thur_new_friend", "user_id")
colnames(Friday) <- c("X", "fri_visit", "fri_post", "fri_spent", "fri_new_friend", "user_id")
colnames(Saturday) <- c("X", "sat_visit", "sat_post", "sat_spent", "sat_new_friend", "user_id")
colnames(Sunday) <- c("X", "sun_visit", "sun_post", "sun_spent", "sun_new_friend", "user_id")

user_id <- c(1:10000)
user_id <- as.data.frame(user_id)
data.frame <- merge(user_id, Monday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Tuesday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Wednesday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Thursday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Friday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Saturday, ID="user_id", all.x=TRUE)
data.frame <- merge(data.frame, Sunday, ID="user_id", all.x=TRUE)
data.frame[is.na(data.frame)] <-0
data.frame$tot_visit <- data.frame$mon_visit + data.frame$tue_visit + data.frame$wed_visit + data.frame$thur_visit + data.frame$fri_visit + data.frame$sat_visit + data.frame$sun_visit
data.frame$tot_post <- data.frame$mon_post + data.frame$tue_post + data.frame$wed_post + data.frame$thur_post + data.frame$fri_post + data.frame$sat_post + data.frame$sun_post
data.frame$tot_new_friend <- data.frame$mon_new_friend + data.frame$tue_new_friend + data.frame$wed_new_friend + data.frame$thur_new_friend + data.frame$fri_new_friend + data.frame$sat_new_friend + data.frame$sun_new_friend
data.frame$tot_spend <- data.frame$mon_spend + data.frame$tue_spend + data.frame$wed_spend + data.frame$thur_spend + data.frame$fri_spend + data.frame$sat_spend + data.frame$sun_spend
Q2 <- data.frame(data.frame$user_id, data.frame$tot_spend, data.frame$tot_visit, data.frame$tot_post, data.frame$tot_new_friend)
Linkage$tot_friends <- apply(Linkage[,2:10001], 1, sum)
Linkage1 <- data.frame(Linkage$X, Linkage$tot_friends)
fix(Linkage1) #changed X to "user_id"
Q2 <- merge(Q2, Linkage1, ID="user_id")
Q2 <- merge(profile, Q2, ID="user_id")
Q2.1 <- Q2[,-2]
Q2 <- Q2.1
rm(Q2.1)
#clear work space
rm(Monday); rm(Tuesday); rm(Wednesday); rm(Thursday); rm(Friday); rm(Saturday); rm(Sunday)
rm(profile); rm(Linkage); rm(Linkage1); rm(data.frame); rm(user_id)



#clear work space
rm(Monday); rm(Tuesday); rm(Wednesday); rm(Thursday); rm(Friday); rm(Saturday); rm(Sunday)
rm(profile); rm(Linkage); rm(Linkage1); rm(data.frame); rm(user_id)
#(b)
#distributuion of number of friends
b.1 <- density(Q2$tot_friends)
b.12 <- density(Q2$tot_new_friend)
plot(b.1)
plot(b.12)
#relationship between number of friends and age
plot(tot_friends ~ age, data=Q2, pch=16, cex=0.8, col="dark blue")
#age affect whether users moving?
Q2.b <-Q2
Q2.b$move <- Q2$hometown - Q2$living #calculate whether a person moved or not by looking at the difference in these numbers
Q2.b$move <- gsub("[1:5]","1",Q2.b$move)
Q2.b$move <- gsub("-","",Q2.b$move) #1 denotes having moved, 0 denotes no move
lm(age ~ move, data=Q2.b)
#Coefficients:
#  (Intercept)        move1  
#     22.5643       0.4671  
# older people are slighlty more likely to move.

#Characterize the relation between age and relationship status.
require(ggplot2)
ggplot(Q2, aes(x=age, y=user_id)) + geom_point(aes(color=relation_status)) +facet_grid(~relation_status)
#for those who are single, majority of them are within age group of 10-30
#for those in a relationship, much of them are within the age froup of 10-30
#for those who are engaged, almost the entirity of the sample is within age group 25-30
#married people are well distributed accross age. 

#Draw scatter plots between the number of total actions, visits, posts and time spent.
Q2.b5 <- data.frame(Q2$tot_friends, Q2$tot_visit, Q2$tot_post, Q2$tot_spend, Q2$total_action)
pairs(Q2.b5)

#How does the sign up date influence the number of new friends they made?
require(lubridate)
Q2.b6 <- Q2
Q2.b6$syear <- year(Q2$sign_up)
Q2.b6$syear <- Q2.b6$syear*365 #calculae number of years in days
Q2.b6$smonth <- month(Q2$sign_up)
Q2.b6$smonth <- Q2.b6$smonth*30 #calculate number of months in days
Q2.b6$sday <- day(Q2$sign_up)
Q2.b6$smd <- Q2.b6$sday + Q2.b6$smonth + Q2.b6$syear #signup date in number of days 
Q2.b6$sign_up_duration <- 735062 - Q2.b6$smd #735062 = 2013*365 + 10*30 + 17 (October 17, 2013 in number of days)
plot(Q2.b6$tot_new_friend ~ Q2.b6$sign_up_duration,
     xlab = "signup duration in number of days",
     ylab ="total number of friends")
# newer uses have more new friends 

#Is there any relation between age and users' actions?
plot(Q2$total_action ~ Q2$age)

#Do earlier users have more friends?
plot(Q2.b6$tot_friends ~ Q2.b6$sign_up_duration,
     xlab = "signup duration in number of days",
     ylab ="total number of friends")

#(c)
samp <- replicate(10000, sample(Q2$user_id, 3000, replace=FALSE))

#Method 1, look at the number of friends (edges) members of the sample have 
m1.edges <- Q2$tot_friends[samp]
hist(m1.edges, breaks=10)

#Method 2, look at the number of MUTUAL friends (edges) members of the sample have 
#call in Linkage again, first. 
Linkage <- read.table("C:/Users/Jeeyoon/Documents/Columbia University/FALL SEMESTER CLASSES/STAT W4242/Homework/Assignment 2/Linkage.csv", header=TRUE, sep=",")
Link <- Linkage[,-1]
m2 <- replicate(10000,Link[sample(ncol(Link), 3000, replace=FALSE),])
m2.1 <- as.data.frame(m2)
m2.edges <- c(1:10000)
method2 <- rep(0,10000)

for(i in 1:length(m2.edges)){
  method2[i] <- mean(apply(as.data.frame(m2.1[,i]), 2, sum)
}

hist(method2)

#True average degree (TAD)
TAD <- mean(apply(Link, 1, sum))


#(d)
#1
require(boot)
d1.a <- glm(total_action ~ age, data=Q2,family=gaussian(link="identity"))
d1.a.cv <- cv.glm(data=Q2, glmfit=d1.a, K=5)
d1.a.cv$delta
d1.a.cv$delta
[1] 107691.6 107687.7
d1.b <- glm(total_action ~ age + tot_friends + tot_post + tot_spend + tot_visit, data=Q2,family=gaussian(link="identity"))
d1.b.cv <- cv.glm(data=Q2, glmfit=d1.b, K=5)
d1.b.cv$delta
[1] 95094.98 95047.37
d1.c <- glm(total_action ~ age + gender + tot_friends + tot_post + tot_spend + tot_visit + tot_new_friend, data=Q2,family=gaussian(link="identity"))
d1.c.cv <- cv.glm(data=Q2, glmfit=d1.c, K=5)
d1.c.cv$delta
[1] 64105.11 64034.18
d1.d <- glm(total_action ~ age + gender + relation_status+ tot_friends + tot_post + tot_spend + tot_visit + tot_new_friend, data=Q2,family=gaussian(link="identity"))
d1.d.cv <- cv.glm(data=Q2, glmfit=d1.d, K=5)
d1.d.cv$delta
[1] 63656.25 63635.51

#2
require(useful)
set.seed(1)
#create data frame with only numeric variable s
Q2.d <- Q2[,-9] 
Q2.d1 <- Q2.d[,-9] 
Q2.d <- Q2.d1
rm(Q2.d1)
Q2.dk2 <- kmeans(x=Q2.d, centers=2)
plot(Q2.dk2)
Q2.dk3 <- kmeans(x=Q2.d, centers=3)
plot(Q2.dk3)
Q2.dk4 <- kmeans(x=Q2.d, centers=4)
plot(Q2.dk4)
Q2.dk5 <- kmeans(x=Q2.d, centers=5)
plot(Q2.dk5)
#check for optimal clusters
# Hartigan's rule
Q2.dk.best <- FitKMeans(x=Q2.d, max.clusters=30, seed=1)
PlotHartigan(Q2.dk.best)
Q2.dk29 <- kmeans(x=Q2.d, centers=29)
plot(Q2.dk29)

Q2.d$cluster <- Q2.dk5$cluster

#gap statistic
require(cluster)
 Q2.dgap <- clusGap(Q2.d, K.max=30, FUNcluster=pam) 

#d
subset1 <- Link[Q2.dk5$cluster==1, Q2.dk5$cluster==1]
subset1$edges <- apply(subset1, 1, sum)
hist(subset1$edges)

subset2 <- Link[Q2.dk5$cluster==2, Q2.dk5$cluster==2]
subset2$edges <- apply(subset2, 1, sum)
hist(subset2$edges)

subset3 <- Link[Q2.dk5$cluster==3, Q2.dk5$cluster==3]
subset3$edges <- apply(subset3, 1, sum)
hist(subset3$edges)

subset4 <- Link[Q2.dk5$cluster==4, Q2.dk5$cluster==4]
subset4$edges <- apply(subset4, 1, sum)
hist(subset4$edges)

subset5 <- Link[Q2.dk5$cluster==5, Q2.dk5$cluster==5]
subset5$edges <- apply(subset5, 1, sum)
hist(subset5$edges)

