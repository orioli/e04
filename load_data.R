# (c) Jose Berengueres, UAEU 2017
# here we load interactions table data for ASONAM PAPER  DATA SET 03 export march 2017 by Dani

library(gdata)
library(ggplot2)
library(reshape)
library(fmsb)

# change this to your working directory
setwd("/Users/jse/Dropbox/0 - academics/0 - happyforce")



### LOAD LIKES ###################################################################################
li = read.csv("data_03/commentInteractions.csv")
li <- li[li$companyAlias!="58a728a",] # remove this company that has low numbers and no comments
li <- droplevels(li)
#levels(li$companyAlias)
#length(unique(li$companyAlias)) ## 34 companies
#dim(li)
#summary(li)
#is.data.frame(li)

# change company alias to short code
levels(li$companyAlias) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ")
li$companyAlias <- as.factor(li$companyAlias)
#li[1:2,]
li$uid <- paste(li$employee,li$companyAlias)




### LOAD COMMENTS ###################################################################################
# file was pre cleaned of emojis with    iconv -c -f utf-8 -t ascii file.txt   
# and                           perl -i.bak -pe 's/[^[:ascii:]]//g' <your file>
# then the file was anonimized further with... anonimizer.R
# 
co = read.csv("data_03/co.csv")
co <- unique(co)
#dim(co)
#summary(co)
#co[1:2,]
# rename as in prev version
colnames(co)[8] <- c("date")
colnames(co)[3] <- c("coa")
colnames(co)[5] <- c("txt")
colnames(co)[6] <- c("like")
colnames(co)[7] <- c("nolike")
colnames(co)[2] <- c("id")
colnames(co)[1] <- c("notused")
co$notused <- NULL
co$X <- NULL
co <- unique(co)
dim(co)
co[1,]
setdiff(unique(co$coa),unique(li$companyAlias)) # use before changing alias names.
co<-co[co$coa!="5474b9cde4b0bf7614b2c66f",] # remove this company that has low numbers and no comments
co<-co[co$coa!="58bf03e5cff4fa0004dd44ef",] # remove this company that has low numbers and no comments
setdiff(unique(co$coa),unique(li$companyAlias)) # use before changing alias names.
setdiff(unique(li$companyAlias),unique(co$coa)) # use before changing alias names.
co <- droplevels(co)
#levels(co$coa)
#co$coa <- factor(co$coa)
levels(co$coa) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK")
length(unique(co$coa)) ## 34 companies

# parse dates
co$date<- gsub("CEST", "", co$date)
co$date<- gsub("CET", "", co$date)
co$date<- gsub("  ", " ", co$date)
co$date<- as.Date(co$date, format = "%a %b %d %H:%M:%S %Y")
#co$uid <- paste(co$id,co$coa)
#co <- co[-c(23796),]
#co[3,]
#co[23796,]

### some stats about comments below -- Table 3
#summary(co)
# ---Length of comments...
co$txt <- as.character(co$txt)
#co[16802:16803,]$txt <- c("Where is the Lavazza backup protocol?__  (come on,")
co$lcomment <- unlist(lapply(co$txt,nchar) )
summary(co)
# make unique id for all employees
co$uid <- paste(co$id,co$coa)
length(unique(co$commentId))
dim(co)
co[1,]


# employes that commented at least once by company
embyco <- aggregate(uid ~ coa, co, function(x) length(unique(x)))
embyco<- embyco[with(embyco, order(-uid, coa)), ]
summary(embyco)


# Table 3 txt/comment length by company normalized by employee count  
txtbyco <- aggregate(txt ~ coa, co, function(x) length(x))
txtbyco<- txtbyco[with(txtbyco, order(txt, coa)), ]

embyco<- embyco[with(embyco, order(coa)), ]
txtbyco<- txtbyco[with(txtbyco, order(coa)), ]
colnames(embyco) <-c("coa","nempl")
colnames(txtbyco) <-c("coa","ncomments")
txtbyemplbyco <- txtbyco$ncomments/embyco$nempl
summary(txtbyemplbyco)
summary(txtbyco)




# load votes ***first*** before running this block
# same normalized by employes who !!!voted on happiness!!! at least once  #################################################
nebyco <- aggregate(uid ~ companyAlias, votes, function(x) length(unique(x)))
nebyco<- nebyco[with(nebyco, order(companyAlias)), ]
summary(nebyco)
dim(nebyco)
# nebyco<-nebyco[1:28,] # hack no longer needed
#setdiff(unique(nebyco$coa),unique(txtbyco$coa))
#length(unique(txtbyco$coa))
#length(unique(nebyco$coa))
#nebyco<-nebyco[nebyco$coa!="573a0671b5ec330003add34a",]
colnames(nebyco) <- c("coa","nempl")
txtbyemplvotedbyco <- txtbyco$ncomments/nebyco$nempl
summary(txtbyemplvotedbyco)

# percentage of non participation: number of empl that voted but not comment
#summary(embyco)
#levels(embyco$coa)
#summary(nebyco)
#levels(nebyco$coa)
noncommenting_rate <- embyco$nempl/nebyco$nempl
summary(noncommenting_rate)


# fig 7  radarchart likes -weekday; likes by publication date of comment
summary(co)
co$weekday <- weekdays(co$date)
#ggplot(co[co$date>max(co$date)-380,], aes(like,colour=weekday))+   geom_density(alpha = .95) 
#check Kolmogorov
hhh <- aggregate(like ~ weekday + uid, co, function(x) mean(x))
hhh[1:5,]
dim(hhh)
ks.test(hhh[hhh$weekday==c("Monday"),]$like, hhh[hhh$weekday==c("Saturday"),]$like,alternative="greater")

#mean likes, etc... received by a comment by weekday
cobyweek <- aggregate(like ~ weekday, co, function(x) length(x))
cobyweek <-cbind(cobyweek, aggregate(like ~ weekday, co, mean))
cobyweek <-cbind(cobyweek, aggregate(nolike ~ weekday, co, mean))
cobyweek <-cbind(cobyweek, aggregate(lcomment ~ weekday, co, mean))
cobyweek <-cbind(cobyweek, aggregate(like ~ weekday, co, sd))
cobyweek <-cbind(cobyweek, aggregate(lcomment ~ weekday, co, sd))
colnames(cobyweek)[2] <- c("N")
cobyweek <- cobyweek[,c(1,2,4,6,8,10,12)]

## vizualize it
wee<- rbind(rep(8.5,7) , rep(5.5,7), cobyweek$like)
colnames(wee)<- cobyweek$weekday
wee
wee<- wee[,c(2,6,7,5,1,3,4)]

radarchart(as.data.frame(wee),axistype=1,
    cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(6.5,8.5,0.5), cglwd=0.8,
    pcol=rgb(0.2,0.5,0.5,0.6) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
    vlcex=1 
    )




### LOAD VOTES (this is the happiness table) ##################################################3
votes = read.csv("data_03/votes.csv")
#is.data.frame(votes)
#colnames(votes)
#dim(votes)
#hist(votes$employee)
#hist(votes$vote)
#votes[1:10,]$voteDate
#745 levels
setdiff(unique(votes$companyAlias),unique(li$companyAlias)) # use before changing alias names.
votes<-votes[votes$companyAlias!="5474b9cde4b0bf7614b2c66f",] # remove this company that has low numbers and no comments
votes<-votes[votes$companyAlias!="58bf03e5cff4fa0004dd44ef",] # remove this company that has low numbers and no comments
votes<-votes[votes$companyAlias!="573a0671b5ec330003add34a",] # remove this company that has low numbers and no comments
setdiff(unique(votes$companyAlias),unique(li$companyAlias))
setdiff(unique(votes$companyAlias),unique(co$coa)) #
length(unique(votes$companyAlias)) ## 36 companies
votes <- droplevels(votes)
length(unique(co$coa)) ## 34 companies
unique(li$companyAlias) # chek that the order of factors is the same accross the three tables...
votes$companyAlias <- factor(votes$companyAlias)
levels(votes$companyAlias) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK")
#length(unique(votes$companyAlias))
votes$uid <- paste(votes$employee,votes$companyAlias)
#levels(votes$companyAlias)
#levels(li$companyAlias)

# parse dates
# votes$dt <- as.POSIXct((votes$voteDate), origin="1970-01-01")
votes$voteDate<- gsub("CEST", "", votes$voteDate)
votes$voteDate<- gsub("CET", "", votes$voteDate)
votes$voteDate<- gsub("  ", " ", votes$voteDate)
votes$voteDate<- as.Date(votes$voteDate, format = "%a %b %d %H:%M:%S %Y")

summary(votes)

# count companies
length(unique(votes$companyAlias))

# count total num of unique employees
length(unique(votes$uid))

# unique empl. per company. Table 2
nebyco <- aggregate(uid ~ companyAlias, votes, function(x) length(unique(x)))
nebyco<- nebyco[with(nebyco, order(-uid, companyAlias)), ]
summary(nebyco)
mean(votes$vote)

# number votes by company
nvbyco <- aggregate(uid ~ companyAlias, votes, function(x) length(x))
nvbyco<- nvbyco[with(nvbyco, order(-uid, companyAlias)), ]
nvbyco<- nvbyco[with(nvbyco, order(-uid, companyAlias)), ]
summary(nvbyco)

# happiness by company
hbyco <- aggregate(vote ~ companyAlias, votes, function(x) mean(x))
colnames(hbyco)<- c("Company","MeanHappyIndex")
hbyco<- hbyco[with(hbyco, order(-MeanHappyIndex, Company)), ]
summary(hbyco)

# average count of votes by employee by company
nvbyco<- nvbyco[with(nvbyco, order(companyAlias)), ]
nebyco<- nebyco[with(nebyco, order(companyAlias)), ]
nve<- cbind(nebyco,nvbyco)
nve$meanVotesEmployee <- nvbyco$uid/nebyco$uid
summary(nve)

##### correlation check ##########
hbyco<- hbyco[with(hbyco, order(Company)), ]
nve <- cbind(nve,hbyco)
colnames(nve) <- c("Company","N","c2","SumVotes","MeanVotesEmployee","c3","MeanHappyIndex")
nve <- nve[,c(1,2,4,5,7)]
summary(nve)
write.csv(nve, file = "nve.csv")
cor(nve[,c(2:5)])
# exclude companies with less than 50 participants.
#nve <- nve[nve$N>50,]
#cor(nve[,c(2:5)])
###################################
colnames(votes)
summary(votes)
obsperiod <- aggregate(voteDate ~., votes, function(x) as.double(( difftime(max(x), min(x) , units = "days"))+1)/7)
summary(obsperiod)

#attach(nve)
#plot(MeanVotesEmployee, MeanHappyIndex, main="Scatterplot Happiness vs. Participation by company", 
#   xlab="Mean Votes per employee", ylab="Mean Happines of Employee ", pch=19)
#abline(lm(MeanHappyIndex~MeanVotesEmployee), col="red")
# not significant correlation

# histogram mean happy index by company
ggplot(nve, aes(x=MeanHappyIndex)) + geom_histogram(binwidth=.05, colour="white") 
       ylab("Frequency") + xlab("Year and Month") +
       theme_bw() + opts(axis.text.x = theme_text(angle=90))
# same
ggplot(nve, aes(MeanHappyIndex)) +  geom_density(alpha = .75)
# histogram happy
ggplot(votes, aes(vote)) +  geom_bar(alpha = .75)

### fig 1 participation histogram by company
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(voteDate,fill=companyAlias))+   geom_bar(alpha = .95)

### fig 2
#votes[votes$vote==1,]$vote <-c("bad")
#votes[votes$vote==2,]$vote <-c("so-so")
#votes[votes$vote==3,]$vote <-c("good")
#votes[votes$vote==4,]$vote <-c("great")
#votes$vote <- factor(votes$vote, levels = c("bad","so-so","good","great"))
ggplot(votes, aes(vote,fill=companyAlias)) +  geom_bar(alpha = 1) 


### fig 4
votes$weekday <- weekdays(votes$voteDate)
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(weekday,fill=companyAlias))+   geom_bar(alpha = .95)


### fig 6 happyness by weekday RADAR CHART
hbyw <- aggregate(vote ~ weekday, votes, function(x) mean(x))
dim(hbyw)
colnames(hbyw)<- c("weekday","MeanHappyIndex")
hbyw<- hbyw[with(hbyw, order(-MeanHappyIndex)), ]

attach(hbyw)
qplot(weekday, data=hbyw, geom="bar", weight= MeanHappyIndex, ylab="Happyness")
hbyw$weekday <- factor(hbyw$weekday, levels = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
radarchart(hbyw)
wee<- rbind(rep(3.0,7) , rep(2.7,7), hbyw$MeanHappyIndex)
colnames(wee)<- hbyw$weekday
wee<- wee[,c(5,7,6,4,2,1,3)]
wee<- wee[,c(1,3,2,4,5,6,7)]

radarchart(as.data.frame(wee),axistype=1,
    cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(2.7,3.0,0.05), cglwd=0.8,
    pcol=rgb(0.2,0.5,0.5,0.6) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
    vlcex=1 
    )

### Kolmogorov TEST CDF 
x <- rnorm(50) +1
y <- rnorm(50) 
ks.test(x,y,alternative="less")

# Do x and y come from the same distribution?
ks.test(votes[votes$weekday==c("Saturday"),]$vote, votes[votes$weekday==c("Tuesday"),]$vote,"less")

hhh <- aggregate(vote ~ weekday + uid, votes, function(x) mean(x))
hhh[1:5,]
ks.test(hhh[hhh$weekday==c("Saturday"),]$vote, hhh[hhh$weekday==c("Tuesday"),]$vote,alternative="less")
########################


### fig 5
hbye <- aggregate(vote ~ uid+weekday, votes, function(x) mean(x))
dim(hbye)
hbye[1,]
colnames(hbye)<- c("uid","weekday","MeanHappyIndex")
ggplot(hbye, aes(MeanHappyIndex,fill=weekday)) +  geom_bar(alpha = 0.5)+ geom_histogram(binwidth = .1)


### fig 3
votes[1,]
vtimeaverage <- aggregate(voteDate ~ uid+companyAlias, votes, function(x) length(x)/ as.double(( difftime(max(x), min(x) , units = "days"))+1))

colnames(vtimeaverage)<- c("uid","ca","votesperday")
vtimeaverage[1222,]
vtimeaverage <- vtimeaverage[vtimeaverage$votesperday<1000,]
dim(vtimeaverage)
#ggplot(vtimeaverage, aes(votesperday,colour=ca)) +  geom_density(alpha = 1)
#ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_bar(alpha = 0.5) + ylim(0,20)
#ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_bar(alpha = 0.5)+ facet_grid(ca ~ .)
summary(vtimeaverage)
#remove outlier
vtimeaverage <- vtimeaverage[!vtimeaverage$votesperday>1,] 
ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_histogram(alpha = 1)

## average vote by company
votes[1,]
v.co.s <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=sum)
v.co.m <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=mean)
v.co.sd <- aggregate(votes$vote, by=list(Category=votes$companyAlias), FUN=sd)
vote.by.co <- cbind(v.co.s,v.co.m$x,v.co.sd$x)
colnames(vote.by.co) <- c("alias","sum","mean","sd")
# ggplot(v3, aes(x =factor(Category), y =x ) )+ geom_bar(stat = "identity")



### BUILD TARGET VARIABLE ########################################## The Churn table
target = read.csv("data_03/lastParticipationExists.csv")
is.data.frame(target)
colnames(target)
dim(target)
summary(target)
hist(target$employee)
target[1:3,]

setdiff(unique(target$companyAlias),unique(votes$companyAlias)) # use before changing alias names.
target<-target[target$companyAlias!="5474b9cde4b0bf7614b2c66f",] # remove this company that has low numbers and no comments
target<-target[target$companyAlias!="58bf03e5cff4fa0004dd44ef",] # remove this company that has low numbers and no comments
target<-target[target$companyAlias!="573a0671b5ec330003add34a",] # remove this company that has low numbers and no comments
target <- droplevels(target)
levels(target$companyAlias) <- c("A","B","C", "D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AJ")


# parse dates
target$lastParticipationDate<- gsub("CEST", "", target$lastParticipationDate)
target$lastParticipationDate<- gsub("CET", "", target$lastParticipationDate)
target$lastParticipationDate<- gsub("  ", " ", target$lastParticipationDate)
target$lastParticipationDate<- as.Date(target$lastParticipationDate, format = "%a %b %d %H:%M:%S %Y")
target[is.na(target$lastParticipationDate),]
target<- na.omit(target)
max(target$lastParticipationDate)
min(target$lastParticipationDate)

# parse target
target$stillExists<- gsub("true", "1", target$stillExists)
target$stillExists<- gsub("false", "0", target$stillExists)
target$stillExists<- as.numeric(target$stillExists)
mean(target$stillExists)
target$churn = 1 - target$stillExists
sum(target$churn)

# fig 8,9
# 0.09  about 10% quit!
ggplot(target[target$stillExists==0,], aes(lastParticipationDate+15,fill=companyAlias)) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
#ggplot(target, aes(lastParticipationDate,fill=companyAlias)) +  geom_histogram(alpha = .95)+ xlab("Churn date") + facet_grid(churn~ .) 
#target$churn <- factor(target$churn, levels=c(1, 0), labels=c("Y", "N"))
ggplot(target, aes(lastParticipationDate,fill= churn)) +  geom_histogram(alpha = .95)+ xlab("Date") 
#target$churn <- factor(target$churn, levels=c("Y","N"), labels=c("Churn", "Last Active"))
ggplot(target, aes(lastParticipationDate,fill= as.factor(churn))) +  geom_density(alpha = .45)+ xlab("Date") 
ggplot(target, aes(lastParticipationDate,fill= as.factor(churn))) +  geom_histogram(alpha = .95)+ xlab("Date") 

#viz vote churners activity
target[1,]
churners = target[target$stillExists==0,]$uid
length(churners)
votes[1,]
vc<- votes[votes$uid %in% churners,]
dim(vc)
### fig XX
vc[1,]

vtimeaverage <- aggregate(voteDate ~ uid+companyAlias, votes, function(x) length(x)/ as.double(( difftime(max(x), min(x) , units = "days"))+1))
colnames(vtimeaverage)<- c("uid","ca","votesperday")
vtimeaverage[12,]
vtimeaverage <- vtimeaverage[vtimeaverage$votesperday<1000,]
dim(vtimeaverage)
#ggplot(vtimeaverage, aes(votesperday,colour=ca)) +  geom_density(alpha = 1)
#ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_bar(alpha = 0.5) + ylim(0,20)
#ggplot(vtimeaverage, aes(votesperday,fill=ca)) +  geom_bar(alpha = 0.5)+ facet_grid(ca ~ .)
summary(vtimeaverage)

#SANITY CHECK
vtimeaverage <- vtimeaverage[!vtimeaverage$votesperday>1,] 
vt <- merge(x = vtimeaverage, y = target, by = "uid", all.x = TRUE)
vt[1:2,]
ggplot(vt, aes(votesperday,fill= lastParticipationDate)) +  geom_histogram(alpha = 1)
plot(vt$lastParticipationDate,vt$votesperday)
plot(vt$lastParticipationDate,vt$numVotes)
ggplot(vt, aes( lastParticipationDate, votesperday),) + geom_point(size = 0.01, aes(color=factor(churn))) 


co[1,]
ctimeaverage <- aggregate(date ~ uid+coa, co, function(x) length(x)/ as.double(( difftime(max(x), min(x) , units = "days"))+1))
colnames(ctimeaverage)<- c("uid","ca","commentsperday")
ct <- merge(x = ctimeaverage, y = target, by = "uid", all.x = TRUE)
ct <- na.omit(ct)

ggplot(ct, aes( lastParticipationDate, commentsperday),) + geom_point(size = 0.01, aes(color=factor(churn))) 

ggplot(ct[ct$commentsperday<1,], aes( lastParticipationDate, commentsperday),) + geom_point(size = 0.01, aes(color=factor(churn))) 


co[co$uid=="316 S",]
# quitters by company
# unique user per company. Table 4
target$uid <- paste(target$employee,target$companyAlias)
qbyco <- aggregate(churn ~ companyAlias, target, function(x) sum(x))
dim(qbyco)
summary(qbyco)
dim(nebyco)
qbyco<- qbyco[with(qbyco, order(companyAlias)), ]
nebyco<- nebyco[with(nebyco, order(companyAlias)), ]
nebyco$companyAlias
qbyco$companyAlias
#qbyco<- qbyco[!qbyco$companyAlias=="ZF",]
#qbyco<- qbyco[!qbyco$companyAlias=="ZG",]
#nebyco<- nebyco[1:28,]

# churnrate ... 
chu <- cbind(qbyco,nebyco)
chu <- chu[,c(1,2,4)]
colnames(chu) <- c("companyAlias","churn","nempl")
chu$churnrate = chu$churn/chu$nempl
chu<- chu[with(chu, order(-churnrate)), ]
summary(chu)

# normalize by observation time
obsperiod <- aggregate(voteDate ~ companyAlias, votes, function(x) as.double
(( difftime(max(x), min(x) , units = "days"))+1)/7)
summary(obsperiod)
dim(obsperiod)
obsperiod<- obsperiod[with(obsperiod, order(companyAlias)), ]
#obsperiod<- obsperiod[1:28,]
colnames(obsperiod)[2] <- c("period")
chu <- cbind(chu,obsperiod$period)
#convert to years
colnames(chu)[5] <- c("period")
chu$yearturnover <- 56*chu$churnrate/chu$period
summary (chu)
aggregate(target$stillExists, by=list(Category=target$companyAlias), FUN=sum)
aggregate(target$stillExists, by=list(Category=target$companyAlias), FUN=mean)
aggregate(target$stillExists, by=list(Category=target$companyAlias), FUN=length)
dim(chu)
target[1,]

dim(votes)
dim(co)
dim(target)

save(votes,file="data_03/votes.Rda")
save(co,file="data_03/co.Rda")
save(target,file="data_03/target.Rda")
save(li,file="data_03/li.Rda")



### LOAD NFM CLUSTERING X X X X X X X X 
nmf = read.csv("data_03/NMF.csv",stringsAsFactors=TRUE, header=TRUE)
li = read.csv("data_03/commentInteractions.csv")
summary(nmf)
### rename companyalias
length(unique(nmf$companyAlias))
d <- levels(li$companyAlias)
d2 <-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI")
length(d)
dict <- cbind(d,d2)
dict <- as.data.frame(dict)
d<- levels(nmf$companyAlias)
d <- as.data.frame(d)
newlabel<- merge(x = d,y = dict, by = "d", all.x = TRUE)$d2
levels(nmf$companyAlias) <- newlabel
nmf$companyAlias
nmf[1,]
save(nmf,file="data_03/nmf.Rda")
### X X X X X X X X X X X X X X X X X X
