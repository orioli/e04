# (c) Jose Berengueres --- JMIR

library(gdata)
library(ggplot2)
library(gbm)
#install.packages("devtools")
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

setwd("C:/Users/cit1/Desktop/00-JMIR/code")
#setwd("/Users/jse/Dropbox/00 - JMIR/code")



### First clean the data
# 1. load churn
target <- read.csv("../input/lastParticipationExists.csv")
#names(target)
names(target)
# parse dates
target$lastParticipationDate<- gsub("UTC", "", target$lastParticipationDate)
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
#as.numeric(target$companyAlias)

ggplot(target[target$stillExists==0,], aes(lastParticipationDate+15,fill=as.factor(as.numeric(companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
#ggplot(target, aes(lastParticipationDate,fill=companyAlias)) +  geom_histogram(alpha = .95)+ xlab("Churn date") + facet_grid(churn~ .)
#target$churn <- factor(target$churn, levels=c(1, 0), labels=c("Y", "N"))
ggsave("../figs_JMIR/fig6.pdf")

ggplot(target, aes(lastParticipationDate,fill= churn)) +  geom_histogram(alpha = .95)+ xlab("Date") 
#target$churn <- factor(target$churn, levels=c("Y","N"), labels=c("Churn", "Last Active"))
ggplot(target, aes(lastParticipationDate,fill= as.factor(churn))) +  geom_density(alpha = .45)+ xlab("Date") 
target$uid <- paste(target$employee,target$companyAlias)
target[1,]





# 2. likes table
li = read.csv("../input/commentInteractions.csv")
li <- droplevels(li)
li$companyAlias <- as.factor(li$companyAlias)
li$uid <- paste(li$employee,li$companyAlias)





# 3. comments table
co <- read.csv("../input/comments.csv")
co <- unique(co)
# rename as in prev version
colnames(co)[7] <- c("date")
colnames(co)[6] <- c("nolike")
colnames(co)[5] <- c("like")
colnames(co)[4] <- c("txt")
colnames(co)[3] <- c("commentid")
colnames(co)[2] <- c("coa")
colnames(co)[1] <- c("id")
co <- unique(co)
uc <- unique(li$companyAlias)
co <- co[co$coa %in% uc,]
co <- droplevels(co)
levels(co$coa)
co$coa <- factor(co$coa)
summary(co)

# parse dates
co$date<- gsub("UTC", "", co$date)
co$date<- gsub("  ", " ", co$date)
co$date<- as.Date(co$date, format = "%a %b %d %H:%M:%S %Y")

co$txt <- as.character(co$txt)
co$lcomment <- unlist(lapply(co$txt,nchar) )
# make unique id for all employees
co$uid <- paste(co$id,co$coa)




# 4. daily happiness table
votes <- read.csv("../input/votes.csv")
votes <- votes[votes$companyAlias %in% uc,]
votes <- droplevels(votes)
votes$companyAlias <- factor(votes$companyAlias)
votes$uid <- paste(votes$employee,votes$companyAlias)

# parse dates
# votes$dt <- as.POSIXct((votes$voteDate), origin="1970-01-01")
votes$voteDate<- gsub("UTC", "", votes$voteDate)
votes$voteDate<- gsub("  ", " ", votes$voteDate)
votes$voteDate<- as.Date(votes$voteDate, format = "%a %b %d %H:%M:%S %Y")

### filter  outliers in churn
embyco <- aggregate(churn ~ companyAlias,target , mean)
embyco2 <- aggregate(churn ~ companyAlias,target , length)
embyco <- cbind(embyco,embyco2$churn)
embyco[order(embyco$churn),]
have.churn <- as.character(embyco[embyco$churn>0.01,]$companyAlias)
mid <- as.character(embyco[embyco$churn>0.001&embyco$churn<0.33,]$companyAlias)
hi <-  as.character(embyco[embyco$churn<1&embyco$churn>0.33,]$companyAlias)




# 5. remove spike periods 4% of days
target$day.number <- as.double( difftime(target$lastParticipationDate, min(target$lastParticipationDate) , units = "days"))
hi.churn.days <- aggregate(churn  ~ day.number,target , sum)
hi.churn.days <- hi.churn.days[order(hi.churn.days$churn),]
spike.days <- hi.churn.days[hi.churn.days$churn>5,]$day.number
length(spike.days)/max(target$day.number)

#lets remove days (day.number)  where more than 10 ppl churned.
dim(target)
target <-  target[!(target$day.number %in% spike.days),]
dim(target)



# 6 .visualize
ggplot(target[target$stillExists==0&(target$companyAlias %in%mid),], aes(lastParticipationDate+15,fill=as.factor(as.numeric(companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date") +  facet_grid(companyAlias~ .)
ggplot(target[target$stillExists==0&!(target$companyAlias %in%hi),], aes(lastParticipationDate+15,fill=as.factor((companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
ggplot(target[target$stillExists==0&(target$companyAlias %in%hi),], aes(lastParticipationDate+15,fill=as.factor(as.numeric(companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
ggplot(target[target$stillExists==0&(target$companyAlias %in%have.churn),], aes(lastParticipationDate+15,fill=as.factor(as.numeric(companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")


### filter
target <- target[target$companyAlias %in% have.churn,]  #do not insert companeis with no churn!!!
votes <- votes[votes$companyAlias %in% have.churn,]
dim(votes)
li <- li[li$companyAlias %in% have.churn,]
dim(li)
co <- co[co$coa%in% outliers,]
dim(co)





# 7. Some histograms
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(voteDate,fill=as.factor(as.numeric(companyAlias)))) +   geom_bar(alpha = .95) + labs(title = "Active users per day")
ggplot(votes, aes(vote,fill=companyAlias)) +  geom_bar(alpha = 1) + labs(title = "Distribution of happiness")
votes$weekday <- weekdays(votes$voteDate)
ggplot(votes[votes$voteDate>max(votes$voteDate)-380,], aes(weekday,fill=companyAlias))+   geom_bar(alpha = .95)






# 8. #### CHECK UIDs #### "The funnel"
length(unique(target$uid))
length(unique(votes$uid ))
length(unique(li$uid    ))
length(unique(co$uid    ))

length(intersect(unique(target$uid), unique(votes$uid) )) # OK
length(intersect(unique(target$uid), unique(co$uid) ))    # OK
length(setdiff(unique(votes$uid), unique(target$uid) )) # OK
length(setdiff(unique(co$uid), unique(target$uid) ))    # OK
length(setdiff(unique(li$uid), unique(target$uid) ))    # OK

# remove duplicated and negative id users from target
# remove negative id
target = target[!duplicated(target$uid), ]
target = target[!(target$employee<1), ]






## fig 9  10
ggplot(target[target$stillExists==0,], aes(lastParticipationDate+15,fill=as.factor(as.numeric(companyAlias)))) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
target$churned <- factor(target$churn, levels=c(1, 0), labels=c("Y", "N"))
ggplot(target, aes(lastParticipationDate,fill= as.factor(churned))) +  geom_histogram(alpha = .95)+ xlab("Date") 
ggsave("../figs_JMIR/fig7.pdf")



dim(target[target$stillExists==0,])  #1174 churn
length(unique(target[target$stillExists==1,]$uid))  # 3652 no churn
length(unique(target$uid))






### 9. Generate features by uid 
t <- target
t[1,]
train <- c()
dim(t[(t$stillExists==0),]) #212
SEGMENT <- 6000000
co[1,]
uid  <- "1 58b5f7cb917c350004dc0b09"
SPAN <- 60
MIN  <- 0
f.name_all <- c()
uid <- "1 58b5f7cb917c350004dc0b09"
for ( uid in unique(t$uid) ){
		#print(uid)
		f.name  <- c()
		f.value <- c()	
		f.name  <- c(f.name,"target","uid","high.churn.company")
		ca <- as.character(t[t$uid==uid,]$companyAlias)
		f.value <- c(f.value,t[t$uid==uid,]$stillExists,uid, ca %in% hi)
		v  <-	votes[votes$uid==uid,]
		l  <-	li[li$uid==uid,]  	 
		v.start  <- min(v$voteDate)
		v.end    <- max(v$voteDate)
		seniority <- as.double( difftime(v.end, v.start , units = "days"))
		v2 <-	votes[votes$uid==uid&votes$voteDate>(v.end-(SPAN/2))&votes$voteDate<v.end,]
	
		if (seniority > (SPAN+1) ){
	    		cc <-	co[co$uid==uid,] 	

			sum.rx.like 	<- sum(cc$like) #received likes
			sum.rx.nolike 	<- sum(cc$nolike) #received dilikes
			sum.rx.likeability 	<- sum.rx.like/(sum.rx.like+sum.rx.nolike)  #likability
			f.name <- c(f.name,
			"sum.rx.like",
			"sum.rx.nolike",
			"sum.rx.likeability")
			f.value <- c(f.value, 
			sum.rx.like,
			sum.rx.nolike,
			sum.rx.likeability) 


			m.rx.like 	<- mean(cc$like)
			m.rx.nolike <- mean(cc$nolike)
			m.rx.likeability <- m.rx.like/(m.rx.like+m.rx.nolike)
			f.name <- c(f.name,
			"m.rx.like",
			"m.rx.nolike",
			"m.rx.likeability")
			f.value <- c(f.value, 
			m.rx.like,
			m.rx.nolike,
			m.rx.likeability) 


			f.name <- c(f.name,
			"seniority",
			"comments.count",
			"votes.count",
			"xlikes.tx.count",
			
			"comments.count.m",
			"votes.count.m",
			"xlikes.tx.count.m"		)
			f.value <- c(f.value, 
			seniority, 
			dim(cc)[1],
			dim(v )[1],
			dim(l )[1], 
			
			dim(cc)[1]/seniority, 
			dim(v )[1]/seniority, 
			dim(l )[1]/seniority)		
		
		

			f.name <- c(f.name,
			"votes.mean", 
			"votes.mean.norm.by.company.mean", 
			"votes.sd", 
			"votes.sd.norm.by.company.mean")

			v.m		<- mean(v$vote)
			v.m.norm 	<- v.m / mean(votes[votes$companyAlias==ca,]$vote)
			v.sd 		<- sd(v$vote)
			v.sd.norm 	<- v.sd / sd(votes[votes$companyAlias==ca,]$vote)

			f.value <- c(f.value, 
			v.m,
			v.m.norm, 
			v.sd, 
			v.sd.norm)


		
			f.name <-c(f.name,
			"likeaity.given",
			"likeability.given.last.half",
			"likeability.change",		
			"vote.trend"  )

			aaa <- length(l[l$liked=="true",]$liked)/length(l$liked)
			allowed_comments <- co[co$date>(v.end-(SPAN/2))&co$date<v.end,]$commentid
			bbb <- length(l[l$liked=="true"&l$commentId %in% allowed_comments,]$liked) / length(l[l$commentId %in% allowed_comments,]$liked)
			vote.trend <-  mean(v2$vote)/v.m

			f.value <- c(f.value, 
			aaa , 
			bbb ,
			bbb/aaa,
			vote.trend  )
			
			if ( dim(v)[1] > MIN ){
				train <-rbind(train, f.value ) 
				f.name_all <- f.name

			}
		}
}



train[1,]
summary(train)

dim(train)
train <- as.data.frame(train)
colnames(train) <- f.name_all

summary(train)
train <- as.data.frame(train)
write.csv(train, file="../input/train_set_exportJAN9_00_mixed_rawbcc001.csv")






### 10. load train set

train2 <-  read.csv("../input/train_set_exportJAN9_00_mixed_rawbcc001.csv", stringsAsFactors=FALSE)
train2$target <- (1 -1*train2$target)
sum(train2$target)
dim(train2)
train2[1,]
colnames(train2)

#rename names for paper
colnames(train2)[25] <- c("happiness.trend")
colnames(train2)[24] <- c("likeability.received.trend")
colnames(train2)[23] <- c("likeability.given.last.month")
colnames(train2)[22] <- c("likeability.given")
colnames(train2)[21] <- c("happiness.sd.normlized.by.company")
colnames(train2)[20] <- c("happiness.sd")
colnames(train2)[19] <- c("happiness.mean.normlized.by.company")
colnames(train2)[18] <- c("happiness.mean")
colnames(train2)[17] <- c("mean.count.of.likes.and.dislikes.given.to.others")
colnames(train2)[16] <- c("voting.frequency")
colnames(train2)[15] <- c("posting.frequency")
colnames(train2)[14] <- c("count.of.likes.and.dislikes.given.to.others")
colnames(train2)[13] <- c("total.count.of.votes")
colnames(train2)[12] <- c("total.count.of.comments")
colnames(train2)[11] <- c("days.using.the.app")
colnames(train2)[10] <- c("likeability.received")
colnames(train2)[9] <- c("mean.count.dislikes.received")
colnames(train2)[8] <- c("mean.count.likes.received")
colnames(train2)[7] <- c("total.sum.likeability.received")
colnames(train2)[6] <- c("total.sum.dislikes.received")
colnames(train2)[5] <- c("total.sum.likes.received")
train2$uid <- NULL
train2$X <- NULL
train2$total.count.of.comments <- NULL
train2$total.sum.likeability.received <- NULL
train2$total.sum.likes.received <- NULL
train2$total.count.of.votes <- NULL
train2$total.sum.dislikes.received <- NULL
train2$count.of.likes.and.dislikes.given.to.others <- NULL
colnames(train2)





summary(train2)
train2[,5] <- as.numeric(as.character(train2[,5]))
ccc <- cor(train2[,c(2,5:dim(train2)[2]) ])[,1:3]
ccc <- as.data.frame(ccc)
ccc <- ccc[order(ccc$target),]
ccc
summary(ccc)

for (k in 2:dim(train2)[2] ){
	train2[,k] <- as.numeric((train2[,k]))
	train2[is.nan(train2[,k]),k] <- 0
	train2[is.na(train2[,k]),k] <- 0
	train2[is.infinite(train2[,k]),k] <- 0
}
ccc <- cor(train2[,c(2,5:dim(train2)[2]) ])[,1:3]
ccc <- as.data.frame(ccc)
ccc <- ccc[order(ccc$target),]
ccc


# way 1
rr <- 0.70
train3  <- train2[1:floor(rr*dim(train2)[1]),]
test3 <- train2[floor(rr*dim(train2)[1]) : dim(train2)[1],]
colnames(test3)
colnames(train3)


# way 2 
#s <- sample(nrow(train2), floor(rr*dim(train2)[1]))
#train3 <- train2[s,]
#test3  <- train2[-s,]
dim(test3)
dim(train3)
test3[1:11,]

summary(train3)
sum(train3$target)
summary(test3)
sapply(train3,function(x) sum(is.na(x)))

# 11. simple model logsitic regression
#
model <- glm( target ~ .,family=binomial(link='logit'),data=train3)
summary(model)

log.reg <- (summary(model))
sc = as.data.frame(log.reg$coefficients)
sc[with(sc, order(-Estimate)), ]

anova(model, test="Chisq")
library(pscl)
pR2(model)
fitted.results <- predict(model,newdata=subset(test3),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
f.predict <- fitted.results
misClasificError <- mean(fitted.results != test3$target)
print(paste('Accuracy',misClasificError))
gbm2 <- model


rr

# 12. GBM
paste(names(train2), collapse = '+')
write.csv(summary(gbm2)$var, file="../input/gbmsummary.csv")

for(k in 1:10) {

	gbm2 <- gbm(  target ~ .,data=train3,
	distribution="bernoulli",
	n.trees=30,
	shrinkage=0.05,	
	interaction.depth=3, 
	bag.fraction = 0.5,
	train.fraction = 0.5,
	cv.folds=5,
	keep.data=TRUE)
	print(summary(gbm2)[1:6,])
	best.iter <- gbm.perf(gbm2,method="OOB") 
	print(best.iter)
	f.predict <- predict(gbm2, test3 , best.iter)


# viz ROC
e <- c()
e <- cbind( test3$target,f.predict)
e[1:33,]
e <- as.data.frame(e)
colnames(e) <- c("actual","pred")
e = e[order(e$pred),]  # employees most risk of quitting  at the top
e$churn <- factor(e$actual, levels=c(1, 0), labels=c("Y", "N"))
ggplot(e, aes(pred,colour=churn)) +  geom_density(alpha = .95)
ggplot(e, aes(pred,colour=churn)) +  geom_bar(alpha = .95) + facet_grid(churn~ .)
# Precision @ measures. Change the theresholds accrodingly ... 
sum(e$actual)
# Precission at 45 out of 116 possible churns = 89% 
dim(e[e$pred>0,])
e[e$pred>0,]
mean(e$actual)
sum(e$actual)
length(e$actual)
mean(e[e$pred>0,]$actual) 
length(e[e$pred>0,]$actual) 
sum(e[e$pred>0,]$actual) 
e[1:10,]
## ROC
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
library(pROC)
plot(roc(e$actual, e$pred, direction="<"), col="green", lwd=3, main="ROC Prediction of employee turnover")


}

summary(gbm2)


# viz ROC
e <- c()
e <- cbind( test3$target,f.predict)
e[1:33,]
e <- as.data.frame(e)
colnames(e) <- c("actual","pred")
e = e[order(e$pred),]  # employees most risk of quitting  at the top
e$churn <- factor(e$actual, levels=c(1, 0), labels=c("Y", "N"))
ggplot(e, aes(pred,colour=churn)) +  geom_density(alpha = .95)
ggsave("../figs_JMIR/churn_spread2.pdf")

ggplot(e, aes(pred,colour=churn)) +  geom_bar(alpha = .95) + facet_grid(churn~ .)
ggsave("../figs_JMIR/churn_spread.pdf")


# Precision @ measures. Change the theresholds accrodingly ... 
sum(e$actual)
# Precission at 45 out of 116 possible churns = 89%
dim(e[e$pred>0,])
e[e$pred>0,]
mean(e$actual)
sum(e$actual)
length(e$actual)
mean(e[e$pred>0,]$actual) 
length(e[e$pred>0,]$actual) 
sum(e[e$pred>0,]$actual) 
e[1:10,]
## ROC
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
library(pROC)
plot(roc(e$actual, e$pred, direction="<"), col="green", lwd=3, main="Receiver Operating Characteristic Curve \n Employee Turnover Prediction")
#ggsave("ROC.pdf")




# 15 clusters on train2 (the whole dataset)
f.predict <- predict(gbm2, test3 , best.iter)
e <- cbind( test3,f.predict)
#f.predict <- predict(gbm2, train2 , best.iter)
#e <- cbind( train2,f.predict)
train2[1,]
names(e[1,])
summary(gbm2)

#ggplot(e, aes(f.predict,colour=as.factor(target))) +  geom_density(alpha = .95)
pa <- ggplot(e, aes(days.using.the.app, 	colour=as.factor(target))) +  geom_density(alpha = .95) + scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
pb <- ggplot(e, aes(likeability.received,	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
pc <- ggplot(e, aes(likeability.given, 	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
pd <- ggplot(e, aes(happiness.trend  , 	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
pe <- ggplot(e, aes(voting.frequency ,	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
pf <- ggplot(e, aes(posting.frequency,	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
aa <- ggplot(e, aes(happiness.sd ,	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
bb <- ggplot(e, aes(happiness.mean,	colour=as.factor(target))) +  geom_density(alpha = .95)+ scale_color_manual(values = c("dark turquoise", "red"))+theme(legend.title=element_blank())
mean(e$target)
ggplot2.multiplot(pa, pb, pc, pd, pe, pf,aa, bb , cols=2) 
ggsave("../figs_JMIR/8x8fig.pdf")



