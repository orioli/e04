library(gbm)
library(ggplot2)

# change this to your working directory
setwd("/Users/jse/Dropbox/0 - academics/0 - happyforce")

# ETL-ed DATA
load(file="data_03/votes.Rda")
load(file="data_03/co.Rda")
load(file="data_03/target.Rda")
load(file="data_03/li.Rda")
load(file="data_03/nmf.Rda")

#### CHECK UIDs #### "The funnel"
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

#### used data after cleanup #### "The funnel 2"
dim( votes[votes$uid%in%unique(target$uid),]) #221k
dim( co[co$uid%in%unique(target$uid),]) #29k
dim( li[li$uid%in%unique(target$uid),]) #29k

### SANITY CHECK
length(unique(target$uid))
mean(target$churn)

## fig 9  10
ggplot(target[target$stillExists==0,], aes(lastParticipationDate+15,fill=companyAlias)) +  geom_histogram(alpha = .95,binwidth=7)+ xlab("Churn date")
target$churned <- factor(target$churn, levels=c(1, 0), labels=c("Y", "N"))
ggplot(target, aes(lastParticipationDate,fill= as.factor(churned))) +  geom_histogram(alpha = .95)+ xlab("Date") 


### MODEL TRAINING AND FEATURES ############################################################
dim(target[target$stillExists==0,])  #1174 churn
length(unique(target[target$stillExists==1,]$uid))  # 3652 no churn
length(unique(target$uid))


### GENERATE FEATURES by UID  ###################################################
t <- target
train <- c()
prediction.day <- max(t$lastParticipationDate) -12*7 # same for every body
dmax = prediction.day 

### CLEAN NAs
co <- co[co$date<prediction.day,]
co <- na.omit(co)
vo <- votes[votes$voteDate<prediction.day,]
vo <- na.omit(vo)
li <- na.omit(li)


### deselect users that already churned when testing and when training as in testing you will not see such users that churned before.
dim(t[(t$lastParticipationDate<prediction.day&t$stillExists==0),])  # 469 users churned before prediction day
t <- t[!(t$lastParticipationDate<prediction.day&t$stillExists==0),]
dim(t[(t$stillExists==0),]) #238
dim(t[(t$lastParticipationDate>prediction.day&t$stillExists==0),])  # 236 users churned after prediction day

#length(setdiff(unique(votes$uid), unique(t$uid) )) # Because we removed churners before dmax... this is not 0 that is ok.
#length(setdiff(unique(co$uid), unique(t$uid) ))    # OK
### END SANITY CHECK
#summary(t)




### BUILD FEATURES
prevalias ="none"
number.of.employees = -1
train <- c()
c.w.f <-c()
# d.w.f <-c() deptarment not available in this data set
# g.w.f <-c()
length(t$uid)
length(unique(t$uid))  # rows ... 
length(unique(target$uid))
dim(target)

for ( uid in unique(t$uid) ){
	print(uid)	
	#uid = "7 P"
	tk <- t[t$uid==uid,]
	tca <- c()

   	# Company wide level features ###########################################################
	if (!(prevalias==as.character(tk$companyAlias) ) ){ # this is to save time
	    tca <- t[t$companyAlias==tk$companyAlias,]
	    number.of.employees = length(tca$employee)
	    prevalias=as.character(tk$companyAlias)
	    coa <- co[co$coa==prevalias,]
	    voa <- vo[vo$companyAlias==as.character(prevalias),]
	    lia <- li[li$companyAlias==as.character(prevalias),]
	    #dim(voa)
	    print(prevalias)

		v.start <- min(voa$voteDate)
		v.end   <- max(voa$voteDate)
       v.diff <- as.double( difftime(v.end, v.start , units = "days"))
     
       	days.since.last.interaction = 0
       	if (dim(voa[voa$companyAlias==prevalias,])[1]>2){
	       	days.since.last.interaction = aggregate(voteDate ~ uid, voa[voa$companyAlias==prevalias,], function(x) mean( as.double(( difftime(dmax, max(x) , units = "days"))+1)))
			days.since.last.interaction = mean(days.since.last.interaction$voteDate)
		}
		
		v.s    <- sum   (voa$vote)/ number.of.employees
		v.m    <- mean  (voa$vote)
		v.sd   <- sd    (voa$vote)
		v.count<- length(voa$vote)/ number.of.employees

		c.s    <- sum (nchar(paste(coa$txt)))  / number.of.employees### is this proxy for all time length ?
		c.m    <- mean(nchar(paste(coa$txt)))  ### normalize by k time company mean?
		c.sd   <- sd  (nchar(paste(coa$txt)))
		c.count<- length(coa$txt)/ number.of.employees

		c.l.s  <- sum((coa$like)) /number.of.employees
		c.l.m  <- mean((coa$like))/ number.of.employees
		c.l.sd <-  sd((coa$like)) / number.of.employees
		
		c.s.t     <- (c.s    /v.diff) / number.of.employees
		c.c.t     <- (c.count/v.diff) / number.of.employees
		v.count.t <- (v.count/v.diff) / number.of.employees	
		
		likes_and_dislikes_received <- length(l_$liked)/v.diff	
		likes_to_all_ratio <- length(lia[lia$liked=="true",]$liked) /length(lia$liked)

		c.w.f <-  c(v.s, v.m, v.sd,v.count,   c.s, c.m , c.sd, c.count,            c.l.s, c.l.m, c.l.sd,       c.s.t,  c.c.t, v.count.t, 
		number.of.employees,days.since.last.interaction ,likes_and_dislikes_received, likes_to_all_ratio)	    

		#d.w.f <- c( d.v.m, d.v.sd, d.c.m, d.c.s.t, number.of.employees.dept)
		length(c.w.f)
	} # Company wide fetures end

	### Individual level features ############################################################3
		c<-co[co$uid==uid,] 
	    v<-vo[vo$uid==uid,]
	    l_<-li[li$uid==uid,]    
	    #uid = "1 AE"
		v.start <- min(v$voteDate)
		v.end   <- max(v$voteDate)
		v.diff <- as.double( difftime(v.end, v.start , units = "days"))
		days.since.last.interaction = as.double( difftime(prediction.day , v.end, units = "days"))	   

		v.s    <- sum   (v$vote)
		v.m    <- mean  (v$vote)
		v.sd   <- sd    (v$vote)
		v.count<- length(v$vote)

		c.s 	<- sum  (nchar(paste(c$txt)))  ### is this proxy for all time length ?
		c.m 	<- mean (nchar(paste(c$txt)))  ### normalize by k time company mean?
		c.sd 	<- sd   (nchar(paste(c$txt)))
		c.count <- length(c$txt) / v.diff

		c.l.s  <-   sum(c$like)
		c.l.m  <-   mean(c$like)
		c.l.sd <-   sd  (c$like)
		
		c.s.t <- c.s/v.diff       # length of comment per day
		c.c.t <- c.count/v.diff	  # count of comments per day
		v.count.t <- v.count/v.diff
		
		likes_and_dislikes_received <- length(l_$liked)/v.diff
		likes_to_all_ratio <- length(l_[l_$liked=="true",]$liked)/length(l_$liked)
        indiv.features <- c( v.m, v.sd, c.s ,c.m, c.sd, c.count, c.l.s, c.l.m, c.l.sd , c.s.t, c.c.t, likes_and_dislikes_received, likes_to_all_ratio)
 		length(indiv.features)
 
 
		### EC features ##################
		ec <- c(v.m/c.w.f[1], v.sd/c.w.f[2], c.m/c.w.f[3], c.count/c.w.f[4])

		### ED features ##################
		#ed <- c(v.m/d.w.f[1], v.sd/d.w.f[2], c.m/d.w.f[3], c.count/d.w.f[4])

		f <-  	c(uid, prevalias,  tk$churn,  c.w.f, ec,  indiv.features)
		print (length(f))  
		#print ((f))  
		train <-rbind(train, f ) 
}
train <- as.data.frame(train)
summary(train)
dim(train)
train[1,] 

cname2 <- c("uid", "company",  "churn",
"v.s","v.m","v.sd","v.count","c.s","c.m","c.sd","c.count","c.l.s","c.l.m","c.l.sd","c.s.t","c.c.t","v.count.t","number.of.employees","days.since.last.interaction","likes_and_dislikes_received","likes_to_all_ratio",
"ec.v.m","ec.v.sd","ec.c.m", "ec.c.count",
"e.v.m", "e.v.sd", "e.c.s" ,"e.c.m", "e.c.sd", "e.c.count", "e.c.l.s", "e.c.l.m", "e.c.l.sd","e.c.s.t", "e.c.c.t","e.likes_and_dislikes_received", "e.likes_to_all_ratio")

length(cname2)		
dim(train)
colnames(train) = cname2
save(train,file="data_03/train_12weeks_3_v2.Rda")




### ADD N.M.F  features
load(file="data_03/train_12weeks_3_v2.Rda")
nmf$uid = paste(nmf$employee,nmf$companyAlias)
train<- merge(x=train, y=nmf, by = "uid", all.x= TRUE) #  all users  
#train<- merge(x=train, y=nmf, by = "uid", all.y= TRUE) # only users with more than 5 interactions
summary(train)
train <- train[-1271,]
train$churn
for (k in 3:dim(train)[2] ){
	train[,k] <- as.numeric(as.character(train[,k]))
	train[is.nan(train[,k]),k] <- c(0)
	train[is.na(train[,k]),k] <- c(0)
	train[is.infinite(train[,k]),k] <- c(0)
}

summary(train)


### ADD companny ID dummy variable...
xx =as.character(train$company)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
xx = substrRight(xx,2)
xx<- as.factor(xx)
company.dummies = model.matrix(~ xx + 0)
dim(company.dummies)
#company.dummies[1:933,1:3]
colnames(company.dummies) <- c("d01","d02","d03","d04","d05","d06","d07","d08","d09","d10","d11","d12","d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25","d26","d27","d28","d29","d30","d31","d32","d33","d34")
train <- cbind(train,company.dummies)
summary(train)
dim(train)
train[train$e.likes_to_all_ratio>1,c(1,2.36,37)]
#train[train$uid=="1 AE",]
$e.likes_to_all_ratio
train[652,]
#save(train,file="data_03/train_12weeks_3withnmf_fitler_zero.Rda")
#write.csv(train, file="data_03/train_12weeks_3_v2.csv")
### SANITY CHECK check churns
train[1:5,]
target[1,]
###



### likability figures
#train$churn <- as.factor(train$churn)
ggplot(train, aes(e.likes_to_all_ratio,fill=churn)) +  geom_density(alpha = .95) +
labs(title = "", y = "Count", x = "Employee Likability: Ratio of likes to (likes + dilsikes) received") +facet_grid(churn~ .)

dim(train[train$e.likes_to_all_ratio>0.25,])
dim(train[train$e.likes_to_all_ratio<0.25,])
mean(train[train$e.likes_to_all_ratio>0.25,]$e.likes_to_all_ratio)

#ggplot(train,aes(e.likes_to_all_ratio, churn))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm',formula=y ~ x )


lm(formula = weight ~ height, data = women)
fit <- lm(formula = churn ~ e.likes_to_all_ratio, data = train)
# probabilty of churn = +.1766 -0.1766 x  e.likes_to_all_ratio
summary(fit)

# split train into train and validations sets
test  <- train[seq(1, dim(train)[1], 2),]
train <- train[seq(0, dim(train)[1], 2),]
sum(test$churn)
sum(train$churn)
dim(train)
dim(test)
train[1:12,]
summary(train)
is.data.frame(train)


#v.s+v.m+v.sd+v.count+c.s+c.m+c.sd+c.count+c.l.s+c.l.m+c.l.sd+c.s.t+c.c.t+v.count.t+number.of.employees+days.since.last.interaction+likes_and_dislikes_received+likes_to_all_ratio+ec.v.m+ec.v.sd+ec.c.m+ec.c.count+e.v.m+e.v.sd+e.c.s+e.c.m+e.c.sd+e.c.count+e.c.l.s+e.c.l.m+e.c.l.sd+e.c.s.t+e.c.c.t+e.likes_and_dislikes_received+e.likes_to_all_ratio+CE_G_interactions_NMF1_d+ CE_G_interactions_NMF1_u+ CE_G_interactions_NMF2_d+ CE_G_interactions_NMF2_u+ CE_G_rel_agree_NMF1_d+ CE_G_rel_agree_NMF1_u+ CE_G_rel_agree_NMF2_d+ CE_G_rel_agree_NMF2_u,	


### TRAIN + EVAL   (add / remove feautures...)
    # GBM Model no NFM
	gbm2 <- gbm(  churn ~ 
e.v.m+e.v.sd+e.c.s+e.c.m+e.c.sd+e.c.count+e.c.l.s+e.c.l.m+e.c.l.sd+e.c.s.t+e.c.c.t+e.likes_and_dislikes_received+e.likes_to_all_ratio,	
	data=train,
	distribution="bernoulli",
	n.trees=800,
	shrinkage=0.05,	
	interaction.depth=3, 
	bag.fraction = 0.5,
	train.fraction = 0.5,
	cv.folds=5,
	keep.data=TRUE)

	
summary(gbm2)	
best.iter <- gbm.perf(gbm2,method="OOB") 
print(best.iter)
test[1,]
f.predict <- predict(gbm2, test[,3:dim(test)[2]] , best.iter)

### check error graphically ####################################
plot(f.predict, test$churn, main="Prediction: Employee quit within 12 weeks days or less",   xlab="predicted risk of quitting ", ylab="actual ", pch=19)
e <-c()
e <- cbind( test$churn,f.predict)
e <- as.data.frame(e)
colnames(e) <- c("actual","pred")
e = e[order(e$pred),]  # employees most risk of quitting  at the top
e$churn <- factor(e$actual, levels=c(1, 0), labels=c("Y", "N"))
ggplot(e, aes(pred,colour=churn)) +  geom_density(alpha = .95)
ggplot(e, aes(pred,colour=churn)) +  geom_bar(alpha = .95) + facet_grid(churn~ .)

# Precision @ measures. Change the theresholds accrodingly ... 
sum(e$actual)
# Precission at 45 out of 116 possible churns = 89%
dim(e[e$pred>-.96,])
mean(e[e$pred>-.96,]$actual) 

# Precission at 238   out of 926  = 91%
dim(e[e$pred>0,])
mean(e[e$pred>0,]$actual)
summary (train)


# fig 12. cluster top three features 22,24
test$churn = as.factor(test$churn)
test$churn <- factor(test$churn, levels=c(1, 0), labels=c("Y", "N"))
  test[1,]
  dim(test)
  summary(test)

  ggplot(test, aes(e.likes_to_all_ratio, ec.v.sd),) + geom_point(size = 0.5,aes(color=churn))   + labs(x = "F1: Employee Likability", y="F3: Relative Stability of Hapiness")  + facet_grid(churn~ .)+ theme(legend.position="none")

  ggplot(test, aes(e.likes_to_all_ratio, ec.v.sd),) + geom_point(size = 0.5,aes(color=churn))   + labs(x = "F1: Employee Likability", y="F3: Relative Stability of Hapiness")  + facet_grid(churn~ .)+ theme(legend.position="none")

qplot(e.likes_to_all_ratio, data=test, geom="bar", fill=factor(churn))

ggplot(test, aes(e.likes_to_all_ratio,colour=churn)) +  geom_bar(alpha = .95) 

ggplot(test, aes(e.likes_to_all_ratio,fill=churn)) +  geom_histogram(alpha = .95) +
labs(title = "", y = "Count", x = "Employee Likability: Ratio of likes to (likes + dilsikes) received")

ggplot(test, aes(e.likes_to_all_ratio,fill=churn)) +  geom_density(alpha = .65) 
+labs(title = "", y = "Churn", x = "Employee Likability: Ratio of likes to (likes + dilsikes) received")
+ facet_grid(churn~. )

ggplot(test, aes(cwf.c.c.t,fill=churn)) +  geom_density(alpha = .65) 
+labs(title = "", y = "Churn", x = "Employee Likability: Ratio of likes to (likes + dilsikes) received")
+ facet_grid(churn~. )

  ggplot(test, aes(e.likes_to_all_ratio, cwf.c.c.t),) + geom_point(size = 0.5,aes(color=churn))   + labs(x = "F2: Posting Frequency", y="F3: Relative Stability of Hapiness")  + facet_grid(churn~ .)+ theme(legend.position="none")

  ggplot(test, aes(e.likes_to_all_ratio, e.v.m),) + geom_point(size = 0.5,aes(color=churn))   + labs(x = "F2: Posting Frequency", y="F3: Relative Stability of Hapiness")  + facet_grid(churn~ .)+ theme(legend.position="none")


votes[1,]


# Some fun stuff: checking if the honey moon effect can be detected...
dd <-c()
#uid = "1 AD"
for ( uid in unique(t$uid) ){
	print(uid)
	v = votes[votes$uid ==uid,]
	max = max(v$voteDate)
	min = min(v$voteDate)
	span = as.double(( difftime(max(max), min(min) , units = "days")))
	#l <- lm(formula = vote ~ voteDate, data = v)
	#summary(l)
	dd <- rbind(dd, cbind(	v$vote,	
							as.double(( difftime(v$voteDate,max(max) , units = "days"))) ,
							rep(span ,length(v$vote))
						  )
				)
}
   
dd<- as.data.frame(dd)
summary(dd)
dd$voteblock <- floor(dd$V2/100)
dd$permanence <- floor(dd$V3>560)
dd$block.permanence <- paste(dd$voteblock,dd$permanence)
dd$block.permanence <- as.factor(dd$block.permanence)
c <- ggplot(dd[dd$V3>0,], aes(x=V2, y=V1))
c+ stat_smooth(method=glm, aes( fill = factor(block.permanence)))  + labs(x = "day of vote", y="Mean Hapiness") 
+ facet_grid(as.factor(voteblock) ~)


 # ggplot(dd, aes(V2, V1),) + geom_point(size = 0.5)     
