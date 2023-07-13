rm(list = ls())
setwd("C:/Users/magom/OneDrive/Desktop/data science for marketing analytics/dsmaproject")
akron_review=read.csv("Akron_review.csv.csv",stringsAsFactors = F)
df_business=read.csv("open_restaurants_publicserver.csv",stringsAsFactors = F)
#Reviews Data only for open Restaurants
akron_review<- akron_review[akron_review$business_id %in% df_business$business_id,]
unique(akron_review$business_id)
write.csv(akron_review, file="df_review.csv")
df_review=read.csv("df_review.csv")




#library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggthemes)
library(tidytext)
library(qdap)
library(lexicon)
library(wordcloud)
library(wesanderson)
library(mice)
library(scales)
library(corrplot)


str(df_review)
df_review$date=as.Date(df_review$date)
head(df_review)
summary(df_review)

# clean data


#inspect pattern of missings with mice
md.pattern(df_review)
#/\     /\
#{  `---'  }
#{  O   O  }
#==>  V <==  No need for mice. This data set is completely observed.
# \  \|/  /
#  `-----'


#
#result: no missing values in this table

# examine outlier
#no negative data in star
sum(df_review$star<0) #ok
sum(df_review$star>5)#ok

ggplot(data=df_review,aes(x='',y=stars))+
  geom_boxplot(outlier.color='red',outlier.alpha=0.5, fill='yellow')+
  geom_text(aes(x='',y=median(stars),label=median(stars)),size=3,hjust=11)+
  xlab(label = '')
#result: no outlier


stars=ggplot(data=df_review,aes(x=stars))+
  geom_histogram(binwidth=0.5,fill='yellow',)+
  labs(title="Review Stars")+theme_minimal()+ 
  theme(plot.title = element_text(color="black", size=20, face="bold.italic",hjust = 0.5))



#useful
useful=df_review%>%
  select(useful,stars)%>%
  subset( useful>0)%>%
  group_by(stars)%>%
  summarise(useful=sum(useful))%>% 
  ggplot(aes(x=stars,y=useful,color=useful,fill=useful))+ geom_col()+
  labs(title="useful reviews")+
  theme_minimal()

#cool
cool=df_review%>%
  select(cool,stars)%>%
  subset( cool>0)%>%
  group_by(stars)%>%
  summarise(cool=sum(cool))%>% 
  ggplot(aes(x=stars,y=cool,color=cool,fill=cool))+ geom_col()+
  labs(title="cool reviews")+theme_minimal()

#funny
funny=df_review%>%
  select(funny,stars)%>%
  subset( funny>0)%>%
  group_by(stars)%>%
  summarise(funny=sum(funny))%>% 
  ggplot(aes(x=stars,y=funny,color=funny,fill=funny))+ geom_col()+
  labs(title="funny reviews")+theme_minimal()


grid.arrange(stars,useful,cool,funny,ncol=2,nrow=2)

#sentiment analysis

#plot top 5 words present in the reviews
words=df_review%>%
  unnest_tokens(input = text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(5)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  labs(title="top 5 words")+theme_minimal()+
  coord_flip()


#categorize the words
df_review%>%
  group_by(review_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(get_sentiments('bing'),multiple = "all")%>%
  group_by(sentiment)

#number of positive and negative word in percentage
df_review %>%
  select(review_id,text)%>%
  group_by(review_id)%>%
  unnest_tokens(output=word,input=text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'),multiple = "all")%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)*100)


#plot
general_sentiment=df_review%>%
  group_by(review_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(get_sentiments('bing'),multiple = "all")%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+
  geom_col()+theme_minimal()+guides(fill=F)+
  coord_flip()+
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling2", type = "continuous"))+
  labs(title="number of positive and negative words")

#sentiment for the reviews grouped by stars
sentiment_stars=df_review %>%
  select(review_id,text,stars)%>%
  group_by(review_id,stars)%>%
  unnest_tokens(output=word,input=text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'),multiple = "all",by = join_by(word))%>%
  group_by(stars,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)*100)%>%
  ggplot(aes(x=stars,y=proportion,fill=sentiment))+geom_col()+
  theme_minimal()+coord_flip()+labs(title="sentiment for the reviews grouped by stars")+
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling2", type = "continuous"))

grid.arrange(words,general_sentiment,sentiment_stars,nrow=1,ncol=3)



voc = read.table("nrc_lexicon.txt", header = F,
                 col.names = c('word','sentiment','num'),
                 sep = '\t',
                 stringsAsFactors = F)
voc = voc[voc$num!=0,]
voc$num = NULL

#emotions in the reviews
emotions=df_review%>%
  group_by(review_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(voc,multiple = "all")%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+
  geom_col()+guides(fill=F)+coord_flip()+theme_minimal()+
  xlab("emotions")+
  scale_fill_manual(values=wes_palette(n=10, name="Darjeeling1", type = "continuous"))+
  labs(title="Emotions in the reviews")


#Ratings of all Reviews based on Emotion Expressed

ratingemo=df_review%>%
  group_by(review_id,stars)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(voc,multiple = "all")%>%
  group_by(sentiment)%>%
  summarize(meanstar = mean(stars))%>%
  ggplot(aes(x=sentiment,y=meanstar,fill=sentiment))+
  geom_col()+theme_minimal()+
  xlab("emotions")+
  scale_fill_manual(values=wes_palette(n=10, name="Darjeeling1", type = "continuous"))+
  labs(title="Avg stars per emotions")
grid.arrange(emotions,ratingemo,nrow=1,ncol=2)



#graphic emotions
df_review%>%
  group_by(review_id,stars)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(voc,multiple = "all")%>%
  group_by(review_id,sentiment,stars)%>%
  count()%>%
  group_by(sentiment,stars)%>%
  summarize(n = sum(n))%>%
  ungroup()%>%
  ggplot(aes(x=stars,y=n,fill=stars))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()+theme_minimal()


#Correlation between emotion expressed and review rating

cordt=df_review%>%
  group_by(review_id,stars)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(voc,multiple = "all")%>%
  group_by(review_id,sentiment,stars)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,stars))%>%
  data.frame()
write.table(cordt, file = "cordt.txt", sep = ",", quote = FALSE, row.names = F)

#regression
df_review%>%
  group_by(review_id,stars)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(voc,multiple = "all")%>%
  group_by(review_id,sentiment,stars)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  ggplot(aes(x=stars,y=n))+
  geom_point(color="yellow")+facet_wrap(~sentiment)+
  geom_smooth(method='lm',se=F,color="black",linetype="dashed")+
  theme_minimal()+
  stat_cor(aes(label=after_stat(r.label)))

#new vocabulary
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)



# examine the sentiment of all reviews.

df_review %>%
  select(review_id,text)%>%
  group_by(review_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('red','green'))+
  guides(fill=F)+
  theme_minimal()+
  labs(title="Numerical Sentiment")
#Wordcloud
wordcloudData = 
  df_review%>%
  group_by(review_id)%>%
  unnest_tokens(output=word,input=text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  data.frame()

wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.25),
          max.words = 100,colors=brewer.pal(8,"Spectral"))


#Comparison Cloud
wordcloudData1 = 
  df_review%>%
  group_by(review_id)%>%
  unnest_tokens(output=word,input=text)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'),multiple = "all")%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData1) = wordcloudData1[,'word']
wordcloudData1 = wordcloudData1[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData1,scale = c(2,0.5),max.words = 100, 
                 rot.per=0,title.size = 1.5,title.bg.colors="white")




#create a new table
sentiment=df_review %>%
  select(review_id,text,date)%>%
  group_by(review_id,date)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()

#join
sentimentable=inner_join(df_review,sentiment,by=c("review_id","date"))

sent=sentimentable%>%
  select(stars,cool,useful,funny,reviewSentiment)

corrplot(cor(sent),method="circle")

prova=sentimentable %>% select(business_id,stars,useful,funny,cool,reviewSentiment,date) %>%
  group_by(business_id,date)%>%
  summarise(avgstars=mean(stars),useful=sum(useful),
            funny=sum(funny),cool=sum(cool), sentiment=mean(reviewSentiment))

#create a new table for checkins
business_data=read.csv("akron_business.csv",header=TRUE, na.strings="NULL")
DailyLevel_data <- read.csv("tips_2018-01-01-2018-11-14-Akron_data.csv",header=TRUE, na.strings="NA") #read csv file. Be aware of the parameters, such as the string for NA's. Usually either "" or "\\NA".
cum_u_names=DailyLevel_data$cum_u_names
Nobs=length(cum_u_names)

load("C:/Users/magom/OneDrive/Desktop/data science for marketing analytics/dsmaproject/nameslist_rev.RData")
DailyLevel_data=cbind(DailyLevel_data,nameslist)  
DailyLevel_data$cum_u_names=NULL

# store the date in date column and remove the old name
DailyLevel_data$date <- as.Date(DailyLevel_data$date_tip) 
DailyLevel_data$date_tip <- NULL

# merge with business data

## now dealing with the missings.
business_data$n_photo[is.na(business_data$n_photo)]=0

business_data1 <- subset(business_data,select = -c(business_id)) # removed this because MICE does not like imputing factors with more than 50 levels

library(mice)

#inspect pattern of missings
md.pattern(business_data1)

#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(business_data1), ncol = ncol(business_data1)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(business_data1)
row.names(predictorMatrix)=colnames(business_data1)
predictorMatrix[c("business_price"),] <- 1 #variables "business_price" can be explained by all other variables
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
business_data1_data_imputed <- mice(business_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
summary(business_data1_data_imputed)
#get one of the complete data sets ( 2nd out of 5)


business_data_complete_data <- complete(business_data1_data_imputed,2)
# bring back the business_id
business_data_complete_data=cbind(business_id=business_data$business_id,business_data_complete_data)

#join the tables
DailyLevel_data=DailyLevel_data%>%
  inner_join(business_data_complete_data,by="business_id")

# make factors out of chr variables
for(j in 1:ncol(DailyLevel_data)){
  if(typeof(DailyLevel_data[,j])=="character")
    DailyLevel_data[,j]=as.factor(DailyLevel_data[,j])
}


# n_photos==NA and cum_max_u_elite==NA are actually zeros, let's replace them with 0 before imputing.

DailyLevel_data$cum_max_u_elite[is.na(DailyLevel_data$cum_max_u_elite)]=0

# some descriptives of the data
library(Hmisc)
describe(DailyLevel_data)

#the complete data sets can be used to estimate your model of choice
#and the results of all 5 models can be combined as in the earlier example
write.csv(DailyLevel_data, file="DailyLevel_data_tip_Imputed.csv")
ml=read.csv("DailyLevel_data_tip_Imputed.csv")
ml$date=as.Date(ml$date)

yelp_data=inner_join(prova,ml,by=c("business_id","date"))


str(yelp_data)

m1=glm(ch_in~cum_n_tips+cum_max_friends+cum_max_us_fans+cum_max_us_tip+I((male+1)/(female+1))+
         business_price+n_photo+avgstars+useful+funny+cool+sentiment, data = yelp_data, family = "binomial")
car::vif(m1)
summary(m1)


library(doParallel)
library(caret)
library(smotefamily)








# predictiv models
# Split randomly
set.seed(66)
yelp_data_na=yelp_data
yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="ch_in") # since the performance evaluations are mainly made
# to check for the minority class - in our case Noch_in


# list of variables in your model
varsin=c("ch_in_string","ch_in","business_price","n_photo","female","male","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip",
         "avgstars","useful","funny","cool","sentiment" )
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/1 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]


BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))


# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))



# class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)
# if yes, maybe you want do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}

# or do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)


############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")


BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))



# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)



makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}



# ----
# the analyses
######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)


############ Naive Bayes

cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")
x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))

# the variable importance
print(varImp(x.modelNB))

# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']

NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")

TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- knn3(BaseFormula_dum, data=x.trainnorm, k=3, prob = FALSE, use.all = TRUE)

x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))


# the variable importance
#print(varImp(x.modelKNN))

# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']

KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")

TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmPoly", cachesize=12000, tolerance=.01,
                    trControl = trainControl(classProbs =  TRUE))

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance
print(varImp(x.modelSVM))

# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


########## Neural network
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


########## TREE

cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)



############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF",mtry=2) 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))
plot(varImp(x.modelRF))
ggplot(varImp(x.modelTree), aes(x = reorder(rownames(importanza_variabili), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Importance of Variables") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


# SOME Summarizing plots:

OverallTDL <- c(NBOutput$TDL,KNNOutput$TDL,LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(NBOutput$GINI,KNNOutput$GINI,LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("NB","KNN","Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift"
        , 
        main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(NBOutput$TimeElapsed,digits=2),"sec"), 
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))
mtext(c(paste(round(NBOutput$PercCorrect,digits=2),"%"),
        paste(round(KNNOutput$PercCorrect,digits=2),"%"),
        paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit+predictionKNN+predictionNB+predictionRF,data=x.evaluate,class="ch_in")

ggplot(lift_obj)








#ML techinques without sentiment


varsin=c("ch_in_string","ch_in","business_price","n_photo","female","male","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip",
         "avgstars","useful","funny","cool")
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/1 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]


BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))


# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))



# class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)
# if yes, maybe you want do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}

# or do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)


############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")


BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))



# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)



makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}



# ----
# the analyses
######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)


############ Naive Bayes

cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")
x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))

# the variable importance
print(varImp(x.modelNB))

# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']

NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")

TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- knn3(BaseFormula_dum, data=x.trainnorm, k=3, prob = FALSE, use.all = TRUE)

x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))


# the variable importance
#print(varImp(x.modelKNN))

# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']

KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")

TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmPoly", cachesize=12000, tolerance=.01,
                    trControl = trainControl(classProbs =  TRUE))

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance
print(varImp(x.modelSVM))

# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


########## Neural network
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


########## TREE

cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)


############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)



############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF",mtry=2) 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))
plot(varImp(x.modelRF))
ggplot(varImp(x.modelTree), aes(x = reorder(rownames(importanza_variabili), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Importance of Variables") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


# SOME Summarizing plots:

OverallTDL <- c(NBOutput$TDL,KNNOutput$TDL,LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(NBOutput$GINI,KNNOutput$GINI,LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("NB","KNN","Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift"
        , 
        main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(NBOutput$TimeElapsed,digits=2),"sec"), 
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))
mtext(c(paste(round(NBOutput$PercCorrect,digits=2),"%"),
        paste(round(KNNOutput$PercCorrect,digits=2),"%"),
        paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit+predictionKNN+predictionNB,data=x.evaluate,class="ch_in")

ggplot(lift_obj)