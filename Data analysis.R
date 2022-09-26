##### APAN 5205 #####
####### Group 10 #####
##### Group project: Yelp #####
##### Section 2. Analysis #####

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#### Part6 : cluster analysis: users
library(dplyr)
library(ggplot2)
library(cluster)

busi_revi_user <- read.csv('busi_revi_user.csv')
head(busi_revi_user)
str(busi_revi_user)
glimpse(busi_revi_user)

##### data preparation ##### 
#transform date, yelp_since from chr to date
busi_revi_user$yelping_since<-as.Date(busi_revi_user$yelping_since,'%Y-%m-%d %H:%M:%S') 
busi_revi_user$date<-as.Date(busi_revi_user$date) 
# calculate and add 'user using days'
busi_revi_user$yelp_days<-as.numeric(Sys.Date()-busi_revi_user$yelping_since)

# remove all unnecessary variables related to users 
user_clst <- busi_revi_user[,c(1,20:42)]
glimpse(user_clst)

# remove duplicate rows by user name: user_id
user_clst1 <- user_clst %>% 
  distinct(user_id, .keep_all = TRUE)

glimpse(user_clst1)


#Clustering algorithms prefer all variables to be of the same class (e.g., numeric, factor).
# Filter out only numeric variables
names(user_clst1)
data_cluster <-user_clst1[,c(4,6:24)]
glimpse(data_cluster)
head(data_cluster)
#scale
data_cluster = scale(data_cluster)
head(data_cluster[,1:20])


##### Apply clustering technique: ##### 
##### Hierarchical Cluster Analysis #####
# d = dist(x = data_cluster,method = 'euclidean') 
# clusters = hclust(d = d,method='ward.D2')
# 
# plot(clusters)
# cor(cophenetic(clusters),d)


##### K-means Cluster ##### 
#explore, set 3 clusters initially
set.seed(617)
km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)
table(km$cluster)

##  Interpret Results: Selecting number of clusters ##
# 1. Total within sum of squares Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# 2. Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# 3. Silhouette Plot
pam(data_cluster,k = 3)$silinfo$avg.width

pam(data_cluster,k = 5)$silinfo$avg.width

silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

# use appropriate cluster number: 4
set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max=10000,nstart=50)

#size of cluster
k_segments = km$cluster
table(k_segments)

##Visualize cluster

#kmeans cluster plots
library(cluster)
clusplot(data_cluster,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')

##Profile Clusters
user_clst2 = cbind(user_clst1, k_segments)


user_clst2 %>%
  select(review_count.y:yelp_days,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(dplyr); library(ggplot2); library(tidyr)
user_clst2 %>%
  select(review_count.y:yelp_days,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,review_count.y:yelp_days)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

##### model based cluster #####
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)


clusters_mclust_3 = Mclust(data_cluster,G=3)
summary(clusters_mclust_3)

clusters_mclust_4 = Mclust(data_cluster,G=4)
summary(clusters_mclust_4)

#Plot of bic (Bayesian Information Criterion)
mclust_bic = sapply(1:10,FUN = function(x) -Mclust(data_cluster,G=x)$bic)
mclust_bic

ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

m_clusters = Mclust(data = data_cluster,G = 4)

m_segments = m_clusters$classification
table(m_segments)


#visualize 
library(psych)
temp = data.frame(cluster = factor(m_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()


clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')

##Visualize cluster

#model based cluster plots
library(cluster)
clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')

##Profile Clusters
user_clst3 = cbind(user_clst1, m_segments)


# visualization of all the variables 
library(dplyr); library(ggplot2); library(tidyr)
user_clst3 %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(dplyr); library(ggplot2); library(tidyr)
user_clst3 %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,review_count.y:yelp_days)%>%
  ggplot(aes(x=var,y=value,fill=factor(m_segments)))+
  geom_col(position='dodge')+
  coord_flip()

# visualization of select variables
avg_comparison = subset(user_clst3,select=-c(user_id,date,name.y,yelping_since))
# Calculate the mean for each category
avg <- avg_comparison %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()
avg
avg %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,review_count.y:yelp_days)%>%
  ggplot(aes(x=var,y=value,fill=factor(m_segments)))+
  geom_col(position='dodge')+
  coord_flip()
# selection of variables that has obvious difference with other clusters 

avg_diff <- avg %>%
  select(m_segments,
         review_count.y,
         elite:average_stars,
         compliment_photos,
         yelp_days)
avg_diff
# visualization of avg_diff
avg_diff %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,review_count.y:yelp_days)%>%
  ggplot(aes(x=var,y=value,fill=factor(m_segments)))+
  geom_col(position='dodge')+
  coord_flip()

#visualization for proportion
install.packages('expss')
library(expss)
avg_diff_prop <- prop_col(avg_diff)


avg_diff_prop %>%
  select(review_count.y:yelp_days,m_segments)%>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,review_count.y:yelp_days)%>%
  ggplot(aes(x=var,y=value,fill=factor(m_segments)))+
  geom_col(position='dodge')+
  coord_flip()

##contrast results of kmeans and model based cluster
table(k_segments)
table(m_segments)


#Combine segment membership with original data
user_clst2 = cbind(user_clst1, k_segments, m_segments)
head(user_clst2)
glimpse(user_clst2)


######################## rating visualization
ggplot(data=user_clst2,mapping = aes(x=average_stars))+
  geom_histogram()

#kmeans 4 segments rating distribution
segment = c('1','2','3','4')

g = lapply(X=1:4,FUN = function(x){
  ggplot(data=user_clst2[user_clst2$k_segments==x,],aes(x=average_stars))+
    geom_histogram()+
    ggtitle(label=paste('k_segment=',x,sep=''))+
    guides(fill=F)+xlab('')+ylab('')})

library(gridExtra)

grid.arrange(grobs = g)

#model based cluster 4 segments rating distribution
segment = c('1','2','3','4')

g = lapply(X=1:4,FUN = function(x){
  ggplot(data=user_clst2[user_clst2$m_segments==x,],aes(x=average_stars))+
    geom_histogram()+
    ggtitle(label=paste('m_segment=',x,sep=''))+
    guides(fill=F)+xlab('')+ylab('')})

grid.arrange(grobs = g)


#density curve
#The issue of unequal sample sizes can be addressed by comparing standardized frequency distributions called density curves.
ggplot(data=user_clst2,aes(x=average_stars,color=factor(k_segments)))+
  geom_density(size=1.2)

ggplot(data=user_clst2,aes(x=average_stars,color=factor(m_segments)))+
  geom_density(size=1.2)
###############################################################################
###############################################################################
###############################################################################
##### Part 7. Prediction: average_stars #####

###############################################################################
##### 7.1 Using K-means #####

##Apply Clustering Solution from Train to Test
#########cluster before predict 
library(caret)

set.seed(617)
split = createDataPartition(y=user_clst1$average_stars,p = 0.8,list = F,groups = 100)
train = user_clst1[split,]
test = user_clst1[-split,]


trainMinusDV = subset(train,select=-c(user_id,date,name.y,yelping_since,average_stars))
testMinusDV = subset(test,select=-c(user_id,date,name.y,yelping_since,average_stars))

glimpse(trainMinusDV)

preproc = preProcess(trainMinusDV)
trainNorm = predict(preproc,trainMinusDV)
testNorm = predict(preproc,testMinusDV)

km = kmeans(x = trainNorm,centers = 4,iter.max=10000,nstart=25)


##Split train and test based on kmeans cluster membership
# install.packages('flexclust')
library(flexclust)
km_kcca = as.kcca(km,trainNorm) # flexclust uses objects of the classes kcca
clusterTrain = predict(km_kcca)
table(clusterTrain)
clusterTest = predict(km_kcca,newdata=testNorm)
table(clusterTest)



#Predict Using Tree

train=subset(train,select=-c(user_id,name.y,yelping_since,date))
test=subset(test,select=-c(user_id,name.y,yelping_since,date))


library(rpart)
library(rpart.plot)
tree = rpart(average_stars~.,train,minbucket=10)
predTree = predict(tree,newdata=test)
sseTree = sum((predTree - test$average_stars)^2); sseTree

#Cluster Then Predict Using Tree


train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
train3 = subset(train,clusterTrain==3)
train4 = subset(train,clusterTrain==4)
test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)
test3 = subset(test,clusterTest==3)
test4 = subset(test,clusterTest==4)

tree1 = rpart(average_stars~.,train1,minbucket=10)
tree2 = rpart(average_stars~.,train2,minbucket=10)
tree3 = rpart(average_stars~.,train3,minbucket=10)
tree4 = rpart(average_stars~.,train4,minbucket=10)
pred1 = predict(tree1,newdata=test1)
pred2 = predict(tree2,newdata=test2)
pred3 = predict(tree3,newdata=test3)
pred4 = predict(tree4,newdata=test4)

sse1 = sum((test1$average_stars-pred1)^2); sse1
sse2 = sum((test2$average_stars-pred2)^2); sse2
sse3 = sum((test3$average_stars-pred3)^2); sse3
sse4 = sum((test4$average_stars-pred4)^2); sse4

predTreeCombine = c(pred1,pred2,pred3,pred4)
average_starsOverall = c(test1$average_stars,test2$average_stars,test3$average_stars,test4$average_stars)
sseTreeCombine = sum((predTreeCombine - average_starsOverall)^2); sseTreeCombine


#Compare Results
paste('SSE for model on entire data',sseTree)
paste('SSE for model on clusters',sseTreeCombine)


###############################################################################
###############################################################################
##### 7.2 Using Model-based clustering with predictive model #####

##Split train and test based on model based cluster membership
m_clusters = Mclust(data = data_cluster,G = 4)
m_segments = m_clusters$classification
table(m_segments)

user_clst3 = cbind(user_clst1, m_segments)
glimpse(user_clst3)
user_clst_m=subset(user_clst3,select=-c(user_id,name.y,yelping_since,date))
glimpse(user_clst_m)

# data partition
set.seed(617)
split = createDataPartition(y=user_clst_m$average_stars,p = 0.8,list = F,groups = 100)
train = user_clst_m[split,]
test = user_clst_m[-split,]

# compare predictive model result between non-cluster data and cluster data

train1 = subset(train,train$m_segments==1)
train2 = subset(train,train$m_segments==2)
train3 = subset(train,train$m_segments==3)
train4 = subset(train,train$m_segments==4)
test1 = subset(test,test$m_segments==1)
test2 = subset(test,test$m_segments==2)
test3 = subset(test,test$m_segments==3)
test4 = subset(test,test$m_segments==4)

#non-cluster data prediction should get rid of segmentation result column
train = train[,-c(21)]
test = test[,-c(21)]

#############################################
## Decision tree
## predict without cluster
library(rpart)
library(rpart.plot)
tree = rpart(average_stars~.,train,minbucket=10)
predTree = predict(tree,newdata=test)
sseTree = sum((predTree - test$average_stars)^2); sseTree

tree_rmse = sqrt(mean((predTree-test$average_stars)^2)); tree_rmse


## Predict after cluster 
tree1 = rpart(average_stars~.,train1,minbucket=10)
tree2 = rpart(average_stars~.,train2,minbucket=10)
tree3 = rpart(average_stars~.,train3,minbucket=10)
tree4 = rpart(average_stars~.,train4,minbucket=10)
pred1 = predict(tree1,newdata=test1)
pred2 = predict(tree2,newdata=test2)
pred3 = predict(tree3,newdata=test3)
pred4 = predict(tree4,newdata=test4)

#sse
sse1 = sum((test1$average_stars-pred1)^2); sse1
sse2 = sum((test2$average_stars-pred2)^2); sse2
sse3 = sum((test3$average_stars-pred3)^2); sse3
sse4 = sum((test4$average_stars-pred4)^2); sse4

#rmse
tree_rmse1 = sqrt(mean((pred1-test1$average_stars)^2)); tree_rmse1
tree_rmse2 = sqrt(mean((pred2-test2$average_stars)^2)); tree_rmse2
tree_rmse3 = sqrt(mean((pred3-test3$average_stars)^2)); tree_rmse3
tree_rmse4 = sqrt(mean((pred4-test4$average_stars)^2)); tree_rmse4

rmsetreecombine=mean(tree_rmse1,tree_rmse2,tree_rmse3,tree_rmse4);rmsetreecombine

predTreeCombine = c(pred1,pred2,pred3,pred4)
average_starsOverall = c(test1$average_stars,test2$average_stars,test3$average_stars,test4$average_stars)
sseTreeCombine = sum((predTreeCombine - average_starsOverall)^2); sseTreeCombine


#Compare Results
paste('SSE for model on entire data',sseTree)
paste('SSE for model on clusters',sseTreeCombine)

paste('RMSE for model on entire data',lm_rmse)
paste('RMSE for model on clusters',rmselmcombine)


#############################################
## Linear model 
## predict without cluster
lm = lm(average_stars~.,train)
lm_pred = predict(lm,newdata=test)
lm_sse = sum((lm_pred - test$average_stars)^2); lm_sse
lm_rmse= sqrt(mean((lm_pred-test$average_stars)^2)); lm_rmse

## Predict after cluster 
lm1 = lm(average_stars~.,train1)
lm2 = lm(average_stars~.,train2)
lm3 = lm(average_stars~.,train3)
lm4 = lm(average_stars~.,train4)
lm_pred1 = predict(lm1,newdata=test1)
lm_pred2 = predict(lm2,newdata=test2)
lm_pred3 = predict(lm3,newdata=test3)
lm_pred4 = predict(lm4,newdata=test4)

lm_sse1 = sum((test1$average_stars-lm_pred1)^2); lm_sse1
lm_sse2 = sum((test2$average_stars-lm_pred2)^2); lm_sse2
lm_sse3 = sum((test3$average_stars-lm_pred3)^2); lm_sse3
lm_sse4 = sum((test4$average_stars-lm_pred4)^2); lm_sse4


# rmse 
lm_rmse1 = sqrt(mean((lm_pred1-test1$average_stars)^2)); lm_rmse1
lm_rmse2 = sqrt(mean((lm_pred2-test2$average_stars)^2)); lm_rmse2
lm_rmse3 = sqrt(mean((lm_pred3-test3$average_stars)^2)); lm_rmse3
lm_rmse4 = sqrt(mean((lm_pred4-test4$average_stars)^2)); lm_rmse4

rmselmcombine=mean(lm_rmse1,lm_rmse2,lm_rmse3,lm_rmse4)

# Combine cluster results
predlmCombine = c(lm_pred1,lm_pred2,lm_pred3,lm_pred4)
lm_avg_starsOverall = c(test1$average_stars,test2$average_stars,test3$average_stars,test4$average_stars)
sselmCombine = sum((predlmCombine - lm_avg_starsOverall)^2); sselmCombine


#Compare Results
paste('SSE for model on entire data',lm_sse)
paste('SSE for model on clusters',sselmCombine)

paste('RMSE for model on entire data',lm_rmse)
paste('RMSE for model on clusters',rmselmcombine)

#############################################
## Random Forest model 
## predict without cluster
library(randomForest)
set.seed(617)
forest = randomForest(average_stars~.,data=train,ntree = 1000)
forest_pred = predict(forest,newdata=test)
sse_forest = sum((forest_pred - test$average_stars)^2); sse_forest
rmse_forest = sqrt(mean((forest_pred-test$average_stars)^2)); rmse_forest

importance(forest)

## Predict after cluster 
forest1 = randomForest(average_stars~.,data=train,ntree = 1000)
forest2 = randomForest(average_stars~.,data=train,ntree = 1000)
forest3 = randomForest(average_stars~.,data=train,ntree = 1000)
forest4 = randomForest(average_stars~.,data=train,ntree = 1000)
forest_pred1 = predict(forest1,newdata=test1)
forest_pred2 = predict(forest2,newdata=test2)
forest_pred3 = predict(forest3,newdata=test3)
forest_pred4 = predict(forest4,newdata=test4)


importance(forest1)
importance(forest2)
importance(forest3)
importance(forest4)
# sse
forest_sse1 = sum((test1$average_stars-forest_pred1)^2); forest_sse1
forest_sse2 = sum((test2$average_stars-forest_pred2)^2); forest_sse2
forest_sse3 = sum((test3$average_stars-forest_pred3)^2); forest_sse3
forest_sse4 = sum((test4$average_stars-forest_pred4)^2); forest_sse4

# rmse 
forest_rmse1 = sqrt(mean((forest_pred1-test1$average_stars)^2)); forest_rmse1
forest_rmse2 = sqrt(mean((forest_pred2-test2$average_stars)^2)); forest_rmse2
forest_rmse3 = sqrt(mean((forest_pred3-test3$average_stars)^2)); forest_rmse3
forest_rmse4 = sqrt(mean((forest_pred4-test4$average_stars)^2)); forest_rmse4

rmse_forest_combine=mean(forest_rmse1,forest_rmse2,forest_rmse3,forest_rmse4)

# Combine cluster results
pred_forest_Combine = c(forest_pred1,forest_pred2,forest_pred3,forest_pred4)
forest_avg_starsOverall = c(test1$average_stars,
                            test2$average_stars,
                            test3$average_stars,
                            test4$average_stars)
sse_forest_Combine = sum((pred_forest_Combine - forest_avg_starsOverall)^2)
sse_forest_Combine

#Compare Results
paste('SSE for model on entire data',sse_forest)
paste('SSE for model on clusters',sse_forest_Combine)

paste('RMSE for model on entire data',rmse_forest)
paste('RMSE for model on clusters',rmse_forest_combine)

###compare model sse based cluster+ three predictive model
df <- data.frame (model  = c("linear", "decision tree", "random forest"),
                  nonclustersse = c(lm_sse, sseTree, sse_forest),
                  clustersse = c(sselmCombine,sseTreeCombine,sse_forest_Combine)
)
df1<-df%>%
  gather("cluster", "sse", 2:3)
library(ggplot2)
df1
ggplot(df1,aes(x=model,y=sse,fill=cluster))+
  geom_bar(stat = 'summary', position='dodge')

###compare model rmse based cluster+ three predictive model
df3 <- data.frame (model  = c("linear", "decision tree", "random forest"),
                  nonclustersse = c(lm_rmse, tree_rmse , rmse_forest),
                  clustersse = c(rmselmcombine,rmsetreecombine,rmse_forest_combine)
)
df4<-df3%>%
  gather("cluster", "rmse", 2:3)
df4
ggplot(df4,aes(x=model,y=rmse,fill=cluster))+
  geom_bar(stat = 'summary', position='dodge')
##################end