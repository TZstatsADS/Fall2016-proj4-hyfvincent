library(rhdf5)
library(lda)
library("topicmodels")
library("ggplot2")
library(randomForest)

setwd("/Users/yifeihu/Documents/semester_3/applied_data_science/project_4")
load("list_data.Rdata")
load("/Users/yifeihu/Documents/semester_3/applied_data_science/project_4/Project4_data/lyr.Rdata")
lyr1<-lyr[,-c(2,3,6:30)]
get_features<-function(x){
  v<-vector()
  #feature 1: number of bars
  length1<-length(x$bars_start)
  v[1]<-length1
  #feature 2: ave length of one bar
  #feature 3: var length of one bar
  if (length1>1){
    diff1<-diff(x$bars_start)
    v[2]<-mean(diff1)
    v[3]<-var(diff1)
  }else if(length1==1){
    v[2]=x$bars_start[1]
    v[3]=0
  }else
  {v[2]<-NA
  v[3]<-NA}
  #feature 4: number of beats
  length2<-length(x$beats_start)
  v[4]<-length2
  if (length2>1){
    diff2<-diff(x$beats_start)
    #feature 5: ave of length of beats
    v[5]<-mean(diff2)
    #feature 6: var of length of beats
    v[6]<-var(diff2)
  }
  else if(length2==1){
    v[5]=x$beats_start[1]
    v[6]=0
  }
  else
  {v[5]<-NA
  v[6]<-NA}
  #feature 7: number of sections
  length3<-length(x$sections_start)
  v[7]<-length3
  if (length3>1){
    diff3<-diff(x$sections_start)
    #feature 8: ave of length of sections
    v[8]<-mean(diff3)
    #feature 9: var of length of sections
    v[9]<-var(diff3)
  }
  else if(length3==1){
    v[8]=x$sections_start[1]
    v[9]=0
  }
  else
  {v[8]<-NA
  v[9]<-NA}
  #segments loudness_max
  length4<-length(x$segments_loudness_max)
  if (length4>1){
    #feature 10: mean of loudness_max
    v[10]<-mean(x$segments_loudness_max)
    #feature 9: var of loudness_max
    v[11]<-var(x$segments_loudness_max)
  }
  else if(length4==1){
    v[10]=x$segments_loudness_max[1]
    v[11]=0
  }
  else
  {v[10]<-NA
  v[11]<-NA}
  #segments_loudness_max_time
  length5<-length(x$segments_loudness_max_time)
  if (length5>1){
    #feature 12: mean of loudness_max_time
    v[12]<-mean(x$segments_loudness_max_time)
    #feature 13: var of loudness_max_time
    v[13]<-var(x$segments_loudness_max_time)
  }
  else if(length5==1){
    v[12]=x$segments_loudness_max_time[1]
    v[13]=0
  }
  else
  {v[12]<-NA
  v[13]<-NA}
  #segments_loudness_start
  length6<-length(x$segments_loudness_start)
  if (length6>1){
    #feature 14: mean of segments loudness start
    v[14]<-mean(x$segments_loudness_start)
    #feature 15: var of segments loudness start
    v[15]<-var(x$segments_loudness_start)
  }
  else if(length6==1){
    v[14]=x$segments_loudness_start[1]
    v[15]=0
  }
  else
  {v[14]<-NA
  v[15]<-NA}
  #segments_pitches
  ncol7<-ncol(x$segments_pitches)
  #feature 16--27: mean of the 12 pitches each in segments
  #feature 28--39: var of the 12 pitches each in segments
  if (ncol7>1){
    for(i in 1:12){
      v[15+i]<-mean(x$segments_pitches[i,])
      v[27+i]<-var(x$segments_pitches[i,])
    }
  }
  else if (ncol7==1){
    for(i in 1:12){
      v[15+i]<-x$segments_pitches[i,1]
      v[27+i]<-0
    }
  }
  else
  {
    for(i in 1:12){
      v[15+i]<-NA
      v[27+i]<-NA
    }
  }
  #segments start
  #feature 40: number of segments
  length8<-length(x$segments_start)
  v[40]<-length8
  #feature 41: ave length of segments
  #feature 42: var length of segments
  if (length8>1){
    diff8<-diff(x$segments_start)
    v[41]<-mean(diff8)
    v[42]<-var(diff8)
  }
  else if(length8==1){
    v[41]=x$segments_start[1]
    v[42]=0
  }
  else
  {v[41]<-NA
  v[42]<-NA}
  #segments_timbre
  ncol9<-ncol(x$segments_timbre)
  #feature 43--54: mean of the 12 timbre each in segments
  #feature 55--66: var of the 12 timbre each in segments
  if (ncol9>1){
    for(i in 1:12){
      v[42+i]<-mean(x$segments_timbre[i,])
      v[54+i]<-var(x$segments_timbre[i,])
    }
  }
  else if (ncol9==1){
    for(i in 1:12){
      v[42+i]<-x$segments_timbre[i,1]
      v[54+i]<-0
    }
  }
  else
  {
    for(i in 1:12){
      v[42+i]<-NA
      v[54+i]<-NA
    }
  }
  #feature 67: number of tatums
  length10<-length(x$tatums_start)
  v[67]<-length10
  #feature 68: ave length of tatums
  #feature 69: var length of tatums
  if (length10>1){
    diff10<-diff(x$tatums_start)
    v[68]<-mean(diff10)
    v[69]<-var(diff10)
  }
  else if(length10==1){
    v[68]=x$tatums_start[1]
    v[69]=0
  }
  else
  {v[68]<-NA
  v[69]<-NA}
  return(v)
}

num_train_set<-length(list_data)
feature_matrix<-matrix(nrow=num_train_set,ncol=69)
for(i in 1:num_train_set){
  feature_matrix[i,]<-t(get_features(list_data[[i]]))
}
feature_matrix <- data.frame(feature_matrix)
formulas <- paste("label ~ X1")

for (i in c(2:69)){
  formulas <- paste(formulas,"+", colnames(feature_matrix)[i], sep = " ")
}
lyr2 <- lyr1[,-1]


crossvalidation <- function(lyr1, feature_matrix,fold,begin, end, stepwid){
  index1 <- seq(begin, end, stepwid)
  rankrate <- array(NA, dim = c(length(index1),1))
  num <- length(feature_matrix[,1]) / fold
  fold_index <- sample(length(feature_matrix[,1]))            
  for (i in c(1:length(index1))){
    summation <- array(0,dim = c(1,1))
    print(index1[i])
    #LDA with index1[i] topics

    #
    index2 <- 4000
    for (k in c(1:1)){
      for (j in c(1:fold)){
          #split the data
          test_index <- fold_index[((j - 1)*num +1): (j*num)]
          test <- feature_matrix[test_index,]
          test <- na.roughfix(test)
          lda_class<-LDA(lyr1[-test_index,-1], index1[i], method = "Gibbs")
          label<- as.factor(apply(lda_class@gamma,1,function(x) which.max(x)))
          features <- data.frame(cbind(feature_matrix[-test_index,],label))
          features_nafix <- na.roughfix(features)
          features_nafix$label <- as.factor(features_nafix$label)
          train <- features_nafix
          
          #multinomial data
          multi_logi <- multinom(as.formula(formulas), data=train, MaxNWts=index2)
          yhat<-predict(multi_logi, newdata = test, "probs")
          #evaluation
          evaluation <-vector()
          pro_multi <- log(yhat %*% exp(lda_class@beta))
          for (t in 1:length(test_index)){
            pro <- -pro_multi[t,]
            r <- rank(pro)
            r_bar <- mean(r)
            l <- lyr2[test_index[t],]
            m <- sum(l > 0)
            sum_rank <- sum(r[which(l>0)])
            evaluation[t] <- 1 / m * 1/ r_bar * sum_rank
          }#for t
        summation <- summation+ mean(evaluation)
      }#for j
    }#for k
    
    rankrate[i] <- summation / fold
    print(rankrate)
  }#for i 
  return(rankrate)
}#funcion

errorrate <- crossvalidation(lyr1, feature_matrix, 5, 5,20, 5)
