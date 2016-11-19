#!!!test part
#read test data h5 file
t1 <- Sys.time()
test_files <- dir('TestSongFile100', recursive = T, full.names = T, pattern = '\\.h5$')
test_data<-vector("list",length=length(test_files))
for (i in 1:length(test_files)){
  test_data[[i]]<-h5read(file=test_files[i],"/analysis")
}
#get the test_data feature
num_test_set<-length( test_data)
test_feature<-matrix(nrow=num_test_set,ncol=69)
for(i in 1:num_test_set){
  test_feature[i,]<-t(get_features(list_data[[i]]))
}

#fix NA
test_feature_nafix <- na.roughfix(test_feature)
#get the output of multinomial logistic regression
yhat_m_l<-predict(multi_logi, newdata = test_feature_nafix, "probs")

#get the probabilities of each word in the vocabulary appearing in the song
pro_multi<-yhat_m_l %*% exp(lda_class@beta)

#combine the columns 2,3,6:30 back give them 0 probability
colnames(pro_multi) <- colnames(lyr1[,-1])
test_lyr <- matrix(0,nrow = length(test_data), ncol = length(lyr[1,-1]))

test_lyr <- data.frame(test_lyr)
colnames(test_lyr) <- colnames(lyr[,-1])
test_lyr[colnames(pro_multi)] <- pro_multi 

#get ranks of the words
rank_test <- t(array(unlist(apply(-test_lyr, 1,rank)), dim = c((length(test_lyr[1,])),length(test_data))))
rank_test <- data.frame(rank_test)
colnames(rank_test) <- colnames(test_lyr)
result <- cbind(0, rank_test)
colnames(result) <- c("dat2$track_id", colnames(test_lyr))
for (i in c(1: length(test_data))){
  result[i,1] = paste("testsong", i, sep = "")
}
#the result
write.csv(result, file = "yh2781_result.csv")
t2 <- Sys.time()