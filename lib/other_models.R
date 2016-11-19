#svm
svmfit=svm(as.formula(formulas_gbm), data=as.data.frame(features_gbm[1:2000,]), kernel="radial", cost=10, gamma=1)
tune.out=tune(svm, as.formula(formulas_gbm), data =features_nafix_gbm[1:2000,], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4) ))


#random forest
feature_forest <- randomForest(as.formula(formulas_gbm), data = features_nafix_gbm[1:2000,], ntree = 500)