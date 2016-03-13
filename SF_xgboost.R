#################load required package

suppressPackageStartupMessages(require("dplyr"))
suppressPackageStartupMessages(require("readr"))
suppressPackageStartupMessages(require("reshape2"))
suppressPackageStartupMessages(require("lubridate"))
suppressPackageStartupMessages(require('ROCR'))
suppressPackageStartupMessages(require('caret'))
suppressPackageStartupMessages(require('nnet'))
suppressPackageStartupMessages(require('Rtsne'))
suppressPackageStartupMessages(require('xgboost'))
suppressPackageStartupMessages(require('corrplot'))
currentDate = Sys.Date()



#########Set the file dir

#set input to require directory
setwd("E:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))



##read file
train = read_csv("train.csv") %>% select( Dates, Category, DayOfWeek, PdDistrict, X, Y ) %>% sample_n(100000)

##lets get levels of category
y.lvl = as.factor(train$Category)
num.class = length(unique((y.lvl)))
rcolnames = as.character(sort(unique(y.lvl)))
y = as.numeric(y.lvl)


test = read_csv("test.csv") %>% select(Id, Dates,  DayOfWeek, PdDistrict, X, Y ) 
train$Id = 123456789
test$Category = "dummy"
train = rbind(train, test)

##lets add month
train$month = month(train$Dates)
train$weeknum = week(train$Dates)
train$years = year(train$Dates)

##lets convert cretain coloum to factor
train$Category = as.factor(train$Category)
train$PdDistrict = as.factor(train$PdDistrict)
train$DayOfWeek = as.factor(train$DayOfWeek)

##lets do pca in X and Y
pca = preProcess(train[, c("X", "Y")],  method=c( "center",  "scale", "pca"))
pc = predict(pca, train[, c("X", "Y")])

##lets replace x and y with pca
train$X = pc$PC1
train$Y = pc$PC2

##lets convvert to factor
train2 = transform(train,
                   DayOfWeek = as.numeric(as.factor(DayOfWeek)),
                   PdDistrict = as.numeric(as.factor(PdDistrict)),
                   month = as.numeric(as.factor(month)),
                   weeknum = as.numeric(as.factor(weeknum)),
                   years = as.numeric(as.factor(years)))

##lets sperate data into test and train
test = train2 %>% filter(Category=="dummy")
train = train2 %>% filter(Id==123456789)

## do some anlysis on train
train = train %>% select(-Id, -Dates, -Category)
test = test %>% select( -Dates, -Category)



##correlation plot
corrplot.mixed(cor(train), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")



##lets prepare data 
# convert data to matrix
train.matrix = as.matrix(train)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(test)
mode(test.matrix) = "numeric"
# convert outcome from factor to numeric matrix 
#   xgboost takes multi-labels in [0, numOfClass)
y = as.matrix(y-1)


# xgboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = num.class,    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 6,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 1 
              # minimum sum of instance weight needed in a child 
)

# set random seed, for reproducibility 
set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 100
system.time( bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                              nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=1) )

tail(bst.cv$dt) 


# index of minimum merror
min.merror.idx = which.min(bst.cv$dt[, test.merror.mean]) 
min.merror.idx 


# minimum merror
bst.cv$dt[min.merror.idx,]

# get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv = max.col(pred.cv, "last")


# confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))



# real model fit training, with full data
system.time( bst <- xgboost(param=param, data=train.matrix, label=y, 
                            nrounds=min.merror.idx, verbose=0) )

# get the trained model
model = xgb.dump(bst, with.stats=TRUE)
# get the feature real names
names = dimnames(train.matrix)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp)


# xgboost predict test data using the trained model
pred <- predict(bst, test.matrix)  
head(pred, 10) 


# decode prediction
pred <- sprintf('%f',pred)
pred <- t(matrix(pred, nrow=num.class))
pred2 <- cbind(test[, "Id"],pred)
dim(pred)

colnames(pred2) <- c('Id', rcolnames)
#names(prediction)
write.csv(pred, 'submission.csv', row.names=FALSE, quote=FALSE)
zip('submission.zip', 'submission.csv')

bob = read.csv("submission.csv")




############################################################Bikram Dahal########################################
