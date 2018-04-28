############# ASSIGNMENT: DIGIT RECOGNITION ###############################
# Submitted by: Mandar Jamble [Applicant ID = APFE1785873]

################# ASSUMPTIONS ####################################
# 1. This code is developed & run on Windows 10 platform
# 2. Working directory is correctly set
# 3. All files are present in the working directory
# 4. All file names are maintained as per UpGrad website
# 5. All packages are installed if not explicitly installed in the code

# CRISP-DM framework applied for problem solving

################### Business Objective ##############################
# 1. Hand-written digits are digitized in the form of pixelated data-sets. Dataset taken from MNIST
# 2. Build a SVM model to accurately classify each digit 

install.packages("kernlab")
install.packages("readr")
install.packages("Rtsne")

library(kernlab) # For SVM model building
library(readr)
library(tidyr) # For data manipulation
library(dplyr) # For data manipulation
library(reshape2) # For visualization
library(ggplot2) # For visualization
library(scales) # For visualization
library(grid) # For visualization
library(gridExtra) # For visualization
library(caret) # For model building
library(stringr)# For string manipulation
library(doParallel) # For parallel processing
library(Rtsne) # For dimensionality reduction

################# CRISP-DM:2 Data collection & Understanding #########################
# Load MNIST data
digits_train <- read.csv("mnist_train.csv", header = F, na.strings = c("", "NA"), stringsAsFactors = F)
digits_test <- read.csv("mnist_test.csv", header = F, na.strings = c("","NA"), stringsAsFactors = F)

dim(digits_train) # 60,000 observations with 785 variables
dim(digits_test) # 10,000 observations with 785 variables

anyNA(digits_train) # No NAs
anyNA(digits_test) # No NAs

head(colnames(digits_train)) # Check for column names

# Rename dependent variable
colnames(digits_train)[1] <- "Digit"
colnames(digits_test)[1] <- "Digit"

# Convert dependent variable to factor
digits_train$Digit <- as.factor(digits_train$Digit)
digits_test$Digit <- as.factor(digits_test$Digit)

# Normalize all attributes to ensure there is no bias of any particular attribute
digits_train[2:ncol(digits_train)] <- as.data.frame(sapply(digits_train[2:ncol(digits_train)], function(x) x/255)) # Normalize all attribute vectors with max pixel value (255)
digits_test[2:ncol(digits_test)] <- as.data.frame(sapply(digits_test[2:ncol(digits_test)], function(x) x/255)) # Normalize all attribute vectors with max pixel value (255)

# Scrub train & test datasets for all-zero (no information) columns
digits_all <- rbind(digits_train, digits_test)
digits_all_zero <- as.integer(sapply(digits_all, function(x) sum(x==0)))
length(which(digits_all_zero==70000)) # 65 attributes contain only zeros, i.e no information pertaining to the digit.  
digits_all <- digits_all[,-which(digits_all_zero==70000)] # Removing 65 (no iformation) columns

digits_train_scrubbed <- digits_all[1:60000,]
digits_test_scrubbed <- digits_all[1:10000,]

# Summary
summary(digits_train) 
summary(digits_test) 

# Split the training data into a smaller dataset 
set.seed(10)
indices = sample(1:nrow(digits_train), 0.25*nrow(digits_train))
digits_train_25 = digits_train[indices, ] # 20% of the training dataset to be used for classifcation

# Dimensionality reduction using T-SNE
tsne <- Rtsne(digits_train_25[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
# Data visualization to identify linear or non-linear hyperplane
colors = rainbow(length(unique(digits_train_25$Digit)))
names(colors) = unique(digits_train_25$Digit)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=digits_train_25$Digit, col=colors[digits_train_25$Digit])

# Dimensionality reduction using PCA
pca = princomp(digits_train_25[,-1])$scores[,1:80]
pca_df <- as.data.frame(pca) # Format to data.frame
pca_df$Digit <- digits_train_25$Digit

# Data visualization to identify linear or non-linear hyperplane
plot(pca, t='n', main="pca", "cex.main"=2, "cex.lab"=1.5)
text(pca, labels=digits_train_25$Digit,col=colors[digits_train_25$Digit])

################
#linear_svm <- ksvm(Digit~ ., data = pca_df, scale = FALSE, kernel = "vanilladot")
#linear_svm_predict <- predict(linear_svm, princomp(digits_test[,-1])$scores[,1:80])
#confusionMatrix(linear_svm_predict,digits_test$Digit)
################

# For parallel processing
sys_cores <- detectCores() # Assign system cores for parallel processing
cores_cluster <- makeCluster(sys_cores) # Assign Cluster for doParallel
registerDoParallel(cores_cluster) # Register for parallel processing

# Model building using Linear Kernel
start_time <- Sys.time() # Record start time
# Build Linear model on entire train data set
linear_svm <- ksvm(Digit~ ., data = digits_train, scale = FALSE, kernel = "vanilladot")
stopCluster(cores_cluster) # Release the cluster
run_time <- Sys.time()-start_time # 11.03623 mins
linear_svm_predict <- predict(linear_svm, digits_test) # Predict on entire test data set

# Confusion matrix using Linear Kernel
confusionMatrix(linear_svm_predict,digits_test$Digit)

# Confusion Matrix Results
#            Reference
# Prediction    0    1    2    3    4    5    6    7    8    9
#          0  957    0    8    4    1   10    9    1    8    7
#          1    0 1122    6    3    1    4    2    8    4    7
#          2    4    3  967   16   10    3   13   21    6    2
#          3    1    2   11  947    1   36    1   10   25   11
#          4    1    0    3    1  942    6    5    8    7   33
#          5    6    1    3   16    2  803   16    1   26    4
#          6    9    2    7    0    4   13  910    0    6    0
#          7    1    1    8    9    2    1    1  957    7   18
#          8    0    4   17   12    3   14    1    3  877    5
#          9    1    0    2    2   16    2    0   19    8  922

# Overall Statistics for Train_data
# Accuracy : 0.9404         
# Kappa : 0.9337         

# Statistics by Class:
#                        Class:0  Class:1  Class:2  Class:3  Class:4  Class:5  Class:6  Class:7  Class:8  Class: 9
# Sensitivity            0.9765   0.9885   0.9370   0.9376   0.9593   0.9002   0.9499   0.9309   0.9004   0.9138
# Specificity            0.9947   0.9961   0.9913   0.9891   0.9929   0.9918   0.9955   0.9947   0.9935   0.9944
# Balanced Accuracy      0.9856   0.9923   0.9642   0.9634   0.9761   0.9460   0.9727   0.9628   0.9469   0.9541


# Model building Using RBF Kernel
registerDoParallel(cores_cluster) # Register for parallel processing
start_time <- Sys.time()
# Build RBF model on entire train data set
RBF_svm <- ksvm(Digit~ ., data = digits_train, scale = FALSE, kernel = "rbfdot")
stopCluster(cores_cluster) # Release the cluster
run_time <- Sys.time()-start_time # Run-time = 26.3 mins
RBF_svm_predict <- predict(RBF_svm, digits_test) # Predict on entire test data set

#confusion matrix - RBF Kernel
confusionMatrix(RBF_svm_predict,digits_test$Digit)

# Confusion Matrix Results
#             Reference
# Prediction     0    1    2    3    4    5    6    7    8    9
#           0  973    0    5    0    0    2    6    0    3    4
#           1    0 1126    2    0    0    0    2    8    0    4
#           2    1    3 1005    2    5    0    0   13    2    1
#           3    0    1    2  993    0    9    0    2    5    7
#           4    0    0    1    0  958    1    2    2    4   13
#           5    2    1    0    3    0  868    4    0    3    1
#           6    1    1    2    0    3    5  941    0    2    1
#           7    1    1    9    6    0    1    0  993    3    8
#           8    2    2    5    5    2    4    3    1  949    4
#           9    0    0    1    1   14    2    0    9    3  966

# Overall Statistics
# Accuracy : 0.9772        
# Kappa : 0.9747        

# Statistics by Class:
#                        Class:0  Class:1  Class:2  Class:3  Class:4  Class:5  Class:6  Class:7  Class:8  Class:9
# Sensitivity            0.9929   0.9921   0.9738   0.9832   0.9756   0.9731   0.9823   0.9660   0.9743   0.9574
# Specificity            0.9978   0.9982   0.9970   0.9971   0.9974   0.9985   0.9983   0.9968   0.9969   0.9967
# Balanced Accuracy      0.9953   0.9951   0.9854   0.9901   0.9865   0.9858   0.9903   0.9814   0.9856   0.9770

# RBF kernel based SVM model has a very high overall accuracy (97.7%) as compared linear-kernel SVM model (94%). RBF model is good enough for digit classification
# Evaluation to identify if cross-validation yields better results


############   Hyperparameter tuning and Cross Validation #####################

# Initializing train_control & metric parameters for Train function
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"


#Set the combinations of Sigma & C
set.seed(1)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,5))

cores_cluster <- makeCluster(sys_cores) # Assign Cluster for doParallel
registerDoParallel(cores_cluster) # Register for parallel processing

# Train on 25% of dataset due to high computational time.
start_time <- Sys.time()
fit.svm <- train(Digit~., data=digits_train_25, method="svmRadial", metric=metric, tuneGrid=grid, trControl=trainControl)
stopCluster(cores_cluster) # Release the cluster
run_time <- Sys.time()-start_time # Runtime = 4.6 hours

print(fit.svm) # Max accuracy = 97.3% for sigma=0.025 & C=5

# Resampling results across tuning parameters:
  
#   sigma  C    Accuracy   Kappa    
#   0.025  0.1  0.9393327  0.9325763
#   0.025  0.5  0.9642664  0.9602857
#   0.025  1.0  0.9701333  0.9668059
#   0.025  5.0  0.9731333  0.9701399 - Best combination
#   0.050  0.1  0.9002010  0.8890992
#   0.050  0.5  0.9598001  0.9553244
#   0.050  1.0  0.9663999  0.9626578
#   0.050  5.0  0.9679998  0.9644357

# Since fit.svm max accuracy of 25% of dataset is almost the same as rbf_svm, there is no need to re-run ksvm with kernerl=rbf.
# Else, ksvm with kernerl=rbf should be re-run with the best combination of sigma (0.025) & C (5) to build the best model 

plot(fit.svm)
