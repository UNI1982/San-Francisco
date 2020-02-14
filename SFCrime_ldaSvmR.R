library(tidyverse)
library(caret)
library(lubridate)
library(kableExtra)
library(data.table)
library(e1071)
library(RColorBrewer)
library(gplots)
library(corrplot)
library(here)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")   
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gplots)) install.packages("gplots", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

################################
# Data Sets acquisition
################################
 # San Francisco datasets:
 # https://www.kaggle.com/c/sf-crime/download/RiTVAa9kf1hu9l7TmtUX%2Fversions%2FNRHocVFvjrC3Q7lrLMcf%2Ffiles%2Ftest.csv.zip
 # https://www.kaggle.com/c/sf-crime/download/RiTVAa9kf1hu9l7TmtUX%2Fversions%2FNRHocVFvjrC3Q7lrLMcf%2Ffiles%2Ftrain.csv.zip
# kaggle requires to login to download the files
 


# Getting data and settip unp workspace using here(). Files were download en data folder, and Rproject will run in any system and computer
 
here()
getwd()
test <- read.csv(here("data","SanFrancisco_test.csv"), stringsAsFactors = FALSE)
train<- read.csv(here("data","SanFrancisco_train.csv"),stringsAsFactors = FALSE) 
str(test)
str(train)
dim(train)
dim(test)

# str and im functions help to know and check the data loaded to have an idea 
# how to set it up for the future model.
# Spliting the train set into subsets for cross validation of the model
#  Checking if both datasets are unique
main_data <- full_join(train,test, by = NULL, type = "full", match = "all")
dim(main_data)
# Successfully jointed, the dimension of the main_data is the  sum of 
# dim(train) + dim(test) 
 
# Spliting the train set into subsets for cross validation of the model
# Set.seed(1) for R version before the 3.6 version
set.seed(1, sample.kind = "Rounding")
sampling_Trainset <- createDataPartition(y = main_data$Category, p = 0.20, list = FALSE)
# creating the data sets shows TREA , which is denominated as Traspassing in the
# FBI crime definition. This is a unique data that will no affect our analysis,
#and it is included.  
training <- main_data[-sampling_Trainset, ]
testing <- main_data[sampling_Trainset,]
summary(training)
# The Id column has NAs that weres added in the Join process , we do not need them

training$Id <- NULL
testing$Id <- NULL

# Category as a factor to be able to validate the model
 
training$Category <-  as.factor(training$Category)
# Factors to be predicted 
levels(training$Category)
# Checking any NA on data sets
anyNA(training) 

# There are 52427 NAs in Category, meaning there is no records on type of crime,
# so these data are not providing any information, so they can be eliminated

training <- training%>%filter(Category !="NA.")%>% droplevels()
levels(training$Category)
anyNA(training) 
summary(training)
# Now training is cleaned

# Cleaning testing data set
testing$Category <-  as.factor(testing$Category)
# Factors to be predicted 
levels(testing$Category)
# Checking any NA on data sets
anyNA(testing) 

# There are 52427 NAs in Category, meaning there is no records on type of crime,
# so these data are not providing any information, so they can be eliminated

testing <- testing%>%filter(Category !="NA.")%>% droplevels()
levels(testing$Category)
anyNA(testing) 
summary(testing)

# Data sets are cleaned and ready to be analysed


############ Data Visualization########################
 
 
Crimes_freq <- training %>% group_by(Category)%>%summarise(Type_crimes = n())%>%arrange(desc(Type_crimes))%>% ungroup()

# Using http://r-statistics.co/ggplot2-Tutorial-With-R.html as refrence for plotting:
Crimes_freq%>% ggplot(aes(reorder(Category,Type_crimes), y = Type_crimes, fill =Type_crimes)) + geom_col() + 
  coord_flip() +theme(legend.position = "top") +labs( x = 'Type of Crimes', title = 'Frequency of Crimes in San Francisco')
nrow(Crimes_freq)

# 36 crimes that our graphic shows larceny, Assault , Vehiclue theft as the more 
# frequent type of crimea in all the districts.

### Setting up database

#LOCATION:
# Longitud lattidue are in spherical coordinates, which have degrees as scale.
# Therefore, those variable need to be in cartesian coordenates to avoid degrees,
# and standarized the data sets.

training$Location_x <- cos(training$Y)*cos(training$X)
training$Location_y <- cos(training$Y)* sin(training$X)
training$Location_z <- sin(training$Y)

# TIME

# The dates as POSIXct and characters as factors.
training$Dates <- ymd_hms(training$Dates)
training$Years <- year(training$Dates)
training$Months <- month(training$Dates)
training$Days <- day(training$Dates)
training$Hours <- hour(training$Dates)
training$PdDistrict <- as.factor(training$PdDistrict)
training$DayOfWeek <- wday(training$Dates)

# Time data is cyclical, so it is more accurate to use radial time approach to set up time data
training$Years_sin <- sin(2*pi*training$Years/365)
training$Years_cos <- cos(2*pi*training$Years/365) 
# For cyclical months
training$Months_sin <- sin(2*pi*training$Months/12) 
training$Months_cos <- cos(2*pi*training$Months/12)

# For cyclical hours and days
training$Days_sin <-  sin(2*pi*training$Days/30) 
training$Days_cos <- cos(2*pi*training$Days/30)
training$Hours_sin <- sin(2*pi*training$Hours/24) 
training$Hours_cos <- cos(2*pi*training$Hours/24) 


# The test-set must have in same paramenters to have a correct validation.

testing$Dates <- ymd_hms(testing$Dates)
testing$Years <- year(testing$Dates)
testing$Months <- month(testing$Dates)
testing$Days <- day(testing$Dates)
testing$Hours <- hour(testing$Dates)
testing$PdDistrict <- as.factor(testing$PdDistrict)
testing$DayOfWeek <- wday(testing$Dates) 
testing$Years_sin <- sin(2*pi*testing$Years/365)
testing$Years_cos <- cos(2*pi*testing$Years/365) 
testing$Months_sin <- sin(2*pi*testing$Months/12) 
testing$Months_cos <- cos(2*pi*testing$Months/12)
testing$Days_sin <-  sin(2*pi*testing$Days/30) 
testing$Days_cos <- cos(2*pi*testing$Days/30)
testing$Hours_sin <- sin(2*pi*testing$Hours/24) 
testing$Hours_cos <- cos(2*pi*testing$Hours/24) 
testing$Location_x <- cos(testing$Y)*cos(testing$X)
testing$Location_y <- cos(testing$Y)* sin(testing$X)
testing$Location_z <- sin(testing$Y) 

 
# ckeking if both data sets have the same columns
 
ifelse(all(sort(names(training)) %in% sort(names(testing))),"Identical data sets", "No ready")
ifelse(all(sapply(training, class) %in% sapply(testing, class)), "Same Classes", "No ready")

# The datasets can be leaner. Address is not required because longitude and latidue are provided.
# descript and resolution are not part of the hypothesis.
# Elimating data that were transformed
 
training[c("Descript","Resolution", "Address","DayOfWeek","X", "Y", "Years","Days","Hours")] <- list(NULL) 
testing[c("Descript","Resolution", "Address","DayOfWeek","X", "Y", "Years","Days","Hours")] <- list(NULL) 

# Ensuring no NAs and normalization
summary(training)
summary(testing)
 
# The datasets are normalized and ready to be used because
# The minimum and maximum do not have a big gaps.

#### CHECKING NORMALIZATION ######

# Checking if the data for time is cyclical
plot.data <- training
plot(plot.data$Months_sin,training$Months_cos, main="Month_sin as Cyclical Data")
 
# Checking data normalization
boxplot(plot.data[ , c(4:6,8:15)], main="Distribution of Normalized Data")
 
# Graphics show a range of -1.0 to 1.0. Data is normalized and ready to be used.

###### Creating Correlation and Heatmaps of Crimes per Dictrict #######

# Guide gplots heatmap.2() features:  https://cran.r-project.org/web/packages/gplots/gplots.pdf. page26 and 31
 
CategoryPdDistrict_data <- training %>% group_by(Category, PdDistrict)%>% summarise(District_crimes = n())
CaPD <- CategoryPdDistrict_data %>% group_by_at(vars(-District_crimes)) %>% mutate(row_id=1:n()) %>% ungroup() %>%  
        spread(key=PdDistrict, value=District_crimes) %>% select(-row_id) 
head(CaPD)
# There are NAs over the districts. These NAs is because is very low crime or 
# no data were collected. So, it necessary to give a zero value instead
CaPD[is.na(CaPD)] <- 0

# CaPD needs to be a data frame to avoid the warning Tibble depreceated
CaPD <- as.data.frame(CaPD)
# Preparing the matrix  
row.names(CaPD) <- CaPD$Category

Matrix1_CaPd <-data.matrix(CaPD[,-1])
Matrix_CaPd <- Matrix1_CaPd[,-1]
head(Matrix_CaPd)
# Checking the correlation between variables to ensure that
# the hypothesis is on the correct direction.
m_cor <- cor(Matrix_CaPd)
corrplot(m_cor, type = "upper",mar=c(0,0,1,0), main="Correlation of Crimes per District")
# The minimum correlation is 0.73, which corroborate how  the Type of crime depend on the district.

# The heat will tell the district with high and low crime nd 
coul2 <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
CrimeDistrict_heatmap3 <- heatmap.2(Matrix_CaPd,                     
                          Colv=FALSE,     
                          srtCol=45,
                          Rowv=FALSE,     
                          dendrogram="none",
                          density.info="histogram",    
                          trace="none",              
                          col = coul2,          
                          cexRow=0.85,cexCol=0.75,main = " Crimes per District")

##### ** Heatmaps Type of Crimes Monthly #################
# Now correlation of category with Time. Months was choosen for simplicity, but it coul be Hours, Years,
CategoryTime_data <- training %>% group_by(Category, Months)%>% summarise(Monthly_crimes = n())
 
CaMonths <- CategoryTime_data %>% group_by_at(vars(-Monthly_crimes)) %>% mutate(row_id=1:n()) %>% ungroup() %>%  
  spread(key=Months, value=Monthly_crimes) %>%     
  select(-row_id) 
CaMonths[is.na(CaMonths)] <- 0
CaMonths <- as.data.frame(CaMonths) 
row.names(CaMonths) <- CaMonths$Category
Matrix1_CaMonths <- data.matrix(CaMonths)
Matrix_CaMonths <- Matrix1_CaMonths[,-1]
head(Matrix_CaMonths)

coul3 <- colorRampPalette(brewer.pal(8, "RdBu"))(25)
heatmap_CategoryPdDistrict <- heatmap.2(Matrix_CaMonths,   
                                        Colv=FALSE,      
                                        srtCol=45,
                                        Rowv=FALSE,      
                                        dendrogram="none",
                                        density.info="histogram",    
                                        trace="none",               
                                        col = coul3,            
                                        cexRow=0.85,cexCol=0.75, xlab = "Months",   main = " Monthly Crimes ")

##### Heatmaps " Monthly Crimes per District" #############
# Now correlation of PdDistrict with Time. The correlation of the district with time and category with time
# allow to understand the intrisic correlation of category with time and districts.
PdDistricTime_data <- training %>% group_by(PdDistrict, Months)%>% summarise(Monthly_crimes = n())
 
PdMonths <- PdDistricTime_data %>% group_by_at(vars(-Monthly_crimes)) %>% mutate(row_id=1:n()) %>% ungroup() %>%  
             spread(key=Months, value=Monthly_crimes) %>%     
             select(-row_id) 
PdMonths[is.na(PdMonths)] <- 0
PdMonths <- as.data.frame(PdMonths) 
row.names(PdMonths) <- PdMonths$PdDistrict
Matrix1_PdMonths <- data.matrix(PdMonths)
Matrix_PdMonths<- Matrix1_PdMonths[,-1]
head(Matrix_PdMonths)
coul4 <- colorRampPalette(brewer.pal(8, "Reds"))(25)
heatmap_CategoryPdDistrict <- heatmap.2(Matrix_PdMonths,                      
                                        Colv=FALSE,      
                                        Rowv=FALSE,      
                                        srtCol=45,
                                        dendrogram="none",
                                        density.info="histogram",    
                                        trace="none",                
                                        col = coul4,           
                                        cexRow=0.85,cexCol=0.75,xlab = "Months",main = " Monthly Crimes per District")

# The heatmaps are telling that the type ofcrimes depends directly on 
# the Districts and on the time of execution.


# the data required to be segmented over the crime type. Therefore , the FBI crime characterization will
# give the correct segemnation of crimes. 

# Grouping according with FBI codes: https://ucr.fbi.gov/nibrs/2011/resources/nibrs-offense-codes/view
FBI_groupA <- c("ARSON","ASSAULT", "BRIBERY", "BURGLARY","FRAUD","DRUG.NARCOTIC","EMBEZZLEMENT","EXTORTION","SECONDARY.CODES",
                "FRAUD","FORGERY.COUNTERFEITING","GAMBLING","KIDNAPPING","LARCENY.THEFT","MISSING.PERSON","PROSTITUTION","ROBBERY","VANDALISM",
                "SEX.OFFENSES.FORCIBLE","SEX.OFFENSES.NON.FORCIBLE","STOLEN.PROPERTY", "VEHICLE.THEFT","WEAPON.LAWS"  )
FBI_groupB <- c("BAD.CHECKS","DISORDERLY.CONDUCT","DRIVING.UNDER.THE.INFLUENCE","DRUNKENNESS","FAMILY.OFFENSES","LIQUOR.LAWS",
                "LOITERING","NON.CRIMINAL","OTHER.OFFENSES","RUNAWAY","SUICIDE","SUSPICIOUS.OCC","TRESPASS","WARRANTS")
FBI_violent <- c("ASSAULT","DRUG.NARCOTIC","KIDNAPPING","ROBBERY","DRUG.NARCOTIC") 
FBI_property <- c(FBI_groupB,FBI_groupA[!FBI_groupA %in% c("ASSAULT","DRUG.NARCOTIC","KIDNAPPING","ROBBERY","DRUG.NARCOTIC")])

# Bucketing Category , crime types to be more specific in the prediction 
Crime_groupA <-   training %>% filter(Category %in% FBI_groupA)%>%droplevels()
Crime_groupA_test <-   testing %>% filter(Category %in% FBI_groupA)%>%droplevels()

Crime_groupB <-   training %>% filter(Category %in% FBI_groupB)%>%droplevels()
Crime_groupB_test <- testing %>% filter(Category %in% FBI_groupB)%>%droplevels()

Crime_violent <-   training %>% filter(Category %in% FBI_violent)%>%droplevels()
Crime_violent_test <-   testing %>% filter(Category %in% FBI_violent)%>%droplevels()

Crime_property<-   training %>% filter(Category %in% FBI_property)%>%droplevels()
Crime_property_test <- testing %>% filter(Category %in% FBI_property)%>%droplevels()





##################### Hypothesis #############################################



# Hypothesis:Crimes (Category) depends on the week + hour +month + 
#                                            year + location (X+Y)
# Let's start looking for the best model to use. LDA is apply to  the whole
# data set to demostrate that it is require to split the Category variable into
# small fractions

Hypothesis <- Category ~ Years_sin + Years_cos + Months_sin+ Months_cos + 
              Hours_sin+ Hours_cos+ Days_sin+ Days_cos+ Location_x+ 
              Location_y+ Location_z

##################### LDA Modelling #############################################

######## Entire San Francisco City ##############

# First Using the entire datasets

model.lda.SF<- train(Hypothesis, method = "lda", data = training)

# Cross-validation 
predictionSF <- predict(model.lda.SF, newdata = testing)
prediction_ldaSF <- factor(predictionSF, levels = levels(testing$Category))

# Accuracy
Accuracy_ldaSF <- confusionMatrix(prediction_ldaSF, testing$Category)$
                  overall["Accuracy"]

# Creating the table that will store all the Accuracies results to compare results
Accuracy_results.SF <- data_frame(method = " LDA on San Francisco City", 
                                Accuracy = Accuracy_ldaSF)

Accuracy_results.SF
# accuracy is 14% 


######## Using Violent data #################

# LDA over Violent crimes , which is the smallest group of crimes

model.lda.Violent <- train(Hypothesis, method = "lda", data = Crime_violent)

# Cross-validation 
predictionViolent <- predict(model.lda.Violent , newdata = Crime_violent_test)
prediction_ldaViolent <- factor(predictionViolent, levels = levels(Crime_violent_test$Category))

# Accuracy
Accuracy_ldaViolent <- confusionMatrix(prediction_ldaViolent, Crime_violent_test$Category)$overall["Accuracy"]

Accuracy_results.SF <- bind_rows(Accuracy_results.SF,
                          data_frame(method=" LDA on Violent Crimes in SF city",
                                     Accuracy = Accuracy_ldaViolent ))
Accuracy_results.SF

######### Using High crimes rates on  San Francisco's districts######### 

# The highest accurary is predicting crimes over specific Districts 

Crimes_district <-  Crime_violent%>% filter(PdDistrict %in% c("BAYVIEW","SOUTHERN"))%>%droplevels()

Crimes_district_test <- Crime_violent_test %>% filter(PdDistrict %in% c("BAYVIEW","SOUTHERN"))%>%droplevels()

model.lda.District <- train(Hypothesis, method = "lda", data = Crimes_district)

# Cross-validation 
predictionDistrict<- predict(model.lda.District, newdata = Crimes_district_test)
prediction_ldaDistrict <- factor(predictionDistrict, levels = levels(Crimes_district_test$Category))

# Accuracy
Accuracy_ldaDistrict <- confusionMatrix(prediction_ldaDistrict, Crimes_district_test$Category)$overall["Accuracy"]
 
Accuracy_results.SF.Districts <- data_frame(method=" LDA on Violent Crime per District",
                                         Accuracy = Accuracy_ldaDistrict )
Accuracy_results.SF.Districts

##################### SVM Modeling ##############################################


# Svm method tuning Group A
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_model <- svm(Hypothesis, data = Crimes_district, method ="radial", trControl = trctrl, preProcess = c("center", "scale"),tuneGrid = grid,tuneLength=10)
 
# better results gives cost= 1
svm_model <- svm(Hypothesis, data = Crimes_district, method ="radial", trControl = trctrl, preProcess = c("center", "scale"),cost= 1,tuneGrid = grid,tuneLength=10)

# Cross validation svm_model2
svm_prediction <- predict(svm_model, newdata = Crimes_district_test)
svm_prediction.District <- factor(svm_prediction, levels = levels(Crimes_district_test$Category))

Accuracy_svm <- confusionMatrix(data = svm_prediction.District, reference = Crimes_district_test$Category)$overall["Accuracy"]

Accuracy_results.SF.Districts <- bind_rows(Accuracy_results.SF.Districts,
                              data_frame(method=" SVM on Violent Crimes per District",
                                         Accuracy = Accuracy_svm ))
Accuracy_results.SF.Districts
 

############### SVM Tunning #########################

District.crimes <-Crimes_district %>% filter(Category %in% c("ASSAULT","ROBBERY"))%>%droplevels() 

District.crimes.test <- Crimes_district_test%>% filter(Category %in% c("ASSAULT", "ROBBERY"))%>%droplevels()

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_model.district.crimes <- svm(Hypothesis, data =District.crimes , method ="linear", 
                               trControl = trctrl, preProcess = c("center", "scale"),
                               tuneGrid = grid,tuneLength=10)

svm_district.crimes <- predict(svm_model.district.crimes, newdata = District.crimes.test)
svm_district.crimes.type <- factor(svm_district.crimes, levels = levels(District.crimes.test$Category))

Accuracy_district_type <- confusionMatrix(data = svm_district.crimes.type, 
                                          reference = District.crimes.test$Category)$overall["Accuracy"] 
 
Accuracy_results.SF.Districts <- bind_rows(Accuracy_results.SF.Districts,
                              data_frame(method=" SVM tunned on Violent Crimes per District",
                                         Accuracy = Accuracy_district_type ))
Accuracy_results.SF.Districts


##################### Random Forest Modelling #############################################


######### Using High crimes rates on  San Francisco's districts######### 

rf_training.districts <-  train(Hypothesis, data =District.crimes, method="rf")
 
 
rf_district_type<- predict(rf_training.districts, newdata = District.crimes.test)
rf_district12_type <- factor(rf_district_type, levels = levels(District.crimes.test$Category))

Accuracy_district_rf <- confusionMatrix(data = rf_district12_type, 
                                          reference = District.crimes.test$Category)$overall["Accuracy"] 
 
Accuracy_results.SF.Districts <- bind_rows(Accuracy_results.SF.Districts,
                              data_frame(method=" RF on Violent crimes per District",
                                         Accuracy = Accuracy_district_rf))
Accuracy_results.SF.Districts
 
############## Random Foresr Tunnig ###########################

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
## mtry###

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(5:15))
rf_mtry <- train(Hypothesis,
                 data = District.crimes,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

## Max nodes## 

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
for (maxnodes in c(30: 40)) {
  set.seed(1234)
  rf_maxnode <- train(Hypothesis,
                      data = District.crimes,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

## ntrees ##

store_maxtrees <- list()
for (ntree in c(400, 450, 500, 550, 600, 800, 1000, 2000,2500, 2700,3000)) {
  set.seed(5678)
  rf_maxtrees <- train(Hypothesis,
                       data = District.crimes,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       nodesize = 14,
                       maxnodes = 30,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#### Random Forest with tunning  results ############

model.RF <- train(Hypothesis,
                 data = District.crimes,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 nodesize = 14,
                 maxnodes = 30,
                 ntree = 2000)
 
RF_fit <- predict(model.RF, newdata = District.crimes.test)
RF_fit2 <- factor(RF_fit, levels = levels(District.crimes.test$Category))

Accuracy_RF_fit <- confusionMatrix(data = RF_fit2, 
                                            reference = District.crimes.test$Category)$overall["Accuracy"] 
 
Accuracy_results.SF.Districts <- bind_rows(Accuracy_results.SF.Districts,
                              data_frame(method=" RF tunned on Violent crimes per District",
                                         Accuracy = Accuracy_RF_fit))
Accuracy_results.SF.Districts
 
Accuracy_results.SF
  

