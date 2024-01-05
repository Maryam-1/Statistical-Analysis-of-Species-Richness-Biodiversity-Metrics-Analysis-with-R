if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")
###########Load the libraries################
library(dplyr)
library(tidyr) # for spliting on the period see below
library(moments) # for calculating moments for skewness etc.
library(reshape2)
par(mfrow=c(1, 1)) 
###############Import Data###############
mydata <-  read.csv("proportional_species_richness_V3.csv")
mydata$period <- as.factor(mydata$period) # must set categorical vars
mydata$dominantLandClass <- as.factor(mydata$dominantLandClass)
names(mydata)

################summaryofthedata###########
my_data_frame <- data.frame(Bird = c(mydata$Bird), Bees= c(mydata$Bees), Hoverflies= c(mydata$Hoverflies), Ladybirds = c(mydata$Ladybirds), Macromoths = c(mydata$Macromoths), Grasshoppers = c(mydata$Grasshoppers_._Crickets), Vascular_plants = c(mydata$Vascular_plants))# split my data into a data frame consist of only 7 variables 
my_sd <- apply(my_data_frame, 2, sd, na.rm = TRUE)# standard deviation of the seven variables 
summary(my_data_frame)
my_sd

names(mydata)
#################create combined variable ###################
mydata$BD7=rowMeans(mydata[, c("Bees","Bird","Hoverflies","Ladybirds","Ladybirds","Macromoths","Grasshoppers_._Crickets","Vascular_plants")])
mydata$BD11=rowMeans(mydata[, c("Bees","Bird","Hoverflies","Ladybirds","Macromoths","Grasshoppers_._Crickets","Vascular_plants","Bryophytes","Butterflies","Carabids","Isopods")])
mydata$BD4=rowMeans(mydata[, c("Bryophytes","Butterflies","Carabids","Isopods")])

my_cor_matrix1 <- cor(my_data_frame)# correlation matrix 
my_cor_matrix1
#########count incidents (both periods) for each land classification for later selection###########
mydata%>%group_by(dominantLandClass)%>%count()%>%
  arrange(dominantLandClass)%>%print(n=45)

all <- c(2:12)
eco_selected <- sample(all,size=7, replace = FALSE)
eco_selected <- c(2,3,7,9,10,11,12)   #allocated taxonomic species
eco_not_selected <- all[!(all%in%eco_selected)]
eco_names <- names(mydata[,2:12])
eco_selected_names <- names(data)[eco_selected]
eco_selected_names

##########Summary statistics including both periods##########
table <- data.frame()
for(i in eco_selected){
  table <- rbind(table,
                 c(eco_names[i-1],
                   round(mean(mydata[,i],na.rm = TRUE),digits = 2),
                   round(median(mydata[,i],na.rm = TRUE),digits = 2),
                   round(min(mydata[,i],na.rm = TRUE),digits = 2),
                   round(max(mydata[,i],na.rm = TRUE),digits = 2),
                   round(sd(mydata[,i],na.rm = TRUE),digits = 2),
                   round(IQR(mydata[,i], na.rm = TRUE), digits = 2),
                   round(skewness(mydata[,i],na.rm = TRUE),digits = 2)
                 ))}
colnames(table) <- c("taxi_group","mean","median","min","max","sd","IQR","skewness")
table%>%arrange(sd,skewness) 

#spliting the data by period and compare these stats before and after 

# Split the data by period
data_by_period <- split(mydata, f = mydata$period)

# Create an empty data frame to store the summary statistics for each period
summary_stats <- data.frame()

# Loop through each period and calculate summary statistics
for (period in unique(mydata$period)) {
  
  # Subset the data for the current period
  period_data <- mydata[mydata$period == period, ]
  
  # Calculate summary statistics for each variable
  means <- sapply(period_data[, eco_selected], mean, na.rm = TRUE)
  medians <- sapply(period_data[, eco_selected], median, na.rm = TRUE)
  sds <- sapply(period_data[, eco_selected], sd, na.rm = TRUE)
  skewness <- sapply(period_data[, eco_selected], skewness, na.rm = TRUE)
  mins <- sapply(period_data[, eco_selected], min, na.rm = TRUE)
  maxs <- sapply(period_data[, eco_selected], max, na.rm = TRUE)
  
  # Combine the summary statistics into a data frame
  period_summary <- data.frame(
    period = period,
    variable = eco_names[eco_selected-1],
    mean = round(means, digits = 2),
    median = round(medians, digits = 2),
    sd = round(sds, digits = 2),
    skewness = round(skewness, digits = 2),
    min = round(mins, digits = 2),
    max = round(maxs, digits = 2)
  )
  
  # Add the summary statistics for the current period to the main data frame
  summary_stats <- rbind(summary_stats, period_summary)
}

# Order the summary statistics by period and variable
summary_stats$period <- as.character(summary_stats$period)

summary_stats <- summary_stats[order(summary_stats$period, summary_stats$variable), ]

######################hypothesis test#################
t.test (mydata$BD7,mydata$BD11)# t test between combined variable BD7 and BD11
t.test(mydata$BD7,mydata$BD4) # t test between combined variable BD7 and BD4

###############Simple Linear Regression. How BD7 matches with BD11##############
hist(mydata$BD7, col = "blue", main = "Histogram of BD7")#histo gram to check the normality 
reg1=lm(mydata$BD7~mydata$BD11, data=mydata)
summary(reg1)
plot(reg1)# to check linearity and normality 

########### Run the regression for each period separately############
reg_Y70 <- lm(mydata$BD7~mydata$BD11, data = subset(mydata, period == "Y70"))
summary(reg_Y70)
reg_Y00 <- lm(mydata$BD7~mydata$BD11, data = subset(mydata, period == "Y00"))
summary(reg_Y00)
##############3 Test data ###########
########3 select  7 allocated predictors to form the trial eco_stat#######
all <- c(2:12)
eco_selected <- sample(all,size=7, replace = FALSE)
eco_selected <- c(2,3,7,9,10,11,12)    
eco_not_selected <- all[!(all%in%eco_selected)]
eco_names <- names(mydata[,2:12])
eco_selected_names <- names(mydata)[eco_selected]
eco_selected_names
# Creating training data set 
trainingRowIndex <- sample(1:nrow(mydata), 0.8*nrow(mydata))  # row indices for 80% training data
trainingData <- mydata[trainingRowIndex, ]  # model training data
testData  <- mydata[-trainingRowIndex, ]%>%na.omit # for test data remove NAs 

# Build the model on training data
lmMod_train <- lm(BD4~.,
                  data=trainingData[c(eco_selected_names,"BD4")],
                  na.action=na.omit,y=TRUE)
summary (lmMod_train)  # model summary
AIC(lmMod_train)
cor(lmMod_train$fitted.values,lmMod_train$y) # cor training data 
Eco_4_Pred <- predict(lmMod_train, testData) # predict to check model on test Data
cor(Eco_4_Pred,testData$BD4)
plot(Eco_4_Pred~testData$BD4)
abline(0,1,col="red")
# mis_fit_to_testData are the residuals for the train model fit to the test data 
mis_fit_to_testData <- testData$BD4-Eco_4_Pred
plot(mis_fit_to_testData~Eco_4_Pred) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

#multiplelinear regression 
## Multipe Linear Regression 
hist(mydata$BD4, col = "blue", main = "Histogram of BD4")
reg2=lm(mydata$BD4~mydata$Bees+mydata$Bird+mydata$Hoverflies+mydata$Ladybirds+mydata$Macromoths+mydata$Grasshoppers_._Crickets+mydata$Vascular_plants,data=mydata)
summary(reg2)# view summary statistics
AIC(reg2) # view AIC value
plot(reg2)
##New regression after removal of BEES 
reg3=lm(mydata$BD4~mydata$Bird+mydata$Hoverflies+mydata$Ladybirds+mydata$Macromoths+mydata$Grasshoppers_._Crickets+mydata$Vascular_plants,data=mydata)
summary(reg3)# view summary statistics
AIC(reg3) # view AIC value


## Open Analysis
library(ggplot2)
ggplot(mydata, aes(x = period, y = BD7)) + 
  geom_boxplot() + 
  labs(x = "Period", y = "BD7") +
  ggtitle("Box Plot of BD7 by Period")
#average values varies with respect year 
Period2=as.numeric(mydata$Period)
t.test(mydata$BD7,mydata$period2)
model <- aov(mydata$BD7 ~ mydata$Location, data = mydata)
summary(model)




