if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")

library(DescTools)
library(dplyr) # enables pipes %>% and more
library(tidyr) # for spliting on the period see below
library(moments)
library(reshape2)
library(ggplot2)
par(mfrow=c(1, 1))

  
setwd('~/RStudio Work/MA334 Stats with R/MA334_Assignment')
Proj_data_all_11 <-  read.csv("proportional_species_richness_NAs_removed.csv")
# HERE USE YOUR ALLOCATED SET OF 5 TAXINOMIC GROUPS 
eco_selected_names <- c("Bees","Hoverflies","Isopods","Macromoths","Vascular_plants")
# CALCULATE YOUR BIODIVERSITY MEASURE AS A MEAN OF YOUR 5 GROUPS 
mean_selected <- rowMeans(Proj_data_all_11[,eco_selected_names]) # mean the 5 columns 
Proj_data <- Proj_data_all_11%>%select("Location",eco_selected_names,
                                       "Easting","Northing","dominantLandClass",      
                                       "ecologicalStatus","period")%>%
  mutate(eco_status_5=mean_selected)
names(Proj_data) # note extra variable eco_status_5


Proj_data$period <- as.factor(Proj_data$period) # must set categorical vars
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass)

table <- data.frame()
for(i in c(2:6)){
  table <- rbind(table,
                 c(names(Proj_data)[i],
                   round(mean(Proj_data[,i],na.rm = TRUE),digits = 2),
                   round(sd(Proj_data[,i],na.rm = TRUE),digits = 2),
                   round(skewness(Proj_data[,i],na.rm = TRUE),digits = 2)
                 ))}
colnames(table) <- c("taxi_group","mean","sd","skewness")
table%>%arrange(sd,skewness) 

summary(eco_selected_names)

names(Proj_data)
cont_vars <- Proj_data%>%select(c(2:8)) # includes easting and northing 
names(cont_vars)
cormat <- round(x = cor(cont_vars,use="pairwise.complete.obs"), digits = 2)


# This creates a data frame which includes all of my assigned BD5 variables.

BD5 <- data.frame(
  Bees = Proj_data_all_11 $Bees,
  Hoverflies = Proj_data_all_11 $Hoverflies,
  Isopods = Proj_data_all_11 $Isopods,
  Macromoths = Proj_data_all_11 $Macromoths,
  Vascular_plants = Proj_data_all_11 $Vascular_plants)
BD5
View(BD5)
#-------------Univariate analysis and basic R programming-----------------------


#1) Present a table which provides the following summary statistics for each of the five
##variables in your BD5 group. This table must provide the 7 statistics:


##a) The six statistics which are commonly found using the summary() command,
##namely Min, 1st Quarter, Median, Mean, 3rd Quarter, Max.

summary(BD5)

#b) In addition, add a column to this table which provides a new statistic, that is the
#20% Winsorized mean for all the variables in the BD5 group. In your R code file
#add comments to the relevant lines. 

# Winsorized mean function. This winsorized mean function is from Lab 2.
winsorized_mean <- function(x, trim_percentage) {
  L <- length(x)
  numchange <- floor(trim_percentage * L / 100)
  
  sorted_x <- sort(x)
  
  # Replace extreme values
  if (numchange >= 1) {
    for (t in 1:numchange) {
      sorted_x[t] <- sorted_x[numchange + 1]
    }
    for (t in seq(L - numchange + 1, L, by = 1)) {
      sorted_x[t] <- sorted_x[L - numchange]
    }
  }
  # Calculate mean of sorted values
  mean_val <- mean(sorted_x)
  
  return(mean_val)
}

# This will set the Winsorized mean percentage to 20% for each variable in BD5.
trim_percentage <- 20  

winsorized_means <- numeric(length = ncol(BD5))

#This will apply the function to each of my BD5 variables. 
winsorized_means <- lapply(BD5, function(column) winsorized_mean(column, trim_percentage))

# This displays winsorized means for each variable
print(winsorized_means)

# I have created the new table including the summary statistics and the winsorized
# in my assignment report. 

#2) Estimate the correlations between all pairs of variables in BD5 and put them into a
#table which consists of 5 rows and 5 columns where each row/column represents a BD5 variable

cor_matrix <- cor(BD5)

print(cor_matrix)
View(cor_matrix)
ncol(BD5)

cor_table <- matrix(NA, nrow = 5, ncol = 5)
cor_table

cor_table[1:5, 1:5] <- cor_matrix[1:5, 1:5]
rownames(cor_table) <- colnames(cor_matrix)[1:5]
colnames(cor_table) <- colnames(cor_matrix)[1:5]
print(cor_table)

#melt the correlation matrix
#melt(cormat)%>%mutate(R2 = value^2)%>%arrange(value)
#melt(cormat)%>%mutate(R2 = value^2)%>%arrange(Var1,value)

#eco_status_5 <- Proj_data%>%pull(eco_status_5)
#eco_status_11 <- Proj_data%>%pull(ecologicalStatus)
#eco_period <- Proj_data%>%pull(period)
#plot(eco_status_5~eco_period)
#plot(eco_status_11~eco_period) 

#3) Perform the boxplot for only one variable in BD5

boxplot(BD5$Vascular_plants, main = 'Boxplot for Vascular plants', ylab = 'Values')

#-------------------------------Hypothesis Tests--------------------------------


# comparing the two by CULMULATIVE DISTRIBUTIONS  
par(mfrow=c(1, 1))  # divide graph area in 1 columns
qqplot(Proj_data$eco_status_5,Proj_data$ecologicalStatus)
abline(0,1,col="red")
# both cdfs together  and do a kolmogorov test H0: distributions are the same
BD5_cdf <- ecdf(Proj_data$eco_status_5)
BD11_cdf <- ecdf(Proj_data$ecologicalStatus)
plot(BD11_cdf,col="red")
lines(BD5_cdf,col="green")
ks.test(Proj_data$eco_status_5,Proj_data$ecologicalStatus)
?ecdf

# following code splits between the two periods to find the change in eco measures

# now investigate the change of BD5 measure between the two periods 
names(Proj_data)
Proj_data_split <- Proj_data%>%select(Location,period,eco_status_5)%>%
  pivot_wider(names_from =period,values_from=eco_status_5)%>%
  mutate(BD5_change=Y00-Y70)
View(Proj_data_split)
hist(Proj_data_split$BD5_change)  # the distribution of the BD5 change 
BD5_change <- Proj_data_split%>%pull(BD5_change)
t.test(BD5_change,mu=0)  # t test with H0: mu=0

names(Proj_data)
Proj_data_all_11_split <- Proj_data%>%select(Location,period,ecologicalStatus)%>%
  pivot_wider(names_from =period,values_from=ecologicalStatus)%>%
  mutate(BD11_change=Y00-Y70)
hist(Proj_data_all_11_split$BD11_change)  # the distribution of the BD5 change 
BD11_change <- Proj_data_all_11_split%>%pull(BD11_change)
t.test(BD11_change,mu=0)  # t test with H0: mu=0

plot(BD11_change~BD5_change) # scatter plot
abline(lm(BD11_change~BD5_change),col="green") # fits a best fit line through data points
abline(0,1,col="red") # plots a 45 degree line
cor(BD11_change,BD5_change) # correlation coefficient


 #-------------Contingency table/comparing categorical variables-----------------


#1) Create two contingency tables which display counts, one for BD11up against
#BD5up and another for the corresponding independent model. 

# here inner join  the dataframes for BD5 and BD11 
Eco_change_BD11 <- Proj_data_all_11_split%>%select(Location,BD11_change)
Eco_change_BD5 <- Proj_data_split%>%select(Location,BD5_change)
Both_eco_change <- inner_join(Eco_change_BD11,Eco_change_BD5,by="Location")
View(Both_eco_change)
# here add two columns for BD5up and BD11up (see assignment brief for definitions)
Both_eco_change <- Both_eco_change%>%
  mutate(BD11up=ifelse(Both_eco_change$BD11_change>0,1,0))%>%
  mutate(BD5up=ifelse(Both_eco_change$BD5_change>0,1,0))
View(Both_eco_change)
table(Both_eco_change$BD11up)  # distribution of BD11up
table(Both_eco_change$BD5up)   # distribution of BD5up
# now the joint distribution , a contingency table to interpret (chapter 7 of the book)
table_up_down <- table(Both_eco_change$BD11up,Both_eco_change$BD5up) # contingency table for interpretation 
colnames(table_up_down) <- c("down","up");rownames(table_up_down) <- c("down","up")
table_up_down

table_BD11up <- table(Both_eco_change$BD11up)

# Create a contingency table for BD5up
table_BD5up <- table(Both_eco_change$BD5up)

# Display the contingency tables
table_BD11up
table_BD5up

GTest(table_up_down) # log likelihood ratio test
summary(table_up_down) # summary also gives the chi squared test (similar p value)

#Independence model for table_up_down

#Calculating row/col sums for the Independence model
Total = rowSums(table_up_down)
table_up_down <- cbind(table_up_down, Total)
Total = colSums(table_up_down)
table_up_down = rbind(table_up_down, Total)

total_table_up_down <- as.data.frame(table_up_down)
total_table_up_down

# Calculate values. Independence model formula from the MA334 Module Set book.
total_table_up_down$down[1:2] <- (total_table_up_down$Total[1:2] / total_table_up_down$Total[3]) * total_table_up_down$down[3]
total_table_up_down$up[1:2] <- (total_table_up_down$Total[1:2] / total_table_up_down$Total[3]) * total_table_up_down$up[3]

#Round Values
rounded_independence_model <- round(total_table_up_down, 0)

#Independence_model Result
independence_model<- rounded_independence_model[1:2, 1:2]
independence_model

#2) Using R, estimate the likelihood-ratio statistic from the two above contingency
#tables. Based on this statistic, compare the proportions of increase in BD5 and
#BD11 at 5% confidence level

GTest(independence_model) # Likelihood ratio test for independence model

#Estimate odds-ratio, sensitivity, specificity, and Youden’s index
#I created these formulas using the MA334 module set book.

table_up_down <- as.data.frame(table_up_down)
table_up_down

odds_ratio <- (table_up_down$down[1] / table_up_down$down[2]) / (table_up_down$up[1] / table_up_down$up[2])

sensitivity <- table_up_down$down[1] / table_up_down$down[3]

specificity <- table_up_down$up[2] / table_up_down$up[3]

youden_index <- sensitivity + specificity - 1

odds_ratio
sensitivity
specificity
youden_index


#------------------------------Simple linear regression-------------------------
slr_model_1 <-lm(Proj_data_all_11$Bird~Proj_data$eco_status_5)

abline(0,1,col="red")
lin_mod <- lm(Proj_data_all_11$Bird~Proj_data$eco_status_5)
abline(lin_mod,col="green")
summary(lin_mod)
# some diagnostics 
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")

#----------------------------Multiple linear regression-------------------------

lmMod <- lm(Proj_data_all_11$Bird~.,
            data=Proj_data[c(eco_selected_names)],y=TRUE)
summary (lmMod)  # model summary
cor(lmMod$fitted.values,lmMod$y) # corelation with the data 
plot(Proj_data_all_11$Bird~lmMod$fitted.values)
abline(0,1,col="red")
# mis_fit_to_testData are the residuals for the train model fit to the test data 
mis_fit_to_Data <- Proj_data_all_11$Bird-lmMod$fitted.values
plot(mis_fit_to_Data~lmMod$fitted.values) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_Data) # check for normality of residuals in prediction
qqline(mis_fit_to_Data,col="red")

AIC(lmMod)
summary(lmMod)
lmMod_reduced <- lm(Proj_data_all_11$Bird~.,
                    data=Proj_data[c("Bees","Hoverflies","Macromoths","Vascular_plants")],y=TRUE)
summary(lmMod_reduced)
AIC(lmMod_reduced,lmMod) # here lmMod is preferred by p and AIC criteria

# now introduce an interaction (see the assignment)
lmMod_interaction <- lm(Proj_data_all_11$Bird~
                          Bees + Hoverflies + Macromoths + Vascular_plants + Hoverflies
                        +Bees * Macromoths,   
                        data=Proj_data,y=TRUE)
summary(lmMod_interaction )
AIC(lmMod,lmMod_reduced,lmMod_interaction) # model with interataction prefered 
cor(lmMod_interaction$fitted.values,lmMod_interaction$y) # corelation slightly improved only 

# now use one period as the training set and one as the test set (see assignment)
table(Proj_data$period)
nrow(Proj_data)
Proj_data_Y70 <- Proj_data_all_11%>%filter(period=="Y70") # training set
Proj_data_Y00 <- Proj_data_all_11%>%filter(period=="Y00") # test set
nrow(Proj_data_Y00);nrow(Proj_data_Y00)

lmMod_70 <- lm(Proj_data_Y70$Bird~.,
               data=Proj_data_Y70[c(eco_selected_names)],y=TRUE)
qqnorm(lmMod_70$residuals);qqline(lmMod_70$residuals,col="red")
plot(lmMod_70$residuals~lmMod_70$fitted.values) # look for unwanted pattern in residuals
abline(0,0,col="red")
Predict_00 <- predict(lmMod_70,Proj_data_Y00)
plot(Predict_00~Proj_data_Y00$Bird)
abline(0,1,col="red") 
mean((Proj_data_Y70$Bird-lmMod_70$fitted.values)^2)  # MSE on train data set 
mean((Proj_data_Y00$Bird-Predict_00)^2)  # MSE on test data (higher)
lmMod_70
#----------------------------------Open Analysis--------------------------------

# Here I have created a data frame for each of the variables 
# I have chosen to use for my open analysis including my BD5 variables


selected_variables <- data.frame(
  Bees = Proj_data_all_11$Bees,
  Hoverflies = Proj_data_all_11$Hoverflies,
  Isopods = Proj_data_all_11$Isopods,
  Macromoths = Proj_data_all_11$Macromoths,
  Vascular_plants = Proj_data_all_11$Vascular_plants,
  dominantLandClass = Proj_data_all_11$dominantLandClass)
View(selected_variables)

Open_analysis_Proj_data <- data.frame(
  Proj_data
)

View(Open_analysis_Proj_data)

# Using the land classification codes 
#I have created a new variable for welsh land classes

wales_land_classes <- c("17w1", "17w2", "17w3", "5w", "6w", "7w", "15w", "18w")

# Here I filter the for the wales land classes within my BD5 variables.
selected_variables <- Proj_data_all_11 %>%
  filter(dominantLandClass %in% wales_land_classes) %>%
  select(Bees, Hoverflies, Isopods, Macromoths, Vascular_plants, dominantLandClass)

# Most dominant land class in BD5 from the wales land class group.
wales_land_class_count <- selected_variables%>%group_by(dominantLandClass)%>%count() %>% arrange(n)
wales_land_class_count

#Rounded mountains/scarps/upper valleys, mid/S Wales
Top_wales_land_class <- c("17w2")
View(Top_wales_land_class)

# Here I have split the data up for each time period and have included my eco_status_5
# which includes the mean values for my BD5 group.
BD5_17w2_Y00 <- Open_analysis_Proj_data %>%
  filter(dominantLandClass %in% Top_wales_land_class, period == 'Y00') %>% 
  group_by(eco_status_5, dominantLandClass, period)

BD5_17w2_Y70 <- Open_analysis_Proj_data %>%
  filter(dominantLandClass %in% Top_wales_land_class, period == 'Y70') %>% 
  group_by(eco_status_5, dominantLandClass, period)

# Here I bind the rows to create my new BD5_wales variable for me to interpret in my report.
BD5_17w2 <- bind_rows(
  data.frame(period = 'Y00', eco_status_5 = BD5_17w2_Y00$eco_status_5),
  data.frame(period = 'Y70', eco_status_5 = BD5_17w2_Y70$eco_status_5)
)

View(BD5_17w2)

# This is my histogram for BD5_wales which I have interpreted in my report.
ggplot(BD5_17w2, aes(x = eco_status_5, fill = period)) +
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "BD5 in Rounded mountains/scarps/upper valleys, mid/S Wales for Y00 & Y70",
       x = "BD5", y = "Frequency") 


# t-test to interpret in my report. 
t_test_result <- t.test(BD5_17w2_Y00$eco_status_5, BD5_17w2_Y70$eco_status_5)

print(t_test_result)

