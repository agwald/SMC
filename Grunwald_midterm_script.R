#load libraries

##ANOVA Analysis libraries
library(reshape2) #to reshape data
library(dplyr) #data preparation
library(e1071) #Skew and kurtosis
library(car) #Levenes test
library(ggplot2) #plotting
library(pander) #table formatting

##LinearRegression libraries
library(leaps) #exhaustive search
library(broom) #residual analysis
library(lmtest) #Durbin-Watson test


#set working directory
setwd('/Users/Amanda Grunwald/Documents/MSBA_quarter 2/R Programming/Github/akg4/Midterm 1/data/')


#read csv file
flightdata <- read.csv('On_Time_On_Time_Performance_2016_11.csv')
flightdelay <- subset(flightdata, Origin=='SFO') #only take data related to SFO departures, only take data for delayed flights

###################################################################
###Question 1: descriptive stats
###################################################################

delay.stats <- data.frame(flightdelay$CarrierDelay,
                          flightdelay$WeatherDelay, 
                          flightdelay$NASDelay, 
                          flightdelay$SecurityDelay, 
                          flightdelay$LateAircraftDelay)

summary(delay.stats, na.rm=TRUE)


#melt data to view data for factors related to flight delay
flightdelay.m <- melt(flightdelay, id.vars=c("DayofMonth"), 
                      measure.vars=c("CarrierDelay", 
                                     "WeatherDelay", 
                                      "NASDelay", 
                                     "SecurityDelay", 
                                     "LateAircraftDelay"))


#Descriptive summary of melted data
flightdelay.desc <- flightdelay.m %>%
  group_by(variable) %>%
  dplyr::summarise(
    count=n(),
    sum=sum(value, na.rm=TRUE),
    min=min(value, na.rm=TRUE),
    max=max(value, na.rm=TRUE),
    mean=mean(value, na.rm=TRUE),
    median=median(value, na.rm=TRUE),
    range=max-min, 
    q1=as.numeric(quantile(value, na.rm=TRUE) [2]), 
    q3=as.numeric(quantile(value, na.rm=TRUE) [4]), 
    iqr=q3-q1,
    sd=sd(value, na.rm=TRUE), 
    var=var(value, na.rm=TRUE), 
    kurt=kurtosis(value, na.rm=TRUE), 
    skew=skewness(value, na.rm=TRUE) 
  )

#print descriptive data
panderOptions("table.style", "simple")
pander(subset(flightdelay.desc), row.names=FALSE)

#examine data for normality
hist(flightdelay.m$value, xlab="Length of Delay (Minutes)", main="Length and Frequency of Delays")

#bar chart
colors = c("yellow", "green", "violet","orange", "cyan") 

#box plot
boxplot(flightdelay.m$value~flightdelay.m$variable, main="Reasons for SFO Departure Delays", ylab="Minutes", names.arg=c("Carrier", "Weather", "NAS", "Security", "LateCraft"), las=2, col=colors)

#bar chart
bp <- barplot(flightdelay.desc$sum, main="Cause of Delays (in minutes) at SFO, Nov. 2016", ylab="Total minutes of delay", xlab="Cause of delay", names.arg=c("Carrier", "Weather", "NAS", "Security", "LateCraft"), las=2, col=colors)


###################################################################
###Question 2: difference in carrier delays
###################################################################
###H0: All carriers have the same mean departure delays. 
###H1: At least 1 carrier has a different mean depararture delay

#ANOVA
fit <- lm(data=flightdelay, DepDelayMinutes~Carrier)
flightdelay.aov <- aov(fit)
flightdelay.aov.summary <- summary(flightdelay.aov)

#print results
print(flightdelay.aov.summary)

#Tukey-Framer procedure, to create set of confidence intervals on the diffs btwn means of levels of a factor
flightdelay.tukey <- TukeyHSD(flightdelay.aov)

#print results
print(flightdelay.tukey)


##Check assumptions

#Create plot, save into var
qqp <- ggplot(flightdelay) + stat_qq(aes(sample=DepDelayMinutes, colour=factor(Carrier))) + guides(col=guide_legend(title="Carriers"))

print(qqp)

#Homogeneity of variance
flightdelay.levene <- leveneTest(fit)

#print results
print(flightdelay.levene)

#results show p-val of 0.08, meaning that we reject the null hypothesis. Variance in groups is not all the same. 

#Plot results of ANOVA
df <- anova(fit)["DF",]
names(df) <- c("between", "within")
alpha <- 0.05

#get F values
flightdelay.f.crit <- qf(alpha, df[,"between"], df[,"within"], lower.tail = FALSE) #value is returning NA?
flightdelay.f.value <- flightdelay.aov.summary[[1]]$F[1]

#Plot boxplot of data set
bp2 <- ggplot(flightdelay, aes(x=Carrier, y=DepDelayMinutes)) + stat_boxplot(geom="errorbar") + geom_boxplot() + labs(y="Minutes Delayed", x="Carriers")

#print results
print(bp2)

#save plot
ggsave("Flightdataboxplot.pdf")


###################################################################
###Question 3: Predicting Arrival Delay for Flights departing from SFO
###################################################################
#H0: The data points available can be used to predict arrival delay (yes, evidence). 
#H1: The data points available cannot be used to predict arrival delay (no evidence). 
#Assumptions: (1) data for Nov is representative of other months in the year, (2) 

#Create a dataframe with only the relevant variables
flightarrival <- data.frame(flightdelay$ArrDelayMinutes, 
                            flightdelay$DayOfWeek,
                            flightdelay$DayofMonth, 
                            flightdelay$DepTime, 
                            flightdelay$AirlineID, 
                            flightdelay$Distance,
                            flightdelay$DestAirportID)

#see the data to verify that it looks correct
names(flightarrival)


#Run the regression analysis
#Calculate regression on all data
flightarrival.m1 <- lm(flightdelay.ArrDelayMinutes~., data=flightarrival) #first model using all variables
flightarrival.m1.summary <- summary(flightarrival.m1) #show results of first model
plot(flightarrival.m1)

#Omit NA values
flightarrival.na <- na.omit(flightarrival)

#Confidence interval
flightarrival.m1.confint <- confint(flightarrival.m1)

#Correlation matrix
flightarrival.cor <- cor(flightarrival.na)

#Calculate all possible regressions
x <- flightarrival.na[2:7] #using data with na.omit to avoid NA/NaN issue in leaps call
y <- flightarrival.na[,1]

#Model selection by exhaustive search
flightarrival.out <- summary(regsubsets(x, y, nbest=2, nvmax=ncol(x)))
flightarrival.regtab <- cbind(flightarrival.out$which, flightarrival.out$rsq, flightarrival.out$adjr2, flightarrival.out$cp)
colnames(flightarrival.regtab) <- c("(Intercept", "DayofWeek", "DayofMonth", "DepTime", "AirlineID", "Dist", "DestAirportID", "R-sq", "R-sq (adj)", "Cp") #header
print(flightarrival.regtab)

#Results of model search conclude that there is no clear explanation of arrival delay.
#Therefore, no alternate model will be better at explaining at arrival delay than the 1st model created.




# #Therefore, choosing a 2-var model to test model variability
# 
# ##Create 2nd model
# flightarrival.m2 <- lm(flightdelay.ArrDelayMinutes~flightdelay.DayOfWeek+flightdelay.DepTime, data=flightarrival)
# flightarrival.m2.summary <- summary(flightarrival.m2)
# print(flightarrival.m2.summary)
# plot(flightarrival.m2)
# 
# #Confidence intervals
# flightarrival.m2.confint <- confint(flightarrival.m2)
# print(flightarrival.m2.confint)
# 
# #Comparing the 2 models
# n <- length(flightarrival$flightdelay.ArrDelayMinutes) #num elements in the data set
# diff <- dim(n) #dimension
# percdiff <- dim(n)
#   
#   #Test each combination of options of elements in data using a LOOP
#   for (k in 1:n){
#     train1 <- c(1:n)
#     train <- train1[train1!=k] #reserve 1 value to be able to test it against model results
#     
#     #create linear model for all except 1 reserved element
#     m1 <- lm(flightdelay.ArrDelayMinutes~., data=flightarrival[train,])
#     
#     #prediction of missing value
#     pred <- predict(m1, newdat=flightarrival[-train,])
#     
#     #versus the real value
#     obs <- flightarrival.na$flightdelay.ArrDelayMinutes[-train]
#     
#     #calculate diff btwn observed and predicted
#     diff[k] <- obs-pred
#     
#     #calculate the rel diff btwn observed and predicted
#     diff[k] <- abs(diff[k])/obs
#   }
# 
# flightarrival.m1.me <- mean(diff) #mean error
# flightarrival.m1.rmse <- sqrt(mean(diff**2)) #root mean square error
# flightarrival.m1.mape <- 100*(mean(percdiff)) #mean absolute percent error

##Check assumptions
#Create data frame with residuals
flightarrival.f <- fortify(flightarrival.m1)

#Linearity
#Residual vs. fitted plot
pl <- ggplot(flightarrival.f, aes(x=.fitted, y=.resid))+
  geom_point() +
  stat_smooth(method="loess") +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")

#Normality
#Normal Q-Q Plot
p2 <- ggplot(flightarrival.f, aes(x=qqnorm(.stdresid, plot.it=FALSE)[[1]], y=.stdresid)) +
  geom_point(na.rm=TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")

flightarrival.skew <- skewness(flightarrival.f$.resid)
print(flightarrival.skew)

flightarrival.kurt <- kurtosis(flightarrival.f$.resid)
print(flightarrival.kurt)

#Equal variance
p3 <- ggplot(flightarrival.f, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("Standardized Residuals"))) +
  ggtitle("Scales-Location")

#Independence - Durbin-Watson test
flightarrival.dw <- dwtest(flightarrival.m1)

#Outlier influence
#Cook's distance histogram
p4 <- ggplot(flightarrival.f, aes(x=seq_along(.cooksd), y=.cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance")

p5 <- ggplot(flightarrival.f, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +
  theme(legend.position="bottom")

## Save Plots
ggsave("linearityAssumption.pdf", p1)
ggsave("normalityAssumption.pdf", p2)
ggsave("equalVarianceAssumptions.pdf", p3)
ggsave("outlierInfluance1Assumptions.pdf", p4)
ggsave("outlierInfluance2Assumptions.pdf", p5)


 