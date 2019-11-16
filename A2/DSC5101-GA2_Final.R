#############################################################################
##                    DSC5101 - Group Assignment 2                          # 
##     An estimation of Volcker Rule Effect in Trading Asset Ratio of BHCs  #
##            Manisha / Qianru / Yin Mingjun / Ashesh                       #
#############################################################################

#Load required library
library(MatchIt)
library(psych)
library(plm)
library(doBy)
library(ggplot2)
library(rpart)
library(sqldf)
library(zoo)
library(dplyr)
library(sm)
library(rpart)
library(pracma)

#Data Preperation
modified_data <- read.csv("DiD_data.csv",header=TRUE)
suppressMessages(attach(modified_data))

#BaseLine Models

#1 - TAR ~ After_DFA (No Control or Fixed Effect)
model1 <- lm(TAR ~ After_DFA, data = modified_data)
summary(model1)

#2 - Model1 + Control covariates
model2 <- lm(TAR ~ After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = modified_data)
summary(model2)

#3 - Model2 + Interaction between After_DFA * Affect_BHC
model3 <- lm(TAR ~ After_DFA * Affected_BHC + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = modified_data)
summary(model3)

#4 - Model3 + Fixed Effect
model4 <- plm(TAR ~ Affected_BHC * After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = modified_data, index = c("id", "Time"))
summary(model4)

#Robustness Test - Propensity score matching

###############Method for propensity Matching################
matchData <- function(dataForPropMatching, testControlRatio) {
  
  #Clean Data passed for propensity matching
  prop_data <- as.data.frame(na.omit(dataForPropMatching))
  #Define variables
  treat=cbind(prop_data$Affected_BHC)
  after=cbind(prop_data$After_DFA)
  t=cbind(prop_data$Time)
  Y=cbind(prop_data$TAR)
  X=cbind(prop_data$Total_Asset,prop_data$Leverage_Ratio,prop_data$ROA,prop_data$Liquidity_Ratio,prop_data$Deposit_Ratio,
          prop_data$Real_Estate_Ratio,prop_data$Credit_Risk_Ratio,prop_data$Cost_Income_Ratio,prop_data$CPP)
  
  #Perform matching
  m.out <- matchit(treat ~ X, data = prop_data, method="nearest",ratio = testControlRatio)
  m.out
  output_data <- match.data(m.out)
  #Save matched banks #
  matched_banks=output_data[c(1,16)]
  #Merge matched banks with full sample
  data_matched=merge(matched_banks,modified_data,by=c('id'))
  return(data_matched)
}

###############Method for plot Propensity Matching################
prop_plot <- function(propdata, num){
  
  pdata <- plm.data(propdata,index=c("id","Time")) # full sample data
  pdata$inter <- pdata$After_DFA * pdata$Affected_BHC
  if(num == 1){
    pdata <- pdata[c(1:4)]
  } else {
    pdata <- pdata[c(1:2,4:5)]
  }
  pdata <- as.data.frame(na.omit(pdata)) # Get rid of missing values
  # collpase trading asset ratio data by time and treat dummy
  pdata <- summaryBy(TAR ~ Affected_BHC+Time, FUN=c(mean,sd), data=pdata)
  # create time variable, t=0 for the quarter during which the Volcker Rule passed
  t=matrix(-20:19,nrow=40,ncol=1)
  pdata <- cbind(pdata,t)
  pdata$Group <- as.character(pdata$Affected_BHC)
  #Plot
  p1=ggplot(pdata, aes(x=t, y=TAR.mean, group=Group,colour=Group))
  p1=p1+geom_line()+geom_point() + geom_smooth(method="loess")+geom_vline(aes(xintercept=0), colour="#BB0000", linetype="dashed")
  return(p1)
}


#Test propensity for 2004 Q3 - Ratio as 1:3
prop_data_1 <- modified_data[which(modified_data$Time == 20040930),]
prop_data_1 <- as.data.frame(na.omit(prop_data_1))
matched_data_1 <- matchData(prop_data_1, 3)

#Test propensity for 2009 Q2 - Ratio as 1:5
prop_data_2 <- modified_data[which(modified_data$Time == 20090630),]
prop_data_2 <- as.data.frame(na.omit(prop_data_2))
matched_data_2 <- matchData(prop_data_2, 5)

#Test propensity for 2004 Q3 - Ratio as 1:5
prop_data_3 <- modified_data[which(modified_data$Time == 20040930),]
prop_data_3 <- as.data.frame(na.omit(prop_data_3))
matched_data_3 <- matchData(prop_data_3, 5)

#Model Propensity checking for Test1
model_prop_1 <- plm(TAR ~ Affected_BHC * After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio, data = matched_data_1, index = c("id", "Time"))
summary(model_prop_1)

#Model Propensity checking for Test2
model_prop_2 <- plm(TAR ~ Affected_BHC * After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio, data = matched_data_2, index = c("id", "Time"))
summary(model_prop_2)

#Model Propensity checking for Test3
model_prop_3 <- plm(TAR ~ Affected_BHC * After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio, data = matched_data_3, index = c("id", "Time"))
summary(model_prop_3)


#######Ploting Propensity Matching########
#Plot Full Sample first
plot <- prop_plot(modified_data, 1)
plot <- plot + labs(title = "Trading asset ratio (full sample)")+labs(x = "Quarter")+labs(y = "Trading asset ratio (percent)")
plot

#Plot Propensity Model 1 - 2004 Q3 - Ratio as 1:3
plot <- prop_plot(matched_data_1, 2)
plot <- plot + labs(title = "Trading asset ratio (2004Q3 with 1:3 Ratio)")+labs(x = "Quarter")+labs(y = "Trading asset ratio (percent)")
plot

#Plot Propensity Model 2 - 2009 Q2 - Ratio as 1:5
plot <- prop_plot(matched_data_2, 2)
plot <- plot + labs(title = "Trading asset ratio (2009Q2 With 1:5 Ratio)")+labs(x = "Quarter")+labs(y = "Trading asset ratio (percent)")
plot

#Plot Propensity Model 3 - 2004 Q3 - Ratio as 1:5
plot <- prop_plot(matched_data_3, 2)
plot <- plot + labs(title = "Trading asset ratio (2004Q3 with 1:5 Ratio)")+labs(x = "Quarter")+labs(y = "Trading asset ratio (percent)")
plot


#Robustness Test - Placebo Test
#Assign banks to control and treatment randomly
#We run the loop multiple times (10 times to see the P-Value)
coefficientVector <- c()
for(i in 1:10){
  modified_data$random_BHC <- sample(c(0,1), size = nrow(modified_data), replace = TRUE)
  model_random <- plm(TAR ~ random_BHC * After_DFA + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = modified_data, index = c("id", "Time"), model = "within")
  coefficientVector[i] <- summary(model_random)$coefficients[12, 4]
}
format(coefficientVector, digits = 4)


###### TOp 10 bank analysis######
#Fetching 10 banks with highest trading ratios
Top10Banks <- (matched_data_1 %>% arrange(desc(TAR)) %>% head(10))[, "id"]
#Fetching other test banks
otherTestBanks <- setdiff((matched_data_1[matched_data_1$Affected_BHC == 1,])[, "id"], Top10Banks)
#Fetching control banks
controlBanks <- matched_data_1[matched_data_1$Affected_BHC == 0,]
#Set Data
top10AnalysisData <- matched_data_1
top10AnalysisData$top10 <- 0
top10AnalysisData[top10AnalysisData$id %in% Top10Banks, "top10"] <- 1
model_Top10 <- plm(TAR ~ (After_DFA * top10) + (After_DFA * Affected_BHC)  + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = top10AnalysisData, index = c("id", "Time"))
summary(model_Top10)


###### Bottom 10 bank analysis######
#Fetching 10 banks with highest trading ratios
bottom10Banks <- (matched_data_1 %>% arrange((TAR)) %>% head(10))[, "id"]
#Fetching other test banks
otherTestBanks <- setdiff((matched_data_1[matched_data_1$Affected_BHC == 1,])[, "id"], bottom10Banks)
#Fetching control banks
controlBanks <- matched_data_1[matched_data_1$Affected_BHC == 0,]
#Set Data
bottom10AnalysisData <- matched_data_1
bottom10AnalysisData$bottom10 <- 0
bottom10AnalysisData[bottom10AnalysisData$id %in% bottom10Banks, "bottom10"] <- 1
model_Bottom10 <- plm(TAR ~ (After_DFA * bottom10) + (After_DFA * Affected_BHC)  + ROA + Leverage_Ratio + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = bottom10AnalysisData, index = c("id", "Time"))
summary(model_Bottom10)

#Extended Inference Tests on whether risk behavior of banks have changed
#Besides testing on whetehr VR has made banks to reduce trading asset ratio, we shall
#also test on whether Banks' general risk taking appetite changed pre & post VR
#We will analyze with the Z_Score_log = log((ROA + Leverage Ratio)/SD_ROA)) which is a normalized
#score on overall bank risk appetite taking account of standard deviation of of ROA
#The standard deveation is calcalted based using 10 quaters moving window
#Natural log is taken as the absolute Z score is highly skewed
#We will take Z_Score-Log as independent variable and regress against
#BHC, DFA, asset trading ratio, time & company fixed effect as well as other control variables

#Z_Score
SD_ROA <- #Need to see how to calculate this
modified_data$SD <- SD_ROA
Z_Score_log <- log((modified_data$ROA + modified_data$Leverage_Ratio)/modified_data$SD)
model_Z <- plm(Z_Score_log ~ Affected_BHC * After_DFA + TAR  + Total_Asset + Credit_Risk_Ratio + Cost_Income_Ratio + Deposit_Ratio + Real_Estate_Ratio + Liquidity_Ratio + CPP, data = modified_data, index = c("id", "Time"))
summary(model_Z)
