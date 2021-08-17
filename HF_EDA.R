# loading the libraries
library(car)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(gplots)
library(plyr) 

library(caret)
library(caTools)
library(glmnet)
library(GGally)
library(DT)
library(pROC)

#loading the data

data=read.csv("heart_failure_clinical_records_dataset.csv")
View(data)


dim(data)
sum(is.na(data)) # no N/A values

#data preprocessing
str(data_clean)

data_clean <- data[complete.cases(data), ]

data_clean$anaemia <- as.factor(mapvalues(data_clean$anaemia,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))
data_clean$diabetes <- as.factor(mapvalues(data_clean$diabetes,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

data_clean$high_blood_pressure <- as.factor(mapvalues(data_clean$high_blood_pressure,
                                                      from=c("0","1"),
                                                      to=c("No", "Yes")))
data_clean$smoking <- as.factor(mapvalues(data_clean$smoking,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))
data_clean$sex <- as.factor(mapvalues(data_clean$sex,
                                      from=c("0","1"),
                                      to=c("Female", "Male")))
data_clean$DEATH_EVENT <- as.factor(mapvalues(data_clean$DEATH_EVENT,
                                              from=c("0","1"),
                                              to=c("No", "Yes")))


attach(data_clean)

############# Visualising distributions ###################
##Categorical Variables
## DEATH_EVENT##

p1<-ggplot(data=data_clean,aes(x = DEATH_EVENT)) +
  geom_bar(stat="count",width=0.7,aes(fill = DEATH_EVENT))+
  geom_text(stat='count', aes(label=..count..), vjust=1,colour="white")
  
p1.1<-ggplot(data_clean, aes(x = DEATH_EVENT)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.. , 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            vjust=1,colour="white")
  

figure <- ggarrange(p1,p1.1,
                    labels = c("Count", "Percentage"),
                    ncol = 2, nrow = 1,
                    common.legend = TRUE)
figure

##others
require(reshape2)

melt.data = melt(select(data_clean,c(2,4,6,10,11,13)),id.vars = "DEATH_EVENT")
head(melt.data)

data %>%
ggplot(data = melt.data, mapping= aes(x = value)) +
  geom_bar(stat="count",width=0.7,fill="#69b3a2") +
  geom_text(stat='count', aes(label=..count..), vjust=1,colour="white")+
  facet_wrap(~ variable, scales = "free")

##########
### Categorical Variables ###
b1<-  ggplot(data_clean, aes(x =sex)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            vjust=1,colour="white",
            position = position_dodge(.1), 
            size = 3)


b2<-  ggplot(data_clean, aes(x =anaemia)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            vjust=1,colour="white",
            position = position_dodge(.1), 
            size = 3)


b3<-  ggplot(data_clean, aes(x =diabetes)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            vjust=1,colour="white",
            position = position_dodge(.1), 
            size = 3)

b4<-  ggplot(data_clean, aes(x =high_blood_pressure)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            vjust=1,colour="white",
            position = position_dodge(.1), 
            size = 3)

b5<- ggplot(data_clean, aes(x =smoking)) +
  geom_bar(aes(fill = DEATH_EVENT)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            vjust=1,colour="white",
            position = position_dodge(.1), 
            size = 3)

figure2 <- ggarrange(b1,b2,b3,b4,b5,
                     #labels = c("Sex", "Anaemia","Diabetes",
                     #           "High Blood Pressure","Smoking"),
                     ncol = 2, nrow = 3,
                     common.legend = TRUE, legend = "bottom")
figure2 
###################################################################
##Numeric variables
#density plots #

require(reshape2)

melt.data = melt(select(data_std,-c(2,4,6,10,11)))
ggplot(data = melt.data, aes(x = value, fill=DEATH_EVENT)) + 
  geom_density(adjust=1.5) +
  facet_wrap(~variable, scales = "free")


#' 
#' 
#' 
#boxplots # --- Identifying outliers ---
melt.data = melt(data_clean)
head(melt.data)

ggplot(data = melt.data, aes(y=value)) + 
  geom_boxplot(outlier.color="red") + 
  facet_wrap(~variable, scales = "free")

####


##Covariation###

c<-ggplot(data = data_clean, mapping = aes(x = DEATH_EVENT, y = time,fill=DEATH_EVENT)) +
  geom_boxplot()+ coord_flip()+theme_minimal()

c1<-ggplot(data = data_clean, mapping = aes(x = DEATH_EVENT, y = serum_creatinine,fill=DEATH_EVENT)) +
  geom_boxplot()+ coord_flip()+theme_minimal()
c2<-ggplot(data = data_clean, mapping = aes(x = DEATH_EVENT, y = age,fill=DEATH_EVENT)) +
  geom_boxplot()+ coord_flip()+theme_minimal()
c3<-ggplot(data = data_clean, mapping = aes(x = DEATH_EVENT, y = ejection_fraction,fill=DEATH_EVENT)) +
  geom_boxplot()+ coord_flip()+theme_minimal()

figure3 <- ggarrange(c,c1,c2,c3,
                     #labels = c("Sex", "Anaemia","Diabetes",
                     #           "High Blood Pressure","Smoking"),
                     ncol = 2, nrow = 2,
                     common.legend = TRUE, legend = "bottom")
figure3 



## correlations ##
###

library(GGally)
ggcorr(data, method = c("everything", "pearson"),
       hjust = 0.75, size = 2.75, color = "grey50",
       label_alpha= TRUE,
       label = TRUE, label_size = 2, layout.exp= 0,) 

a=chisq.test(smoking, sex, correct=FALSE)

######################################### 
 


