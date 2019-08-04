##################################################
##    PREDICT NO SHOW IN MEDICAL APPOINTMENT    ##
##                                              ##
## Author: Maria Bugge                          ##
## Created: 25.07.19                            ##
## Last modified:                               ##
##################################################

# 1 - set up ----------------------------------
df <- read.csv('med_noshow.csv')
library(chron);library(ggplot2)
library(dplyr)
library(caret)

# 2 - Cleaning --------------------------------
df$PatientId <- format(round(df$PatientId),scientific = FALSE)
df$No.show <- ifelse(df$No.show=='Yes',1,0)
df$ScheduledTime<-chron(times=substr(df$ScheduledDay,12,nchar(as.character(df$ScheduledDay))-1),format='H:M:S')
df$ScheduledDay<-as.POSIXct(df$ScheduledDay)
#No time for appointment day so we keep only date
df$AppointmentDay <- as.POSIXct(df$AppointmentDay)
#How many days between scheduled day and appointment day?
df$DaysBetween <- as.numeric(as.character(difftime(df$AppointmentDay,df$ScheduledDay,unit='days')))

#Removing spelling mistakes
colnames(df) <- c('PatientID','AppointmentID','Gender','ScheduledDay','AppointmentDay','Age','Neighborhood',
                  'Scholarship','Hypertension','Diabetes','Alcoholism','Handicap',
                  'SMS_received','No.show','ScheduledTime','DaysBetween')
df <- df[,c('PatientID','AppointmentID','Gender','ScheduledDay','ScheduledTime','AppointmentDay','DaysBetween',
            'Age','Scholarship','Hypertension','Diabetes','Alcoholism','Handicap','SMS_received','No.show')]
#Checking for NAs
df[is.na(df)] #no NA
#Converting the dummies to factors 
df$Scholarship<-as.factor(df$Scholarship)
df$Hypertension <- as.factor(df$Hypertension)
df$Diabetes <- as.factor(df$Diabetes)
df$Alcoholism <- as.factor(df$Alcoholism)
df$Handicap <- as.factor(df$Handicap)
df$SMS_received <- as.factor(df$SMS_received)
df$No.show <- as.factor(df$No.show)
#Checking for outliers:
print(sort(unique(df$Age))) #-1 and 115 seem to be outliers... 
print(sort(unique(df$DaysBetween))) #-6, -1 and all over 160 considered to be outliers
#We will remove the outliers:
df <- df[df$Age>=0 & df$Age < 115 & df$DaysBetween>=0 & df$DaysBetween<160,]

# Checking that binary variables are in fact binary
print(sort(unique(df$Hypertension)));print(sort(unique(df$Diabetes)));print(sort(unique(df$Alcoholism)));
print(sort(unique(df$Scholarship)));print(sort(unique(df$SMS_received)));
print(sort(unique(df$No.show)))


# 3 - Probability of showing up (continuous variables) -------------------

#3a) No show based on days between scheduled and appointment
df_3a <- df %>% group_by(DaysBetween) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
ggplot(data=df_3a,aes(x=DaysBetween,y=prob_noshow)) +geom_point() + geom_smooth(method='lm',formula=y~x)

#3b) No show based on Age
df_3b <- df %>% group_by(Age) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
ggplot(data=df_3b,aes(x=Age,y=prob_noshow)) +geom_point() + 
  geom_smooth(method='lm',formula=y~x) + ylim(0,1)

#3c) No show based on what time (hour) the appointment was scheduled at
df_3c <- df;df_3c$Hour <- substr(df_3c$ScheduledTime,1,2)
df_3c <- df_3c %>% group_by(Hour) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
ggplot(data=df_3c,aes(x=Hour,y=prob_noshow)) +geom_point() + 
  geom_smooth(method='lm',formula=y~x) + ylim(0,1)

#Neither the time of day of scheduled appointment nor the days between scheduled and appointment itself seem to be good indicator of no show
#Age however may be. Indeed, the probability of not showing up decreases with the age. 


# 4 - Probability of showing up (categorical variables) -------------------

#4a) Based on what weekday the appointment was scheduled on
df_4a <- df %>% group_by(weekdays(ScheduledDay)) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
ggplot(data=df_4a,aes(x=`weekdays(ScheduledDay)`,y=prob_noshow)) +geom_bar(stat='identity') + 
  geom_smooth(method='lm',formula=y~x) + ylim(0,1)

#4b) Based on handicap
df_4b <- df %>% group_by(Handicap) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
ggplot(data=df_4b,aes(x=Handicap,y=prob_noshow)) +geom_bar(stat='identity') + 
  geom_smooth(method='lm',formula=y~x) + ylim(0,1)

#4c) Based on remaining dummies (alcoholic,sms_received,diabetes,hypertension,scholarship,gender)
df_4c_1 <- df %>% group_by(Gender) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
df_4c_2<- df %>% group_by(Alcoholism) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
df_4c_3<- df %>% group_by(SMS_received) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
df_4c_4<- df %>% group_by(Diabetes) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
df_4c_5<- df %>% group_by(Hypertension) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))
df_4c_6<- df %>% group_by(Scholarship) %>% summarise (prob_noshow=round(sum(No.show==1)/n(),2))

df_4c_1$Var <- 'Gender';df_4c_2$Var <- 'Alcoholism';df_4c_3$Var <- 'SMS_received'
df_4c_4$Var <- 'Diabetes';df_4c_5$Var <- 'Hypertension';df_4c_6$Var <- 'Scholarship'
colnames(df_4c_1) <- c('Value','prob_noshow','Var');colnames(df_4c_2) <- c('Value','prob_noshow','Var')
colnames(df_4c_3) <- c('Value','prob_noshow','Var');colnames(df_4c_4) <- c('Value','prob_noshow','Var')
colnames(df_4c_5) <- c('Value','prob_noshow','Var');colnames(df_4c_6) <- c('Value','prob_noshow','Var')
binded <- rbind(df_4c_1,df_4c_2,df_4c_3,df_4c_4,df_4c_5,df_4c_6)

ggplot(data=binded)+geom_bar(aes(x=Var,y=prob_noshow,fill=Value),stat='identity',position='dodge')

#Higher probability of not showing up when SMS received = 1 
#Higher probability of not showing up when Scholarship = 1
#Higher probability of not showing up when Hypertension = 0 and diabetes = 0

# 5 - Classification -----------------------------
#based on previous analysis the variables I will use are: 
#      Age
#     ----- Handicap
#      SMS received
#      Scholarship 
#      Hypertension
#      Diabetes

#5a) Preparing data ----
#All our categorical variables need to have binary format (0-1) --> transforming handicap
df$Handicap0 <- ifelse(df$Handicap==0,1,0);df$Handicap1 <- ifelse(df$Handicap==1,1,0)
df$Handicap2 <- ifelse(df$Handicap==2,1,0);df$Handicap3 <- ifelse(df$Handicap==3,1,0)
df$Handicap4 <- ifelse(df$Handicap==4,1,0)

dataset <- df[,c('Age','Handicap0','Handicap1','Handicap2','Handicap3',
                 'SMS_received','Scholarship','Hypertension','Diabetes','No.show')]

#5b) Train/test (75/25)----
train_ind <- sample(seq_len(nrow(dataset)), size = floor(0.75 * nrow(dataset)))
train_set <- dataset[train_ind,]
test_set <- dataset[-train_ind,]

# We will use 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#5c) Algorithm 1: LDA (Linear discriminant analysis)
lda_fitting <- train(No.show~., data=train_set, method="lda", metric=metric, trControl=control)
print(lda_fitting) #accuracy: 0.79 
confusionMatrix(predict(lda_fitting, test_set), test_set$No.show)$table

#5d) Algorithm 2: CART (classification and regression trees)
cart_fitting <- train(No.show~., data=train_set, method="rpart", metric=metric, trControl=control)
print(cart_fitting) #accuracy: 0.79
confusionMatrix(predict(cart_fitting,test_set),test_set$No.show)$table








