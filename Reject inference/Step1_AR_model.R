##libraries

library(sqldf)
library(Hmisc)
library(ROCR)
library(Rcmdr)

## Reading data-----------------------------------------------------------------
rd <- read.csv("D:/R-projects/Reject Inference/Raw Data Reject Inference v1.csv", sep=";", quote="")

## Adding Approved target variable----------------------------------------------
rd$approved<-ifelse(rd$decision=='APPROVED',1,0)

## Adding month of decision variable----------------------------------------------
rd$month_decision<-substring(rd$decision_date,4,10)

## Approved/Reject Investigation by Months----------------------------------------
#Approved/Reject odds are stable over period by months with about 0.44%
#some high value at 11.2014 can be explained by lower number of applicants than in other periods

ar<-table(rd$decision,rd$month_decision)
ratio<-ar["APPROVED",]/ar["REJECTED",]
name<-c("08.2014","09.2014","10.2014","11.2014","12.2014","01.2015","02.2015","03.2015","04.2015","05.2015")
barplot(ratio[name])
barplot(ar[,name])
ar

## IV calculation--------------------------------------------------------------
# 0.05 is a minimum value I used to take a predictor for futher analysis
IV_all(rd,"approved")
res

## Constructing new predictors--------------------------------------------------
#gender_empl_type - combination of Gender and Employment_type
#monthly_debt - ratio of requested Amount and Term
#empl_len_age - ratio of Employment_length and Age of client
rd$gender_empl_type<-as.factor(paste0(rd$employment_type,"_",rd$gender))
rd$monthly_debt<-rd$amount/rd$term
rd$empl_len_age<-rd$employment_length/rd$client_age_at_application_date

## WOE grouping-----------------------------------------------------------------
# Procedure moving from original values of predictors to their Weight of Evidence
WOE(rd,"amount","approved")
rd$grp_amount<-cut(rd$amount,breaks=c(100,400,500,600,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_amount<-ifelse(rd$amount<400,-0.6896799,ifelse(rd$amount<500,-0.3696245,ifelse(rd$amount<600,-0.1112442,0.2899255)))

WOE(rd,"grp_client_age_at_application_date","approved")
rd$grp_client_age_at_application_date<-cut(rd$client_age_at_application_date,breaks=c(18,25,29,32,35,38,47,55,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_client_age_at_application_date<-ifelse(is.na(rd$client_age_at_application_date)==TRUE,-1.5913114,ifelse(rd$client_age_at_application_date<25,-1.5913114,ifelse(rd$client_age_at_application_date<29,-0.4270438,ifelse(rd$client_age_at_application_date<32,-0.2465131,ifelse(rd$client_age_at_application_date<35,-0.1103281,ifelse(rd$client_age_at_application_date<38,0.4387710,ifelse(rd$client_age_at_application_date<47,0.8304404,ifelse(rd$client_age_at_application_date<55,0.7014681,0.4044341))))))))

WOE(rd,"gender","approved")
rd$woe_gender<-ifelse(rd$gender=="Male",-0.4423322,0.5370756)

WOE(rd,"grp_income","approved")
rd$grp_income<-cut(rd$income,breaks=c(0,1199,1500,2000,2500,3000,Inf),include.lowest=TRUE, right=TRUE)
rd$woe_income<-ifelse(rd$income<1200,-1.6021450,ifelse(rd$income<=1500,-0.4821421,ifelse(rd$income<=2000,-0.2103799,ifelse(rd$income<2500,0.1757034,ifelse(rd$income<3000,0.4893917,0.7608488)))))

WOE(rd,"grp_employment_type","approved")
rd$grp_employment_type<-as.factor(ifelse(rd$employment_type %in% c("1","2","4"),paste(rd$employment_type),"other"))
rd$woe_employment_type<-ifelse(rd$employment_type==1,0,ifelse(rd$employment_type==2,0.7366256429,ifelse(rd$employment_type==4,-0.998305081,-0.9721874903)))

WOE(rd,"grp_employment_length","approved")
rd$grp_employment_length<-ifelse(rd$employment_length>=3,9,rd$employment_length)
rd$woe_employment_length<-ifelse(rd$employment_length==0,-0.1231829,ifelse(rd$employment_length==1,0.2301834,ifelse(rd$employment_length==2,-0.1071930,-0.5192142)))

WOE(rd,"grp_zip_part","approved")
#rd$grp_zip_part<-as.factor(ifelse(rd$zip_part %in% c(11,15,58,38,41,59,78,82,26,44,40,37,97,83,22,42,87,66,63,21,34,64,86,20,62,33,32,31,80,9,85,5,2),rd$zip_part,0))
rd$grp_zip_part<-as.factor(ifelse(rd$zip_part %in% c(13,11,19,17,18,15,74,12,73,16),100,ifelse(rd$zip_part %in% c(48,57,58,14,23,75,10,47,56,46,76),101,ifelse(rd$zip_part %in% c(38,41,59,50),102,ifelse(rd$zip_part %in% c(78,77,72,70,43,82,26,44,40),103,ifelse(rd$zip_part %in% c(28,27,54,68,98,37,25,97,83,96),104,ifelse(rd$zip_part %in% c(22,42,55,88,87,66,63,39,21),105,ifelse(rd$zip_part %in% c(89,34,99,64,86,20,84,62),106,ifelse(rd$zip_part %in% c(33,32,60,61,35,30,67,31,80,95),107,ifelse(rd$zip_part %in% c(9,7,91,81,85,6,8,5,1,2,3),108,109))))))))))
rd$woe_zip_part<-ifelse(rd$zip_part %in% c(13,11,19,17,18,15,74,12,73,16),-0.565438295,ifelse(rd$zip_part %in% c(48,57,58,14,23,75,10,47,56,46,76),-0.379247064,ifelse(rd$zip_part %in% c(38,41,59,50),-0.272551336,ifelse(rd$zip_part %in% c(78,77,72,70,43,82,26,44,40),-0.163347673,ifelse(rd$zip_part %in% c(28,27,54,68,98,37,25,97,83,96),-0.075991626,ifelse(rd$zip_part %in% c(22,42,55,88,87,66,63,39,21),-0.008118921,ifelse(rd$zip_part %in% c(89,34,99,64,86,20,84,62),0.095713497,ifelse(rd$zip_part %in% c(33,32,60,61,35,30,67,31,80,95),0.227669368,ifelse(rd$zip_part %in% c(9,7,91,81,85,6,8,5,1,2,3),0.570465292,0.203250247)))))))))

WOE(rd,"grp_phone_part","approved")
rd$grp_phone_part<-as.factor(ifelse(rd$phone_part=="+48 60","60",ifelse(rd$phone_part=="+48 50","50",ifelse(rd$phone_part %in% c("+48 69","+48 66"),"66_69",ifelse(rd$phone_part %in% c("+48 79","+48 51","+48 78","+48 88"),"51_78_79_88",ifelse(rd$phone_part %in% c("+48 72","+48 53"),"53_72","others"))))))
rd$woe_phone_part<-ifelse(rd$phone_part=="+48 60",0.5511591,ifelse(rd$phone_part=="+48 50",0.2671050,ifelse(rd$phone_part %in% c("+48 69","+48 66"),0.1495065,ifelse(rd$phone_part %in% c("+48 79","+48 51","+48 78","+48 88"),-0.1849348,ifelse(rd$phone_part %in% c("+48 72","+48 53"),-0.3179759,-0.6799441)))))

WOE(rd,"grp_email_domain","approved")
rd$grp_email_domain<-as.factor(ifelse(rd$email_domain %in% c("poczta.onet","op","tlen"),"poczta.onet_op_tlen",ifelse(rd$email_domain=="onet","onet",ifelse(rd$email_domain %in% c("interia","wp"),"interia_wp",ifelse(rd$email_domain %in% c("gmail","o2"),"gmail_o2","others")))))
rd$woe_email_domain<-ifelse(rd$email_domain %in% c("poczta.onet","op","tlen"),0.2873584199,ifelse(rd$email_domain=="onet",-0.637541049,ifelse(rd$email_domain %in% c("interia","wp"),0.0004437913,ifelse(rd$email_domain %in% c("gmail","o2"),-0.1238708908,0.0869890525))))

WOE(rd,"grp_days_since_registration","approved")
rd$grp_days_since_registration<-cut(rd$days_since_registration,breaks=c(-Inf,1,31,91,231,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_days_since_registration<-ifelse(rd$days_since_registration<1,0.2727763,ifelse(rd$days_since_registration<31,-0.3481104,ifelse(rd$days_since_registration<91,-0.4369743,ifelse(rd$days_since_registration<231,-0.8285291,-1.2552323))))

WOE(rd,"grp_days_since_previous_reject","approved")
rd$grp_days_since_previous_reject<-cut(rd$days_since_previous_reject,breaks=c(1,16,81,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_days_since_previous_reject<-ifelse(is.na(rd$days_since_previous_reject)==TRUE | rd$days_since_previous_reject==0,0.3021293,ifelse(rd$days_since_previous_reject<16,-0.2296434,ifelse(rd$days_since_previous_reject<81,-0.9762595,-0.7248809)))

WOE(rd,"grp_number_of_rejected_1m","approved")
rd$grp_number_of_rejected_1m<-ifelse(rd$number_of_rejected_1m==0,0,1)
rd$woe_number_of_rejected_1m<-ifelse(rd$number_of_rejected_1m==0,0.1052549,-0.5382254)

WOE(rd,"number_of_rejected_3m","approved")
rd$woe_number_of_rejected_3m<-ifelse(rd$number_of_rejected_3m==0,0.1927243,ifelse(rd$number_of_rejected_3m==1,-0.4188721,ifelse(rd$number_of_rejected_3m==2,-0.7818705,-1.3392023)))

WOE(rd,"number_of_rejected_6m","approved")
rd$woe_number_of_rejected_6m<-ifelse(rd$number_of_rejected_6m==0,0.2317443,ifelse(rd$number_of_rejected_6m==1,-0.3456619,ifelse(rd$number_of_rejected_6m==2,-0.6871606,-1.4150477)))

WOE(rd,"woe_debt_to_income_ratio","approved")
rd$grp_debt_to_income_ratio<-cut(rd$debt_to_income_ratio,breaks=c(0,0.12,0.2,0.25,0.37,0.44,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_debt_to_income_ratio<-ifelse(is.na(rd$debt_to_income_ratio)==TRUE | rd$debt_to_income_ratio>=0.44,-0.8541004,ifelse(rd$debt_to_income_ratio>=0.37,-0.3137335,ifelse(rd$debt_to_income_ratio>=0.25,-0.1448825,ifelse(rd$debt_to_income_ratio>=0.2,0.2358234,ifelse(rd$debt_to_income_ratio>=0.12,0.3438288,0.1559157)))))

WOE(rd,"grp_monthly_debt","approved")
rd$grp_monthly_debt<-cut(rd$monthly_debt,breaks=c(0,12,16,18,21,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_monthly_debt<-ifelse(rd$monthly_debt<12,-0.8416235,ifelse(rd$monthly_debt<16,-0.4058530,ifelse(rd$monthly_debt<18,-0.1336418,ifelse(rd$monthly_debt<21,0.2509537,0.3806600))))

WOE(rd,"grp_empl_len_age","approved")
rd$grp_empl_len_age<-cut(rd$empl_len_age,breaks=c(-Inf,0.015,0.025,0.03,0.06,0.09,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_empl_len_age<-ifelse(rd$empl_len_age<0.015,-0.1232662,ifelse(rd$empl_len_age<0.025,0.9457746,ifelse(rd$empl_len_age<0.03,0.7011764,ifelse(rd$empl_len_age<0.06,-0.1607778,ifelse(rd$empl_len_age<0.09,-0.3508342,-0.9238905)))))

WOE(rd,"grp_gender_empl_type","approved")
rd$grp_gender_empl_type<-as.factor(ifelse(rd$gender_empl_type %in% c("2_Female","1_Female"),"1_2_F",ifelse(rd$gender_empl_type %in% c("2_Male"),"2_M",ifelse(rd$gender_empl_type %in% c("4_Female","9_Female","11_Female"),"4_9_11_F",ifelse(rd$gender_empl_type %in% c("1_Male"),"1_M","others")))))
rd$woe_gender_empl_type<-ifelse(rd$gender_empl_type %in% c("2_Female","1_Female"),0.7063853,ifelse(rd$gender_empl_type %in% c("2_Male"),0.4195826,ifelse(rd$gender_empl_type %in% c("4_Female","9_Female","11_Female"),0.1655805,ifelse(rd$gender_empl_type %in% c("1_Male"),-0.4953875,-1.1642933))))

# WOE END---------------------------------------------------------------------------


## select WOE variables for further analysis--------------------------------
ar_var <- colnames(rd[,colnames(rd)>'woe' & colnames(rd)<'wof'])

##correlation check---------------------------------------------------------------
corr_matrix<-as.matrix(cor(rd[,ar_var]))
corr_matrix>0.6 & corr_matrix<1
res[order(res),]

## Sample splitting randomly on Training and Testing set with 70/30 proportion---------------------
table(rd$month_decision,rd$approved)
inTrain<-createDataPartition(y=rd$approved,p=0.7,list=FALSE)
train<-rd[inTrain,]
test<-rd[-inTrain,]

## Approval rate check over total and splitted samples--------------------------------
# Approval rate in stable in each subset - 0.306 - 0.307.
sum(rd$approved)/nrow(rd)
sum(train$approved)/nrow(train)
sum(test$approved)/nrow(test)

## LOGISTIC REGRETION TRAINING--------------------------------------------------------
#All variables in model
ar_model<-glm(approved~woe_amount+woe_client_age_at_application_date+woe_gender+woe_income+woe_employment_type+woe_employment_length+woe_zip_part+woe_phone_part+woe_email_domain+woe_days_since_registration+woe_days_since_previous_reject+woe_number_of_rejected_1m+woe_number_of_rejected_3m+woe_number_of_rejected_6m+woe_debt_to_income_ratio+woe_monthly_debt+woe_empl_len_age+woe_gender_empl_type,data=train,family=binomial)
summary(ar_model, cor=TRUE)

# Exclude high correlation (>0.6) and p(1-significant)>0.1 variables
ar_model<-glm(approved~woe_client_age_at_application_date+woe_zip_part+woe_phone_part+woe_email_domain+woe_days_since_previous_reject+woe_debt_to_income_ratio+woe_monthly_debt+woe_gender_empl_type,data=train,family=binomial)
summary(ar_model, cor=TRUE)
ar_model_coef<-coefficients(summary(ar_model))

#ROC curve plot and AUC calculation
train$score<-predict(ar_model)
pred<-prediction(train$score,train$approved)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC plot AR model")
auc.tmp<-performance(pred,"auc")
ar_auc<-as.numeric(auc.tmp@y.values)
ar_auc

# Stepwise variable selection
# include a predictor to the model till it leads to increase AUC 
ar_model_step<-stepwise(ar_model,direction="forward",criterion='AIC')
summary(ar_model_step, cor=TRUE)
ar_model_step_coef<-coefficients(summary(ar_model_step))

# SCORING THE SAMPLE-------------------------------------------------------------
train$score<-predict(ar_model_step)
train$prob<-predict(ar_model_step,type=c("response"))

# MODEL VALIDATION
train$grp_score<-cut2(train$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(approved) as good, sum(1-approved) as bad, count(*) as total from train group by 1 order by 2")
#copy result to excel

#TEST Sample Validation----------------------------------------------------------
test$score<-predict(ar_model_step,test)
test$grp_score<-cut2(test$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(approved) as good, sum(1-approved) as bad, count(*) as total from test group by 1 order by 2")

#----------------------------------------------------------------------
# Train 2 other models on 01-04.2015 subsample and 03-04.2015 subsample 
# and take a mean estimate for predictors ------------------------------------------------------
# TRAIN only on 01.02.03.04.2015 sample
train_15<-train[train$month_decision %in% c("01.2015","02.2015","03.2015","04.2015"),]
ar_model<-glm(approved~woe_client_age_at_application_date+woe_bank+woe_zip_part+woe_phone_part+woe_email_domain+woe_days_since_previous_reject+woe_debt_to_income_ratio+woe_monthly_debt+woe_empl_len_age+woe_gender_empl_type,data=train_15,family=binomial)

# Stepwise variable selection
ar_model_step<-stepwise(ar_model,direction="forward",criterion='AIC')
summary(ar_model_step, cor=TRUE)

# AUC calculation
train_15$score<-predict(ar_model_step)
pred<-prediction(train_15$score,train_15$approved)
auc.tmp<-performance(pred,"auc")
ar_auc<-as.numeric(auc.tmp@y.values)
ar_auc

# TRAIN only on 03.04.2015 sample---------------------------------------------------------------
train_15_34<-train[train$month_decision %in% c("03.2015","04.2015"),]
ar_model<-glm(approved~woe_client_age_at_application_date+woe_bank+woe_zip_part+woe_phone_part+woe_email_domain+woe_days_since_previous_reject+woe_debt_to_income_ratio+woe_monthly_debt+woe_empl_len_age+woe_gender_empl_type,data=train_15_34,family=binomial)
summary(ar_model, cor=TRUE)

# Stepwise variable selection
ar_model_step<-stepwise(ar_model,direction="forward",criterion='AIC')
summary(ar_model_step, cor=TRUE)

#AUC calculation
train_15_34$score<-predict(ar_model_step)
pred<-prediction(train_15_34$score,train_15_34$approved)
auc.tmp<-performance(pred,"auc")
ar_auc<-as.numeric(auc.tmp@y.values)
ar_auc
#--------------------------------------------------------------------------------------------

# Total model Estimates as average of three models
ar_model_avg<-ar_model
ar_model_avg_coef<-ar_model_coef
ar_model_avg_coef[,1]<-c(-0.676066667,0.73097,1.012393333,0.79573,0.632406667,0.336503333,0.65805,1.09877,1.1054366667,0.870563333)
ar_model_avg[[1]]<-c(ar_model_avg_coef[,1])
ar_model_avg
#-------------------------------------------
# AVG model validation by month
test_04_2015<-rd[rd$month_decision=="04.2015",]
test_04_2015$score<-predict(ar_model_avg,test_04_2015)
test_04_2015$grp_score<-cut2(test_04_2015$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(approved) as good, sum(1-approved) as bad, count(*) as total from test_04_2015 group by 1 order by 2")

# AVG model validation on test subsample 08-09.2014
# this subsample never been included into training set----------------------------------------------------------
test_2014<-test[test$month_decision %in% c("08.2014","09.2014","10.2014","11.2014","12.2014"),]
test_2014$score<-predict(ar_model_avg,test_2014)
test_2014$grp_score<-cut2(test_2014$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(approved) as good, sum(1-approved) as bad, count(*) as total from test_2014 group by 1 order by 2")

# Scorecard constructing--------------------------------------------------------
sqldf("select grp_client_age_at_application_date, sum(approved), sum(1-approved),count(*), woe_client_age_at_application_date from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_zip_part, sum(approved), sum(1-approved),count(*), woe_zip_part from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_phone_part, sum(approved), sum(1-approved),count(*), woe_phone_part from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_email_domain, sum(approved), sum(1-approved),count(*), woe_email_domain from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_days_since_previous_reject, sum(approved), sum(1-approved),count(*), woe_days_since_previous_reject from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_debt_to_income_ratio, sum(approved), sum(1-approved),count(*), woe_debt_to_income_ratio from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_monthly_debt, sum(approved), sum(1-approved),count(*), woe_monthly_debt from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_empl_len_age, sum(approved), sum(1-approved),count(*), woe_empl_len_age from test_04_2015 group by 1,5 order by 1")
sqldf("select grp_gender_empl_type, sum(approved), sum(1-approved),count(*), woe_gender_empl_type from test_04_2015 group by 1,5 order by 1")

#Population Stability Index ---------------------------------------------------------
#Split 04_2015 subset into 10 equal populated bins and use these breaks for all other periods
# calculate PSI using formula in excel
test_04_2015<-rd[rd$month_decision=="08.2014",]
test_04_2015$score<-predict(ar_model_avg,test_04_2015)
test_04_2015$grp_score<-cut(test_04_2015$score,breaks=c(-Inf,-3.0092,-2.4603,-2.0622,-1.6802,-1.2776,-0.8565,-0.448,0.0144,0.5925,Inf),include.lowest=TRUE, right=FALSE)
sqldf("select grp_score, median(score) med_score, count(*) as total from test_04_2015 group by 1 order by 2")


## Scoring total sample----------------------------------------------------------------
#assign acc_prob as probability to be accepted (Approved)
rd$acc_prob<-predict(ar_model_avg,rd)

