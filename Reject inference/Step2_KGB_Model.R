##libraries

library(sqldf)
library(Hmisc)
library(caret)
library(ROCR)
library(Rcmdr)

## Target selecting , target2 was selected as more appropriate Good/Bad classifier 
summary(rd$target1)

table(rd$target1,rd$target2)
sqldf("select issue_date,settledate,max_delay,estended_days,decision,current_behaviour,reason,processed_by from rd where target1='BAD' and target2='BAD' limit 10")
table(rd[rd$target2=="BAD","month_decision"])
table(rd$target2,rd$current_behaviour)

## Adding taget "BAD" variable
rd$bad<-ifelse(rd$target2=="BAD",1,ifelse(rd$target2=="GOOD",0,-1))
table(rd$bad,rd$target2)
table(rd$bad,rd$month_decision)

## Creating subset without target2 is UNKNOWN
kgb<-rd[rd$bad!=-1,]

## Approved/Reject Investigation by Months----------------------------------------
#BAD rate is stable over period by months up to 01.2015 with about 0.18% value
br<-table(kgb$target2,kgb$month_decision)
ratio<-br["BAD",]/(br["GOOD",]+br["BAD",])
name<-c("08.2014","09.2014","10.2014","11.2014","12.2014","01.2015","02.2015","03.2015","04.2015","05.2015")
barplot(ratio[name])
barplot(br[,name])
br

#AS we don't have full information since 02.2015 (didn't pass 90 days) I use subset of 08.2014-01.2015 for further analysis
kgb<-rd[rd$bad!=-1 & rd$month_decision %in% c("08.2014","09.2014","10.2014","11.2014","12.2014","01.2015"),]
name<-c("decision_date","issue_date","settledate","max_delay","estended_days","decision","current_behaviour","amount","loan_fees","term","application_day_of_month","application_time_of_day","client_age_at_application_date","gender","bank","income","working_time","employment_type","employment_length","official_document_type","newsletter","zipcode","zip_part","phone","phone_part","email_domain","days_since_registration","days_since_previous_reject","number_of_rejected_1m","number_of_rejected_3m","number_of_rejected_6m","debt_to_income_ratio","reason","processed_by","target1","target2","bad","month_decision","gender_empl_type","monthly_debt","empl_len_age")
kgb<-kgb[,name]

## IV calculation--------------------------------------------------------------
# 0.05 is a minimum value I used to take a predictor for futher analysis
IV_all(kgb,"bad")
res

## WOE grouping-----------------------------------------------------------------
# Procedure moving from original values of predictors to their Weight of Evidence
WOE(kgb,"grp_phone_part","bad")
kgb$grp_phone_part<-as.factor(ifelse(kgb$phone_part %in% c("+48 60"),"60",ifelse(kgb$phone_part %in% c("+48 50","+48 66"),"50_66",ifelse(kgb$phone_part=="+48 69","69",ifelse(kgb$phone_part=="+48 79","79",ifelse(kgb$phone_part %in% c("+48 88","+48 78"),"78_88",ifelse(kgb$phone_part %in% c("+48 72","+48 51"),"51_72","others")))))))
kgb$woe_phone_part<-ifelse(kgb$phone_part %in% c("+48 60"),-0.62785969,ifelse(kgb$phone_part %in% c("+48 50","+48 66"),-0.31090238,ifelse(kgb$phone_part=="+48 69",-0.10618977,ifelse(kgb$phone_part=="+48 79",0.03273308,ifelse(kgb$phone_part %in% c("+48 88","+48 78"),0.27966386,ifelse(kgb$phone_part %in% c("+48 72","+48 51"),0.41460250,0.64178374))))))

WOE(kgb,"grp_email_domain","bad")
kgb$grp_email_domain<-as.factor(ifelse(kgb$email_domain %in% c("poczta","interia"),"poczta_interia",ifelse(kgb$email_domain %in% c("gmail","op"),"gmail_op",ifelse(kgb$email_domain %in% c("wp","vp"),"wp_vp",ifelse(kgb$email_domain %in% c("onet","o2"),"onet_o2","others")))))
kgb$woe_email_domain<-ifelse(kgb$email_domain %in% c("poczta","interia"),-0.06967765,ifelse(kgb$email_domain %in% c("gmail","op"),-0.02527276,ifelse(kgb$email_domain %in% c("wp","vp"),0.02260385,ifelse(kgb$email_domain %in% c("onet","o2"),0.23996429,-0.30453309))))

WOE(kgb,"grp_loan_fees","bad")
kgb$grp_loan_fees<-ifelse(kgb$loan_fees==0,0,ifelse(kgb$loan_fees<131,130,999))
kgb$woe_loan_fees<-ifelse(kgb$loan_fees==0,-0.1948844,ifelse(kgb$loan_fees<131,0.7238935,0.9220361))

WOE(kgb,"grp_zip_part","bad")
kgb$grp_zip_part<-as.factor(ifelse(kgb$zip_part %in% c("30","66","72","27","39","60","55","0","4"),"0_4_30_66_60_55_72_27_39",ifelse(kgb$zip_part %in% c("2","1","83","80","20","61","3","31","11"),"2_1_83_80_20_61_3_31_11",ifelse(kgb$zip_part %in% c("67","64","34","82","41","42","33","26","8","43","59","97","50","93"),"67_64_34_82_41_42_33_26_8_43_59_97_50_93",ifelse(kgb$zip_part %in% c("5","7","21","85","62","87","38","95","22"),"5_7_21_85_62_87_38_95_22",ifelse(kgb$zip_part %in% c("96","86","81","58","44","14"),"96_86_81_58_44_14",ifelse(kgb$zip_part %in% c("40","89","63","37","76","9","88","98"),"40_89_63_37_76_9_88_98","others")))))))
kgb$woe_zip_part<-ifelse(kgb$zip_part %in% c("30","66","72","27","39","60","55","0","4"),-0.46978542,ifelse(kgb$zip_part %in% c("2","1","83","80","20","61","3","31","11"),-0.18497567,ifelse(kgb$zip_part %in% c("67","64","34","82","41","42","33","26","8","43","59","97","50","93"),-0.01563053,ifelse(kgb$zip_part %in% c("5","7","21","85","62","87","38","95","22"),0.10132964,ifelse(kgb$zip_part %in% c("96","86","81","58","44","14"),0.21320665,ifelse(kgb$zip_part %in% c("40","89","63","37","76","9","88","98"),0.40508309,-0.07305275))))))

WOE(kgb,"newsletter","bad")
kgb$woe_newsletter<-ifelse(kgb$newsletter=="t",-0.1120533,0.6838745)

WOE(kgb,"grp_gender_empl_type","bad")
kgb$grp_gender_empl_type<-as.factor(ifelse(kgb$gender_empl_type %in% c("9_Female","1_Female","2_Female"),"9_1_2_F",ifelse(kgb$gender_empl_type %in% c("11_Female","2_Male","1_Male"),"11_F_2_1_M","others")))
kgb$woe_gender_empl_type<-ifelse(kgb$gender_empl_type %in% c("9_Female","1_Female","2_Female"),-0.1491338,ifelse(kgb$gender_empl_type %in% c("11_Female","2_Male","1_Male"),0.1260128,0.2827210))

WOE(kgb,"grp_monthly_debt","bad")
kgb$grp_monthly_debt<-cut(kgb$monthly_debt,breaks=c(0,15,21,Inf),include.lowest=TRUE, right=FALSE)
kgb$woe_monthly_debt<-ifelse(kgb$monthly_debt<15,-0.60816855,ifelse(kgb$monthly_debt<21,-0.02991389,0.43370119))

WOE(kgb,"woe_debt_to_income_ratio","bad")
kgb$grp_debt_to_income_ratio<-cut(kgb$debt_to_income_ratio,breaks=c(0,0.11,0.17,0.24,0.33,Inf),include.lowest=TRUE, right=FALSE)
kgb$woe_debt_to_income_ratio<-ifelse(is.na(kgb$debt_to_income_ratio)==TRUE,0.1939908881,ifelse(kgb$debt_to_income_ratio<0.11,-0.4524117848,ifelse(kgb$debt_to_income_ratio<0.17,-0.1325414055,ifelse(kgb$debt_to_income_ratio<0.24,0.0001737593,ifelse(kgb$debt_to_income_ratio<0.33,0.1274637733,0.1939908881)))))

WOE(kgb,"grp_client_age_at_application_date","bad")
kgb$grp_client_age_at_application_date<-cut(kgb$client_age_at_application_date,breaks=c(0,27,30,34,44,55,Inf),include.lowest=TRUE, right=FALSE)
kgb$woe_client_age_at_application_date<-ifelse(kgb$client_age_at_application_date<27,0.37457025,ifelse(kgb$client_age_at_application_date<30,0.13606831,ifelse(kgb$client_age_at_application_date<34,-0.03197480,ifelse(kgb$client_age_at_application_date<44,-0.15971984,ifelse(kgb$client_age_at_application_date<55,-0.07610115,0.12355278)))))

WOE(kgb,"grp_term","bad")
kgb$grp_term<-cut(kgb$term,breaks=c(0,30,Inf),include.lowest=TRUE, right=FALSE)
kgb$woe_term<-ifelse(kgb$term<30,0.34657728,-0.08339255)

WOE(kgb,"grp_days_since_previous_reject","bad")
kgb$grp_days_since_previous_reject<-ifelse(is.na(kgb$days_since_previous_reject)==TRUE,0,ifelse(kgb$days_since_previous_reject<30,30,99))
kgb$woe_days_since_previous_reject<-ifelse(is.na(kgb$days_since_previous_reject)==TRUE,-0.05685488,ifelse(kgb$days_since_previous_reject<30,0.33343435,0.07025949))

WOE(kgb,"gender","bad")
kgb$woe_gender<-ifelse(kgb$gender=="Female",-0.09748922,0.13351925)

WOE(kgb,"grp_income","bad")
kgb$grp_income<-cut(kgb$income,breaks=c(0,1601,2220,3000,4500,Inf),include.lowest=TRUE, right=FALSE)
kgb$woe_income<-ifelse(kgb$income<1601,0.13395205,ifelse(kgb$income<2220,0.08268278,ifelse(kgb$income<3000,-0.02979479,ifelse(kgb$income<4500,-0.06462979,-0.28007008))))

WOE(kgb,"days_since_registration","bad")
kgb$grp_days_since_registration<-as.factor(ifelse(kgb$days_since_registration==0,"0",ifelse(kgb$days_since_registration<51,"1-51",">51")))
kgb$woe_days_since_registration<-ifelse(kgb$days_since_registration==0,-0.05946505,ifelse(kgb$days_since_registration<51,0.35214180,0.19070781))

WOE(kgb,"grp_number_of_rejected_6m","bad")
kgb$grp_number_of_rejected_6m<-as.factor(ifelse(kgb$number_of_rejected_6m==0,"0","1"))
kgb$woe_number_of_rejected_6m<-ifelse(kgb$number_of_rejected_6m==0,-0.05753602,0.24642831)


## select WOE variables for further analysis--------------------------------
kgb_var <- colnames(kgb[,colnames(kgb)>'woe' & colnames(kgb)<'wof'])
kgb_var

##correlation check---------------------------------------------------------------
corr_matrix<-as.matrix(cor(kgb[,kgb_var]))
corr_matrix>0.6 & corr_matrix<1

## Sample splitting randomly on Training and Testing set with 70/30 proportion---------------------
table(kgb$month_decision,kgb$bad)
inTrain_kgb<-createDataPartition(y=kgb$bad,p=0.7,list=FALSE)
train_kgb<-kgb[inTrain_kgb,]
test_kgb<-kgb[-inTrain_kgb,]

## Bad rate check over total and splitted samples--------------------------------
# Bad rate in stable in each subset - 0.180 - 0.182.
sum(kgb$bad)/nrow(kgb)
sum(train_kgb$bad)/nrow(train_kgb)
sum(test_kgb$bad)/nrow(test_kgb)

## LOGISTIC REGRETION TRAINING--------------------------------------------------------
#All variables in model
kgb_model<-glm(bad~woe_phone_part+woe_email_domain+woe_loan_fees+woe_zip_part+woe_newsletter+woe_gender_empl_type+woe_monthly_debt+woe_debt_to_income_ratio+woe_client_age_at_application_date+woe_term+woe_days_since_previous_reject+woe_gender+woe_income+woe_days_since_registration+woe_number_of_rejected_6m,data=train_kgb,family=binomial)
summary(kgb_model, cor=TRUE)

# Exclude high correlation (>0.6) and p(1-significant)>0.1 variables
kgb_model<-glm(bad~woe_phone_part+woe_email_domain+woe_loan_fees+woe_zip_part+woe_gender_empl_type+woe_monthly_debt+woe_debt_to_income_ratio+woe_client_age_at_application_date+woe_term+woe_days_since_previous_reject+woe_days_since_registration,data=train_kgb,family=binomial)
summary(kgb_model, cor=TRUE)
kgb_model_coef<-coefficients(summary(kgb_model))

#ROC curve plot and AUC calculation
train_kgb$score<-predict(kgb_model)
pred<-prediction(train_kgb$score,train_kgb$bad)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC plot KGB model")
auc.tmp<-performance(pred,"auc")
kgb_auc<-as.numeric(auc.tmp@y.values)
kgb_auc


# Stepwise variable selection
# include a predictor to the model till it leads to increase AUC 
kgb_model_step<-stepwise(kgb_model,direction="forward",criterion='AIC')
summary(kgb_model_step, cor=TRUE)
kgb_model_step_coef<-coefficients(summary(kgb_model_step))

pred<-prediction(train_kgb$score,train_kgb$bad)
auc.tmp<-performance(pred,"auc")
kgb_auc<-as.numeric(auc.tmp@y.values)
kgb_auc

# MODEL VALIDATION
# SCORING THE SAMPLE-------------------------------------------------------------
train_kgb$score<-predict(kgb_model_step)
train_kgb$prob<-predict(kgb_model_step,type=c("response"))

train_kgb$grp_score<-cut2(train_kgb$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(1-bad) as good, sum(bad) as bad, count(*) as total from train_kgb group by 1 order by 2")
#copy result to excel

#TEST Sample Validation----------------------------------------------------------
test_kgb$score<-predict(kgb_model_step,test_kgb)
test_kgb$grp_score<-cut2(test_kgb$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(1-bad) as good, sum(bad) as bad, count(*) as total from test_kgb group by 1 order by 2")

# re-shaping training and testing set and re-building model 2 times
inTrain_kgb<-createDataPartition(y=kgb$bad,p=0.7,list=FALSE)
train_kgb<-kgb[inTrain_kgb,]
test_kgb<-kgb[-inTrain_kgb,]

kgb_model<-glm(bad~woe_phone_part+woe_email_domain+woe_loan_fees+woe_zip_part+woe_gender_empl_type+woe_bank+woe_monthly_debt+woe_debt_to_income_ratio+woe_client_age_at_application_date+woe_days_since_registration,data=train_kgb,family=binomial)
summary(kgb_model, cor=TRUE)

kgb_model_step<-stepwise(kgb_model,direction="forward",criterion='AIC')
summary(kgb_model_step, cor=TRUE)

# Total model Estimates as average of three models
kgb_model_avg<-kgb_model_step
kgb_model_avg_coef<-coefficients(summary(kgb_model_avg))
kgb_model_avg_coef[,1]<-c(-1.498756667,0.843426167,1.035891,0.814191167,1.001990233,0.8850669,1.134476267,0.876192933,0.879320467,0.7520165,0.597131933)
kgb_model_avg[[1]]<-c(kgb_model_avg_coef[,1])
kgb_model_avg

# AVG model validation by month
kgb_test_month<-kgb[kgb$month_decision=="01.2015",]
kgb_test_month$score<-predict(kgb_model_avg,kgb_test_month)
kgb_test_month$grp_score<-cut2(kgb_test_month$score,g=10)
sqldf("select grp_score, median(score) med_score, sum(1-bad) as good, sum(bad) as bad, count(*) as total from kgb_test_month group by 1 order by 2")

# Scorecard constructing--------------------------------------------------------
sqldf("select grp_loan_fees, woe_loan_fees from kgb group by 1,2 order by 1")
sqldf("select grp_phone_part, woe_phone_part from kgb group by 1,2 order by 1")
sqldf("select grp_monthly_debt, woe_monthly_debt from kgb group by 1,2 order by 1")
sqldf("select grp_zip_part, woe_zip_part from kgb group by 1,2 order by 1")
sqldf("select grp_gender_empl_type, woe_gender_empl_type from kgb group by 1,2 order by 1")
sqldf("select grp_debt_to_income_ratio, woe_debt_to_income_ratio from kgb group by 1,2 order by 1")
sqldf("select grp_email_domain, woe_email_domain from kgb group by 1,2 order by 1")
sqldf("select grp_days_since_registration, woe_days_since_registration from kgb group by 1,2 order by 1")
sqldf("select grp_client_age_at_application_date, woe_client_age_at_application_date from kgb group by 1,2 order by 1")

#Population Stability Index ---------------------------------------------------------
#Split 04_2015 subset into 10 equal populated bins and use these breaks for all other periods
# calculate PSI using formula in excel
kgb_test_month<-kgb[kgb$month_decision=="08.2014",]
kgb_test_month$score<-predict(kgb_model_avg,kgb_test_month)
kgb_test_month$grp_score<-cut(kgb_test_month$score,breaks=c(-Inf,-2.42,-2.15,-1.91,-1.74,-1.55,-1.38,-1.156,-0.87,-0.48,Inf),include.lowest=TRUE, right=FALSE)
sqldf("select grp_score, median(score) med_score, count(*) as total from kgb_test_month group by 1 order by 2")

#-----------------------------------------------------------------------------------
# Scoring the whole data set--------------------------------------------------------
rd$grp_loan_fees<-ifelse(rd$loan_fees==0,0,ifelse(rd$loan_fees<131,130,999))
rd$woe_loan_fees<-ifelse(rd$loan_fees==0,-0.1948844,ifelse(rd$loan_fees<131,0.7238935,0.9220361))

rd$grp_phone_part<-as.factor(ifelse(rd$phone_part %in% c("+48 60"),"60",ifelse(rd$phone_part %in% c("+48 50","+48 66"),"50_66",ifelse(rd$phone_part=="+48 69","69",ifelse(rd$phone_part=="+48 79","79",ifelse(rd$phone_part %in% c("+48 88","+48 78"),"78_88",ifelse(rd$phone_part %in% c("+48 72","+48 51"),"51_72","others")))))))
rd$woe_phone_part<-ifelse(rd$phone_part %in% c("+48 60"),-0.62785969,ifelse(rd$phone_part %in% c("+48 50","+48 66"),-0.31090238,ifelse(rd$phone_part=="+48 69",-0.10618977,ifelse(rd$phone_part=="+48 79",0.03273308,ifelse(rd$phone_part %in% c("+48 88","+48 78"),0.27966386,ifelse(rd$phone_part %in% c("+48 72","+48 51"),0.41460250,0.64178374))))))

rd$grp_monthly_debt<-cut(rd$monthly_debt,breaks=c(0,15,21,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_monthly_debt<-ifelse(rd$monthly_debt<15,-0.60816855,ifelse(rd$monthly_debt<21,-0.02991389,0.43370119))

rd$grp_zip_part<-as.factor(ifelse(rd$zip_part %in% c("30","66","72","27","39","60","55","0","4"),"0_4_30_66_60_55_72_27_39",ifelse(rd$zip_part %in% c("2","1","83","80","20","61","3","31","11"),"2_1_83_80_20_61_3_31_11",ifelse(rd$zip_part %in% c("67","64","34","82","41","42","33","26","8","43","59","97","50","93"),"67_64_34_82_41_42_33_26_8_43_59_97_50_93",ifelse(rd$zip_part %in% c("5","7","21","85","62","87","38","95","22"),"5_7_21_85_62_87_38_95_22",ifelse(rd$zip_part %in% c("96","86","81","58","44","14"),"96_86_81_58_44_14",ifelse(rd$zip_part %in% c("40","89","63","37","76","9","88","98"),"40_89_63_37_76_9_88_98","others")))))))
rd$woe_zip_part<-ifelse(rd$zip_part %in% c("30","66","72","27","39","60","55","0","4"),-0.46978542,ifelse(rd$zip_part %in% c("2","1","83","80","20","61","3","31","11"),-0.18497567,ifelse(rd$zip_part %in% c("67","64","34","82","41","42","33","26","8","43","59","97","50","93"),-0.01563053,ifelse(rd$zip_part %in% c("5","7","21","85","62","87","38","95","22"),0.10132964,ifelse(rd$zip_part %in% c("96","86","81","58","44","14"),0.21320665,ifelse(rd$zip_part %in% c("40","89","63","37","76","9","88","98"),0.40508309,-0.07305275))))))

rd$grp_gender_empl_type<-as.factor(ifelse(rd$gender_empl_type %in% c("9_Female","1_Female","2_Female"),"9_1_2_F",ifelse(rd$gender_empl_type %in% c("11_Female","2_Male","1_Male"),"11_F_2_1_M","others")))
rd$woe_gender_empl_type<-ifelse(rd$gender_empl_type %in% c("9_Female","1_Female","2_Female"),-0.1491338,ifelse(rd$gender_empl_type %in% c("11_Female","2_Male","1_Male"),0.1260128,0.2827210))

rd$grp_debt_to_income_ratio<-cut(rd$debt_to_income_ratio,breaks=c(0,0.11,0.17,0.24,0.33,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_debt_to_income_ratio<-ifelse(is.na(rd$debt_to_income_ratio)==TRUE,0.1939908881,ifelse(rd$debt_to_income_ratio<0.11,-0.4524117848,ifelse(rd$debt_to_income_ratio<0.17,-0.1325414055,ifelse(rd$debt_to_income_ratio<0.24,0.0001737593,ifelse(rd$debt_to_income_ratio<0.33,0.1274637733,0.1939908881)))))

rd$grp_email_domain<-as.factor(ifelse(rd$email_domain %in% c("poczta","interia"),"poczta_interia",ifelse(rd$email_domain %in% c("gmail","op"),"gmail_op",ifelse(rd$email_domain %in% c("wp","vp"),"wp_vp",ifelse(rd$email_domain %in% c("onet","o2"),"onet_o2","others")))))
rd$woe_email_domain<-ifelse(rd$email_domain %in% c("poczta","interia"),-0.06967765,ifelse(rd$email_domain %in% c("gmail","op"),-0.02527276,ifelse(rd$email_domain %in% c("wp","vp"),0.02260385,ifelse(rd$email_domain %in% c("onet","o2"),0.23996429,-0.30453309))))

rd$grp_days_since_registration<-as.factor(ifelse(rd$days_since_registration==0,"0",ifelse(rd$days_since_registration<51,"1-51",">51")))
rd$woe_days_since_registration<-ifelse(rd$days_since_registration==0,-0.05946505,ifelse(rd$days_since_registration<51,0.35214180,0.19070781))

rd$grp_client_age_at_application_date<-cut(rd$client_age_at_application_date,breaks=c(0,27,30,34,44,55,Inf),include.lowest=TRUE, right=FALSE)
rd$woe_client_age_at_application_date<-ifelse(rd$client_age_at_application_date<27,0.37457025,ifelse(rd$client_age_at_application_date<30,0.13606831,ifelse(rd$client_age_at_application_date<34,-0.03197480,ifelse(rd$client_age_at_application_date<44,-0.15971984,ifelse(rd$client_age_at_application_date<55,-0.07610115,0.12355278)))))

#KGB_SCORE
rd$kgb_score<-predict(kgb_model_avg,rd)
rd$kgb_prob<-predict(kgb_model_avg,rd,type=c("response"))

#Population Stability Index  for score groups on whole data set--------------------------------------
kgb_test_month<-rd[rd$month_decision=="04.2015",]
kgb_test_month$grp_score<-cut(kgb_test_month$kgb_score,breaks=c(-Inf,-2.42,-2.15,-1.91,-1.74,-1.55,-1.38,-1.156,-0.87,-0.48,Inf),include.lowest=TRUE, right=FALSE)
sqldf("select grp_score, median(kgb_score) med_score, count(*) as total from kgb_test_month group by 1 order by 2")
