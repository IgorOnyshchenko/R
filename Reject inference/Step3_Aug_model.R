##libraries

library(sqldf)
library(Hmisc)
library(caret)
library(ROCR)
library(Rcmdr)


name<-c("decision_date","issue_date","settledate","max_delay","estended_days","decision","current_behaviour","amount","loan_fees","term","application_day_of_month","application_time_of_day","client_age_at_application_date","gender","bank","income","working_time","employment_type","employment_length","official_document_type","newsletter","zipcode","zip_part","phone","phone_part","email_domain","days_since_registration","days_since_previous_reject","number_of_rejected_1m","number_of_rejected_3m","number_of_rejected_6m","debt_to_income_ratio","reason","processed_by","target1","target2","bad","month_decision","gender_empl_type","monthly_debt","empl_len_age"
        ,"grp_loan_fees","woe_loan_fees","grp_phone_part","woe_phone_part","grp_monthly_debt","woe_monthly_debt","grp_zip_part","woe_zip_part","grp_gender_empl_type","woe_gender_empl_type","grp_debt_to_income_ratio","woe_debt_to_income_ratio","grp_bank","woe_bank","grp_email_domain","woe_email_domain","grp_days_since_registration","woe_days_since_registration","grp_client_age_at_application_date","woe_client_age_at_application_date"
        ,"acc_prob","kgb_prob","kgb_score")

dt2<-rd[,name]

write.csv(dt2,"d:/R-projects/Reject Inference/DATA_kgb_score.csv")

# aug2MENTATION
#know good/bad set
dt_kgb2<-dt2[dt2$bad!=-1,]
dt_kgb2$weight<-1

#unknown set
dt_ugb1_1<-dt2[dt2$bad==-1 & dt2$decision=="APPROVED",]
dt_ugb1_1$bad<-1
dt_ugb1_1$weight<-dt_ugb1_1$kgb_prob

dt_ugb1_0<-dt2[dt2$bad==-1 & dt2$decision=="APPROVED",]
dt_ugb1_0$bad<-0
dt_ugb1_0$weight<-(1-dt_ugb1_0$kgb_prob)

#Rejected set
dt_ugb2_1<-dt2[dt2$bad==-1 & dt2$decision=="REJECTED",]
dt_ugb2_1$bad<-1
dt_ugb2_1$weight<-dt_ugb2_1$acc_prob*dt_ugb2_1$kgb_prob

dt_ugb2_0<-dt2[dt2$bad==-1 & dt2$decision=="REJECTED",]
dt_ugb2_0$bad<-0
dt_ugb2_0$weight<-dt_ugb2_0$acc_prob*(1-dt_ugb2_0$kgb_prob)

#augmented set
dt_aug2<-rbind(dt_kgb2,dt_ugb1_0,dt_ugb1_1,dt_ugb2_0,dt_ugb2_1)
dt_aug2$weights<-trunc(dt_aug2$weight*100)

#Export data with weights
name<-c("decision_date","issue_date","settledate","max_delay","estended_days","decision","current_behaviour","amount","loan_fees","term","application_day_of_month","application_time_of_day","client_age_at_application_date","gender","bank","income","working_time","employment_type","employment_length","official_document_type","newsletter","zipcode","zip_part","phone","phone_part","email_domain","days_since_registration","days_since_previous_reject","number_of_rejected_1m","number_of_rejected_3m","number_of_rejected_6m","debt_to_income_ratio","reason","processed_by","target1","target2","bad","month_decision","gender_empl_type","monthly_debt","empl_len_age",
        "acc_prob","kgb_prob","kgb_score","weight","weights")
dt_w<-dt_aug2[,name]
write.csv(dt_w,"d:/R-projects/Reject Inference/DATA_weights.csv")

### Sample splitting randomly on Training and Testing set with 70/30 proportion---------------------
table(dt_aug2$month_decision,dt_aug2$bad)
sqldf("select month_decision, sum(case when bad=0 then 1 else 0 end) as good, sum(case when bad=1 then 1 else 0 end) as bad, sum(case when bad=-1 and decision='APPROVED' then 1 else 0 end) as unknown, sum(case when decision='REJECTED' then 1 else 0 end) as rejected, count(*) as total from dt2 group by 1 order by 1")
dt_aug2<-dt_aug2[dt_aug2$month_decision %in% c("01.2015","12.2014","11.2014"),]

inTrain_aug2<-createDataPartition(y=dt_aug2$bad,p=0.7,list=FALSE)
train_aug2<-dt_aug2[inTrain_aug2,]

#model
aug2_model<-glm(bad~woe_phone_part+woe_email_domain+woe_loan_fees+woe_zip_part+woe_gender_empl_type+woe_bank+woe_monthly_debt+woe_debt_to_income_ratio+woe_client_age_at_application_date+woe_days_since_registration,data=train_aug2,family=binomial,weight=weights)
summary(aug2_model, cor=TRUE)

# Total model Estimates as average of three models
aug2_model_avg<-aug2_model
aug2_model_avg_coef<-coefficients(summary(aug2_model_avg))
aug2_model_avg_coef[,1]<-c(-1.601826,0.923604667,0.735898667,1.042699667,1.105224667,1.262692,0.917715333,0.986995667,0.838340667,0.813358333,0.698767333)
aug2_model_avg[[1]]<-c(aug2_model_avg_coef[,1])
summary(aug2_model_avg)

# AVG MODEL VALIDATION
kgb$aug2_score<-predict(aug2_model_avg,kgb)
kgb$grp_aug2_score<-cut2(kgb$aug2_score,g=10)
sqldf("select grp_aug2_score, median(aug2_score) med_score, sum(1-bad) as good, sum(bad) as bad, count(*) as total from kgb group by 1 order by 2")

# AVG model validation by month
kgb_test_month<-kgb[kgb$month_decision %in% c("01.2015","12.2014","11.2014"),]
kgb_test_month$aug2_score<-predict(aug2_model_avg,kgb_test_month)
kgb_test_month$grp_aug2_score<-cut2(kgb_test_month$score,g=10)
sqldf("select grp_aug2_score, median(aug2_score) med_score, sum(1-bad) as good, sum(bad) as bad, count(*) as total from kgb_test_month group by 1 order by 2")

#Population Stability Index for known good bad---------------------------------------------------------
kgb_test_month<-kgb[kgb$month_decision=="08.2014",]
kgb_test_month$aug2_score<-predict(aug2_model_avg,kgb_test_month)
kgb_test_month$grp_aug2_score<-cut(kgb_test_month$aug2_score,breaks=c(-Inf,-2.64,-2.35,-2.1,-1.92,-1.71,-1.55,-1.31,-1.04,-0.68,Inf),include.lowest=TRUE, right=FALSE)
sqldf("select grp_aug2_score, median(aug2_score) med_score, count(*) as total from kgb_test_month group by 1 order by 2")

#Population Stability Index  for score groups on whole data set--------------------------------------
kgb_test_month<-rd[rd$month_decision=="04.2015",]
kgb_test_month$aug2_score<-predict(aug2_model_avg,kgb_test_month)
kgb_test_month$grp_aug2_score<-cut(kgb_test_month$aug2_score,breaks=c(-Inf,-2.46,-2.12,-1.89,-1.69,-1.51,-1.32,-1.11,-0.89,-0.57,Inf),include.lowest=TRUE, right=FALSE)
sqldf("select grp_aug2_score, median(aug2_score) med_score, count(*) as total from kgb_test_month group by 1 order by 2")


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

#aug2_SCORE
rd$aug_score<-predict(aug2_model_avg,rd)
rd$aug_prob<-predict(aug2_model_avg,rd,type=c("response"))


#Export data after scoring
name<-c("decision_date","issue_date","settledate","max_delay","estended_days","decision","current_behaviour","amount","loan_fees","term","application_day_of_month","application_time_of_day","client_age_at_application_date","gender","income","working_time","employment_type","employment_length","official_document_type","newsletter","zipcode","zip_part","phone","phone_part","email_domain","days_since_registration","days_since_previous_reject","number_of_rejected_1m","number_of_rejected_3m","number_of_rejected_6m","debt_to_income_ratio","reason","processed_by","target1","target2","bad","month_decision","gender_empl_type","monthly_debt","empl_len_age",
        "acc_prob","kgb_prob","kgb_score","aug_prob","aug_score")
dt<-rd[,name]
write.csv(dt,"d:/R-projects/Reject Inference/DATA_aug2_score.csv")
