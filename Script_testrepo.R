DAP_translation<-unique(translation_file_old_and_new_UBN_DAP)
is.na(DAP_translation)
DAP_translation<-DAP_translation[c(2:170,172:334),]
DAP_translation$dap<-as.numeric(DAP_translation$dap)
Contact_rund_DAP<- left_join(Contact_rund, DAP_translation, by= "dap")
Contact_rund_DAP<- Contact_rund_DAP[,c(1:5,36,6:35)]
Contact_varken_DAP<- left_join(Contact_varken, DAP_translation,by= "dap")
Contact_varken_DAP<- Contact_varken_DAP[, c(1:5,64,6:63)]

DAP_relaties<-N_veegroep_perdap_2015_2019_pc2_SLEUTEL2017Q4
DAP_relaties_rund<- DAP_relaties[DAP_relaties$vg=="RU",]
DAP_relaties_varken<- DAP_relaties[DAP_relaties$vg=="VA",]
DAP_relaties_rund_koppelcontact<-DAP_relaties_rund[,c(2,4,5)]
DAP_relaties_varken_koppelcontact<-DAP_relaties_varken[,c(2,4,5)]

colnames(DAP_relaties_rund_koppelcontact)[1]<- "dap_new"
colnames(DAP_relaties_rund_koppelcontact)[2]<- "Year"
colnames(DAP_relaties_rund_koppelcontact)[3]<- "n_relaties_DAP"
DAP_relaties_rund_koppelcontact$Year<-as.character(DAP_relaties_rund_koppelcontact$Year)
Contact_rund_DAP<-left_join(Contact_rund_DAP, DAP_relaties_rund_koppelcontact, by= c("dap_new", "Year"))
Contact_rund_DAP<-Contact_rund_DAP[!duplicated(Contact_rund_DAP$contact_nr.),]
Contact_rund_DAP<-Contact_rund_DAP[,c(1:6,37,7:36)]

colnames(DAP_relaties_varken_koppelcontact)[1]<- "dap_new"
colnames(DAP_relaties_varken_koppelcontact)[2]<- "Year"
colnames(DAP_relaties_varken_koppelcontact)[3]<- "n_relaties_DAP"
DAP_relaties_varken_koppelcontact$Year<-as.character(DAP_relaties_varken_koppelcontact$Year)
Contact_varken_DAP<-left_join(Contact_varken_DAP, DAP_relaties_varken_koppelcontact, by= c("dap_new", "Year"))
Contact_varken_DAP<-Contact_varken_DAP[!duplicated(Contact_varken_DAP$contact_nr.),]
Contact_varken_DAP<-Contact_varken_DAP[,c(1:6,65,7:64)]

DAP_relaties_rund_2015<- DAP_relaties_rund[DAP_relaties_rund$jaar==2015,]
DAP_relaties_rund_2016<-DAP_relaties_rund[DAP_relaties_rund$jaar==2016,]
DAP_relaties_rund_2017<-DAP_relaties_rund[DAP_relaties_rund$jaar==2017,]
DAP_relaties_rund_2018<-DAP_relaties_rund[DAP_relaties_rund$jaar==2018,]
DAP_relaties_rund_2019<-DAP_relaties_rund[DAP_relaties_rund$jaar==2019,]

DAP_relaties_varken_2015<- DAP_relaties_varken[DAP_relaties_varken$jaar==2015,]
DAP_relaties_varken_2016<-DAP_relaties_varken[DAP_relaties_varken$jaar==2016,]
DAP_relaties_varken_2017<-DAP_relaties_varken[DAP_relaties_varken$jaar==2017,]
DAP_relaties_varken_2018<-DAP_relaties_varken[DAP_relaties_varken$jaar==2018,]
DAP_relaties_varken_2019<-DAP_relaties_varken[DAP_relaties_varken$jaar==2019,]

length(unique(DAP_relaties_varken_2015$dap))
list_DAP_peryear<-list(DAP_relaties_rund_2015,DAP_relaties_rund_2016,DAP_relaties_rund_2017,DAP_relaties_rund_2018,DAP_relaties_rund_2019,
                       DAP_relaties_varken_2015,DAP_relaties_varken_2016, DAP_relaties_varken_2017, DAP_relaties_varken_2018, DAP_relaties_varken_2019)


DAP_analysis_rund<- DAP_relaties_rund_koppelcontact
DAP_analysis_rund<- DAP_analysis_rund[!is.na(DAP_analysis_rund$dap_new),]

DAP_analysis_rund<-aggregate(Contact_rund_DAP$contact_nr. ~Contact_rund_DAP$dap_new + Contact_rund_DAP$Year,FUN= length )
DAP_analysis_rund<- left_join(DAP_analysis_rund, DAP_relaties_rund_koppelcontact, by= c("Contact_rund_DAP$dap_new" = "dap_new", "Contact_rund_DAP$Year"="Year"))

length(unique(DAP_analysis_rund$`Contact_rund_DAP$dap_new`))
DAP_analysis_rund<-DAP_analysis_rund[!is.na(DAP_analysis_rund$n_relaties_DAP),]
colnames(DAP_analysis_rund)[1]<-"DAP_new"
colnames(DAP_analysis_rund)[2]<-"Year"
colnames(DAP_analysis_rund)[3]<-"Contact"
colnames(DAP_analysis_rund)[4]<-"Relations"

plot(DAP_analysis_rund$cat_Relations)
quantile(DAP_analysis_rund$Relations, 1.0)
DAP_analysis_rund$cat_Relations<-cut(DAP_analysis_rund$Relations, breaks= c(0,10,50,100,150,200,250,300,400,670 ), 
                                     labels= c("until 10", "11 to 50", "51 to 100", "101 to 150","151 to 200", "201 to 250", "251 to 300", "301 to 400", "above 400"))

####Analyse DAP's LMER####
plot(DAP_analysis_rund$Relations, DAP_analysis_rund$Contact)

install.packages("optimx")
library(optimx)
library(lme4)
interceptmodel<- glmer(Contact ~ 1 + (1|DAP_new), data = DAP_analysis_rund, family= "poisson")
summary(interceptmodel)
model1<- glmer(Contact ~  factor(cat_Relations) + (1|DAP_new), data = DAP_analysis_rund, family= "poisson", control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(model1) 
model2<- glm(Contact ~  factor(cat_Relations), data = DAP_analysis_rund, family= "poisson")
summary(model2) ##worse than model 1
model3<-glmer(Contact ~  factor(cat_Relations)+ factor(Year) + (1|DAP_new), data = DAP_analysis_rund, family= "poisson", control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(model3) ###Year only 2018 signficant
model4<-glmer(Contact ~  cat_Relations+ Year+ (1|DAP_new), data = DAP_analysis_rund, family= "poisson", control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
summary(model4)

res <- residuals(model4)
fit <- fitted(model4)
plot(fit,res)

