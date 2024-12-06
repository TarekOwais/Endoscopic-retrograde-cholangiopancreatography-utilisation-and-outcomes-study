require(Hmisc)
require(gmodels)
require(ggplot2)
require(finalfit)
require(tidyverse)
require(dplyr)

df<-read.csv(file.choose())
view(df)
dim(df)
df_demog<-df %>% distinct(hospitalid, .keep_all = TRUE)
#general demographics
df_demog$year<-factor(df_demog$year)
table(df_demog$year)
table(df_demog$referralsite)
table(df_demog$gender)
df_demog$gender<-factor(df_demog$gender,levels = c("Male","Female"))
summary(df_demog$age)
df_demog$age.group<-ifelse(df_demog$age==40|df_demog$age<40,"<=40",
                     ifelse(df_demog$age>40&df_demog$age<65,"41-64",">=65"))

df_demog$age.group<-factor(df_demog$age.group,
                     levels =c("<=40","41-64",">=65") )
table(df_demog$age.group)

#creating the demographics table
explanatory.demog<-c("age.group","gender","referralsite","year")
demographics.table<-df_demog %>%
  summary_factorlist(dependent = NULL, explanatory.demog)

write.csv(demographics.table,"native papilla tables//demographics table filtered.csv")


#general demographics for the original df
df$year<-factor(df$year)
table(df$year)
table(df$referralsite)
table(df$gender)
df$gender<-factor(df$gender,levels = c("Male","Female"))
summary(df$age)
df$age.group<-ifelse(df$age==40|df$age<40,"<=40",
                     ifelse(df$age>40&df$age<65,"41-64",">=65"))

df$age.group<-factor(df$age.group,
                     levels =c("<=40","41-64",">=65") )
table(df$age.group)

#pancreatitis table
explanatory.pancreaitis<-c("age.group", "gender")
pancreatits.by.age.gender.table<-df %>% 
  summary_factorlist(dependent = "Pancreatitis", explanatory.pancreaitis, total_col = TRUE, p=TRUE)

#patient history variables
table(df$Pancreatitis)
df$Pancreatitis<-factor(df$Pancreatitis, levels = c("Yes","No","Unknown"))

table(df$GBS)
df$GBS<-factor(df$GBS, levels = c("Yes", "No", "Unknown"))

table(df$Cholecystitis)
df$Cholecystitis<-factor(df$Cholecystitis, levels = c("Yes", "No", "Unknown"))

table(df$Cholecystectomy)
df$Cholecystectomy<-factor(df$Cholecystectomy, levels = c("Yes", "No", "Unknown"))

table(df$othersdz)
df$othersdz<-factor(df$othersdz, levels = c("Yes", "No", "Unknown"))

table(df$HTN)
df$HTN<-factor(df$HTN, levels = c("Yes", "No", "Unknown"))

df$DM<-factor(df$DM, levels = c("Yes", "No", "Unknown"))

df$CAD<-factor(df$CAD, levels = c("Yes", "No", "Unknown"))

df$smoking<-factor(df$smoking, levels = c("Yes", "No", "Unknown"))

table(df$malignancy)
df$malignancy<-factor(df$malignancy, levels = c("Biliary", "Pancreas", "others", "None", "Unknown"))

df$FamilyHx<-factor(df$FamilyHx, levels = c("Yes", "No", "Unknown"))
#creating the family history table

explanatory.history<-c("Pancreatitis","GBS","Cholecystitis","Cholecystectomy","othersdz","HTN",
                       "DM", "CAD", "smoking","malignancy","FamilyHx")

patient.history.table<-df %>% 
  summary_factorlist(dependent = NULL, explanatory.history)

#write.csv(patient.history.table,"F:\\cross sec\\study-11 ERCP\\ERCP2\\tables\\patient history table2.csv")
################################################


#ercp variables
table(df$preercp)
df$preercp<-factor(df$preercp, levels = c("Yes", "No", "Unknown"))

table(df$procedure)
#############################33
table(df$indication)#dont report or report alone
df$Indication<-ifelse(df$indication=="Periampullary lesion","Periampullary lesion",
                      ifelse(df$indication=="Jaundice with pancreatic malignancy"|df$indication=="Jaundice with biliary malignancy"|df$indication== "Jaundice with other malignancy","Obstructive jaundice with known malignancy",
                             ifelse(df$indication=="Jaundice /Biliary strictures","Obstructive jaundice: unknown biliary stricture suspected of malignancy",
                                    ifelse(df$indication=="Jaundice", "Obstructive jaundice: Others",
                                           ifelse(df$indication=="Jaundice/cholangitis", "Acute cholangitis",
                                                  ifelse(df$indication=="CBD dilation", "Dilated CBD on image /without jaundice",
                                                         ifelse(df$indication=="Jaundice post cholecystectomy"|df$indication=="Bile leak"|df$indication=="Jaundice with altered anatomy","Post operative complications",
                                                                ifelse(df$indication=="Pancreatic pseudocyst"|df$indication=="Prophylactic PD stent placement"|df$indication=="Pancreatic duct dilation"|df$indication=="Pancreatic duct leak"|df$indication=="recurrent pancreatitis","Pancreatic disease other than malignancy",
                                                                       ifelse(df$indication=="Stent removal"|df$indication=="Stent exchange"|df$indication=="Retained stent removal","Stent removal/exchange","Obstructive jaundice with known stones")))))))))


table(df$Indication)
##########################################

table(df$cannulation)
df$cannulation<-factor(df$cannulation, levels = c("Yes", "No"))

df$pancinjection<-factor(df$pancinjection,levels = c("Yes","No"))


df$resultcannulation<-factor(df$resultcannulation
                             ,levels = c("successful","unsuccessful","None"))

table(df$resultcannulation)

df$visualdodenum<-factor(df$visualdodenum, levels = c("Yes","No"))
table(df$visualdodenum)


df$visualalteredanatomy<-factor(df$visualalteredanatomy,levels = c("Yes","No"))

df$canalizesmallpapilla<-factor(df$canalizesmallpapilla,levels = c("Yes","No"))

df$cannalizefloppypapilla<-factor(df$cannalizefloppypapilla,levels = c("Yes","No"))

df$cannalizediverticulum<-factor(df$cannalizediverticulum,levels = c("Yes","No"))

df$IntSphincterotomy<-factor(df$IntSphincterotomy,levels = c("Yes","No"))

df$IntCbdclearance<-factor(df$IntCbdclearance, levels = c("Yes","No"))

df$plasticstent<-factor(df$plasticstent, levels = c("Yes","No"))

df$SEMS<-factor(df$SEMS, levels = c("Yes","No"))

df$RFA<-factor(df$RFA, levels = c("Yes","No"))

df$stricturedilation<-factor(df$stricturedilation, levels = c("Yes","No"))

df$failuretocross<-factor(df$failuretocross, levels = c("Yes","No"))


df$cholangioscope<-factor(df$cholangioscope, levels = c("Yes","No"))

df$Pancsphincterotomy<-factor(df$Pancsphincterotomy,levels = c("Yes","No"))

df$pancstoneextraction<-factor(df$pancstoneextraction,levels = c("Yes","No"))


df$pancstricturedilation<-factor(df$pancstricturedilation,levels = c("Yes","No"))

df$pancstenting<-factor(df$pancstenting, levels = c("Yes","No"))

df$stricture<-factor(df$stricture,levels = c("Yes","No"))

df$ERCPtechbiopsy<-factor(df$ERCPtechbiopsy,levels = c("Brush","Foreceps","Snare polypectomy","None" ))
table(df$ERCPtechbiopsy)

df$eus<-factor(df$eus,levels = c("Yes","No") )
table(df$eus)

df$complications[df$complications=="Mild pancreatitis"]<-"PEP"
df$complications[df$complications=="Cardiopulmonary instability ducring procedure"]<-"Cardiopulmonary instability during procedure"

table(df$complications)
df$complications<-factor(df$complications, levels=c(
  "Cardiopulmonary instability during procedure", "Early Bleeding",
  "Infection", "PEP","Perforation", "Late Bleeding","Death","Others", "None"
  
))

table(df$complications)

df$failuremanagment<-factor(df$failuremanagment,levels = c("EUS evaluation of pancreas", "PTC with randizuve" ,"PTD" ,
                                                           "Second attempt","Surgical intervention","None"))

df$visualpapilla<-factor(df$visualpapilla, levels=c("Yes", "No"))
table(df$visualpapilla)

df$IntSphincterotomy<-factor(df$IntSphincterotomy, levels = c("Yes", "No"))
#renamig the cannulation variable
df$pancannulation<-df$cannulation
df$result.CBD.cannulation<-df$resultcannulation


#the tecbnique
df$technique[df$technique=="Ampulla S/P surgery"]<-"Cannulation with altered anatomy/obstruction"
df$technique[df$technique=="Choledochoduodenostomy cannulation"]<-"Cannulation with altered anatomy/obstruction"
df$technique[df$technique=="Randezvous cannulation"]<-"Cannulation with altered anatomy/obstruction"
table(df$technique)

df$Techniques<-factor(df$technique,
                      levels = c("Sphincterotomy",
                                 "Double wire cannulation",
                                 "Precut papillotomy",
                                 "Cannulation without sphincterotomy",
                                 "Previous sphincterotomy",
                                 "Cannulation with altered anatomy/obstruction",
                                 "Failure to attempt cannulation",
                                 "Cannulation with pancreatic papillotomy"))


table(df$Techniques)
#preparint the table


explanatory.ercp<-c("procedure","Indication"  ,"pancannulation"  ,          "pancinjection"   ,       "result.CBD.cannulation"   ,   "technique",             
                    "outcome"  ,              "visualdodenum" ,         "visualpapilla"   ,       "visualalteredanatomy" , 
                    "canalizesmallpapilla" ,  "cannalizefloppypapilla", "cannalizediverticulum" , "failuremanagment"  ,    
                    "IntSphincterotomy" ,     "IntCbdclearance"  ,      "plasticstent" ,          "SEMS"     ,             
                    "RFA"   ,                 "stricturedilation"  ,    "failuretocross"   ,      "cholangioscope" ,       
                    "Pancsphincterotomy"  ,   "pancstoneextraction"  ,  "pancstricturedilation" , "pancstenting" ,         
                    "stricture" ,             "ERCPtechbiopsy"   ,      "resultstricture"  ,      "eus"   ,                
                    "eusresult"   ,           "hospitalstay" ,          "complications")

ercp.table<-df %>% summary_factorlist(dependent = NULL, explanatory.ercp)

#write.csv(ercp.table,"E:\\cross sec\\study-11 ERCP\\ERCP2\\tables\\ercp table.csv")



#first regression model result of cbd cannulation
# table(df$result.CBD.cannulation)
# #creting new variable for the results of cbd cannultion without the non level
# df$result.of.cbd.cannulation<-df$result.CBD.cannulation
# df$result.of.cbd.cannulation[df$result.of.cbd.cannulation=="None"]<-NA
# table(df$result.of.cbd.cannulation)
# df2<-df
# df2<-df2[complete.cases(df2$result.of.cbd.cannulation),]
# df2<-df2[complete.cases(df2),]
# 
# view(df2)
# table(df2$result.of.cbd.cannulation)

#############################################################################################################


###########################
#final calculations
df$complications_bi<-ifelse(df$complications=="None",0,1)
df$complications_bi<-as.factor(df$complications_bi)
table(df$complications_bi)
#complications by age group table
complications.by.age.group<-df %>% 
  summary_factorlist(dependent = "age.group", explanatory = "complications", total_col = TRUE)
#write.csv(complications.by.age.group,"E:\\cross sec\\study-11 ERCP\\ERCP2\\tables\\ complications by age group table.csv")
#factors associated with complications
df$preercp.drop<-factor(df$preercp, levels = c("Yes", "No"))
table(df$preercp.drop)

explanatory_complications<-c("age.group","gender", "preercp.drop", "pancannulation" )

complications.table<-df %>% 
  summary_factorlist(dependent = "complications_bi", explanatory_complications,total_col = TRUE, p=TRUE)
#write.csv(complications.table,"tables/complications table no techniques.csv")

complicatins.df<-data.frame(as.numeric(df$complications_bi) ,as.numeric(df$gender),
                            as.numeric(df$preercp), as.numeric(df$pancannulation),
                            as.numeric(df$Techniques))
view(complicatins.df)
comp.corr<-cor(complicatins.df)
comp.corr<-cor
view(comp.corr)

explanatory_complications_model<-c( "preercp", "pancannulation","nativepapilla" )
complications.model<-df %>% 
  finalfit(dependent = "complications_bi", explanatory_complications_model)
write.csv(complications.model,"native papilla tables/ complications model final.csv")

##.	Mild pancreatitis [from complications] and pancreatic duct contrast injection[paninjetion, panannulation]? 

pep<-df %>% 
  filter(df$complications=="PEP"|df$complications=="None")
dim(pep)
table(pep$complications)
pep$complications<-droplevels(pep$complications)
#
#pep.outcomes
table(pep$outcome)
#outcomes binomial [norma ercp vs others]
pep$Outcome<-ifelse(pep$outcome=="Normal ERCP","Normal ERCP","Others")
explanatory.pep<-c("age.group","gender","pancannulation","nativepapilla")
pep.table<-pep %>% 
  summary_factorlist(dependent = "complications", explanatory.pep, total_col = TRUE, p=TRUE)
  
write.csv(pep.table,"native papilla tables/ pep nativepapilla table.csv")

explanatory.pep.model<-c("age.group","gender","pancannulation","nativepapilla")

pep.model<-pep %>% 
  finalfit(dependent = "complications", explanatory.pep.model)
write.csv(pep.model,"native papilla tables/ pep model2.csv")
##########################################
#success rate of pancannulation within the nativepapailla
CrossTable(df$nativepapilla, df$resultcannulation)
success_rete_table<-df %>% 
  summary_factorlist(dependent = "nativepapilla", explanatory = "resultcannulation", total_col = TRUE, p=TRUE)
write.csv(success_rete_table, "native papilla tables/cannulation success rate table.csv")
