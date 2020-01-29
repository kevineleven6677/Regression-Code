setwd("C:/Users/user/Desktop/Chellange")
library("readxl")
library(VIM)
data=read_excel("shelf-life-study-data-for-analytics-challenge_prediction.xlsx")
data=data.frame(data)
head(data)
View(data)
colnames(data)
data1=data[,3:15]
colnames(data1)=c("PorudctType","BaseIngredicent","ProcessType","SampleAge_w",
                  "DifferFromFresh","StorageCondition","PackageingStabilizerAdded","TransparentWindow","ProcessingAgent",
                  "PreservativeAdded","Moisture","Oxygen","Hexanal")

table1=as.data.frame(
  cbind(c(
  sum(is.na(data1$PorudctType)),
  sum(is.na(data1$BaseIngredicent)),
  sum(is.na(data1$ProcessType)),
  sum(is.na(data1$SampleAge_w)),
  sum(is.na(data1$DifferFromFresh)),
  sum(is.na(data1$StorageCondition)),
  sum(is.na(data1$PackageingStabilizerAdded)),
  sum(is.na(data1$TransparentWindow)),
  sum(is.na(data1$ProcessingAgent)),
  sum(is.na(data1$PreservativeAdded)),
  sum(is.na(data1$Moisture)),
  sum(is.na(data1$Oxygen)),
  sum(is.na(data1$Hexanal))
  ),
  c(class(data1$PorudctType),class(data1$BaseIngredicent),
         class(data1$ProcessType),class(data1$SampleAge_w),class(data1$DifferFromFresh),
         class(data1$StorageCondition),class(data1$PackageingStabilizerAdded),class(data1$TransparentWindow),
         class(data1$ProcessingAgent),class(data1$PreservativeAdded),class(data1$Moisture),
         class(data1$Oxygen),class(data1$Hexanal)))
  ,row.names = colnames(data1)
    )
colnames(table1)=c("Missing","type")
table1

data2=kNN(data1,k = 10)

table2=as.data.frame(
  cbind(c(
          sum(is.na(data2$PorudctType)),
          sum(is.na(data2$BaseIngredicent)),
          sum(is.na(data2$ProcessType)),
          sum(is.na(data2$SampleAge_w)),
          sum(is.na(data2$DifferFromFresh)),
          sum(is.na(data2$StorageCondition)),
          sum(is.na(data2$PackageingStabilizerAdded)),
          sum(is.na(data2$TransparentWindow)),
          sum(is.na(data2$ProcessingAgent)),
          sum(is.na(data2$PreservativeAdded)),
          sum(is.na(data2$Moisture)),
          sum(is.na(data2$Oxygen)),
          sum(is.na(data2$Hexanal))
  ),
  c(class(data2$PorudctType),class(data2$BaseIngredicent),
    class(data2$ProcessType),class(data2$SampleAge_w),class(data2$DifferFromFresh),
    class(data2$StorageCondition),class(data2$PackageingStabilizerAdded),class(data2$TransparentWindow),
    class(data2$ProcessingAgent),class(data2$PreservativeAdded),class(data2$Moisture),
    class(data2$Oxygen),class(data2$Hexanal)))
  ,row.names = colnames(data2)[3:15]
)
colnames(table2)=c("Missing","type")
table2
##For continuous factor (col=4,5,9,11,12,13)
summary(data2[,c(4,5,9,11,12,13)])
pairs(data2[,c(4,5,9,11,12,13)])
cor(data2$SampleAge_w,data2$DifferFromFresh)
cor(data2$SampleAge_w,data2$ProcessingAgent)
cor(data2$SampleAge_w,data2$Moisture)
cor(data2$SampleAge_w,data2$Oxygen)
cor(data2$SampleAge_w,data2$Hexanal)
cor(data2$DifferFromFresh,data2$ProcessingAgent)
cor(data2$DifferFromFresh,data2$Moisture)
cor(data2$DifferFromFresh,data2$Oxygen)
cor(data2$DifferFromFresh,data2$Hexanal)
cor(data2$ProcessingAgent,data2$Moisture)
cor(data2$ProcessingAgent,data2$Oxygen)
cor(data2$ProcessingAgent,data2$Hexanal)
cor(data2$Moisture,data2$Oxygen)
cor(data2$Moisture,data2$Hexanal)
cor(data2$Oxygen,data2$Hexanal)
##for qualitative factor(col=1,2,3,6,7,8,10)
par(mfrow=c(3,3))
barplot(table(data2$PorudctType),main="Barplot for ProductType")
barplot(table(data2$BaseIngredicent),main="Barplot for BaseIngredicent")
barplot(table(data2$ProcessType),main="Barplot for ProcessType")
barplot(table(data2$StorageCondition),main="Barplot for StorageCondition")
barplot(table(data2$PackageingStabilizerAdded),main="Barplot for PackageingStabilizerAdded")
barplot(table(data2$TransparentWindow),main="Barplot for TransparentWindow")
barplot(table(data2$PreservativeAdded),main="Barplot for PreservativeAdded")
#since TWIP all is "N" the design metrix will singular we'll delet this variable


#outlier checking
par(mfrow=c(1,2))
qqnorm(data2$Oxygen, main="QQPlot for Oxygen")
qqline(data2$Oxygen)
qqnorm(data2$Oxygen[-205], main="QQPlot for Oxygen[-205]")
qqline(data2$Oxygen[-205])

which.max(data2$Oxygen)


model2=lm(DifferFromFresh~PorudctType+BaseIngredicent+ProcessType+SampleAge_w+StorageCondition+
                          PackageingStabilizerAdded+ProcessingAgent+PreservativeAdded+Moisture+
                          Oxygen+Hexanal,data=data2)
summary(model2)
anova(model2)

model3=lm(DifferFromFresh~PorudctType+BaseIngredicent+SampleAge_w+StorageCondition+
                          PackageingStabilizerAdded+Hexanal,data=data2)
summary(model3)


Full=model2
Base=lm(DifferFromFresh~SampleAge_w,data=data2)
step(Full,scope = list(upper=Full,lower=~1),direction = "backward",trace = TRUE,k = log(dim(data2)[1]))
step(Full,scope = list(upper=Full,lower=~1),direction = "backward",trace = TRUE,k=2)
step(Base,scope = list(upper=Full,lower=~1),direction = "forward",trace = TRUE,k = log(dim(data2)[1]))
step(Base,scope = list(upper=Full,lower=~1),direction = "forward",trace = TRUE,k=2)


model_AIC=lm(DifferFromFresh~PorudctType+SampleAge_w+StorageCondition+
               PackageingStabilizerAdded+Hexanal+ProcessingAgent,data=data2)
summary(model_AIC)
anova(model_AIC)
par(mfrow=c(2,2))
plot(model_AIC)
plot(model_BIC)

model_BIC=lm(DifferFromFresh~PorudctType+SampleAge_w+StorageCondition+
               Hexanal,data=data2)

summary(model2)
anova(model2)
summary(model3)
anova(model3)
summary(model_BIC)
anova(model_BIC)
summary(model_AIC)
anova(model_AIC)

