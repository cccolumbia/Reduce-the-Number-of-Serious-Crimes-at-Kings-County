
#data <- read.table("CRIMEDATA.txt")
#colnames(data) <- c("Identification Number","County Name","State","Land area","Total population","Percent of population aged 18-34","Percent of the population 65 or older","Number of active physicians","Number of hospital beds","Total serious crimes","Percent high school graduates","Percent bachelor??s degree","Percent below poverty level","Percent unemployment","Per capita income","Total personal income","Geographic region")

#set.seed(1556)
#index <- sample(1:nrow(data),250,replace = F)
#data <- data[index,]

############## Read data first from data.csv

geographic.region <- c("NE", "NC", "S", "W")
for(i in 1:max(data$`Geographic region`)){
  data$`Geographic region`[data$`Geographic region`==i] <- geographic.region[i]
}

data <- data[,-1]
 
X <- subset(data,select=-c(1,2,`Total serious crimes`))
X <- cbind(data$`Total serious crimes`,X)
colnames(X)[1] <- "Y"
 
#EDA
 
n <- nrow(data)
kings <- data[data$`County Name`=="Kings",][1,]
quantiles <- apply(X[,-14],2,quantile,probs=0.75)
kings
kings[2,] <- NA
rownames(kings) <- c(1,2)
for (i in 3:15){
  kings[2,i] <- sum(data[,i]<=kings[1,i])/n
  #kings[2,i] <- ifelse(kings[2,i]<0.5,1-kings[2,i],kings[2,i])
}



library(ggplot2)
ggplot(data=X,aes(log(Y)))+
  geom_histogram(bins=100)
X$Y <- log(X$Y)


ggplot(data=X,mapping=aes(x=`Geographic region`,y=Y))+
  geom_boxplot()


TukeyHSD(aov(X$Y~X$`Geographic region`))


#Correlation of variables

cor.table <- cor(X[,1:13])
#Total population&Number of active physicians Number of hospital beds Total personal income >0.9
#`Percent of population aged 18-34` `Percent of the population 65 or older` -0.6
#`Number of active physicians` Number of hospital beds 0.95
#`Percent high school graduates` `Percent bachelor??s degree`   0.7
#`Percent high school graduates``Percent below poverty level``Percent unemployment` -0.6


#Overall regression

model <- lm(Y~.,data=X)
s.gen <- summary(model)
s.gen



library(car)
Anova(lm(X$Y~X$`Total population`+X$`Per capita income`+X$`Total personal income`),type="3")



X2 <- X[,-c(2,5,6,7,8,9,11)]
model2 <- lm(Y~.,data=X2)
s.gen2 <- summary(model2)
s.gen2




#Partial F test for those nonsignificant variables


nsig.variable <- rownames(s.gen$coefficients)[s.gen$coefficients[,'Pr(>|t|)']>0.5] 
nsig.variable
#partial F
X.reduced <- X[,-c(2,5,6,7,8,9,11)]
model.reduced <- lm(Y~.,data=X.reduced)
anova(model.reduced,model)



summary(model.reduced)



X3 <-data.frame("popu.land.ratio"=X$`Total population`/X$`Land area`)
X3 <- cbind(X3,X[,-c(2)])
cor.table2 <- cor(X3[,-13])




model3 <- lm(Y~.,data=X3)
summary(model3)



TukeyHSD(aov(X3$popu.land.ratio~X3$`Geographic region`))
TukeyHSD(aov(X3$`Total population`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Percent of population aged 18-34`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Percent of the population 65 or older`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Number of active physicians`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Number of hospital beds`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Percent high school graduates`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Percent bachelor??s degree`~X3$`Geographic region`))
TukeyHSD(aov(X3$`Percent below poverty level`~X3$`Geographic region`)) ###Difference
TukeyHSD(aov(X3$`Per capita income`~X3$`Geographic region`)) ###Difference



substract.index <- c(5,6,8,9,11)
X4 <- X3[,-substract.index]
model4 <- lm(Y~.,data=X4)
summary(model4)



anova(model4,model3)
#Reject `Percent below poverty level` `Per capita income` 



X3.H.edu <- X3[X3$`Percent bachelor??s degree`<=median(X3$`Percent bachelor??s degree`),]
X4.H.edu <- X3.H.edu[,-c(4,5,11,8,13)]
model4.H.edu <- lm(Y~.,data=X4.H.edu)
summary(model4.H.edu)



X3.L.edu <- X3[X3$`Percent bachelor??s degree`>=median(X3$`Percent bachelor??s degree`),]
X4.L.edu <- X3.L.edu[,-c(4,5,11,8,13)]
model4.L.edu <- lm(Y~.,data=X4.L.edu)
summary(model4.L.edu)


#Education Level

X.edu.level <- subset(X3,select=c(Y,`Percent bachelor??s degree`,`Percent high school graduates`))
X.edu.level$`Percent bachelor??s degree` <- ifelse(X.edu.level$`Percent bachelor??s degree`>median(X.edu.level$`Percent bachelor??s degree`),1,0) #1 for High
X.edu.level$`Percent high school graduates` <- ifelse(X.edu.level$`Percent high school graduates`>=median(X.edu.level$`Percent high school graduates`),1,0)
cor(X.edu.level$`Percent bachelor??s degree`,X.edu.level$`Percent high school graduates`)

t.test(X.edu.level$Y[X.edu.level$`Percent bachelor??s degree`==1],X.edu.level$Y[X.edu.level$`Percent bachelor??s degree`==0])

t.test(X.edu.level$Y[X.edu.level$`Percent high school graduates`==1],X.edu.level$Y[X.edu.level$`Percent high school graduates`==0])

#X.edu.level <- subset(X3,select=c(Y,`Percent bachelor??s degree`,`Percent high school graduates`))
#X.edu.level$`Percent bachelor??s degree` <- ifelse(X.edu.level$`Percent bachelor??s degree`>median(X.edu.level$`Percent bachelor??s degree`),"H","L") #1 for High
#X.edu.level$`Percent high school graduates` <- ifelse(X.edu.level$`Percent high school graduates`>=median(X.edu.level$`Percent high school graduates`),"H","L")

#TukeyHSD(aov(X.edu.level$Y~X.edu.level$`Percent bachelor??s degree`))
#TukeyHSD(aov(X.edu.level$Y~X.edu.level$`Percent high school graduates`))



X.region <- subset(X,select=c(Y,`Geographic region`))
anova(lm(Y~.,data=X.region))



TukeyHSD(aov(X.region$Y~X.region$`Geographic region`))


TukeyHSD(aov(X$`Percent bachelor??s degree`~X.region$`Geographic region`))
TukeyHSD(aov(X$`Percent high school graduates`~X.region$`Geographic region`))



interaction.plot(X.region$`Geographic region`,X.edu.level$`Percent bachelor??s degree`,Y)



X.new  <-X3
X.new$`Total population` <- X$`Total population`
X.new$`Percent of active physicians` <-( X.new$`Number of active physicians`/X.new$`Total population`)*100
X.new$`Percent of hospital beds` <- (X.new$`Number of hospital beds`/X.new$`Total population`)*100
X.new <- subset(X.new,select=-c(`Number of active physicians`,`Number of hospital beds`,`Total population`))



model3.new <- lm(Y~.,data=X.new)
summary(model3.new)





X4.new <- X.new[,-c(5,7,8)]
model4.new <- lm(Y~.,data=X4.new)
summary(model4.new)


summary(model4)


library(MASS)
stepAIC(model3,direction = "backward")


X.NE <- X[X$`Geographic region`=="NE",]
X.NE <- subset(X.NE,select=-`Geographic region`)
cor.table.NE <- cor(X.NE)


Anova(lm(X.NE$Y~X.NE$`Total population`+X.NE$`Per capita income`+X.NE$`Total personal income`),type="3")


X.NE <- X3[X3$`Geographic region`=="NE",]
#X.NE <- X[X$`Geographic region`=="NE",]
X.NE <- subset(X.NE,select=-`Geographic region`)
X.NE <- subset(X.NE,select=-`Total personal income`)
model3.NE <- lm(Y~.,data=X.NE)
summary(model3.NE)


X4.NE <- X.NE[,-c(5,8)]
model4.NE <- lm(Y~.,data=X4.NE)
summary(model4.NE)


anova(model3.NE,model4.NE)


summary(model4)


stepAIC(model3.NE,direction = "backward")













