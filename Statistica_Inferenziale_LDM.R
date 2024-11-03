####################################
# Progetto Statistica Inferenziale #
# Lorenzo De Marco                 #
####################################
setwd("/Users/lorenzodemarco/Desktop/MasterProfessionAI/")
#1)
data<-read.delim("./Statistica Inferenziale/dataset_neonati.csv",sep=",")
dim(data)
min(data$Anni.madre)
#2)
str(data)


#3) 
library(ggplot2)

# Anni madre
summary(data)

data[data$Anni.madre<10,]
ggplot(data,aes(Anni.madre))+geom_histogram(color="red",fill="white")+theme_classic()+ggtitle("Anni della madre al momento del parto")+
  geom_vline(xintercept = 28,color="black")+  geom_vline(xintercept = 28.16,color="indianred")

moments::skewness(data$Anni.madre)
moments::kurtosis(data$Anni.madre)
ggplot(data,aes(Gestazione))+geom_histogram(color="indianred",fill="white")+theme_classic()+ggtitle("Mesi Gestazione")+
  geom_vline(xintercept = 39,color="black")+  geom_vline(xintercept = 38.98,color="indianred")


ggplot(data,aes(Peso))+geom_histogram(color="navy",fill="white")+theme_classic()+ggtitle("Peso alla nascita")+
  geom_vline(xintercept =3300,color="black")+  geom_vline(xintercept = 3284 ,color="indianred")


ggplot(data,aes(Lunghezza))+geom_histogram(color="grey",fill="white")+theme_classic()+ggtitle("Lunghezza")+
  geom_vline(xintercept =500.0,color="black")+  geom_vline(xintercept = 494.7,color="indianred")

ggplot(data,aes(Cranio))+geom_histogram(color="blue",fill="white")+theme_classic()+ggtitle("Cranio")+
  geom_vline(xintercept = 340,color="black")+  geom_vline(xintercept = 340,color="indianred")



gridExtra::grid.arrange()
ggplot()+geom_bar(data,mapping=aes(x=factor(Fumatrici),color=Fumatrici,stat="identity",group=factor(Fumatrici)))
p1<-data %>% 
  ggplot(aes(x = factor(Fumatrici), y = ..count../sum(..count..))) +
  geom_bar(data,mapping=aes(fill=factor(Fumatrici))) +
  labs(y = "Proportion", x= "Fumatrici")+theme_classic()

p2<-data %>% 
  ggplot(aes(x = factor(Sesso), y = ..count../sum(..count..))) +
  geom_bar(data,mapping=aes(fill=Sesso)) +
  labs(y = "Proportion", x= "Sesso")+theme_classic()


p3<-data %>% 
  ggplot(aes(x = factor(Ospedale), y = ..count../sum(..count..))) +
  geom_bar(data,mapping=aes(fill=Ospedale)) +
  labs(y = "Proportion", x= "Ospedale")+theme_classic()

p4<-data %>% 
  ggplot(aes(x = factor(Tipo.parto), y = ..count../sum(..count..))) +
  geom_bar(data,mapping=aes(fill=Tipo.parto)) +
  labs(y = "Proportion", x= "Tipo.parto")+theme_classic()

gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)
#####################

ggplot()+geom_boxplot(data,mapping=aes(Sesso,Anni.madre,color=Sesso))+theme_classic()

t.test(Anni.madre~Sesso,  data=data)

ggplot()+geom_boxplot(data,mapping=aes(Sesso,Peso,color=Sesso))+theme_classic()
test<-t.test(Peso~Sesso,  data=data)
test$conf.int
ggplot()+geom_boxplot(data,mapping=aes(Sesso,Cranio,color=Sesso))+theme_classic()
test<-t.test(Cranio~Sesso,  data=data)
test
ggplot()+geom_boxplot(data,mapping=aes(Sesso,Lunghezza,color=Sesso))+theme_classic()
test<-t.test(Lunghezza~Sesso,  data=data)
test

boxplot(data$Anni.madre,main="Anni madre")

table(data$Fumatrici)/(2396+104)

ggplot(data,mapping=aes(Anni.madre,Peso))+geom_point()+ 
  geom_smooth(method = "lm", se = FALSE)+theme_classic()

p1<-ggplot(data,mapping=aes(Lunghezza,Peso))+geom_point()+theme_classic()+geom_smooth(method = "lm", se = FALSE)

p2<-ggplot(data,mapping=aes(Cranio,Peso))+geom_point()+theme_classic()+geom_smooth(method = "lm", se = FALSE)
gridExtra::grid.arrange(p1,p2,ncol=2)


ggplot()+geom_point(data,mapping=aes(Cranio,Lunghezza))



############################
media_popolazione<-3300
media_dati <- mean(data$Peso)
deviazione_standard_dati <- sd(data$Peso)

# Calcolo lo Z-score
z_score <- (media_dati -media_popolazione ) / (deviazione_standard_dati / sqrt(length(data$Peso)))

# Specifica il livello di significatività (alpha)
alpha <- 0.05  # Puoi cambiare il livello di significatività a seconda delle tue esigenze

# Trova il valore critico Z per il tuo livello di significatività
valore_critico_Z <- qnorm(1 - alpha / 2)  # Per un test bidirezionale

# Confronta il valore Z con il valore critico Z
cat("Z-score:", z_score, "\n")
cat("Valore critico Z:", valore_critico_Z, "\n")
if (abs(z_score) > valore_critico_Z) {
  cat("La media dei dati è diversa da", media_popolazione, "con un livello di significatività di", alpha, "\n")
} else {
  cat("Non ci sono prove sufficienti per concludere che la media dei dati sia diversa da", media_popolazione, "con un livello di significatività di", alpha, "\n")
}



############## Chi squared Test
table(data$Tipo.parto,data$Ospedale)
table(data$Ospedale)
ospedali <- c("Ospedale 1", "Ospedale 2", "Ospedale 3")
parti_cesarei <- c(242, 254, 232)


df<-data.frame(ospedali,parti_cesarei)

chisq.test(df$parti_cesarei)

ggplot()+geom_bar(data,mapping=aes(Ospedale,fill=Tipo.parto,stat="identity"))+theme_classic()


##Analisi multidimensionale

names(data)
model=lm(Peso ~., data = data)
summary(model)



model2<-update(model,~.-Ospedale)
summary(model2)
anova(model,model2)

BIC(model,model2)
car::vif(model2)




model3<-update(model2,~.-Anni.madre )
summary(model3)
anova(model2,model3)

BIC(model,model2,model3)
car::vif(model3)

model4<-update(model3,~.-Fumatrici)

summary(model4)
BIC(model,model2,model3,model4)


cor(data$Cranio,data$Lunghezza)


library(MASS)

stepwise<-MASS::stepAIC(model,direction = "both",k=log(nrow(data)))

summary(stepwise)
summary(model4)

######### Modello migliore





par(mfrow=c(2,2))
plot(stepwise)

shapiro.test(residuals(stepwise))

lmtest::bptest(stepwise)

lmtest::dwtest(stepwise)

lev<-hatvalues(stepwise)
plot(lev)
p<-sum(lev)
soglia=2*p/nrow(data)
abline(h=soglia,col=2)
length(lev[lev>soglia])


#outliers
plot(rstudent(stepwise))
abline(h=c(-2,2))
car::outlierTest(stepwise)


cook<-cooks.distance(stepwise)
plot(cook)
abline(h=4/nrow(data))


summary(stepwise)
ggplot(data)+
  geom_point(aes(Gestazione,Peso,color=factor(N.gravidanze)))+
  geom_smooth(aes(Gestazione,Peso),method="lm",se=F)+theme_classic()


ggplot(data)+
  geom_point(aes(Lunghezza,Peso,color=factor(Sesso)))+
  geom_smooth(aes(Lunghezza,Peso),method="lm",se=F)+theme_classic()

ggplot(data)+
  geom_point(aes(Cranio,Peso,color=factor(Sesso)))+
  geom_smooth(aes(Cranio,Peso),method="lm",se=F)+theme_classic()


predict(stepwise,)

summary(stepwise)
##
#Fai la tua migliore previsione per il peso 
#di una neonata, considerato che la madre 
#è alla terza gravidanza e partorirà alla 39esima 
#settimana. Niente misure dall’ecografia.
head(data)
predict(stepwise,data.frame("N.gravidanze"=3,"Gestazione"=39,"Sesso"="F","Lunghezza"=mean(data$Lunghezza),"Cranio"=mean(data$Cranio)),na.action = "na.pass")
