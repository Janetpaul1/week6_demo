?beavers
str(beaver2)
View(beaver2)
?factor
is.factor(beaver2)

#H0: BODY TEMPARATURE IS NOT AFFECTED BY ACTIVITY
#H1:BODY TEMPERATURE AFFECTED by ACTIVITY

beavers_data<-beaver2
beaver2
#factorization for active variable
beavers_data$activ<-factor(beavers_data$activ, labels = c("no","yes"))
beavers_data$activ
is.factor(beavers_data$activ)
str(beavers_data)
#activity is an independent variable
#plot histogram
install.packages("vcd")
library(vcd)
install.packages("ggplot2")
library(ggplot2)
?hist

ggplot(beavers_data,aes(x=temp))+geom_histogram()+theme_bw()
ggplot(beavers_data,aes(x=temp))+geom_histogram(breaks=seq(36,38,.2))+
  theme_bw()+labs(x="temp",y="Activity")+scale_y_continuous(breaks =seq(0,60,5) )
install.packages("lattice")
library(lattice)

window(20,10)
attach(beavers_data)
histogram(~temp | activ,
          data=beavers_data,main="distribution of beavers activity data",
          xlab="temparatures(degrees)",ylab="Activity %")
 detach(beavers_data)
attach(beavers_data)
windows(16,10)
qqnorm(temp)
qqline(temp,col="red")
#qqplot
opar<-par(no.readonly=TRUE)
windows(20,10)
par(mfrow=c(1,2))
with(beavers_data,{
  qqnorm(temp[activ=="yes"],
         main = "beavers activity data")
  qqline(temp[activ=="yes"])
})
with(beavers_data,{
  qqnorm(temp[activ=="no"],
         main = "beavers Inactive data")
  qqline(temp[activ=="no"])
})
str(beavers_data)


#formal test for normality
#data is not normally distributed
normality_test <-shapiro.test(beavers_data$temp)
normality_test
normality_test$p.value



