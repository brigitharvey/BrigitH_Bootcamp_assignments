#Assignment 2 Bootcamp Day 2 Noa

ChickWeight=ChickWeight
data<-ChickWeight
data<-within(data, {
Chick<-factor(Chick)
Diet<-factor(Diet)
Time <- factor(Time)
})

#Question 1
#'~' is identifying diet as the independent variable

data.t0<-data[data$Time ==0,] #isolating the variable time to only on day 0
boxplot(weight~Diet, data=data.t0, xlab= "Diet Type", ylab= "Weight", main= "Weights of Chicks to Diet Treatments on Day 0")
#'data=data set that you are working with' is to keep it clean

anova1=aov(weight~Diet, data=data.t0) # set up the statistical test
summary(anova1) # look at the results of the statistical test, says that there is a difference between the 3 species, but doesn't specify which is ifference from which
TukeyHSD(anova1)
# weight by diet is not significant on Day 0, p=0.346

#Question 2
data.t21<-data[data$Time ==21,]
boxplot(weight~Diet, data=data.t21, xlab= "Diet Type", ylab= "Weight", main= "Weights of Chicks to Diet Treatments on Day 21")

anova2=aov(weight~Diet, data=data.t21) # set up the statistical test
summary(anova2) # look at the results of the statistical test, says that there is a difference between the 3 species, but doesn't specify which is ifference from which
TukeyHSD(anova2)
# weight by diet is significant on Day 21, p=0.00686
# Diet 3 is different from Diet 1 p=0.0046959


#Question 3
summary(aov(weight~Diet*Time+Error(Chick), data=data))

# There is an effect of Diet on Chick growth p=0.000799

#Question 4
plot(as.numeric(data$Time), data$weight, pch=19)

cols=sub('1',"hotpink",data$Diet) #substitute setosa with word hotpink in that data
cols=sub('2',"plum",cols)
cols=sub('3',"cornflowerblue",cols)
cols=sub('4', "purple", cols)

plot(as.numeric(data$Time),data$weight, xlab='Time',ylab='Weight', main= "Effect of Diet on Chick", # same as above...
     col=cols, # add color assigned to each species
     pch=16,las=1,cex.axis=1.5,cex.lab=1.5)
legend("topleft", title= "Diet", legend=unique(data$Diet), text.col=c("hotpink", "plum", "cornflowerblue", "purple"),pch=16, col=c("hotpink", "plum", "cornflowerblue", "purple"))

#Question 5
plot(NA, ylim=c(0,370), xlim=c(0,21), xlab="Time", ylab="Weight", main="Individual Chick Growth Over Time")
colors=rainbow(50)
for(ID in 1:50) {
  use.rows=which(data$Chick == ID)
  lines(x=data$Time[use.rows],
  y=data$weight[use.rows], col=colors[ID])
}