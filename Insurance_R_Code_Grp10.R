####################----Insurance Charges Analysis----####################
#Name:Snehal Shetye

install.packages("ggplot2")
library(ggplot2)
install.packages("leaps")
library(leaps)
###################---.Importing dataset---#################
insurance <- read.csv("C:/Users/sneha/Downloads/insurance/insurance.csv")

##creating data for smokers and non-smokers
insurance_smoker<-insurance[insurance$smoker=="yes",]
insurance_non_smoker<-insurance[insurance$smoker=="no",]
#creating data for male and female
insurance_male<-insurance[insurance$sex=="male",]
insurance_female<-insurance[insurance$sex=="female",]





###########################--1.Graphs--##########################

#1a.
#Creating bin of age -> (18-29),(30-40),(41-50) and (51-70) 
for (i in 1:nrow(insurance)){
  if(insurance$age[i] >= 18 & insurance$age[i] <= 29){
    insurance$age_bin[i] = '18-29 years'
  }
  else if(insurance$age[i] >= 30 & insurance$age[i] <= 40){
    insurance$age_bin[i] = '30-40 years'
  }
  else if(insurance$age[i] >= 41 & insurance$age[i] <= 50){
    insurance$age_bin[i] = '41-50 years'
  }
  else if(insurance$age[i] > 50 & insurance$age[i] <= 70){
    insurance$age_bin[i] = '51-70 years'
  }
}

#Plot of Insurance charges against different age groups
boxplot(charges~age_bin,data=insurance,xlab="Age Band",ylab="Charges",main="Insurance
        charges for each age group")
##Insurance charges are increasing with respect to age




#1b.
#Insurance charges for smokers and non-smokers 
p<-ggplot(data=insurance,mapping=aes(x=smoker,y=charges,color=smoker))
p+geom_boxplot()+ggtitle("Insurance Charges for Smokers/Non-smokers")

#Insurance charges for "smokers & non-smokers" across  "sex"
p<-ggplot(data=insurance,mapping=aes(x=age,y=charges,color=smoker))
p+geom_point()+facet_wrap(~sex) +ggtitle("Charges for Smokers/Non-smokers & Males/Females")




#1c.Density of Insurance charges for sex
ggplot(insurance, aes(x = charges, fill = sex)) + geom_density(alpha = 0.5)+
  ggtitle("Density of charges for Males and Females")


#1d.Number of smokers across different regions
p<-ggplot(data=insurance_smoker,mapping=aes(x=region))
p+geom_bar()+coord_cartesian(ylim=c(50,100)) +ylab("Number of smokers") +
  ggtitle("Number of smokers in each region")

#Median of insurance charge for smokers across regions
med_1<-aggregate(charges~region,insurance_smoker,median) 
p<-ggplot(data=insurance_smoker,mapping=aes(x=region,y=charges))
p+geom_point()+stat_summary(fun.y="median",color="red",size=3,geom="point")+
  geom_text(data = med_1, aes(label = charges, y = charges ),size=3,col="black")+
  ggtitle("Median of the insurance charges for smokers accross regions")




#1f.BMI across regions for male and female smokers
med<-aggregate(bmi~region+sex,insurance_smoker,median) 
p<-ggplot(data=insurance_smoker,mapping=aes(x=sex,y=bmi,color=sex))
p+geom_boxplot()+facet_wrap(~region)+
  geom_text(data = med, aes(label = bmi, y = bmi  -2),size=3,col="black")+
  ggtitle("BMI accross regions for male and female smokers")




#1g.Distribution of age across regions
med_2<-aggregate(age~region+sex,insurance,median) 
p<-ggplot(data=insurance,mapping=aes(x=sex,y=age))
p+geom_boxplot()+facet_wrap(~region)+
  geom_text(data = med_2, aes(label = age, y = age + -2),size=3,col="black")+
  ggtitle(" Distribution of age across regions")

#age is uniformly distributed across regions in the data




#1h. Insurance charges against # of children covered by health insurance
boxplot((charges)~children,data=insurance,xlab="Number of children covered by health insurance",
        ylab="Insurance charges",main="Insurance charges against number of children ")




#1i.Insurance Charges against male and female
med_4<-aggregate(charges~sex,insurance,median) 
p<-ggplot(data=insurance,mapping=aes(x=sex,y=charges))
p+geom_boxplot()+ylab("Insurance charges")+xlab("sex")+
  ggtitle("          Insurance charges against sex")+
  geom_text(data = med_4, aes(label = charges, y = charges+2500),size=4,col="black")
#Overall, Insurance charges are not affected by gender




#1j.Creating bin for BMI (16-25 =Normal), (26-30= overweight),(>30 = Obese)
for (i in 1:nrow(insurance)){
  if(insurance$bmi[i] >= 15.95 & insurance$bmi[i] <= 25){
    insurance$bmi_bin[i] = '16-25 (Normal)'
  }
  else if(insurance$bmi[i] >= 26 & insurance$bmi[i] <= 30){
    insurance$bmi_bin[i] = '26-30 (Overweight)'
    
  }
  else if(insurance$bmi[i] >= 31 & insurance$bmi[i] <= 54){
    insurance$bmi_bin[i] = '31-54 (Obese)'
  }}
#plotting insurance charges against bmi_bin
med_4<-aggregate(charges~bmi_bin,insurance,median) 
p<-ggplot(data=insurance,mapping=aes(x=bmi_bin,y=charges))
p+geom_boxplot()+
  geom_text(data = med_4, aes(label = charges, y = charges + 2000),size=3,col="black")+
  ggtitle("Insurance charges for BMI groups")









###########################2.Advanced statistics#####################



#2a. Compute 95% CI of insurance charges for smokers in southeast region
xbar=mean(insurance_smoker$charges[insurance_smoker$region=="southeast"])
s=sqrt(var(insurance_smoker$charges[insurance_smoker$region=="southeast"]))
z=qnorm(0.975)
n=nrow(insurance_smoker[insurance_smoker$region=="southeast",])
lb=xbar-(z*s/(sqrt(n)))
Ub=xbar+(z*s/sqrt(n))
CI_95_charges_smoker_southeast<-c(lb,Ub)
#true mean of insurance charges of the population who smokes in southeast region 
#will be between $32518.21 and $37171.78



#2b. Test the hypothesis-> avg insurance charges for smokers in southeast region
#                 are less than $38K

#Ho : mu =$ 38000(avg insurance charges for smokers in southeast region =$38K) , 
#Ha: mu<38000 (avg insurance charges for smokers in southeast region are less than $38K)
x_bar=mean(insurance_smoker$charges[insurance_smoker$region=="southeast"])
mu_zero=38000
s=sqrt(var(insurance_smoker$charges[insurance_smoker$region=="southeast"]))
n=nrow(insurance_smoker[insurance_smoker$region=="southeast",])
z=(x_bar-mu_zero)/(s/sqrt(n))
zaplha=qnorm(0.05)
#or
p=pnorm(z)
p
#p values is less than 0.05 therefore we reject null hypothesis.That's why,
#avg insurance charges for smokers in southeast region are less than $38K




#2c. Compute 95% CI of bmi for the smokers in southeast region
xbar=mean(insurance_smoker$bmi[insurance_smoker$region=="southeast"])
s=sqrt(var(insurance_smoker$bmi[insurance_smoker$region=="southeast"]))
z=qnorm(0.975)
n=nrow(insurance_smoker[insurance_smoker$region=="southeast",])
lb=xbar-(z*s/(sqrt(n)))
Ub=xbar+(z*s/sqrt(n))
CI_95_bmi<-c(lb,Ub)
# true mean of the bmi of the population who smokes in southeast region
#will be between 31.66 and 34.54 



#2d.Test the hypothesis -> There is a difference between the mean of the
#                          insurance charges for smokers and non-smokers
              

#Ho : mu_non_smoker_charge =mu_smoker_charge  (There is no difference between the mean of the
#                                              insurance charges for smokers and non smokers), 
#Ha: mu_non_smoker_charge != mu_smoker_charge (There is difference between the mean of the
#                                              insurance charges for smokers and non smokers)

t.test(insurance_smoker$charges,insurance_non_smoker$charges)
#t = 32.752, df = 311.85, p-value < 2.2e-16 therfore we will reject null hypothesis
#That's why There is difference between the mean of the
#insurance charges for smokers and non-smokers






#2e. Compute 95% CI for diff between mean of
###               insurance charges for smokers and non-smokers---##############


n1<-nrow(insurance_smoker)
n2<-nrow(insurance_non_smoker)
#n1 and n2 both are greater than 30
x1_bar_minus_x2_bar<-mean(insurance_smoker$charges) - mean(insurance_non_smoker$charges)
z=qnorm(0.975)
s1=sqrt(var(insurance_smoker$charges))
s2=sqrt(var(insurance_non_smoker$charges))
lb<- x1_bar_minus_x2_bar - z*sqrt((s1*s1/n1) + (s2*s2/n2))
Ub<- x1_bar_minus_x2_bar + z*sqrt((s1*s1/n1) + (s2*s2/n2))
CI_95_diff_mean_charge<-c(lb,Ub)

#we are 95% sure that population will have average insurance charge difference for 
#smokers and non-smokers between $22202.72 & $25029.21





#2f.Test the hypothesis that the variance of the insurance
#                 charges of smokers is different from non-smokers

#Ho : var_smoker =var_non_smoker  (variance of  insurance
#                                  charges of smokers and non-smokers is same), 
#Ha: var_smoker != var_non_smoker (Variance of the insurance
#                                  charges of smokers is different from non-smokers


var_smoker<-var(insurance_smoker$charges)
var_non_smoker<-var(insurance_non_smoker$charges)
var_smoker>var_non_smoker
f=var_smoker/var_non_smoker
falpha=qf(0.95,(nrow(insurance_smoker)-1),(nrow(insurance_non_smoker)-1))
##or
p=1-pf(f,(nrow(insurance_smoker)-1),(nrow(insurance_non_smoker)-1))
p
## f>falpha or p<0.05 we reject null hypo . therfore variance of the insurance
###                 charges of the smokers are different from  non-smokers



#2g.a).Test the hypothesis that the region and sex are independent
#Ho : region and sex are independent 
#Ha : region and sex are not independent 
chisq.test(insurance$region,insurance$sex)
#p value >0.05 we failed to reject null hypothesis 
#We can say region and sex are independent


#2g.b).Test the hypothesis that the region and smoker are independent
#Ho : region and smoker are independent 
#Ha : region and smoker are not independent 
chisq.test(insurance$region,insurance$smoker)
#p value >0.05 we failed to reject null hypothesis 
#We can say region and smoker are independent





###########################3. Linear Model#####################

#3a. Creating dummy variables for sex, smoke and region
insurance$sex_dummy=ifelse(insurance$sex=="male",1,0)
insurance$smoke_dummy=ifelse(insurance$smoker=="yes",1,0)
insurance$region_dummy<-ifelse(insurance$region=="northeast",0,
                               ifelse(insurance$region=="northwest",1,
                                      ifelse(insurance$region=="southeast",2,3)))

#converting region to factor
insurance$region_dummy<-as.factor(insurance$region_dummy)

#As we see all the continuous variables are having linear relationship with Insurance charges
#there is no need to transform any continuous variable


#3b.
#Let's use backward variable selection method to identify best model
fit.f_1 <- lm(charges~age+sex_dummy+bmi+children+smoke_dummy+region_dummy,data = insurance)
drop1(fit.f_1,charges~age+sex_dummy+bmi+children+smoke_dummy+region_dummy , test = "F")
# p value for sex_dummy is high "0.693348" therefore removing this variable 

fit.f_2 <- lm(charges~age+bmi+children+smoke_dummy+region_dummy,data = insurance)
drop1(fit.f_2,charges~age+bmi+children+smoke_dummy+region_dummy , test = "F")
# p value for region_dummy is high "0.096315" therefore removing this variable 

fit.f_3 <- lm(charges~age+bmi+children+smoke_dummy,data = insurance)
drop1(fit.f_3,charges~age+bmi+children+smoke_dummy , test = "F")
#All the remaining variables are significant (since p value is less than 0.05)



#Let's use exhaustive method to identify Top 3 models


all <- regsubsets(x=cbind(age,sex_dummy,bmi,children,smoke_dummy,region_dummy), y=charges,  method = "exhaustive", 
                  data=insurance, all.best = FALSE, nbest = 3)

summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(nrow(insurance)-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
output
#output will give you table having all the values from which we found below top 3 models
#age,bmi,children,smoke_dummy
#age,bmi,smoke_dummy
#age,smoke_dummy

##Therefore, from exhaustive and backward method we conclude that,
#model having variables age, bmi, children, smoke_dummy will be best


#That's why our final model is
fit<-lm(charges~age+bmi+children+smoke_dummy,data=insurance)

#3c.
#Correlation check between independent variables
cor(insurance$age,insurance$bmi)
cor(insurance$bmi,insurance$children)
cor(insurance$age,insurance$children)
#Also, there is no correlation between independent variables
#Therefore, no problem of multicollinearity 


#3d.
#to check if there are any influential points present in the data
cooks.distance(fit)[cooks.distance(fit)>1]
#There is no data available for cooks.distance >1. That's why there are no
#influential points present in the data

#to check leverage
#We created X matrix
X<-as.matrix(cbind(1,(insurance$age),
                   insurance$bmi,insurance$children,
                   insurance$smoke_dummy))
#Created Hat Matrix
H<-X%*%solve((t(X)%*%X))%*%t(X)
head(H)
#Diagonal elements of Hat matrix
diag(H)[diag(H)>2*(4+1)/1338]
#there are around 52 leverage points present in the data


#3e.
#Summary of our final model
summary(fit)

#Normality check of residuals of our final model
qqnorm(rstudent(fit))

##Residual plot of our final model
#residual plot for smokers
plot(fitted.values(fit)[insurance$smoke_dummy==1],rstudent(fit)[insurance$smoke_dummy==1]
     ,col=2,pch=20,xlab = "fitted values", ylab = "Studentized residuals", main = 
       "Residual plot",xlim=c(0,60000))
#residual plot for non-smokers
points(fitted.values(fit)[insurance$smoke_dummy==0],rstudent(fit)[insurance$smoke_dummy==0]
     ,col=6,pch=20,xlab = "fitted values", ylab = "Studentized residuals")

