library(MatchIt)
library(ggplot2)
library(dplyr)

?lalonde

###### Load the data
dta <- read.csv("/Users/sarwaridas/Desktop/IDS\ 702/TeamProjects/Project1/lalonde.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)

###### View properties of the data

head(dta)
dim(dta)
glimpse(dta)

f_cols= c('treat','black','hispan','married','nodegree') #changing to factor dtype
dta[f_cols] = lapply(dta[f_cols], as.factor) #changing to factor dtype
#View(dta)
nulls = sapply(dta, function(col) sum(length(which(is.na(col))))); nulls #checking for nulls
summary(dta) #Outlier exists in incomes!!
dta_full=dta
dta<-dta[!(dta$re74>=30000 | dta$re75>=30000 |dta$re78>=30000 ),]
dim(dta_full)
dim(dta) #removed 3 people

# We still need to put a lower limit on income 
under_18= (dta[(dta$age<18),]); dim(under_18) 
## should we get rid of the 68 people who are under 18?
under_18_withno_income= (under_18[(under_18$re74 == 0 & under_18$re75 == 0  & under_18$re78 == 0  ),]); dim(under_18_withno_income)
## should we get rid of the 18 people who are under 18 and have no income - ever?


#REMOVING OUTLIERS IN INCOME AND (UNDER 18 & NO INCOME)
dta= (dta[!(dta$re74 == 0 & dta$re75 == 0  & dta$re78 == 0 & dta$age<18 ),]); dim(dta)

#In total, I drop 14 people

###### Exploratory data analysis
#correlations b/w numeric vars
round(cor(dta[c('age','educ','re74','re75','re78')], use="complete.obs", method="pearson"),2)

#Checking if treatment & control group have equal characteristics
#aggregate(dta[c('age','educ','re74','re75')], list(dta$treat), length) 
aggregate(dta[c('age','educ','re74','re75')], list(dta$treat), mean)

#checking age
t.test(age~treat,data=dta,alternative="greater") #significant! --- limitation
#checking educ
t.test(educ~treat,data=dta,alternative="greater") #not sigf
#checking baseline sal
t.test(re74~treat,data=dta,alternative="greater") #sigf - addressed in study/by design

#We can compare growth in income, not income

dta$treat_as_names= dta$treat
levels(dta$treat_as_names)<- c("Control","Treatment")

#By race= black
prop.table(table(dta$treat_as_names,dta$black),margin=1)
#By race= Hispanic
prop.table(table(dta$treat_as_names,dta$hispan),margin=1)

#By married
prop.table(table(dta$treat_as_names,dta$married),margin=1)

#By no-degree
prop.table(table(dta$treat_as_names,dta$nodegree),margin=1)

#Lots of potential problems to talk about


#How did income increase for people in the control group?
x<-as.matrix(aggregate(dta[c('re74','re75','re78')], list(dta$treat_as_names), mean)[-1])
row.names(x) <- c("control","treatment")
barplot(x, beside=TRUE, col=c('seagreen', 'darkblue'), legend=row.names(x),args.legend = list(x = "top")) 

#we need to do more outlier analysis - this doesn't look too right

# Response - re78
#Is the distribution of the response variable normal? - Nope
hist(dta$re78,xlab="Wages",main="Distribution of 78wages",col=rainbow(10))

#Is the distribution of the logged response variable normal? - Somewhat, but still skewed
hist(log(dta$re78),xlab="Wages",main="Distribution of log(78wages)",col=rainbow(10))
#We can try a box cox later

#EDA with response

colnames(dta)
ggplot(dta,aes(x=treat_as_names, y=re78, fill=treat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Treatment vs income") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip()

ggplot(dta,aes(x=black, y=re78, fill=treat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Treatment vs income") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip()

ggplot(dta,aes(x=hispan, y=re78, fill=treat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Treatment vs income") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip()

ggplot(dta,aes(x=married, y=re78, fill=treat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Treatment vs income") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip()

ggplot(dta,aes(x=nodegree, y=re78, fill=treat)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Treatment vs degree") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip()

ggplot(dta,aes(x=re78, y=age)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="re78 vs age") #refer to reading for this relationship

ggplot(dta,aes(x=re78, y=educ)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="re78 vs educ") #discrete so not very interesting

ggplot(dta,aes(x=re78, y=re74)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="re78 vs re74") #linear as expected

ggplot(dta,aes(x=re78, y=re75)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="re78 vs re75") #linear as expected

#any other 2-way relationships worth exploring?

ggplot(dta,aes(x=educ, y=age)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="educ vs age") #Huh?! Maybe older people in this dataset didn't study too much?

dta$has_a_degree= dta$nodegree
levels(dta$has_a_degree) = c("Has degree","No Degree")
aggregate(dta[c('age')], list(dta$has_a_degree), mean) #guess not. Could be an issue for discrete var

ggplot(dta,aes(x=nodegree, y=age, fill=nodegree)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="age vs degree") + 
  theme_classic() + theme(legend.position="Top")+
  coord_flip() #degree holders have a higher median age as expected

## looking for interactions
##interactions to check: 
#black and hispan (-age, educ, married, nodegree, re74/75)
#age


summary(lm(re78~age+educ+treat+re75+re74+age*treat,data=dta))


colnames(dta)



table(dta$nodegree, dta$age)

?lalonde






 
