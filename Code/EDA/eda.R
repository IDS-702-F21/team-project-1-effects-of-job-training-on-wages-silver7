library(ggplot2)
library(dplyr)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(gridExtra)
rm(list = ls())

wages <- read.csv("/Users/michellevan/Documents/mids/Stats/homework/team1/TeamProject1/lalondedata.txt",
                    stringsAsFactors = FALSE, sep = ",")
wages

str(wages)
head(wages)

wages$training <- factor(wages$treat)
wages$black <- factor(wages$black)
wages$hisp <- factor(wages$hisp)
wages$married <- factor(wages$married)
wages$nodegree <- factor(wages$nodegree)
wages$age_f <- factor(wages$age)

summary(wages[,-1])
#not balanced ratio between training (185) and no training (429)
#hispanic -not balanced - hispanic (72) vs non-hispanic(542)

ggplot(wages, aes(re78)) + geom_histogram( color='black') + theme_classic() + theme(legend.position='none')  + labs(title="Distribution of 1978 wages", y='1978') 
#heavily right-skewed

wages$log_78 <- log(wages$re78)
ggplot(wages, aes(log_78)) + geom_histogram( color='black') + theme_classic() + theme(legend.position='none')  + labs(title="Distribution of 1978 wages", y='1978') 

ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78", x="Training", y="Wages ") 



ggplot(wages, aes(x=training, y=re75, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 75", x="Training", y="Wages ") 


ggplot(wages, aes(x=re75, y=re78)) + 
  geom_point() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 75", x="Training", y="Wages ") 

ggplot(wages, aes(x=re74, y=re78)) + 
  geom_point() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 75", x="Training", y="Wages ") 

ggplot(wages, aes(x=training, y=re74, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 74", x="Training", y="Wages ") 

ggplot(wages, aes(x=age, y=re78)) + 
  geom_point() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Wages 78 v Age", x="Age", y="Wages 78 ") 

ggplot(wages, aes(x=age_f, y=re78, fill=age_f)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Wages 78 v Age", x="Age", y="Wages 78 ") 

ggplot(wages, aes(x=black, y=re78, fill=black)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Wages 78 v Black", x="Black", y="Wages 78 ") 

ggplot(wages, aes(x=hisp, y=re78, fill=hisp)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='Reds') + 
  labs(title="Wages 78 v Hispanic", x="Hispanic", y="Wages 78 ") 

ggplot(wages, aes(x=married, y=re78, fill=married)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='Greens') + 
  labs(title="Wages 78 v Married", x="Married", y="Wages 78 ") 


ggplot(wages, aes(x=nodegree, y=re78, fill=nodegree)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='Blues') + 
  labs(title="Wages 78 v No Degree", x="No Degree", y="Wages 78 ") 

#INTERACTIONS
ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78 vs Black", x="Training", y="Wages ") +
  facet_wrap(~ black)
#Blacks have lower wages (generally)

ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78 vs Hispanics", x="Training", y="Wages ") +
  facet_wrap(~ hisp)
#roughly the same but not enough hisp 

ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78 vs Married", x="Training", y="Wages ") +
  facet_wrap(~ married)
#married overall higher

ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78 vs No Degree", x="Training", y="Wages ") +
  facet_wrap(~ nodegree)
#roughly the same

ggplot(wages, aes(x=training, y=re78, fill=training)) + 
  geom_point() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Training v Wages 78 vs Married", x="Training", y="Wages ") +
  facet_wrap(~ re74)

#LINEARITY
ggplot(wages, aes(x=training, y=wages$residual)) + 
  geom_point(alpha=.7) + geom_hline(yintercept=0, col='red3') + theme_classic() +
  labs(title='Residuals v Date', x='Date', y='Residuals')

chisq.test(table(wages[,c('re78', 'training')]))
#0.3446 p value

wages_re <- lm(re78 ~ training + age + educ + black + hispan + married + nodegree + re74 + re75, data=wages)
summary(wages_re)
plot(wages_re, which =1:5)
str(smoking)

n <- nrow(wages)
Model_backward1 <- step(wages_re, direction='backward', trace=0, k=log(n))
Model_back_AIC1 <- step(wages_re, direction='forward', trace=0)
Model_back_AIC1$call
Model_backward1$call

NullModel <- lm(re78 ~ 1, data=wages)

stepwise1_AIC <- step(NullModel, scope=formula(wages_re), direction='both', trace =0)
stepwise1_AIC$call
stepwise1_BIC <- step(NullModel, scope=formula(wages_re), direction='both', trace =0, k=log(n))
stepwise1_BIC$call

re_inter <- lm(re78 ~ training + age + educ + black + hispan + married + nodegree + re74 + re75 + training*age 
               + training*black + training*hispan + training*married + training*nodegree , data=wages)

summary(re_inter)
plot(re_inter, which =1:5)
str(smoking)

n <- nrow(wages)
Model_backward1 <- step(re_inter, direction='backward', trace=0, k=log(n))
Model_back_AIC1 <- step(re_inter, direction='forward', trace=0)
Model_back_AIC1$call
Model_backward1$call

NullModel <- lm(re78 ~ 1, data=wages)

stepwise1_AIC <- step(NullModel, scope=formula(re_inter), direction='both', trace =0)
stepwise1_AIC$call
stepwise1_BIC <- step(NullModel, scope=formula(re_inter), direction='both', trace =0, k=log(n))
stepwise1_BIC$call


anova(wages_re, re_inter)
#no differences



wages$non_zero_re78<- 99
wages$non_zero_re78[wages$re78 <= 0] <- 0
wages$non_zero_re78[wages$re78 > 0] <- 1
wages$non_zero_re78_fact <- factor(wages$non_zero_re78)


summary(wages[,-1])

apply(table(wages[,c('non_zero_re78_fact', 'training')]) / sum(table(wages[,c('non_zero_re78_fact', 'training')])), 2, function(x) x/sum(x))
apply(table(wages[,c('non_zero_re78_fact', 'black')]) / sum(table(wages[,c('non_zero_re78_fact', 'training')])), 2, function(x) x/sum(x))

apply(table(wages[,c('non_zero_re78_fact', 'married')]) / sum(table(wages[,c('non_zero_re78_fact', 'training')])), 2, function(x) x/sum(x))
apply(table(wages[,c('non_zero_re78_fact', 'nodegree')]) / sum(table(wages[,c('non_zero_re78_fact', 'training')])), 2, function(x) x/sum(x))
apply(table(wages[,c('non_zero_re78_fact', 'educ')]) / sum(table(wages[,c('non_zero_re78_fact', 'training')])), 2, function(x) x/sum(x))


ggplot(wages, aes(y=non_zero_re78_fact, x=re74, fill=non_zero_re78_fact)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Re74 vs Re78", x="Re74", y="Re78 ") 
  
ggplot(wages, aes(y=non_zero_re78_fact, x=re75, fill=non_zero_re78_fact)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Re75 vs Re78", x="Re75", y="Re78 ") 


re  <- glm(non_zero_re78_fact ~ training + age + educ + black + hisp + married + nodegree + re74 + re75  , family=binomial(link=logit), data = wages)
summary(re)
rawresid1 <- residuals(re, 'resp')


binnedplot(x=fitted(re), y=rawresid1, xlab='Wage probabilities', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


binnedplot(x=wages$age, y=rawresid1, xlab='Parity', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


binnedplot(x=wages$educ, y=rawresid1, xlab='Parity', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


binnedplot(x=wages$re74, y=rawresid1, xlab='Parity', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


binnedplot(x=wages$re75, y=rawresid1, xlab='Parity', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(re) >= 0.5, '1', '0')), 
                            as.factor(wages$non_zero_re78_fact), positive = '1')
Conf_mat$table
Conf_mat$overall['Accuracy']
Conf_mat$byClass[c('Sensitivity', 'Specificity')]


par(mfcol=c(2,1))
binnedplot(y=wages$non_zero_re78_fact[wages$black==0], 
           wages$training[wages$black==0], 
           xlab='Mothers Weight', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Premature', 
           main='Premature v Mother"s Weight vs No Smoke', 
           col.int='white')

binnedplot(y=smoking$premature[smoking$smoke==1], 
           smoking$mpregwt[smoking$smoke==1], 
           xlab='Mothers Weight', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Premature', 
           main='Premature v Mother"s Weight vs Smoke', 
           col.int='white')

re  <- glm(non_zero_re78_fact ~ training + age + educ + black + hisp + married + nodegree + re74 + re75  , family=binomial(link=logit), data = wages)

re_inter1  <- glm(non_zero_re78_fact ~ training + age + educ + black + hisp + married + nodegree + re74 + re75
                  + training*age + training*black + training*hisp + training*married + training*nodegree + training*educ, family=binomial(link=logit), data = wages)
summary(re_inter1)

anova(re, re_inter1, test='Chisq')
#training*age
n <- nrow(wages)
Model_backward2 <- step(re_inter1, direction='backward', trace=0, k=log(n))
Model_back_AIC2 <- step(re_inter1, direction='forward', trace=0)
Model_back_AIC2$call
Model_backward2$call

n <- nrow(wages)

step(glm(non_zero_re78_fact~1, data=wages, family=binomial), scope=formula(re_inter1), direction='both', trace=0, k=log(n))
step(glm(non_zero_re78_fact~1, data=wages, family=binomial), scope=formula(re_inter1), direction='both', trace=0)





