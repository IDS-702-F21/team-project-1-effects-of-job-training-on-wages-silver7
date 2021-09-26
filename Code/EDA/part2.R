library(data.table)
library(ggplot2)
library(magrittr)
library(readr)
library(ggplot2)
library(dplyr)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(gridExtra)
library(magrittr)

outcome = re78 - re74  

df <- fread("/Users/michellevan/Documents/mids/Stats/homework/team1/lalonde.csv")
df[,outcome := re78 - re74]
df[,b := as.numeric(re78>0)]
df[,bf := factor(re78>0)]
df[,race := c("black", "other")[black+1]]
df[hispan==1,race := "hispanic"]
df[,race := factor(race)]
df[,nodegree_f := factor(nodegree)]
df[,married_f := factor(married)]
df[,treat_f := factor(treat)]
df[,growth := re78 - re74]
df[treat==0,growth := re78 - re75]

df$non_zero_re78<- 99
df$non_zero_re78[df$re78 <= 0] <- 0
df$non_zero_re78[df$re78 > 0] <- 1
df$non_zero_re78_fact <- factor(df$non_zero_re78)


summary(df[,-1])


table(df[,c('non_zero_re78_fact', 'treat')])
table(df[,c('non_zero_re78_fact', 'treat')]) / sum(table(df[,c('non_zero_re78_fact', 'treat')]))

apply(table(df[,c('non_zero_re78_fact', 'treat')]) / sum(table(df[,c('non_zero_re78_fact', 'treat')])), 2, function(x) x/sum(x))


table(df[,c('non_zero_re78_fact', 'race')])
table(df[,c('non_zero_re78_fact', 'race')]) / sum(table(df[,c('non_zero_re78_fact', 'race')]))

apply(table(df[,c('non_zero_re78_fact', 'race')]) / sum(table(df[,c('non_zero_re78_fact', 'race')])), 2, function(x) x/sum(x))

table(df[,c('non_zero_re78_fact', 'married')])
table(df[,c('non_zero_re78_fact', 'married')]) / sum(table(df[,c('non_zero_re78_fact', 'married')]))

apply(table(df[,c('non_zero_re78_fact', 'married')]) / sum(table(df[,c('non_zero_re78_fact', 'married')])), 2, function(x) x/sum(x))

table(df[,c('non_zero_re78_fact', 'educ')])
table(df[,c('non_zero_re78_fact', 'educ')]) / sum(table(df[,c('non_zero_re78_fact', 'educ')]))

apply(table(df[,c('non_zero_re78_fact', 'educ')]) / sum(table(df[,c('non_zero_re78_fact', 'educ')])), 2, function(x) x/sum(x))


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

re  <- glm(non_zero_re78_fact ~ treat + age + educ + race + married + nodegree + re74 + re75, family=binomial(link=logit), data = df)
summary(re)
rawresid1 <- residuals(re, 'resp')
# age, raceother, re75 p value low

binnedplot(x=fitted(re), y=rawresid1, xlab='Wage probabilities', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')
#one outside 95% conf level

binnedplot(y=df$non_zero_re78, df$age, xlab='Nonzero prob', ylim=c(0,1),  col.pts='navy', ylab ='Age', main='Binned Smoke and Premature cases', col.int='white')

#Interactions
par(mfcol=c(2,1))
binnedplot(y=df$non_zero_re78[df$treat==0], 
           df$age[df$treat==0], 
           xlab='Aget', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Non-zero p', 
           main='Non-zero v Age vs No Training', 
           col.int='white')

binnedplot(y=df$non_zero_re78[df$treat==1], 
           df$age[df$treat==1], 
           xlab='Age', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Non-zero p', 
           main='Non-zero v Age vs No Training', 
           col.int='white')

# differences between the two - Age v training - need to add in model



ggplot(df, aes(y=non_zero_re78_fact, x=re74, fill=non_zero_re78_fact)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Re74 vs Training vs Prob", x="Re74", y="Re78 ") +
  facet_wrap(~ treat)


ggplot(df, aes(y=non_zero_re78_fact, x=re75, fill=non_zero_re78_fact)) + 
  geom_boxplot() + 
  theme(legend.position='none') + 
  scale_fill_brewer(palette='YlOrBr') + 
  labs(title="Re75 vs Training v Prob", x="Re75", y="Re78 ") +
  facet_wrap(~ treat)

par(mfcol=c(2,1))

binnedplot(y=df$non_zero_re78[df$treat==0], 
           df$educ[df$treat==0], 
           xlab='Aget', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Non-zero p', 
           main='Non-zero v Educ vs No Training', 
           col.int='white')

binnedplot(y=df$non_zero_re78[df$treat==1], 
           df$educ[df$treat==1], 
           xlab='Aget', 
           ylim=c(0,1),  
           col.pts='navy', 
           ylab ='Non-zero p', 
           main='Non-zero v Educ vs No Training', 
           col.int='white')
#not enough points in training = 1 - might be worth adding into our model

#Training v race v outcome
interact_df = data.frame(
  treat0 = df[df$treat==0,] %$% table(race, b) %>% prop.table(1) %>% c %>% extract(4:6),
  treat1 = df[df$treat==1,] %$% table(race, b) %>% prop.table(1) %>% c %>% extract(4:6),
  race = levels(df$race)
)

interact_df

#Training v educ v outcome
interact_df1 = data.frame(
  treat0 = df[df$treat==0,] %$% table(nodegree, b) %>% prop.table(1) %>% c %>% extract(3:4),
  treat1 = df[df$treat==1,] %$% table(nodegree, b) %>% prop.table(1) %>% c %>% extract(3:4),
  nodegree = 0:1
)

interact_df1

#re_inter  <- glm(non_zero_re78_fact ~ treat + age + educ + race + married + nodegree + re74 + re75 
                 + treat*race + treat*age , family=binomial(link=logit), data = df)
re_inter  <- glm(non_zero_re78_fact ~ treat + age + educ + race + married + nodegree + re74 + re75 
           + treat*race + treat*educ + treat*age + treat*nodegree, family=binomial(link=logit), data = df)
summary(re_inter)
rawresid1 <- residuals(re_inter, 'resp')


binnedplot(x=fitted(re_inter), y=rawresid1, xlab='Wage probabilities', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')

anova(re, re_inter, test='Chisq')
#interactions have an effect

n <- nrow(df)
Model_backward2 <- step(re_inter, direction='backward', trace=0, k=log(n))
Model_back_AIC2 <- step(re_inter, direction='forward', trace=0)
Model_back_AIC2$call
Model_backward2$call


step(glm(non_zero_re78_fact~1, data=df, family=binomial), scope=formula(re_inter), direction='both', trace=0, k=log(n))
step(glm(non_zero_re78_fact~1, data=df, family=binomial), scope=formula(re_inter), direction='both', trace=0)

#use backwardsBIC - gives - treat + age + re74 + treat*age


final_model <- glm(non_zero_re78_fact ~ treat + age + re74 + treat*age , family=binomial(link=logit), data = df)
summary(final_model)
rawresid2 <- residuals(final_model, 'resp')
binnedplot(x=fitted(re), y=rawresid2, xlab='Wage probabilities', 
           col.int='red4', ylab='Avg residuals', main='binned residual plot', col.pts='navy')


Conf_mat_fin <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= mean(df$non_zero_re78), '1', '0')), 
                                as.factor(df$non_zero_re78), positive = '1')
Conf_mat_fin$table
Conf_mat_fin$overall['Accuracy']
Conf_mat_fin$byClass[c('Sensitivity', 'Specificity')]


roc(df$non_zero_re78, fitted(final_model), plot=T, print.thres='best', legacy.axes=T,     
    print.auc=T, col='red3')
