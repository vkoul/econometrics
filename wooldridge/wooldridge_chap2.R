# install.packages("wooldridge")
library(wooldridge)
library(tidyverse)


# Ques 1 ------------------------------------------------------------------

# Question c1 
data('k401k')
?k401k

# avg participation rate 
mean(k401k$prate)
mean(k401k$mrate)

plot(k401k$mrate, k401k$prate)

cor(k401k$mrate, k401k$prate)

# model
m1 <- lm(prate ~ mrate, data = k401k)
m1

# prediction
predict(m1, data.frame(mrate = 3.5))



# Ques 2 ------------------------------------------------------------------
data('ceosal2')

# average tenure and salary 
mean(ceosal2$salary)
mean(ceosal2$ceoten, na.rm = TRUE)

sum(ceosal2$ceoten == 0)
max(ceosal2$ceoten)

# ceo ten vs salary 
plot(ceosal2$ceoten, ceosal2$salary)
cor(ceosal2$ceoten, ceosal2$salary)

## model 
salmodel <- lm(log(salary) ~ ceoten, data = ceosal2)

summary(salmodel)

### The output is neither statistically significant, neither the R2 of the model is high

## The expected percentage increase in salary would be : 
salmodel$coefficients[2]



# Ques 3 ------------------------------------------------------------------
data("sleep75")

cor(sleep75$totwrk, sleep75$sleep)
par(mar = c(5, 4, 4, 2))
plot(sleep75$totwrk, sleep75$sleep)
mean(sleep75$totwrk)
mean(sleep75$sleep)


## model 
sleepmodel <- lm(sleep ~ totwrk, data = sleep75)
summary(sleepmodel)

# if a person doesn't work, he will spend 3586 minutes sleeping and then for every
# additional minute of working, his sleep will reduce by 0.15 minutes

# although the model can explain the reality only for 10% 

## effect of 2 hours of work 
2*60 * sleepmodel$coefficients[2]

# so the sleep will decline by 18 minutes. Based on the average
# sleep time of 3266 minutes, the effect doesn't seem to be very large

summary(sleep75$sleep)


# Ques 4 ------------------------------------------------------------------
data('wage2')

wage2 %>% select(IQ, wage) %>% summary()

sd(wage2$IQ)

plot(wage2$IQ, wage2$wage)
cor(wage2$IQ, wage2$wage)

# model
iqmodel <- lm(wage ~ IQ, data = wage2)
summary(iqmodel)

## So with every point increase in the IQ, the wage will increase by 
## 8 units. Although with a low R2 of 0.09, this doesn't explain 
## the reality well

## increase in wage for 15 points of change in IQ

15 * iqmodel$coefficients[2]

# so the wage will increase by 124 units.


## Part2: trying to model for a percentage change 

iqmodel2 <- lm(log(wage) ~ log(IQ), data = wage2)

summary(iqmodel2)
summary(iqmodel)


# Now if we say, the IQ changed by 15 points then the percentage 
## change in wages would be 

15* iqmodel2$coefficients[2]

## So what this suggests is that with 15% change in IQ, wage 
## will increase by 12.5%


# Ques 5 ------------------------------------------------------------------

data('rdchem')

# relationship check 
plot(rdchem$rd, rdchem$sales)
cor(rdchem$rd, rdchem$sales)

# linear model 

# since they are asking for the elasticity values
## i will do it in the log form 

rdmodel <- lm(log(sales) ~ log(rd), data = rdchem)

summary(rdmodel)

## So the model says, for every 1% increase in the rd, 
## the sales will increase by 0.84% 


# Ques 6 ------------------------------------------------------------------

data("meap93")

plot(meap93$expend, meap93$math10)
cor(meap93$expend, meap93$math10)

mathmodel <- lm(math10 ~ log(expend), data = meap93)

summary(mathmodel)
mathmodel$coefficients[2]

## the spend has a negligible effect on math pass rate

## Yes, since the coefficient tells us about the percentage change
## the b1/10 is the appropriate value

# very low r2

### Math10 score change by changing spending 
0.10 * mathmodel$coefficients[2]

# So around 1% change in the spending
summary(meap93$math10)


# Ques 7 ------------------------------------------------------------------

data("charity")

mean(charity$gift)
mean(charity$gift == 0)
sum(charity$gift == 0)

## So only 40% of the people gave charity and on an average they paid 7.4 guilder

## mailings
summary(charity$mailsyear)


## model 
giftmodel <- lm(gift ~ mailsyear, data = charity )
summary(giftmodel)


## So without any mails, people usually pay 2 guilders and then with the mail
## remainder it increases by 2.6 guilders

charity %>% filter(gift != 0) %>% select(gift) %>% summary()



# Ques 8 `------------------------------------------------------------------




# Ques 9 ------------------------------------------------------------------

data("countymurders")

countymurders <- countymurders %>% filter(year == 1996)

## zero murders
sum(countymurders$murders == 0)
mean(countymurders$murders == 0)

## atleast one execution
sum(countymurders$execs >= 1)
mean(countymurders$execs >= 1)

max(countymurders$execs)

## model
murdermodel <- lm(murders ~ execs, data = countymurders)
summary(murdermodel)


### the executions increase the murders, however, the model has a very 
## low explainability based on R2 And it suggestes that capital punishment 
## has no deterrent impact on the murders

## Smallest number of murder predicted 
murdermodel$coefficients[1] + murdermodel$coefficients[2]

## Zero execution and murder
countymurders %>% filter(murders == 0)%>% filter(execs == 0)

## In the case of zero execution, the overall model atleast predicts 
## 5 murders based on the equation


## SLR is not well suited because the relationship is not easily captured in linear terms
plot(countymurders$murders, countymurders$execs)


# Ques 10 -----------------------------------------------------------------
data('catholic')

catholic %>% select(math12, read12) %>% summary()
catholic %>% select(math12, read12) %>% summarize(
  sd_math12 = sd(math12, na.rm = TRUE),
  sd_read12 = sd(read12, na.rm = TRUE)
)

## EDA
plot(catholic$read12, catholic$math12)
cor(catholic$read12, catholic$math12)


# MODEL
mathscore <- lm(math12 ~ read12, data = catholic)
summary(mathscore)

## The relationship is significant, but explains only 50% of the reality 


# Not supposed by the coefficients. This looks like the case of a confounding
## effect. the intelligence or preparation have an impact on the grades
## Although there are deviation hence the value is 0.7

summary(lm(read12 ~ math12, data = catholic))

# interesting for the inverse model too, the value is around 0.7

# No hiring more for reading won't help. This is a case of confounding


# Ques 11 -----------------------------------------------------------------
data("gpa1")

count(gpa1)

## gap summary
summary(gpa1$colGPA)
sum(gpa1$PC == 1)
mean(gpa1$PC == 1)

## model 
gpamodel <- lm(colGPA ~ PC, data = gpa1)

summary(gpamodel)

## The relationship is significant, however the R2 is very low and it shows
## its very hard to explain the GPA based on the PC ownership

## Nope, not at all. NO causal effect can be established. 


























