suppressPackageStartupMessages(library(tidyverse))
#msleep


#1. 	Run cor.test for the relationship between total sleep and body weight. 
#You should not round these values.

Q1<- cor.test(msleep$sleep_total, msleep$bodywt)

Q1

#2.	Create a correlation matrix for the relations among total sleep, 
#rem sleep, brain weight, and body weight.  Make sure to remove missing values.

relation_matrix <- summarise(msleep,sleep_total, 
                          sleep_rem, brainwt, bodywt) %>% 
  na.omit(relation_matrix)



matrix_cor <-cor(relation_matrix)

Q2 <- round(matrix_cor,2)

Q2
 





#3 Run a regression predicting body weight by vore. 
#Assign the coefficients to Q3
bodywt_vore <- select(msleep,bodywt,vore)


Q3 <- lm(bodywt_vore)

Q3


AIC(Q3, k = 1)

#4.	Create a regression predicting bodywt by vore and REM sleep.  
#Compared to the model in Q3, which one has the better AIC?

reg_bodywt<- lm(msleep$bodywt ~ msleep$vore + msleep$sleep_rem, data = msleep)

reg_bodywt

find_aic <- AIC(reg_bodywt, k = 2)

Q4 <- round(find_aic, 2)



#5.	Create a logistic regression predicting whether or not an animal is a 
#carnivore or herbivore based on sleep total

modelx<- msleep %>% select (vore, sleep_total) %>%
  filter(vore != "omni" & vore != "insecti") %>% 
  mutate(vorebin = ifelse(vore == 'carni', 0, 1))



Q5 <-glm(vorebin ~ sleep_total, data = modelx, family=binomial(link="logit"))


Q5

