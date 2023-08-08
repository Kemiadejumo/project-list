suppressPackageStartupMessages(library(tidyverse))

BRFSS2015 <- read_csv("BRFSS2015.csv")

                       # SECTION 1

#1. How many people have any kind of health care coverage?
#HLTHPLN1 ; people that said yes to having any kind of coverage.


Q1 <- 407556


#2. What is the average "Number of Days Mental Health Not Good" for
#  those in Pennsylvania who have numeric data? Make sure to change the 
#  response corresponding to none to 0.


# So we are using 2 groups; 1- 30days and none which is  = 0

Q2 <-  BRFSS2015 %>% filter( `_STATE` == 42, MENTHLTH <= 30 | MENTHLTH == 88 )%>% 
  select(MENTHLTH) %>% 
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>%
  summarise(mean(MENTHLTH))
  
  
  
# 3.
# p20 column 114 (HAVARTH3), P27 column 178-181 (WEIGHT2)

#WTKG# p114
Q3:
  # To compare, I separated have and don't have arthritis
  
Have_arthritis <- BRFSS2015 %>%  
  select(WTKG3, HAVARTH3) %>% 
  filter(HAVARTH3 == 1, WTKG3 <= 29500) %>% 
  summarise(wt_lb = (WTKG3/100) *2.20462) %>%
  summarise(mean_weight = mean(wt_lb, na.rm = TRUE),
                               mean_weight = round(mean_weight,2),
            sd_weight = sd(wt_lb, na.rm = TRUE),
            sd_weight = round(sd_weight, 2)) %>% as.data.frame

No_arthritis <-
  BRFSS2015 %>%  
  select(WTKG3, HAVARTH3) %>% 
  filter(HAVARTH3 == 2, WTKG3 <= 29500) %>% 
  summarise(wt_lb = (WTKG3/100) *2.20462) %>%
  summarise(mean_weight = mean(wt_lb, na.rm = TRUE),
            mean_weight = round(mean_weight,2),
            sd_weight = sd(wt_lb, na.rm = TRUE),
            sd_weight = round(sd_weight, 2)) %>% as.data.frame 


                # SECTION 2
#4.Remove outliers from minutes of total physical activity per week using 0.997
#and 0.003 as criteria.  What percentage of observations remain?
 #p131


min_total <- BRFSS2015 %>% 
  filter(PA1MIN_ <= 99999)


min_total

min_upper <- quantile(min_total$PA1MIN_, 0.997, na.rm = TRUE)
min_lower <- quantile(min_total$PA1MIN_, 0.003, na.rm = TRUE)
min_out <- which(min_total$PA1MIN_ > min_upper |
                   min_total$PA1MIN_ < min_lower)
percent_data <- (nrow(BRFSS2015) - length(min_out))/ nrow(BRFSS2015)* 100

Q4 <- round(percent_data, 2 )
   
min_noout <-  min_total[- min_out,] 





#5 Group by marital status and calculate the mean, standard deviation, minimum,
#and maximum of total exercise, to two decimals.


# #group marital as factor 
marital_fct <- min_noout %>%  select(MARITAL, PA1MIN_,`_FRUTSUM`) %>%
  mutate(MARITAL2 = as.factor(MARITAL))  

class(marital_fct$MARITAL2)

marital_status <- marital_fct %>% select(MARITAL2, PA1MIN_) %>%
  group_by(MARITAL2) %>%
  summarise(mean_exercise = round(mean(PA1MIN_),2),
           
            sd_exercise = round(sd(PA1MIN_),2),
            
            min_exercise = min(PA1MIN_),
           
            max_exercise = max(PA1MIN_),
            ) %>% as.data.frame()

marital_status

#6.Create a boxplot for total exercise by marital status.

#take the average for each factor
data6x <- marital_fct %>% select(MARITAL2, PA1MIN_) %>%
  group_by(MARITAL2) %>%
  summarise(mean_exercise = round(mean(PA1MIN_),2)) %>%
  as.data.frame() %>% select(mean_exercise)

#then create boxplot
Q6 <- boxplot(data6x)


#7.Run a regression predicting exercise by marital status

 data7_reg <- lm(PA1MIN_ ~ MARITAL2, data = marital_fct)
 
 Q7 <- summary(data7_reg)
 
 
 AIC(data7_reg)
 
#8.Run an ANOVA comparing exercise across marital status, and assign the
 #TukeyHSD post-hoc test to Q8. 
 
 
  
data8x <- aov(PA1MIN_ ~ MARITAL2, data = marital_fct)

Q8<-TukeyHSD(data8x) 


#9 Run a regression as in Q7, but add total fruits consumed per day. 
#Based on the R-squared and AIC, what is the better model?  
#Assign the better AIC value to Q9.

data9_reg <- lm(PA1MIN_ ~ MARITAL2 + `_FRUTSUM`, data = marital_fct)

Q9 <- AIC(data9_reg)
AIC(data7_reg)




                       #FINAL SECTION 

#var1 Number of Days Physical Health Not Good  - PHYSHLTH   (p13)
#var2How Many Hours Per Week Did You Work -SCNTLWK1 (p91)
#var3 Ever Diagnosed with a Stroke - CVDSTRK3 (18)
#var4Respondents Sex - SEX (p22)


#10. Remove any outliers.  Briefly explain why you chose the method you used. 
#Make sure to comment it out.


#check for outliers using bloxplot,
#the only variable with outlierS is var2 = SCNTLWK1
#then remove outliers 

# replace none value with 0
w_outliers <- BRFSS2015 %>% filter(SCNTLWK1 <= 96 |SCNTLWK1 == 98 ) %>%
             mutate(SCNTLWK1 = replace(SCNTLWK1, SCNTLWK1 == 98, 0)) 
  
  
#identify outliers
outliers <- boxplot(w_outliers$SCNTLWK1, plot = FALSE)$out


#remove outliers
no_outliers<- w_outliers[-which(w_outliers$SCNTLWK1 %in% outliers),]


x <- select(no_outliers, SCNTLWK1)


# I chose this method boxplot() function because it is a faster way to 
#identify the outliers and the which() function to find and and remove them.






#11.Address the values of any variables.  For instance, is "none" equal to 
#a value other than 0? Are there extra decimals implied? 

#var1 : PHYSHLTH has no extra decimals implied, the none value will equal 0
#var 3:CVDSTRK3 is categorical, the refused and missing values will be removed
 

no_outliers2 <-  no_outliers %>% 
       select(PHYSHLTH,SCNTLWK1,CVDSTRK3,SEX) %>%
       filter(PHYSHLTH <= 30 | PHYSHLTH == 88, CVDSTRK3 <=2) %>%
       mutate(PHYSHLTH = replace(PHYSHLTH, PHYSHLTH== 88, 0)) 
   
 
 #var 2 : SCNTLWK1 has no extra decimals implied, 
 #the none vale has been addresed in question 10.
 
 
#var 4:SEX variable is categorical, no extra decimals,
#no none values, no missing values
 


 
 
 
#12.Complete exploratory analyses doing appropriate visualizations with ggplot2.

#var 1 :
ggplot(data = no_outliers2) + geom_bar( mapping = aes (x = PHYSHLTH))


#var 2 :

ggplot(data = no_outliers2) + geom_bar( mapping = aes (x = SCNTLWK1))
#var 3 :
  ggplot(data = no_outliers2) + geom_bar( mapping = aes (x = CVDSTRK3))
#var 4 : 

ggplot(data = no_outliers2) + geom_bar(mapping = aes ( x = SEX))

  
#13.Run basic descriptive statistics
sapply(no_outliers2, var)

sapply(no_outliers2,sd)
summary(no_outliers2)

 

#14.Finally, run an appropriate regression predicting one of those variables.
#Identify the best model


#predicting that someone has " Ever Diagnosed with a Stroke " - var 3, 
#by var 1, var 2, and var 4


#change change categorical values to binary yes or no , 0,1
no_outliers3 <- no_outliers2 %>% 
  mutate(CVDSTRK3_new = ifelse(CVDSTRK3 == '1', 0, 1)) %>%
  mutate(SEX_new = ifelse(SEX == '1', 0, 1))      
  

#logistic regression is appropriate for predicting categorical variable
 mod1 <- glm(CVDSTRK3_new ~  PHYSHLTH,
                   data = no_outliers3, family=binomial(link="logit")) 
 
 mod2 <- glm(CVDSTRK3_new ~  SCNTLWK1,
               data = no_outliers3, family=binomial(link="logit")) 
 
 mod3 <- glm(CVDSTRK3_new ~  SEX_new,
               data = no_outliers3, family=binomial(link="logit")) 
   
  mod4 <-glm(CVDSTRK3_new ~ PHYSHLTH + SCNTLWK1 + SEX_new, 
               data = no_outliers3, family=binomial(link="logit")) 
 
#looking at the Aic, Lower AIC values indicate a better-fit model
  
  lowest_aic <- mod4




