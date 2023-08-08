suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
nycflights13::flights



#1.What is the mean distance of flights for each of the carriers AA, EV, and FL?

Q1<- flights %>% select(carrier, distance) %>%
  filter( carrier == "AA" | carrier == "FL" | carrier == "EV") %>%
  group_by(carrier) %>% 
  summarise(mean_dis = mean(distance), mean_dis = round(mean_dis,2)) %>%
  as.data.frame(flights) 
  




#2.2.	For the month with the highest number of flights, what is that value? 
#Hint: use head(1)

 
  
daily <- group_by(flights, month, day)
(per_day   <- summarise(daily, flights = n()))
Q2 <- summarise(per_day, flights = sum(flights)) %>% 
  arrange(desc(flights)) %>% head(1)


#3.	Find the five shortest minimum distances, called min_dist, 
#by origin/destination combination

Q3 <- flights %>%
  select(origin,dest,distance) %>% 
   group_by(origin, dest)%>%
     summarise(min_dist = min(distance))%>%
     arrange(min_dist)%>% head(5)




  
  


#4. What five days of the year had the highest mean distance when 
#leaving from JFK?  Sort in descending order.

Q4 <-
  flights %>% select(month,day,distance,origin) %>%
  filter(origin == "JFK") %>%
group_by(month,day) %>%
  summarise(mean_distance = mean(distance),
            mean_distance = round(mean_distance,2)) %>%
  arrange(desc(mean_distance))%>% head(5) %>% as.data.frame


  

#5.Calculate the maximum arrival delay for flights to Boston and Atlanta, separately

Q5<- filter(flights, dest== "BOS" | dest == "ATL")%>% 
  select(dest,arr_delay) %>%
  group_by(dest) %>%
  summarise_at(vars(arr_delay), list(mean_dis = max), na.rm = TRUE)




