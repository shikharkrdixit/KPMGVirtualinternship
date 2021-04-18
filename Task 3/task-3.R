setwd("D:/R/KPMG Virtual Internship/Task 3")
library(readxl)
library(tidyverse)
library(plotly)
conn <- "./KPMG_VI_New_raw_data_update_final.xlsx"
data1 <- read_excel(conn,sheet="Transactions",skip = 1)
data2 <- read_excel(conn,sheet = "NewCustomerList",skip=1)
data3 <- read_excel(conn,sheet="CustomerDemographic",skip=1)
data4 <- read_excel(conn,sheet="CustomerAddress",skip=1)


data1 <- drop_na(data1)
data2 <- drop_na(data2)
data3 <- drop_na(data3)
data4 <- drop_na(data4)


data3 <- data3 %>%
  mutate(gender=recode(gender,Femal="F",Female="F",Male="M"))
data4 <- data4 %>%
  mutate(state= recode(state, "New South Wales" = "NSW", "Victoria" = "VIC"))
data2 <- data2 %>% 
  mutate(gender = recode(gender, Female = "F",
                         Male = "M"))




data3$DOB<-as.Date(as.numeric(data3$DOB,na.rm=TRUE), origin = "1899-12-30")
data2$DOB<-as.Date(as.numeric(data2$DOB,na.rm=TRUE), origin = "1899-12-30")
data1$product_first_sold_date <-as.Date(as.numeric(data1$product_first_sold_date,na.rm=TRUE), origin = "1899-12-30")


data5 <- merge(data3, data4, by="customer_id", all=TRUE)
data6 <- merge(data1,data5,by="customer_id",all=TRUE,na.rm=T)
data6 <- data6 %>% filter(state != "NA")


library(ggplot2)
data5 <- merge(data3, data4, by="customer_id", all=TRUE)
data6 <- merge(data1,data5,by="customer_id",all=TRUE,na.rm=T)
data6 <- data6 %>% filter(state != "NA")



data6.1<-data6
data6.1<-data6[data6$order_status=="Approved",]
data7.1<- data6.1 %>% 
  select(customer_id,list_price,standard_cost,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  group_by(customer_id,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  summarise (freq=n(),total_revenue=sum(list_price),avg_spend=mean(list_price),median_spend= median(list_price))
data7.1 <- data7.1 %>% filter(gender != "NA")
data6.1$DOB<-as.Date(as.numeric(data6.1$DOB,na.rm=TRUE), origin = "1899-12-30")
names(data7.1)
data6.1$Year <- substring(data6.1$DOB,1,4)
data6.1$Age <- 2021-as.numeric(data6.1$Year)




ggplot(data6.1)+
  aes(x=brand,y=Age)+
  geom_boxplot() + aes(fill=brand)+
  labs(x="brand",y="Age Range",title="Graph for suggesting Brand values w.r.t Age Range")+
  theme_dark()+
  theme(
    axis.text.y=element_blank())

data6.1 <- data6.1 %>% filter(state != 'NA')





ggp <- ggplot(data6.1)+
  aes(x=data6.1$state,y=data6.1$Age)+
  geom_bar(stat='identity') +
  aes(fill=state) +
  labs(x="state",y="Age",title="Age presence in States")+
  theme_bw()+
  theme(
    axis.text.y=element_blank())
ggplotly(ggp)


data2$Year <- substring(data2$DOB,1,4)


data2$Age <- 2021-as.numeric(data2$Year)
data2[data2$Year>2021,c("Year","Age")]
data2 <- subset(data2,data2$Year<2020)
name
ggplot(data2)+
  aes(x=state,y=Age)+
  geom_bar(stat='identity')+
  aes(fill=state)+
  labs(x="State",y="Age",title="Age Presence in different states in the New Customers Dataset")+
  theme(
    axis.text.y=element_blank())




data7.1$Year <- substring(data7.1$DOB,1,4)
data7.1$Age <- 2021-as.numeric(data7.1$Year)
ggplot(data7.1)+
  aes(x=state,y=freq) +
  geom_bar(stat='identity',alpha=0.5)+
  aes(fill=state) +
  labs(x="state",y="freq",title="State wise frequency distribution")+
  theme_bw()


head(data6.1$past_3_years_bike_related_purchases,2)

ggplot(data6.1)+
  aes(x=state,y=past_3_years_bike_related_purchases)+
  geom_bar(stat='identity',alpha=2) + geom_jitter(alpha=0.02)+
  aes(fill=state) + theme_dark()+
  labs(x="state",y="Past 3 years history",title="Past 3 years history in each state(Old Data)")
unique(data6.1$state)

names(data2)
ggplot(data2)+
  aes(x=state,y=past_3_years_bike_related_purchases)+
  geom_bar(stat='identity',alpha=2)+
  aes(fill=state) + theme_dark()+
  labs(x="state",y="Past 3 years history",title="Past 3 years history in each state(New Data)")


ggplot(data7.1)+
  aes(x=data7.1$gender,data7.1$freq)+
  geom_bar(stat='identity')+
  aes(fill=gender)+
  labs(x="gender",y="frequency",title = "Gender wise frequency(Old Data")

ggplot(data2)+
  aes(x=state,y=gender)+
  geom_bar(stat='identity')+
  aes(fill=gender)+
  labs(x="gender",y="state",title="Gender Presence in each State(New Data")

