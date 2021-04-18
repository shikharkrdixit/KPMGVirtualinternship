setwd("D:/R/KPMG Virtual Internship/Task 2")
conn <- "D:/R/KPMG Virtual Internship/Task 2/KPMG_VI_New_raw_data_update_final.xlsx"

data1 <- read_excel(conn,sheet = "Transactions",skip=1)

data2 <- read_excel(conn,sheet = "NewCustomerList",skip=1)

data3 <- read_excel(conn,sheet = "CustomerDemographic",skip=1)

data4 <- read_excel(conn,sheet = "CustomerAddress",skip=1)

data1 <- drop_na(data1)
sum(is.na(data1))
nrow(data1)
ncol(data1)

data2 <- drop_na(data2)
sum(is.na(data2))
nrow(data2)
ncol(data2)

data3 <- drop_na(data3)
sum(is.na(data3))
nrow(data3)
ncol(data3)

data4 <- drop_na(data4)
sum(is.na(data4))
nrow(data4)
ncol(data4)

levels(factor(data3$gender))
unique(data3$gender)
data3 <- data3 %>%
  mutate(gender=recode(gender,Femal="F",Female="F",Male="M"))



levels(factor(data4$state))
data4 <- data4 %>%
  mutate(state= recode(state, "New South Wales" = "NSW", "Victoria" = "VIC"))

##df2
levels(factor(data2$gender))
data2 <- data2 %>% 
  mutate(gender = recode(gender, Female = "F",
                         Male = "M"))





data3$DOB<-as.Date(as.numeric(data3$DOB,na.rm=TRUE), origin = "1899-12-30")
data2$DOB<-as.Date(as.numeric(data2$DOB,na.rm=TRUE), origin = "1899-12-30")
data1$product_first_sold_date<-as.Date(as.numeric(data1$product_first_sold_date,na.rm=TRUE), origin = "1899-12-30")




data5 <- merge(data3, data4, by="customer_id", all=TRUE)
data6 <- merge(data1,data5,by="customer_id",all=TRUE,na.rm=T)
data6 <- data6 %>% filter(state != "NA")
names(data6)
library(ggplot2)
ggplot(data6)+
  aes(x=data6$state,y=data6$past_3_years_bike_related_purchases)+
  geom_bar(stat='identity')+
  aes(fill=data6$state)+
  labs(x="States",y="Past three years Bike Related Purchases",title="Statewise Bike related purchases in last three years")


data7<- data6 %>% 
  select(customer_id,list_price,standard_cost,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  group_by(customer_id,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  summarise (freq=n(),total_revenue=sum(list_price), avg_spend=mean(list_price), median_spend= median(list_price))
drop_na(data7)
head(data7$property_valuation,2)
ggplot(data7) +
  aes(x=data7$state,data7$total_revenue)+
  geom_violin()+aes(fill=data7$state)+
  labs(x="States",y="Revenue",title="Violin Plot suggesting State v/s Revenue Graph")

data6.1<-data6
data6.1<-data6[data6$order_status=="Approved",]

data7.1<- data6.1 %>% 
  select(customer_id,list_price,standard_cost,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  group_by(customer_id,first_name,last_name,gender,past_3_years_bike_related_purchases,DOB,job_title,job_industry_category,wealth_segment,deceased_indicator,owns_car,tenure,address,postcode,state,country,property_valuation)%>%
  summarise (freq=n(),total_revenue=sum(list_price),avg_spend=mean(list_price),median_spend= median(list_price))
data7.1 <- data7.1 %>% filter(gender != "NA")
ggplot(data7.1)+
  aes(x=data7.1$gender,y=data7.1$freq)+
  geom_bar(stat='identity')+
  aes(fill=data7.1$gender)+
  labs(x="Gender",y="Frequency",title = "Gender wise Frequency")






