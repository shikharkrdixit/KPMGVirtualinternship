library(readxl)
setwd("D:/R/KPMG Virtual Internship/Task 1/")
conn <- "D:/R/KPMG Virtual Internship/Task 1/KPMG_VI_New_raw_data_update_final.xlsx"

data1 <- read_excel(conn,sheet = "Transactions",skip=1)
head(data1,2)

data2 <- read_excel(conn,sheet = "NewCustomerList",skip=1)
head(data2,2)

data3 <- read_excel(conn,sheet = "CustomerDemographic",skip=1)
head(data3,2)

data4 <- read_excel(conn,sheet = "CustomerAddress",skip=1)
head(data4,2)


sum(is.na(data1))
sum(is.na(data3))
sum(is.na(data4))


unique(data3$gender)

length(unique(data1$customer_id[!is.na(data1$customer_id)]))


length(unique((data3$customer_id[!is.na(data3$customer_id)])))


length(unique(data4$customer_id[!is.na(data4$customer_id)]))
nrow(data1)
nrow(data3)
nrow(data4)
