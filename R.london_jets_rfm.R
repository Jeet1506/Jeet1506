# RFM ANALYSIS USING LONDON JETS DATA

install.packages("readxl")
library(readxl)                                 
install.packages("rfm")
library(rfm)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("zoo")
library(zoo)

data <- read_excel("london_jets.xls", sheet = "Customer")
data1 <- data %>% select(CustID,Num_Games,Avg_Seats,Tot_Sales,LastTransYear,LastTransMonth)%>%
  mutate(day = 01)

data1$LastTransdate <- as.Date(paste(data1$LastTransYear,data1$LastTransMonth,data1$day),
                               "%Y %m %d")

str(data1)


colSums(is.na(data1))
summary(data1)

analysis_date <- as_date("2001-12-02")

data2 <- data1 %>% mutate(recency_days = analysis_date - LastTransdate)

rfm_result <- rfm_table_customer(data = data2,
                                   customer_id = CustID,
                                   n_transactions = Num_Games,
                                   recency_days = recency_days,
                                   total_revenue = Tot_Sales,
                                   analysis_date = analysis_date)

rfm_result1 = as.data.frame(rfm_result$rfm)
rfm <- rfm_result$rfm
rfm
str(rfm_result)

rfm_result <- data.frame(rfm_result)


write.csv(rfm_result,"london_jets_rfm.csv")

segment_titles <- c("best customer","loyal customer","likely to be loyal customer",
                     "big spender","new customer","almost lost","lost customer",
                     "lost non-valuable customer")

r_low <- c(4,1,3,1,2,1,1,1)
r_high <- c(5,5,5,5,4,3,2,2)

f_low <- c(4,4,1,1,1,3,2,1)
f_high <- c(5,5,3,5,3,5,4,2)

m_low <- c(4,1,1,4,1,3,1,1)
m_high <- c(5,5,3,5,3,5,3,2)

divisions <- rfm_segment(rfm_result,segment_titles,r_low,r_high,f_low,f_high,m_low,m_high)

cust_segment <- divisions %>% count(segment) %>% arrange(desc(n)) %>% rename(Segment = segment,
                                                                             Frequency = n) %>%
  mutate(Percentage = Frequency/sum(Frequency)*100)
View(cust_segment)

write.csv(cust_segment,"london_jets_cust_segment.csv")

london_jet_cust_group <- divisions %>%
  group_by(segment) %>%
  summarise(MEDIAN_F = median(transaction_count),
            MEDIAN_R = median(recency_days),
            MEDIAN_M = median(amount)) %>%
  arrange(desc(MEDIAN_M))
write.csv(london_jet_cust_group,"London_Jet_Cust_Group.csv")

install.packages("ggplot2") 
library(ggplot2)

rfm_histograms(rfm_result)
rfm_bar_chart(rfm_result)
rfm_order_dist(rfm_result)
rfm_heatmap(rfm_result)

barplot(cust_segment$Percentage~cust_segment$Segment,main ="Bar Plot of Cust_Segment",
        xlab = "segments",ylab = "%customer",axis_names = TRUE )
