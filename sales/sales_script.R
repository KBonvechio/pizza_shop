# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# use a loop to load all datasets 
i <- 1
for (salesdata in all_sales){
  datafile <- read_csv(paste0("./sales/", salesdata))
  assign(paste0("dataset_", i), datafile)
  i <- i + 1
}

all_sales

############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code

# Answer
# I was able to use the full_join() function to combine 12 of the 16 datasets in
# a piping operation. However, there were some values in four of the datasets 
# that had matching rows with the dataset I was trying to join them with (i.e.,
# each set of datasets had some rows with identical values for each variable/
# column), so I had to use the bind_rows() function (also in dplyr) to add the 
# final four datasets. I also used the select() function to remove the extra 
# "date" column that was carried over from the merge with dataset_15. This is
# the only way I could get the sales_data dataset to contain all 896 rows of 
# data.

sales_data <- dataset_1 %>%
  full_join(dataset_2, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_4, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_6, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_7, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_8, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_9, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_10, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_13, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_14, by = c("day", "month", "year", "pizza", "number")) %>%
  full_join(dataset_15, by = c("day", "month", "year", "pizza", "number")) %>%
  select("day", "month", "year", "pizza", "number") %>%
  full_join(dataset_16, by = c("day", "month", "year", "pizza", "number")) %>%
  bind_rows(dataset_3, dataset_5, dataset_11, dataset_12)

  ########################################

##### 3. Create summaries #####
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")