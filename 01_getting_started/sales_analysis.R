# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----

library(tidyverse)
library(readxl)

# 2.0 Importing Files ----

bikes_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikeshops_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----

orderlines_tbl
glimpse(orderlines_tbl)
# 4.0 Joining Data ----

#left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ") %>%
  
  separate(col = location,
           into = c("city", "state"),
           sep = ", ") %>%
  
  mutate(total.price = price * quantity) %>%
  
  select(-...1, -gender) %>%
  
  select(-ends_with(".id")) %>%
  
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price, everything()) %>%
  
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

view(bike_orderlines_wrangled_tbl)
  
  
  
  
# 6.0 Business Insights ----
# 6.1 Sales by Year ----

library(lubridate)
# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  
  summarise(sales = sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

  
  
# Step 2 - Visualize

sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales)) +
           geom_col(fill = "#2DC6D6") +
           geom_label(aes(label = sales_text)) +
           geom_smooth(method = "lm", se = FALSE) +
           scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                             decimal.mark = ",", 
                                                             prefix = "", 
                                                             suffix = " €")) +
           labs(
                title    = "Revenue by year",
                subtitle = "Upward Trend",
                x = "", # Override defaults for x and y
                y = "Revenue"
               )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

view(sales_by_year_cat_1_tbl)
# Step 2 - Visualize

sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() +
  
  facet_wrap(~ category_1) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )
  

# 7.0 Writing Files ----

# 7.1 Excel ----

install.packages("writexl")
#library("writexl")
#bike_orderlines_wrangled_tbl %>%
  #write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 8.0 Challenge----
# Revenue by state
# Step 1 - Manipulate

revenue_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(total_price, state) %>%
  
  group_by(state) %>%
  summarise(revenue = sum(total_price)) %>%
  
  mutate(revenue_text = scales::dollar(revenue, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
view(revenue_by_state_tbl)

# Step 2 - Visualize

revenue_by_state_tbl %>%
  ggplot(aes(x = state, y = revenue)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = revenue_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# Revenue by year and location
# Step 1 - Manipulate
revenue_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(year, state) %>%
  summarize(revenue = sum(total_price)) %>%
  
  mutate(revenue_text = scales::dollar(revenue, big.mark = ".", 
                                       decimal.mark = ",", 
                                       prefix = "", 
                                       suffix = " €"))
view(revenue_by_year_state_tbl)

# Step 2 - Visualize

revenue_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = state)) +
  
  # Geometries
  geom_col() +
  
  facet_wrap(~ state) +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    fill = "City" # Changes the legend name
  )