
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "C:\\Users\\FuHung\\Documents\\GitHub\\CSX-DataScience\\Kaggle Presentation"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

#orders每筆訂單&user
#ordert每筆訂單
#orderp每筆訂單
#products每種商品
# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

#對應所有商品的aisles和departments
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

#把ordert的user_id補上
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]
#挑出orders屬於orderp的資料合併成orders_products
orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()


# Products ----------------------------------------------------------------
prd <- orders_products %>%#prior only
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),#該產品訂單量
    prod_reorders = sum(reordered),#該產品reorder訂單量
    prod_first_orders = sum(product_time == 1),#prod_first_orders 屬於某人的第一筆訂單
    prod_second_orders = sum(product_time == 2)#prod_second_orders 屬於某人的第二筆訂單
  )
#同一個人reorder這項產品的機率
prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
#expected reorder 次數
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
#reorder佔總order比例
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders


prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%#prior only
  group_by(user_id) %>%#依user分組
  summarise(
    user_orders = max(order_number),#user_orders 該用戶總訂單
    user_period = sum(days_since_prior_order, na.rm = T),#user_period 該用戶從第一筆訂單到最後一筆訂單總時長
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)#user_mean_days_prior_order 兩筆訂單平均間隔
  )

us <- orders_products %>%#prior only
  group_by(user_id) %>%#依user分組
  summarise(
    user_total_products = n(),#總購買產品數
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),#reorder時再次購買該件商品的機率
    user_distinct_products = n_distinct(product_id)#購買的產品種類數
  )

users <- users %>% inner_join(us)#users和us合併
users$user_average_basket <- users$user_total_products / users$user_orders#某用戶一張訂單平均買幾件商品

us <- orders %>%
  filter(eval_set != "prior") %>%#挑出非prior資料(train,test)
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)#users和us合併(train,test)

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),#某用戶購買該產品的次數
    up_first_order = min(order_number),#第幾筆訂單中首次出現該產品
    up_last_order = max(order_number),#第幾筆訂單中最後一次出現該產品
    up_average_cart_position = mean(add_to_cart_order))#平均購物車位置

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%#合併產品訊息
  inner_join(users, by = "user_id")#合併用戶訊息

data$up_order_rate <- data$up_orders / data$user_orders#該用戶訂單包含該產品的機率
data$up_orders_since_last_order <- data$user_orders - data$up_last_order#最後一次購買該產品後的新訂單數
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)#首次購買該產品後該用戶訂單包含該產品的機率

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))#將training data裡reorder一欄加入

rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain <- train %>% sample_frac(0.1)
X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)

test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)

