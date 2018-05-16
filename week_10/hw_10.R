rm(list = ls())
library(data.table)
library(arules)
library(plyr)
library(dplyr)

# Loading Data -------------------------------------------------------------

path <- "C:\\Users\\FuHung\\Documents\\GitHub\\CSX-DataScience\\Kaggle Presentation"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
products <- fread(file.path(path, "products.csv"))

# Reshaping Data -------------------------------------------------------------

products <- products %>%
  inner_join(aisles) %>% 
  inner_join(departments) %>% 
  select(-aisle_id,-department_id)

rm(aisles,departments)

orderp <- orderp %>% select(order_id,product_id) %>% left_join(products)
ordert <- ordert %>% select(order_id,product_id) %>% left_join(products)
orderpt <- rbind(orderp,ordert) 

rm(ordert,orderp,products)
gc()

orderpt <- orderpt %>% select(order_id,aisle)
samplenumber <- sample(unique(orderpt$order_id),50000)
sampleorder <- filter(orderpt,order_id %in% samplenumber)
rm(orderpt)

# Applying Model-------------------------------------------------------------
trans <- as(split(sampleorder[,"aisle"], sampleorder[,"order_id"]), "transactions")
itemFrequencyPlot(trans, support=0.15)
basket_rules <- apriori(trans,
                        parameter = list(sup = 0.1, conf = 0.5,target="rules",minlen = 2))

inspect(sort(basket_rules,by = 'lift'))
rulesort <- sort(basket_rules,by = 'lift')[1:20]


# Visualization -------------------------------------------------------------
library(arulesViz)
plot(rulesort)

plot(rulesort,method="grouped") 

plot(rulesort,method="graph",control=list(type="items"))

plot(rulesort,method = "paracoord",control = list(reorder = TRUE))

