# CSDA1050
# TNG e-commerce and retail project

order_products_train = read.csv("order_products_train.csv")
kable(head(order_products_train,5))
str(order_products_train)

order_products_prior=read.csv("order_products_prior.csv")
order_products_prior1=fread("order_products_prior.csv")
kable(head(order_products_prior,5))
str(order_products_prior)
glimpse(order_products_prior)

departments=read.csv("departments.csv")
kable(head(departments,5))
summary(departments)
str(departments)

orders = read.csv("orders.csv")
kable(head(orders,5))
str(orders)

products_subset = read.csv("products_subset.csv")
kable(head(products_subset,5))
str(products_subset)


products_subset %>% head()
ggplot(data = products_subset) + geom_bar(mapping = aes(x=product_name))

products = products_subset [, c(2,3,4,5)]
kable(head(products, 5))
str (products)

aisles =read.csv("aisles.csv")
kable(head(aisles, 5))
str (aisles)

#recode variables
orders = orders %>% mutate (order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))


library(arules)
library(arulesViz)
aisles=read.csv("aisles.csv")
head(aisles,5)
summary(aisles)
str(aisles)
sample_submission=read.csv("sample_submission.csv")
head(sample_submission,10)

# merge order_products_train and products_subset on product_id, to include the product names in the dataset of interest 
order_productnames_train=merge(order_products_train,products_subset, by="product_id")

head(order_productnames_train,5)
summary(order_productnames_train)
str(order_productnames_train)




# merge order_products_train and orders on order_id
order_orderid_train=merge(order_products_train,orders, by="order_id")
head(order_orderid_train,5)
order_orderid_train_select=order_orderid_train[, c(2,4,5)]
head(order_orderid_train_select,5)





# select order_id and order_name
order_productnames_train_select=order_productnames_train[, c(2,6)]
head(order_productnames_train_select,5)
summary(order_productnames_train_select)
str(order_productnames_train_select)
#convert the order_id to a vector of factors
order_productnames_train_select$order_id=factor(order_productnames_train_select$order_id)

# rename the order_productnames_train_select table to transaction table
transaction_table = order_productnames_train_select
head(transaction_table, 5)

# view content of the transaction table 
summary(transaction_table)
str(transaction_table)
write.csv(transaction_table, "transaction_table.csv")

# convert the dataset to sparse dataset
transaction_1 <- read.transactions(file.choose(),
                                   format = "single", sep=",", cols =c("order_id", "product_name"))
# file is "transaction_table.csv"

head(transaction_1,5)

# training apriori function on the transaction_1 dataset
rules = apriori (transaction_1, parameter =list (support=0.004,
                                                 confidence =0.005))

rules = apriori (transaction_1, parameter =list (support=0.004,
                                                 confidence =0.05))

LIST(head(transaction_1,5))

rules_confidence <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(rules_confidence))


head (transaction_1)
summary(transaction_1)
str(transaction_1)

# most frequent items
itemFrequencyPlot(transaction_1, topN=10)
itemFrequencyPlot(transaction_1, topN=20)

# least frequent items

least_items= barplot(sort(itemFrequency(transaction_1), decreasing = TRUE))
least_items
least_items= barplot(sort(itemFrequency(transaction_1), decreasing = TRUE, 10))

inspect(head(sort(rules, by = "lift"), 10))
plot (rules)
plot (rules, method ="grouped")


plot(rules, method ="graph")

plot (head(sort(rules, by ="lift"),5), method = "graph")

plot (head(sort(rules, by ="lift"),50), method = "graph")

dim(order_orderid_train_select)

# merge order_products_train and orders on order_id
order_orderid_train=merge(order_products_train,orders, by="order_id")
head(order_orderid_train,5)
order_orderid_train_select_1=order_orderid_train[, c(5,2,4)]
kable(head(order_orderid_train_select_1,5))

dim(order_orderid_train_select_1)

order_orderid_train_select_2=order_orderid_train[, c(5,2)]
head(order_orderid_train_select_2,5)

dim(order_orderid_train_select_2)

hist(order_orderid_train_select_2$user_id)
hist(order_orderid_train$user_id)

# convert the product_id to a vector of factors
order_orderid_train_select_1$product_id=factor(order_orderid_train_select_1$product_id)
summary(order_orderid_train_select_1)
str(order_orderid_train_select_1)


# convert the user_id to a vector of factors
order_orderid_train_select_1$user_id=factor(order_orderid_train_select_1$user_id)
summary(order_orderid_train_select_1)
str(order_orderid_train_select_1)

# check the dimensions of the dataset
dim(order_orderid_train_select_1)
levels(order_orderid_train_select_1$user_id)
levels(order_orderid_train_select_1$product_id)
levels(order_orderid_train_select_1$reordered)
sort(unique(order_orderid_train_select_1$reordered), decreasing = F)
head(order_orderid_train_select_1)
transaction_2 = as.data.frame(acast(order_orderid_train_sel
                                    
# Hour of day buying distribution

orders %>%
  ggplot(aes(x=order_hour_of_day))+
  geom_histogram(stat="count", fill = "mediumpurple2")

# Day of week buying distribution
orders %>%
  ggplot(aes(x=order_dow))+
  geom_histogram(stat="count", fill = "purple")

# Recode variables

orders = orders%>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))

# A look at the prior orders

eval_set_prior = orders %>% filter (eval_set == "prior") %>% count (order_number) %>% ggplot(aes(order_number,n))+ geom_line(color = "purple")+geom_point(color="purple")

#
eval_set_prior
eval_set_prior + ggtitle("Plot of prior order") +
  xlab ("order number") + ylab ("order count")

eval_set_prior = order_orderid_train %>% filter (eval_set == "prior") %>% count (order_id) %>% ggplot(aes(order_id,n))+ geom_line(color = "purple")+geom_point(color="purple")


orders %>%
  ggplot(aes(x=days_since_prior_order))+
  geom_histogram(stat="count", fill ="skyblue1")

kable(head(orders,5))

# graphical representation of the aisles organization within departments
products_departments = products %>% group_by(department_id, aisle_id) %>% summarise(n=n())
kable (head(products_departments,5))
dim (products_departments)
str (products_departments)

products_departments = products %>% group_by(department_id, aisle_id)
products_departments = products_departments %>% left_join(departments, by = "department_id", aisles, by = "aisle_id")
products_departments = products_departments %>% left_join(departments, by = "department_id")
products_departments = products_departments %>% left_join( aisles, by = "aisle_id")
kable (head (products_departments,5))

products_view = order_products_train %>% group_by(product_id) %>% summarize (count=n()) %>% left_join(products, by"product_id") %>%
  
  group_by(department_id, aisle_id) %>%
  summarize(sumcount=sum(count)) %>%
  left_join(products_departments, by = c("department_id" , "aisle_id")) %>% mutate (onesize = 1)



# products ordered distribution
product_bought <- order_products_train %>%
  group_by(product_id) %>%
  summarize (count=n()) %>%
  left_join(products) %>%
  left_join(departments) %>%
  left_join(aisles) %>%
  group_by(department, aisle) %>%
  summarize(count2 =sum(count))

treemap(product_bought, index =c("department", "aisle"), vSize = "count2", title = " ", palette = "Set2", border.col="#FFFFFF")


# products organization in aisle and department
product_arrangement_1 <- products %>%
  group_by(aisle_id, department_id) %>%
  summarize (count=n()) %>%
  ungroup() %>%
  left_join(aisles) %>%
  left_join(departments) %>%
  group_by(department, aisle) %>%
  summarize(count2 =sum(count))

treemap(product_arrangement_1, index =c("department", "aisle"), vSize = "count2", title = " ", palette = "Set2", border.col="#FFFFFF")

