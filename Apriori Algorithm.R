install.packages('arules')
install.packages('arulesViz')
install.packages('plyr')
install.packages('dplyr')
install.packages('ggplot2')

library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(ggplot2)


#Getting the data
groceries <- read.csv(file.choose(), header =TRUE)

#checking missing values
colSums(is.na(groceries))

# Aggregating and sorting the data
groceries_agg <- groceries %>% 
        group_by(Member_number) %>% 
        dplyr::summarise(., count = n()) %>%
        arrange(count)

#Visualization
ggplot(groceries_agg, aes(x = count)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, colour = "#333333", fill = "#FFFFFF") +
  geom_density(alpha = .2, fill = "#3399FF") +
  ggtitle("Distribution of Grocery Purchases by Customer") +
  xlab("Number of Purchases") +
  ylab("Density") +
  theme_dark()


#making new table for single session purchased
trans <- plyr::ddply(groceries, c("Member_number", "Date"), function(df) paste(df$itemDescription, collapse = ","))
trans$V1 <- gsub("\"", "", trans$V1) # get rid of extra " characters
trans$cart <- paste(trans$Date, trans$Member_number, sep = "-") #combine into a cart
head(trans)

#Code for write out csv file
trans$Member_number <- NULL
trans$Date <- NULL

write.csv(trans, file = "transactions.csv", quote = FALSE, row.names = TRUE)

#Creating Association Rules
data <- arules::read.transactions(file = "transactions.csv", rm.duplicates = TRUE, format = "basket", sep = ",", cols = 1)

#View the frequency of different items
itemFrequencyPlot(data, support = 0.05, col = c("blue", "red", "green"))

rules <- arules::apriori(data, parameter = list(minlen = 2, 
                                                sup = 0.002, 
                                                conf = 0.02, 
                                                target = "rules"))

summary(rules)

#Inspect
arules::inspect(rules[1:10])

#Visualization of Rules
top.conf <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
plot(top.conf[1:20], method="graph")

subrules <- head(rules, n = 10, by = "confidence")

plot(subrules, method = "graph",  engine = "htmlwidget")

plot(subrules, method="paracoord")
