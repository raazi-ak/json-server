library(arules)
library(arulesViz)
library(ggplot2)

# Read the dataset
dataset <- read.csv("C:/Users/Student/Desktop/apriori.csv", stringsAsFactors = FALSE)

# Remove the TransactionID column
dataset$TransactionID <- NULL

# Split the items in each transaction into a list
transactions_list <- strsplit(as.character(dataset$Items), ",\\s*")

# Convert the list of transactions to a transaction object
transactions <- as(transactions_list, "transactions")

# Run Apriori algorithm
basket_rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.5, target = "rules"))

# Inspect the top 3 rules
inspect(basket_rules[0:3])

# Flatten the list of transactions into a vector of items
all_items <- unlist(transactions_list)

# Create a data frame with item frequencies
item_freqs <- as.data.frame(table(all_items))
colnames(item_freqs) <- c("Item", "Frequency")

# Order items by frequency in descending order
item_freqs <- item_freqs[order(-item_freqs$Frequency), ]

# Plot the bar plot of item frequencies
ggplot(item_freqs, aes(x = reorder(Item, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Item", y = "Frequency", title = "Most Commonly Bought Items") +
  theme_minimal()

# Plot a graph of the association rules
plot(basket_rules, method = "graph", main = "Graph Plot of Association Rules")
