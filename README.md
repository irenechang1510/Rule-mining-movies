Movies analysis
================
Irene N. Chang
12/24/2020

## Rule mining for movies and Apriori algorithm

```{r}
library(dplyr)
library(ggplot2)
library(arules)
library(htmlwidgets)
library(arulesViz)
library(readr)

Movie_subset <- read_csv("Movie_subset.csv")
Movie_subset %>% glimpse()
any(is.na(Movie_subset)) ## No missing data
```

Number of distinct movies and users

```{r}
n_distinct(Movie_subset$userId)
n_distinct(Movie_subset$movieId)
```

Distribution of the number of movies watched by users

```{r}
Movie_subset %>% 
	group_by(userId) %>%
	summarize(nb_movies = n_distinct(movieId)) %>%
	ggplot(aes(x = nb_movies)) +
	geom_bar() +
	ggtitle("Distribution of number of movies watched")

data_list <- split(Movie_subset$title,
									Movie_subset$userId)
movie_trx <- as(data_list, "transactions")
image(movie_trx[1:100, 1:100]) ##plot the matrix
summary(movie_trx) # density: 0.042
```

Frequency plots

```{r}
par(mfrow=c(2, 1))
itemFrequencyPlot(movie_trx,
									type = "relative",
									topN = 10,
									horiz = TRUE,
									main = 'Relative item frequency')
itemFrequencyPlot(movie_trx,
									type = "absolute",
									topN = 10,
									horiz=T,
									main = "Absolute item frequency")
#Least popular items
par(mar=c(2,20,2,2), mfrow=c(1,1))
barplot(sort(table(unlist(LIST(movie_trx))))[1:10],
				horiz = TRUE,
				las = 1,
				main = 'Least popular items')
```

Mining the dataset

```{r}
itemsets <- apriori(movie_trx, parameter = list(support = 0.4,
																								target = 'frequent itemsets'))
inspect(sort(itemsets, by = "support", decreasing = T)[1:5]) # 5 most popular items
```

Extract the set of most frequent itemsets

```{r}
itemsets_minlen2 = apriori(movie_trx, parameter = 
													 	list(support = 0.3,
													 			 minlen = 2,
													 			 target = 'frequent'
													 	))

# Inspect the five most popular items
inspect(sort(itemsets_minlen2, 
						 by='support', decreasing = T)[1:5])
# 5 least popular 
inspect(sort(itemsets_minlen2, 
						 by='support', decreasing = F)[1:5])
```

Pick the parameters

```{r}
# Number of extracted rules with varying conf levels and supp level of 40%
confidenceLevels = seq(from=0.95, to=0.5, by=-0.05)
rules_sup04 = NULL
for (i in 1:length(confidenceLevels)) {
	rules_sup04[i] = 
		length(apriori(movie_trx,
									 parameter=list(sup=0.4, 
									 							 conf=confidenceLevels[i],
									 							 target="rules")))
}

qplot(confidenceLevels, rules_sup04, 
			geom=c("point", "line"),xlab="Confidence level",
			ylab="Number of rules found", 
			main="Apriori with a support level of 40%") +
	theme_bw()

rules_sup03 <- c(10, 26, 52, 90, 129, 162, 194, 220, 238, 254)
nb_rules <- data.frame(rules_sup04, rules_sup03,
											confidenceLevels)
ggplot(data=nb_rules, aes(x=confidenceLevels)) +
	# Lines and points for rules_sup04
	geom_line(aes(y=rules_sup04, colour="Support level of 40%")) + 
	geom_point(aes(y=rules_sup04,colour="Support level of 40%")) +
	# Lines and points for rules_sup03
	geom_line(aes(y=rules_sup03, colour="Support level of 30%")) +
	geom_point(aes(y=rules_sup03,colour="Support level of 30%")) + 
	# Polishing the graph
	theme_bw() + ylab("") +
	ggtitle("Number of extracted rules with apriori")

# choose min support of 30% and confidence of 90%
```

### EXTRACT MOVIES RULES

```{r}
# Extract rules with the apriori
rules_movies = apriori(movie_trx,
											 parameter = list(supp = 0.3,
											 								 conf = 0.9,
											 								 minlen = 2, 
											 								 target = "rules"))
summary(rules_movies)
# top rules by lift
inspect(head(sort(rules_movies, by = "lift", decreasing = T), 5))

rules_red = is.redundant(rules_movies)
rules.pruned = rules_movies[!rules_red]
inspect(head(sort(rules.pruned, by="confidence")))

inspectDT(rules_movies)
```

Plot rules interactive

```{r}
plot(rules_movies,
		 measure = c("confidence", "lift"),
		 shading = "support",
		 jitter = 1,
		 engine = "html")

plot(rules_movies, method = "matrix",
		 shading ="confidence",
		 engine = "html"
)

plot(rules_movies, 
		 method = "grouped",
		 measure = "lift",
		 shading = "confidence")

plot(rules_movies, 
		 method = "paracoord", 
		 shading = "confidence")

plot(rules_movies,
		 method = "graph",
		 engine = "htmlwidget")
```
```{r}
# Retrieve the top 10 rules with highest confidence
top10_rules_movies <- head(sort(rules_movies,
															 by = "confidence"), 10)
plot(top10_rules_movies,
		 method = "graph",engine = "htmlwidget")

# AMong rules with high confidence only The Matrix is not part of LOTR
```

###  Q: Which watched movies are most likely to lead to a recommendation of the movie Pulp Fiction?
```{r}
# Extract rules with Pulp Fiction on the right side
pulpfiction_rules_rhs = apriori(movie_trx, 
																parameter = list(supp = 0.3,
																								 conf = 0.5), 
																appearance = list(default = "lhs",
																									rhs = "Pulp Fiction")) 

# Inspect the first rules
inspect(head(pulpfiction_rules_rhs))
# Find rules with highest lift
inspect(head(sort(pulpfiction_rules_rhs, by="lift"), 10))
inspect(head(sort(pulpfiction_rules_rhs, by = "confidence")))

# Pulp Fiction on the lhs
pulpfiction_rules_lhs = apriori(movie_trx, 
																parameter = list(supp = 0.3,
																								 conf = 0.5, 
																								 minlen = 2), 
																appearance = list(
																	default = "rhs",
																	lhs = "Pulp Fiction"))
summary(pulpfiction_rules_lhs)
inspect(head(pulpfiction_rules_lhs))
```
