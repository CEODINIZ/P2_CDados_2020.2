# Tea

db <- read.table("tea.csv", sep = ";", header = TRUE)

colnames(db)

tea <- db[, c(13, 14, 16, 15, 17, 2, 20, 22, 19)]

tea$age <- cut(tea$age,
               breaks = c(0, 24, 34, 44, 59, 200),
               labels = c("15-24", "25-34", "35-44", "45-59", "60+"))

table(tea$age)

summary(tea)

trn <- as(tea, "transactions")
summary(trn)

rules <- apriori(trn, parameter = list(sup = 0.2, conf = 0.8))

inspect(rules[1])

sum(tea$age == "15-24" & tea$variety == "flavoured") / sum(tea$age == "15-24")

inspect(rules[1:10])
inspect(head(rules, n = 10, by = c("confidence", "lift")))

target <- subset(rules, subset = (! rhs %in% "place.of.purchase=supermarket"))
inspect(target)

