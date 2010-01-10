marriage.data <- read.csv('gay_marriage.csv', header = TRUE, sep = '\t')

court.rulings <- subset(marriage.data, Type.of.Decision == 'Court Ruling')
congressional.decisions <- subset(marriage.data, Type.of.Decision == 'Congressional Decision')
direct.votes <- subset(marriage.data, Type.of.Decision == 'Direct Vote')
indirect.votes <- subset(marriage.data, Type.of.Decision != 'Direct Vote')

with(court.rulings, binom.test(sum(Promoted), length(Promoted)))
with(congressional.decisions, binom.test(sum(Promoted), length(Promoted)))
with(direct.votes, binom.test(sum(Promoted), length(Promoted)))
with(indirect.votes, binom.test(sum(Promoted), length(Promoted)))

marriage.data$Direct <- with(marriage.data,
                             ifelse(Type.of.Decision == 'Direct Vote', 1, 0))

logit.fit <- glm(Promoted ~ Direct,
                data = marriage.data,
                family = binomial(link = "logit"))
summary(logit.fit)
