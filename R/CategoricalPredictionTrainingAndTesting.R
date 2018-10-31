# Stock Market Data
library(ISLR)
head(Smarket)
str(Smarket)
table(Smarket$Direction) # show the frequencies with respect to the direction variable

contrasts(Smarket$Direction) # Indicates that R has created a dummy variable with a 1 for 'up'

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +Volume, family = binomial, data = Smarket)
summary(glm.fit)

glm.probs <- predict(glm.fit, type = 'response') # predict P(Y=1 | X)
# default for binamial model is prediction of log-odds

glm.probs[1:10]

glm.pred <- rep('Down', dim(Smarket)[1])
glm.pred[glm.probs > 0.5] <- 'Up'
table(glm.pred)
table(glm.pred, Smarket$Direction)

mean(glm.pred == Smarket$Direction)

# Does this imply that the logistic regression is working beter than random guessing?
# But this is for the training data
# To test the performance, we need to use different data than the data that are used to train the model.

train <- subset(Smarket, Smarket$Year < 2005)

Smarket.2005 <- subset(Smarket, Smarket$Year ==2005)
dim(Smarket.2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = 'response') # setting prediction for the testing set FROM the training set

glm.pred <- rep('Down', dim(Smarket.2005)[1])
glm.pred[glm.probs > 0.5] = 'Up'

table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction) # accuracy is 48%, worse than random guessing

# Insurance data: You can find the insurance data from HuskyCT
# Create a categorical variable lowcharge which equals 1 if insurance$charges < 7000 and equals 0 otherwise
# Run the logit regression of this on age, sex, bmi, smokre, region
# Split the dara by choosing 1000 observations for training and by using the other observations for test.
# Assess the accuracy of this model