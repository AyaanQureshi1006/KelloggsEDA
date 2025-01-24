#loading the dataset
data=read.csv('cornflakes2.csv')
head(data)

# Age
mean(data$Age)
median(data$Age)
sd(data$Age)
hist(data$Age, main = "Histogram of Age",col='Blue')

# Gender
library(scales)
gender_freq <- table(data$Gender)
gender_freq
# Calculate the percentage of each gender
gender_percent <- prop.table(gender_freq) * 100
# Create a pie chart with percentage labels
pie(gender_freq, labels = paste0(names(gender_freq), "\n", round(gender_percent, 1), "%"),
    main = "Gender Proportion", col = c('Red','Blue'))

# Weight (in kg)
mean(data$Weight..kg.)
median(data$Weight..kg.)
sd(data$Weight..kg.)
IQR(data$Weight..kg.)
summary(data$Weight..kg.)
hist(data$Weight..kg., main = "Histogram of Weight (in kg)",col='Red')

# Annual Household Income
mean(data$Annual.Household.Income..In.lakhs.)
median(data$Annual.Household.Income..In.lakhs.)
sd(data$Annual.Household.Income..In.lakhs.)
IQR(data$Annual.Household.Income..In.lakhs.)
summary(data$Annual.Household.Income..In.lakhs.)
boxplot(data$Annual.Household.Income..In.lakhs., main = "Boxplot of Annual Household Income")
hist(data$Annual.Household.Income..In.lakhs.,main="Histogram of Annual Household Income",col='Green')

# Diabetes
diab_freq <- table(data$Diabete)
diab_freq
diab_percent <- prop.table(diab_freq) * 100
pie(diab_freq, labels = paste0(names(diab_freq), "\n", round(diab_percent, 1), "%"),
    main = "Diabetes Proportion", col = c('Red','Blue'))

data$Diabete[data$Diabete=='Yes']=1
data$Diabete[data$Diabete=='No']=0
head(data)

data$Diabete <- as.numeric(data$Diabete)
mean(data$Diabete[data$Diabete == 1], na.rm = TRUE)
mean(data$Diabete[data$Diabete == 0], na.rm = TRUE)
barplot(table(data$Diabete), main = "Diabetes Frequency")

# Flavors Consumed
flavor_freq <- table(data$Flavors.Consumed)
flavor_freq
barplot(flavor_freq, main = "Flavors Consumed Frequency",col="Sky Blue")
pie(flavor_freq,main="Flavors Consumed Frequency")

# Bowls per Week
mean(data$Bowls.Per.Week)
sd(data$Bowls.Per.Week)
median(data$Bowls.Per.Week)
IQR(data$Bowls.Per.Week)
summary(data$Bowls.Per.Week)
boxplot(data$Bowls.Per.Week, main = "Boxplot of Bowls per Week")
hist(data$Bowls.Per.Week, main = "Histogram of Bowls per Week",col="Pink")

# Time of Consumption
time_freq <- table(data$Time.of.Consumption)
time_freq
barplot(time_freq, main = "Time of Consumption Frequency",col="Yellow")
pie(time_freq, main="Time of Consumption Frequency")

# Flavor Rating
mean(data$Flavor.Rating)
median(data$Flavor.Rating)
sd(data$Flavor.Rating)
summary(data$Flavor.Rating)
hist(data$Flavor.Rating, main = "Histogram of Flavor Rating",col="Pink")

# Monthly Spend on Cornflakes
mean(data$Monthly.Spend.on.Cornflakes)
median(data$Monthly.Spend.on.Cornflakes)
sd(data$Monthly.Spend.on.Cornflakes)
IQR(data$Monthly.Spend.on.Cornflakes)
summary(data$Monthly.Spend.on.Cornflakes)
hist(data$Monthly.Spend.on.Cornflakes, main = "Histogram of Monthly Spend on Cornflakes",col='Orange')

# Likelihood to Continue Buying
likeliness_freq <- table(data$Likelihood.to.continue.buying)
likeliness_freq
barplot(likeliness_freq, main = "Likelihood to Continue Buying Frequency",col="Black")
pie(likeliness_freq, main = "Likelihood to Continue Buying Frequency")

# Likelihood to Recommend
recommend_freq <- table(data$Likelihood.to.recommend)
recommend_freq
barplot(recommend_freq, main = "Likelihood to Recommend Frequency",col="Blue")
pie(recommend_freq, main = "Likelihood to Recommend Frequency")

# Residence
residence_freq <- table(data$Residence)
residence_freq
barplot(residence_freq, main = "Residence Frequency")
pie(residence_freq, main = "Residence Frequency")

# Packaging Rating
mean(data$Packaging.Rating)
median(data$Packaging.Rating)
sd(data$Packaging.Rating)
IQR(data$Packaging.Rating)
summary(data$Packaging.Rating)
rating=table(data$Packaging.Rating)
rating
barplot(rating, main = "Barplot of Packaging Rating",col="Purple")
pie(rating, main = "Barplot of Packaging Rating")

# Health Rating
mean(data$Health.Rating)
median(data$Health.Rating)
sd(data$Health.Rating)
IQR(data$Health.Rating)
summary(data$Health.Rating)
health=table(data$Health.Rating)
health
barplot(health, main="Barplot of Health Rating")
pie(health, main="Pie Chart of Health Rating")


# Correlation Analysis
names(data)
selected_columns <- c("Age", "Weight (in kg)", "Monthly Spend on Cornflakes", 
                      "Bowls per Week", "Flavor Rating", "Packaging Rating", 
                      "Health Rating")
# Calculate correlation matrix
correlation_matrix <- cor(data[selected_columns], use = "pairwise.complete.obs")
# Print correlation matrix
print(correlation_matrix)

#Regression Analysis
# Perform regression analysis to predict Monthly Spend on Cornflakes based on Age, Weight (in kg), and Flavor Rating
model <- lm(Monthly.Spend.on.Cornflakes ~ Age + Weight..kg. + Flavor.Rating, data = data)
# Summary of the regression model
summary(model)

#RegressionB
# Predicting Weight (in kg) based on Age, Flavor Rating, and Bowls per Week
model <- lm(Weight..kg. ~ Age + Bowls.Per.Week, data = data)
summary(model)

#Hypothesis Testing
# Hypothesis testing for association between Monthly Spend and Bowls per Week
# Null Hypothesis (H0): There is no association between Monthly spend and Bowls per week
# Alternative Hypothesis (H1): There is an association between Monthly spend and Bowls per week
# Create a contingency table
cont_table <- table(data$Monthly.Spend.on.Cornflakes, data$Bowls.Per.Week)
# Perform chi-square test
chi_sq_result <- chisq.test(cont_table)
chi_sq_result
# Determine acceptance or rejection of the null hypothesis
alpha <- 0.05  # Set the significance level
if (chi_sq_result$p.value < alpha) {
  print("Reject the null hypothesis: There is an association between Monthly  spend and Bowls per week")
} else {
  print("Fail to reject the null hypothesis: There is no association between Monthly spend and Bowls per Week")
}

#ANOVA
# Perform ANOVA
anova_result <- aov(Monthly.Spend.on.Cornflakes ~ Flavors.Consumed, data = data)
# Summary of ANOVA
summary(anova_result)

# Perform ANOVA
anova_result <- aov(Bowls.Per.Week ~ Flavor.Rating + Packaging.Rating, data = data)
# Summary of ANOVA
summary(anova_result)

#MANOVA
#Assuming 'Flavor.Rating', 'Packaging.Rating', and 'Health.Rating' are the dependent variables, and 'Gender' is the independent variable
# Perform MANOVA
manova_result <- manova(cbind(Flavor.Rating, Packaging.Rating, Health.Rating) ~ Gender, data = data)
# Extract the test statistics
summary(manova_result, test = "Pillai")
summary(manova_result, test = "Wilks")
summary(manova_result, test = "Hotelling-Lawley")
summary(manova_result, test = "Roy")

#MLE
# Sample data
ratings <- data$Flavor.Rating
# Define the likelihood function for normal distribution
likelihood <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  -sum(dnorm(ratings, mean = mu, sd = sigma, log = TRUE))
}

# Use optimization to find the MLE
mle_result <- optim(c(mean(ratings), sd(ratings)), likelihood, control = list(fnscale = -1))

# Estimated mean and standard deviation
mle_mean <- mle_result$par[1]
mle_sd <- mle_result$par[2]

# Print the results
cat("MLE estimate for mean:", mle_mean, "\n")
cat("MLE estimate for standard deviation:", mle_sd, "\n")

#PLOTS
# Bar chart for Gender
ggplot(data, aes(x = Flavors.Consumed, fill = Flavors.Consumed)) + 
  geom_bar() +
  labs(title = "Flavors Consumed", x = "Flavors.Consumed", y = "Count")

# Histogram for Age
ggplot(data, aes(x = Bowls.Per.Week)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Bowls consumed per week", x = "Bowls Per Week", y = "Frequency")

# Pie Chart for Gender
gender_freq <- table(data$Gender)
pie(gender_freq, main = "Gender Proportion", col = c("skyblue", "pink"),
    labels = paste0(names(gender_freq), "\n", round(prop.table(gender_freq)*100, 1), "%"))

# Scatter Plot for Age and Weight (in kg)
plot(data$Bowls.Per.Week, data$`Weight..kg.`, 
     xlab = "Bowls Per Week", ylab = "Weight (in kg)",
     main = "Scatter Plot of Age vs. Weight (in kg)")

# Box Plot for Flavor Rating
ggplot(data, aes(x = Flavor.Rating, y = `Weight..kg.`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot of Flavor Rating and Weight (in kg)",
       x = "Flavor Rating", y = "Weight (in kg)")

# Install and load the plotrix package
install.packages("plotrix")
library(plotrix)
# Create a bubble chart
# Create a bubble chart
bubble(data$Age, data$Flavor.Rating, size = data$Health.Rating, 
       main = "Bubble Chart", xlab = "Age", ylab = "Flavor Rating", 
       col = "skyblue", border = "black")

#Violin Plot
# Install and load the vioplot package
install.packages("vioplot")
library(vioplot)
# Create a violin plot
vioplot(data$Flavor.Rating, 
        col="skyblue", 
        horizontal=TRUE, 
        names="Flavor Rating",
        main="Violin Plot of Flavor Rating")

# Convert data to a matrix
ratings_matrix <- table( data$Health.Rating,data$Flavor.Rating)
custom_palette <- colorRampPalette(c("white","yellow","orange", "red"))
# Create a heatmap
heatmap(ratings_matrix, 
        Colv = NA, Rowv = NA,
        col = custom_palette(256),
        xlab = "Health Rating",
        ylab = "Flavor Rating",
        main = "Heatmap of Flavor Rating vs. Health Rating")


#Dashboard
---
  title: "Cornflakes Consumption Analysis Dashboard"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
vertical_layout: fill
---
  
{r setup, include=FALSE}
library(flexdashboard)
library(ggplot2)
library(plotly)

# Load data
data <- read.csv("cornflakes2.csv")