# Load necessary libraries
library(ggplot2)
library(dunn.test)  # For post hoc tests
library(dplyr)

# Load your dataset 
data <- read.csv("C:/Users/palla/Downloads/archive (9)/AB_NYC_2019.csv") 

# Data Cleaning & Outlier Treatment (capping method)
upper_cap <- 750  # Define the upper cap
lower_cap <- 0    # Define the lower cap

data$capped_price <- pmin(pmax(data$price, lower_cap), upper_cap)

# QQ Plot for 'capped_price' variable
qqnorm(data$capped_price)
qqline(data$capped_price)
# Randomly sample your data (e.g., 500 observations)
set.seed(123)  # for reproducibility
sample_data <- sample(data$capped_price, 500)

# Perform Shapiro-Wilk test on the sample
shapiro.test(sample_data)
# The Shapiro-Wilk normality test has been performed on your sample data, and the results indicate that the data is not normally distributed. 
#This is evident from the extremely small p-value (< 2.2e-16), 
#which suggests that the data significantly deviates from a normal distribution.


## Checking the assumptions of Kruskal Wallis Test

#1. Normality test: We used Q-Q plots and histogram test to check the normality assumption for each group and found that it was not normally distributed.

#2. Homogeneity of variance assumption: We can use the Bartlett's test to check the homogeneity of variance assumption.

# Bartlett's test for homogeneity of variance
bartlett.test(capped_price ~ room_type, data = data_capped)

#The output of the Bartlett's test of homogeneity of variances shows the following:

#1. The test was performed on the price variable with room_type as the grouping variable.
#2. The test statistic is the Bartlett's K-squared value, which is 4464.1.
#3. The degrees of freedom (df) are 2, which is the number of groups minus 1.
#4. The p-value is less than 2.2e-16, which is very small and indicates strong evidence against the null hypothesis of equal variances.

#Therefore, we reject the null hypothesis of equal variances and conclude that the variances of the price variable are not equal across the different room_type groups. Therefore, the Kruskal-Wallis test should be used.

# Statistical Testing for Hypothesis 1: Room Types and Pricing
# Kruskal-Wallis test
kruskal_result <- kruskal.test(capped_price ~ room_type, data = data)
print(kruskal_result)

# Post hoc tests (Dunn's test) to compare groups if Kruskal-Wallis is significant
dunn_result <- dunn.test(data[["capped_price"]], data[["room_type"]], method = "bonferroni")
print(dunn_result)


####The Kruskal-Wallis test is a non-parametric test that checks if there are statistically significant differences between three or more independent groups. In your case, it is used to determine if there are significant differences in prices among different room types (Entire home/apt, Private room, Shared room). Here are the key results and their interpretation:

##1. **Kruskal-Wallis Chi-Squared Value**: The Kruskal-Wallis chi-squared value is 22418, and it has 2 degrees of freedom (df = 2). This value is used to assess whether there are statistically significant differences in prices between the room types. In this case, the chi-squared value is extremely large, indicating a substantial difference among the groups.

##2. **P-Value**: The p-value associated with the Kruskal-Wallis test is less than 2.2e-16, which is an extremely small value. The small p-value suggests strong evidence against the null hypothesis, indicating that there are significant differences in prices among the room types. In other words, the room type has a significant effect on the pricing.

##The Kruskal-Wallis test tells you that there is a significant difference among the room types, but it doesn't specify which groups are different from each other. To determine where the differences lie, you can perform post hoc tests.

##3. **Post Hoc Tests (Dunn's Test)**: Post hoc tests are conducted when you have significant results in an ANOVA or Kruskal-Wallis test to determine which specific group(s) differ from each other. In your case, Dunn's test was performed using the Bonferroni correction method. It provides specific comparisons between the groups.

##- "Entire home/apt - Private room": The adjusted p-value is 0.0000, indicating a significant difference in pricing between Entire home/apartment listings and Private room listings.

##- "Entire home/apt - Shared room": The adjusted p-value is 0.0000, indicating a significant difference in pricing between Entire home/apartment listings and Shared room listings.

##- "Private room - Shared room": The adjusted p-value is 3.390397e-38, which is also extremely small. This shows a significant difference in pricing between Private room listings and Shared room listings.

##The results of the post hoc test confirm that there are significant differences in pricing between all pairs of room types, which aligns with the Kruskal-Wallis test results.

##In summary, both the Kruskal-Wallis test and the post hoc tests indicate that room type has a significant effect on pricing, and there are significant differences in prices among the room types.


# Data Visualization
# Histogram of Capped Prices by Room Type
ggplot(data, aes(x = capped_price, fill = room_type)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(x = "Capped Price", y = "Frequency") +
  ggtitle("Distribution of Capped Prices by Room Type")

# Box Plot of Price by Room Type
ggplot(data, aes(x = room_type, y = capped_price)) + 
  geom_boxplot(fill = "lightblue") +
  labs(x = "Room Type", y = "Capped Price (USD)") +
  ggtitle("Box Plot of Airbnb Capped Prices by Room Type")


