library(dplyr)
library(ggplot2)
hospitals = read.csv("hospitals.csv")
 #a) How big is the dataset?
dim(hospitals)
#b) What are the names of the columns? 
colnames(hospitals)
#c) What data types are each column? 
str(hospitals)
#d) Are there missing values?
any(is.na(hospitals))

## ggplot / dplyr questions

#e) Which hospital has the lowest number of beds?
hospitals %>% filter( Beds ==  min(Beds))
  # hospital numbers 1064 and 1751 have the lowest amount of beds - 3 beds each

#f) Which hospital has the lowest expense?
hospitals %>% filter(Total.Expense == min(Total.Expense))
  # hospital number 826 has the lowest expense 

#g) How many hospitals deliver babies?
hospitals %>% filter(Births.or.Not == 1) %>%
  count()
#h) Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospitals, aes(Beds, Total.Expense)) + 
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense", title = "Scatterplot of Beds vs Total Expense")
#i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospitals, aes(Admissions, Total.Expense)) +
  geom_point() +
  labs(x = "Number of Admissions",y = "Total Expense", title = "Scatterplot of Admissions vs Total Expense" )
#j) Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
hospitals.births.yes <- hospitals %>%
  filter(Births.or.Not == 1)
ggplot(hospitals.births.yes,  aes(Beds, Total.Expense)) + 
  geom_point() +
  labs(x = "Number of Beds", y = "Total Expense", title = "Scatterplot of Beds vs Total Expense with In-Hospital Birth Centers")
#k) One more question that you believe would be useful.
    # beds and admissions 

  ## Descriptive Analytics
#i. For Pie Chart:One slice should be labeled Admissions. Choose another attribute for the second slice.

# Load the ggplot2 package
library(ggplot2)

admis.vs.person <- data.frame(
  Category = c("Admissions", "Personnel"),
  Value = c(sum(hospitals$Admissions, na.rm = TRUE), sum(hospitals$Personnel, na.rm = TRUE))
)

ggplot(admis.vs.person, aes(x = "", y = Value, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(fill = "Category", x= " ", y = " ", title = "Pie Chart of Admissions vs Personnel") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#ii. For the column / barcharts: Ensure that one column is titled Admissions. Choose a different attribute for the other column.
total_values_bar <- data.frame(
  Category = c("Admissions", "Outpatient Visits"),
  Value = c(sum(hospitals$Admissions, na.rm = TRUE), sum(hospitals$`Outpatient Visits`, na.rm = TRUE))
)

ggplot(total_values_bar, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Total Count", title = "Bar Chart of Admissions vs Outpatient Visits") +
  theme_minimal()
#iii. For Line Chart: one of the lines should represent Expense; choose another attribute for the second line.

ggplot(data = hospitals, aes(x = `Hospital.Number`)) +
  geom_line(aes(y = `Total.Expense`, color = "Total Expense")) +
  geom_line(aes(y = Admissions, color = "Admissions")) +
  labs(x = "Hospital Number", y = " ", title = "Line Chart of Total Expense and Admissions") +
  scale_color_manual(values = c("Total Expense" = "orange", "Admissions" = "purple")) +
  theme_minimal()


#3. Perform one simple regression (one predictor) to provide recommendations.

lm(`Total.Expense` ~ Admissions, data = hospitals)
summary(lm(`Total.Expense` ~ Admissions, data = hospitals))

#ii. The dependent variable should be Total Expense. Choose an independent variable from one of the remaining attributes.

#iii. What is the value of the 𝑅!?
    # r squared = .733
#iv. What does the 𝑅^2 measure in this case? ( Hint: Percentage of variation in ... explained by ...)
  # This means that approximately 73.3% of the variation in Total Expense is explained by the number of Admissions

#v. What are the pvalues ? How many pvalues are reported, why ? 
  # What does each pvalue mean? (Hint: pvalues have are related to generalizing from the sample to the population: what is the sample here, what is the population?) (1-2 sentences)

#> answers
#> p values are .57 for intercept and <2e-16
#> 2 pvalues are reported because there are only 2 coefficients
#> sample is the set we are using and the population is the entire dataset of hosptials

#vi. Explain R square, pvalues.
#vii. What would be the right attribute size (independent variable) 
    # that seems most appropriate to lead you in the expense range of $55– $75 million?
#> answer
# for 55 million
(55000000 + 2610.91) / 25.59 
    # 2149379
# for 75 million
(75000000 + 2610.91) / 25.59
    # 2930934
# the range is 2149379 - 2930934




# multivariate regression
# 4. Perform one multivariate regression to provide recommendations.
mvreg <-  lm(Total.Expense ~ Admissions + Outpatient.Visits, data = hospitals)
summary(mvreg)
# i. The dependent variable should be Total Expense (the column in the data set). Choose two independent variables from one of the remaining attributes.
# ii. What is the value of the 𝑅^2?
    #The value of the R-squared is 0.8043.

#   iii. What does the 𝑅! measure in this case? ( Hint: Percentage of
#                                                  variation in ... explained by ...)
    # 80.43% of the variation in Total Expense is explained by the combined variation 
#                                                   in Admissions and Outpatient Visits.

# iv. What are the pvalues ? How many pvalues are reported, why ?
    # Three p-values are reported: one for the intercept, one for Admissions, 
#                                             and one for Outpatient Visits.

#   What does each pvalue mean? (Hint: pvalues have are related to generalizing from the sample to the population: what is the sample here, what is the population?) (1-2 sentences)
#       The intercept is statistically significant.
##    Admissions have a significant positive relationship with Total Expense.
###     Outpatient Visits have a significant positive relationship with Total Expense.


# v. Explain R square, pvalues.
    # r squared is how well the data fits the model, meaning how well do the independent variables 
#               make the model better
    # p values determine how significant the coeffients are, if the pvalue is lower than 0.05, there is a 
  #           positive relationship between the independent and dependent variables


ggplot(hospitals, aes(Beds, Admissions)) +
  geom_point()

# i think the hospital should be medium sized, considering hospitals tend to be understaffed
  # having more beds available is always necessary considering the hospitals are full most of the time
  #  having a medium sized hospital would meet demand
# the only problem would be, admissions and outpatient visits scale with expenses, 
#         meaning there are going to be more expenses with more beds/ admissions 


