#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)
library(stringr)

# Capitalize category labels
yrbss$Grade <- sapply(yrbss$grade, str_to_sentence)
yrbss$Gender <- sapply(yrbss$gender, str_to_sentence)

# Convert grades col to factor, arrange rows of df by ascending order of grade
yrbss$Grade <- factor(yrbss$Grade, levels = c("9", "10", "11", "12", "Other", NA))

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

# First remove rows with missing values in grade, gender, physically_active_7d
yrbss_clean <- yrbss %>% drop_na(Grade, Gender, physically_active_7d)

avg_phys_plot <- aggregate(
  x = yrbss_clean$physically_active_7d, 
  by = list(Grade = yrbss_clean$Grade, Gender = yrbss_clean$Gender), 
  FUN = mean
) |>
  ggplot(mapping = aes(x = Grade, y = x, group = interaction(Gender), color=Gender)) + 
  geom_point() + 
  labs(title="Mean Physical Activity of Youth Among Grades and Gender", x="Grade", y="Average # of Days") +
  geom_line()

avg_phys_plot

# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

# Drop all rows with missing values, add column with bmi
yrbss_clean <- yrbss %>% drop_na(Grade, Gender, weight, height, physically_active_7d)
yrbss_clean$bmi <- yrbss_clean$weight / (yrbss_clean$height)^2
phys_bmi <- subset(yrbss_clean, Gender == "Female" & Grade == 12) 

# Plot 1: Line plot showing average BMI over # of physically active days
phys_bmi_plot_1 <- aggregate(
  x = phys_bmi$bmi, 
  by = list(Phys = phys_bmi$physically_active_7d), 
  FUN = mean
) |>
  ggplot(
    mapping = aes(x = Phys, y = x)
  ) + 
  geom_line() + 
  labs(title=str_wrap("Physical Activity vs Average BMI Among Female Students in Grade 12", width = 50), x="# of Physically Active Days", y="Average BMI (kg/m^2)") +
  theme(plot.title = element_text(hjust = 0.5))

phys_bmi_plot_1

# Plot 2: Box and whisker plot of BMI over # of physically active days
phys_bmi_plot_2 <- ggplot(
  data = phys_bmi, 
  mapping = aes(x = factor(physically_active_7d), y = bmi)
) + 
  geom_boxplot() + 
  labs(title="Physical Activity vs BMI Among Female Students in Grade 12", x="# of Physically Active Days", y="BMI (kg/m^2)")

phys_bmi_plot_2

# Push your completed code to your GitHub repository
