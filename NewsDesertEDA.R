library(knitr)
library(dplyr)

# Read the CSV file
csv_data <- read.csv("FullDemographicData.csv")


#########DESCRIPTIVE STATISTICS####################################

# Replace 'null' with NA and convert income columns to numeric
csv_data$X2014Median.Income[csv_data$X2014Median.Income == "null"] <- NA
csv_data$X2018Median.Income[csv_data$X2018Median.Income == "null"] <- NA
csv_data$X2022Median.Income[csv_data$X2022Median.Income == "null"] <- NA

csv_data$X2014Median.Income <- as.numeric(as.character(csv_data$X2014Median.Income))
csv_data$X2018Median.Income <- as.numeric(as.character(csv_data$X2018Median.Income))
csv_data$X2022Median.Income <- as.numeric(as.character(csv_data$X2022Median.Income))

# Filter rows with valid turnout values and non-missing income for all years
filtered_data <- csv_data %>%
  filter(X2014Turnout != 0 & X2014Turnout <= 1) %>%
  filter(X2018Turnout != 0 & X2018Turnout <= 1) %>%
  filter(X2022Turnout != 0 & X2022Turnout <= 1) %>%
  filter(!is.na(X2014Median.Income) & !is.na(X2018Median.Income) & !is.na(X2022Median.Income))

# Define columns of interest for each year
columns_2014 <- c("X2014Turnout", "NP2014", "X2014BlackProp", "X2014HispProp", "X2014Median.Income", "X2014Age")
columns_2018 <- c("X2018Turnout", "NP2018", "X2018BlackProp", "X2018HispProp", "X2018Median.Income", "X2018Age")
columns_2022 <- c("X2022Turnout", "NP2022", "X2022BlackProp", "X2022HispProp", "X2022Median.Income", "X2022Age")

# Function to calculate descriptive stats
get_stats <- function(data, columns) {
  data %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), list(
      Mean = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      Variance = ~ var(., na.rm = TRUE),
      Min = ~ min(., na.rm = TRUE),
      Max = ~ max(., na.rm = TRUE)
    ), .names = "{col}_{fn}"))
}

# Get stats for each year
stats_2014 <- get_stats(filtered_data, columns_2014)
stats_2018 <- get_stats(filtered_data, columns_2018)
stats_2022 <- get_stats(filtered_data, columns_2022)

# Print tables
print(stats_2014)
print(stats_2018)
print(stats_2022)

# LaTeX tables
kable(stats_2014, format = "latex", booktabs = TRUE, caption = "Descriptive Statistics for 2014")
kable(stats_2018, format = "latex", booktabs = TRUE, caption = "Descriptive Statistics for 2018")
kable(stats_2022, format = "latex", booktabs = TRUE, caption = "Descriptive Statistics for 2022")

#########DIFF-IN-DIFF##############################################################################
library(dplyr)

# Load CSV data
csv_data <- read.csv("FullDemographicData.csv")

# Define a function to calculate summary stats for a given condition and year range
calculate_summary_stats <- function(data, condition, year_start, year_end) {
  filtered_data <- subset(data, eval(parse(text = condition)))
  
  col_start <- paste0("X", year_start, "Turnout")
  col_end <- paste0("X", year_end, "Turnout")
  
  changedata <- filtered_data %>%
    filter(!is.na(.data[[col_start]]) & !is.na(.data[[col_end]])) %>%
    filter(.data[[col_start]] != 0 & .data[[col_end]] != 0) %>%
    mutate(changeVote = .data[[col_end]] - .data[[col_start]]) %>%
    filter(changeVote < 1)
  
  summary_stats <- changedata %>%
    summarize(
      mean = mean(changeVote, na.rm = TRUE),
      n = n(),
      variance = var(changeVote, na.rm = TRUE)
    ) %>%
    mutate(condition = condition,
           year_range = paste0(year_start, "-", year_end))
  
  return(summary_stats)
}

# Define NP-based conditions for 2014–2018
conditions_2014_2018 <- c(
  "NP2014 >= 2 & NP2018 >= 2",
  "NP2014 == 1 & NP2018 == 1",
  "NP2014 == 0 & NP2018 == 0",
  "NP2014 >= 2 & NP2018 == 1",
  "NP2014 >= 2 & NP2018 == 0",
  "NP2014 == 1 & NP2018 == 0"
)

# Define NP-based conditions for 2018–2022
conditions_2018_2022 <- c(
  "NP2018 >= 2 & NP2022 >= 2",
  "NP2018 == 1 & NP2022 == 1",
  "NP2018 == 0 & NP2022 == 0",
  "NP2018 >= 2 & NP2022 == 1",
  "NP2018 >= 2 & NP2022 == 0",
  "NP2018 == 1 & NP2022 == 0"
)

# Helper to calculate and plot each year range
process_and_plot <- function(conditions, year_start, year_end) {
  stats_list <- list()
  for (cond in conditions) {
    stat <- calculate_summary_stats(csv_data, cond, year_start, year_end)
    stats_list <- append(stats_list, list(stat))
  }
  stats_df <- bind_rows(stats_list)
  
  # Plot
  ggplot(stats_df, aes(x = condition, y = mean)) +
    geom_bar(stat = "identity", fill = "#2c7fb8") +
    labs(
      title = paste("Mean Change in Voter Turnout (", year_start, "-", year_end, ")", sep = ""),
      x = "NP Condition",
      y = "Mean Change in Turnout"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot for 2014–2018
plot_2014_2018 <- process_and_plot(conditions_2014_2018, 2014, 2018)
print(plot_2014_2018)

# Plot for 2018–2022
plot_2018_2022 <- process_and_plot(conditions_2018_2022, 2018, 2022)
print(plot_2018_2022)



##################T-TEST 2014-2018##################################################
HealthyToDesert <- subset(csv_data, NP2014 == 1 & NP2018 == 0)
HealthyToHealthy <- subset(csv_data, NP2014 == 1 & NP2018 == 1)

cleanedHealthyToHealthy <- HealthyToHealthy %>%
  filter(X2018Turnout!=0 & X2014Turnout!=0) %>%
  mutate(changeVote = X2018Turnout - X2014Turnout) %>%
  filter(changeVote < 1)

cleanedHealthytoDesert <- HealthyToDesert %>%
  filter(X2018Turnout!=0 & X2014Turnout!=0) %>%
  mutate(changeVote = X2018Turnout - X2014Turnout) %>%
  filter(changeVote < 1)
length(cleanedHealthytoDesert$changeVote)
length(cleanedHealthyToHealthy$changeVote)

t_test_result <- t.test(cleanedHealthyToHealthy$changeVote, 
                        cleanedHealthytoDesert$changeVote)


print(t_test_result)


##################T-TEST 2018-2022##################################################


HealthyToDesert <- subset(csv_data, NP2018 >= 2 & NP2022 == 0)
HealthyToHealthy <- subset(csv_data, NP2018 >= 2 & NP2022 >= 2)

cleanedHealthyToHealthy <- HealthyToHealthy %>%
  filter(X2022Turnout!=0 & X2018Turnout!=0) %>%
  mutate(changeVote = X2022Turnout - X2018Turnout) %>%
  filter(changeVote < 1)

cleanedHealthytoDesert <- HealthyToDesert %>%
  filter(X2022Turnout!=0 & X2018Turnout!=0) %>%
  mutate(changeVote = X2022Turnout - X2018Turnout) %>%
  filter(changeVote < 1)
length(cleanedHealthytoDesert$changeVote)
length(cleanedHealthyToHealthy$changeVote)

t_test_result <- t.test(cleanedHealthyToHealthy$changeVote, 
                        cleanedHealthytoDesert$changeVote, var.equal=F)


print(t_test_result)


#############REGRESSIONS########################################################

# Replace "null" with NA for all income columns
csv_data$X2014Median.Income[csv_data$X2014Median.Income == "null"] <- NA
csv_data$X2018Median.Income[csv_data$X2018Median.Income == "null"] <- NA
csv_data$X2022Median.Income[csv_data$X2022Median.Income == "null"] <- NA

# Convert to numeric
csv_data$X2014Median.Income <- as.numeric(csv_data$X2014Median.Income)
csv_data$X2018Median.Income <- as.numeric(csv_data$X2018Median.Income)
csv_data$X2022Median.Income <- as.numeric(csv_data$X2022Median.Income)

# ---------- 2014 to 2018 ----------
changedata_1418 <- csv_data %>%
  filter(X2018Turnout != 0 & X2014Turnout != 0) %>%
  filter(!is.na(X2018Median.Income) & !is.na(X2014Median.Income)) %>%
  mutate(
    changeVote = X2018Turnout - X2014Turnout,
    changeBlack = X2018BlackProp - X2014BlackProp,
    changeHisp = X2018HispProp - X2014HispProp,
    changeAge = X2018Age - X2014Age,
    changeIncome = X2018Median.Income - X2014Median.Income,
    changeNP = NP2018 - NP2014
  ) %>%
  filter(changeVote < 1 & changeNP > -20)

# Optional visualization
plot(changedata_1418[, c("changeVote", "changeNP", "changeBlack", "changeHisp", "changeAge")])

# Linear model 2014–2018
model_1418 <- lm(changeVote ~ changeNP + changeBlack + changeHisp + changeAge + changeIncome, data = changedata_1418)
cat("\n--- Regression Summary: 2014–2018 ---\n")
summary(model_1418)

# ---------- 2018 to 2022 ----------
changedata_1822 <- csv_data %>%
  filter(X2022Turnout != 0 & X2018Turnout != 0) %>%
  filter(!is.na(X2022Median.Income) & !is.na(X2018Median.Income)) %>%
  mutate(
    changeVote = X2022Turnout - X2018Turnout,
    changeBlack = X2022BlackProp - X2018BlackProp,
    changeHisp = X2022HispProp - X2018HispProp,
    changeAge = X2022Age - X2018Age,
    changeIncome = X2022Median.Income - X2018Median.Income,
    changeNP = NP2022 - NP2018
  ) %>%
  filter(changeVote < 1 & changeNP > -20)

# Optional visualization
plot(changedata_1822[, c("changeVote", "changeNP", "changeBlack", "changeHisp", "changeAge")])

# Linear model 2018–2022
model_1822 <- lm(changeVote ~ changeNP + changeBlack + changeHisp + changeAge + changeIncome, data = changedata_1822)
cat("\n--- Regression Summary: 2018–2022 ---\n")
summary(model_1822)


############2014-2018 Party Affiliation Control #################


csv_data$X2014Median.Income[csv_data$specific_column == "null"] <- NA
csv_data$X2018Median.Income[csv_data$specific_column == "null"] <- NA

csv_data$X2014Median.Income <- as.numeric(csv_data$X2014Median.Income)
csv_data$X2018Median.Income <- as.numeric(csv_data$X2018Median.Income)

changedata = csv_data %>%
  filter(X2018Turnout != 0 & X2014Turnout != 0) %>%
  filter(!is.na(X2018Median.Income) & !is.na(X2014Median.Income)) %>%
  filter(Affiliation2022 == "Republican")%>%
  mutate(
    changeVote = X2018Turnout - X2014Turnout,
    changeBlack = X2018BlackProp - X2014BlackProp,
    changeHisp = X2018HispProp - X2014HispProp,
    changeAge = X2018Age - X2014Age,
    changeIncome = as.numeric(X2018Median.Income) - as.numeric(X2014Median.Income),
    changeNP = NP2018 - NP2014
  ) %>%
  filter(changeVote < 1 & changeNP > -20)


plot(changedata[,c("changeVote", "changeNP", "changeBlack", "changeHisp", "changeAge")])

model1 <- lm(changeVote ~ changeNP + changeBlack + changeHisp + changeAge + changeIncome, data = changedata)
summary(model1)

####################2018-2022 Party Affiliation Control#################

csv_data$X2018Median.Income[csv_data$specific_column == "null"] <- NA
csv_data$X2022Median.Income[csv_data$specific_column == "null"] <- NA

csv_data$X2018Median.Income <- as.numeric(csv_data$X2018Median.Income)
csv_data$X2022Median.Income <- as.numeric(csv_data$X2022Median.Income)

changedata = csv_data %>%
  filter(X2022Turnout != 0 & X2018Turnout != 0) %>%
  filter(!is.na(X2022Median.Income) & !is.na(X2018Median.Income)) %>%
  filter(Affiliation2022 == "Republican")%>%
  mutate(
    changeVote = X2022Turnout - X2018Turnout,
    changeBlack = X2022BlackProp - X2016BlackProp,
    changeHisp = X2022HispProp - X2018HispProp,
    changeAge = X2022Age - X2018Age,
    changeIncome = as.numeric(X2022Median.Income) - as.numeric(X2018Median.Income),
    changeNP = NP2022 - NP2018
  ) %>%
  filter(changeVote < 1 & changeNP > -20)

nrow(changedata)



plot(changedata[,c("changeVote", "changeNP", "changeBlack", "changeHisp", "changeAge")])

model1 <- lm(changeVote ~ changeNP + changeBlack + changeHisp + changeAge + changeIncome, data = changedata)
summary(model1)


