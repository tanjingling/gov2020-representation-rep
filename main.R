# libraries
library(tidyverse)
library(readxl)
library(stargazer)

##### Data Prep ####

# Load our new data
## data on party control and share of women in congress from 1964-2008
party_control <- read_csv('Our Data/party-women_control.csv')
## public opinion data and policy outcome data
df <- read_excel("Our Data/Main dataset for United States, from Gilens 2012.xlsx")

# Code from Mathisen
# logit transformation function
logitTransform <- function(p) { log(p/(1-p)) }

# New main variables
df$outcome <- df$OUTCOME
df$all <- df$PREDALL_SW # predicted value for all voters

# Calc preferences for males and use logit transformation for odds-ratio
df$male <- df$MALE_FAV/(df$MALE_FAV+df$MALE_OPP)
df$male <- ifelse(df$SWITCHER==1,1-df$male,df$male)
df$male_logit <- logitTransform(df$male)

# same with females
df$female <- df$FEMALE_FAV/(df$FEMALE_FAV+df$FEMALE_OPP)
df$female <- ifelse(df$SWITCHER==1,1-df$female,df$female)
df$female_logit <- logitTransform(df$female)

# recode the outcome to binary
df$outcome <- car::recode(df$outcome, "0=0; 1=1;2=1;3=1;4=1; else=NA") 

# gender policy diff variables
df$genderdiff <- abs(df$male-df$female)
df$genderdiff_nonabs <- df$male-df$female

# Mean centering variables 
df$all_mean <- df$all-mean(df$all,na.rm=T)
df$YEAR_mean <- df$YEAR-mean(df$YEAR,na.rm=T)

# Gender diff, DK (don't know) included
df$male_withDK <- df$MALE_FAV/(df$MALE_FAV+df$MALE_OPP+df$MALE_DK)
df$male_withDK <- ifelse(df$SWITCHER==1,1-df$male_withDK,df$male_withDK)
df$female_withDK <- df$FEMALE_FAV/(df$FEMALE_FAV+df$FEMALE_OPP+df$FEMALE_DK)
df$female_withDK <- ifelse(df$SWITCHER==1,1-df$female_withDK,df$female_withDK)
df$genderdiff_nonabs_withDK <- df$male_withDK-df$female_withDK

# Merge in party_control data into Gilens
main <- df %>%
  left_join(party_control, by = c("YEAR" = "year")) %>%
  select(-ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

# partisan control scale
main <- main %>%
  mutate(
    # Assign numeric values based on party control for each branch
    pres_score = ifelse(pres_control == "D", 0.5, 0),
    house_score = ifelse(house_control == "D", 0.25, 0),
    senate_score = ifelse(senate_control == "D", 0.25, 0),
    
    # Calculate total control scale
    party_control_scale = pres_score + house_score + senate_score
  )

# New Outcome Variable
## have some observations that indicate policy was passed half way through a year
## not sure what to do so I just floored those observations
main <- main %>%
  mutate(OUTCMYEAR = floor(OUTCMYEAR))

## new logic of outcome variable from Gilens
expanded_df <- main %>%
  rowwise() %>%
  mutate(
    outcome_first_year = ifelse(OUTCMYEAR == YEAR, 1, 0),
    outcome_second_year = ifelse(OUTCMYEAR == YEAR + 1, 1, 0)
  ) %>%
  ungroup() %>%
  # Ensure obs_type has no missing values by assigning a default category
  mutate(
    obs_type = case_when(
      outcome_first_year == 1 ~ "first_year",
      outcome_second_year == 1 ~ "second_year",
      TRUE ~ "no_adoption"
    )
  ) %>%
  # Duplicate rows based on obs_type with a default weight of 1 if not second_year
  uncount(weights = ifelse(obs_type == "second_year", 2, 1), .id = "obs_id") %>%
  mutate(
    outcome = case_when(
      obs_id == 1 & obs_type == "first_year" ~ 1,
      obs_id == 1 & obs_type == "second_year" ~ 0,
      obs_id == 2 & obs_type == "second_year" ~ 1,
      TRUE ~ 0
    ),
    YEAR = YEAR + ifelse(obs_id == 2 & obs_type == "second_year", 1, 0),
    weight = ifelse(obs_type == "first_year", 1, 0.5)
  ) %>%
  select(-outcome_first_year, -outcome_second_year, -obs_id, -obs_type)

##### Descriptive Analysis #####

# Figure 1: Density of Gender Difference by Policy Outcome (Passed or Not Passed)
figure1 <- ggplot(expanded_df %>% filter(!is.na(outcome)), aes(x = genderdiff_nonabs, 
                                             fill = factor(outcome))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Gender Difference by Policy Outcome", 
       x = "Gender Difference (Male - Female)", fill = "Policy Outcome") +
  scale_fill_manual(values = c("blue", "green"), 
                    labels = c("Not Passed", "Passed")) +
  theme_minimal()
ggsave("figures/figure_1.png", figure1, width = 16, height = 9, dpi = 300)

# Figure 2: Density of Gender Differences with Gender Gap
figure2 <- ggplot(expanded_df %>% filter(!is.na(outcome), genderdiff > 0.1), 
       aes(x = genderdiff_nonabs, fill = factor(outcome))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Gender Difference by Policy Outcome (with Gender Gap)", 
       x = "Gender Difference (Male - Female)", fill = "Policy Outcome") +
  scale_fill_manual(values = c("blue", "green"), labels = c("Not Passed", "Passed")) +
  theme_minimal()
ggsave("figures/figure_2.png", figure2, width = 16, height = 9, dpi = 300)

# Figure 3: Density of Gender Differences with Gender Gap and Party Controls
## Define party control categories with party_control_scale
expanded_df <- expanded_df %>%
  mutate(party_control_category = case_when(
    party_control_scale == 0 ~ "Full Republican",
    party_control_scale == 0.25 ~ "Mostly Republican",
    party_control_scale == 0.5 ~ "Divided",
    party_control_scale == 0.75 ~ "Mostly Democrat",
    party_control_scale == 1 ~ "Full Democrat"
  ))


## Density plot faceted by outcome and detailed party control levels, excluding NA values
figure3 <- ggplot(expanded_df %>% filter(!is.na(outcome), genderdiff > 0.1, !is.na(party_control_category)), 
       aes(x = genderdiff_nonabs, fill = factor(outcome))) +
  geom_density(alpha = 0.5) +
  facet_grid(party_control_category ~ outcome, 
             labeller = labeller(outcome = c(`0` = "Not Passed", `1` = "Passed"))) +
  labs(title = "Density of Gender Difference by Policy Outcome and Party Control",
       x = "Gender Difference (Male - Female)",
       y = "Density",
       fill = "Policy Outcome") +
  scale_fill_manual(values = c("blue", "green"), labels = c("Not Passed", "Passed")) +
  theme_minimal()
ggsave("figures/figure_3.png", figure3, width = 16, height = 9, dpi = 300)


##### Regression Analysis #####

m <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
stargazer(m, title = "Effect of Party Control on Gender Responsiveness")

# new dataframe to only analyze policies with a gender gap
gendered <- expanded_df %>%
  filter(genderdiff > 0.1)

# Model 1: Treatment Effect
m1 <- glm(outcome ~ genderdiff_nonabs * party_control_scale, data = gendered,
          family = binomial(link = "logit"))

# Figure 4: Predict Probabilites from Logit Results

## Create a prediction grid with 0.25 increments for party control scale
pred_grid_m1 <- expand.grid(
  genderdiff_nonabs = seq(min(gendered$genderdiff_nonabs, na.rm = TRUE), 
                          max(gendered$genderdiff_nonabs, na.rm = TRUE), length.out = 100),
  party_control_scale = seq(0, 1, by = 0.25)
)

## Get predicted probabilities and standard errors using m1
predictions_m1 <- predict(m1, newdata = pred_grid_m1, type = "link", se.fit = TRUE)
pred_grid_m1$predicted_prob <- plogis(predictions_m1$fit)  # Convert log-odds to probabilities
pred_grid_m1$lower_ci <- plogis(predictions_m1$fit - 1.96 * predictions_m1$se.fit)  # Lower bound
pred_grid_m1$upper_ci <- plogis(predictions_m1$fit + 1.96 * predictions_m1$se.fit)  # Upper bound



## Plot with confidence intervals as ribbons for m1
figure4 <- ggplot(pred_grid_m1, aes(x = genderdiff_nonabs, y = predicted_prob, color = factor(party_control_scale))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = factor(party_control_scale)), 
              alpha = 0.2, color = NA) +
  labs(title = "Predicted Probability of Policy Passage with Confidence Intervals",
       x = "Gender Difference (Male - Female)",
       y = "Predicted Probability of Policy Passage",
       color = "Party Control Scale",
       fill = "Party Control Scale") +
  scale_color_manual(values = c("red", "orange", "purple", "green", "blue"), 
                     labels = c("Full Republican", "Mostly Republican", "Divided", "Mostly Democrat", "Full Democrat")) +
  scale_fill_manual(values = c("red", "orange", "purple", "green", "blue"), 
                    labels = c("Full Republican", "Mostly Republican", "Divided", "Mostly Democrat", "Full Democrat")) +
  theme_minimal()
ggsave("figures/figure_4.png", figure4, width = 16, height = 9, dpi = 300)

## Regression Model for Specific Issue Areas

#Foreign policy
foreign_policy <- expanded_df %>%
  filter(XL_AREA == "foreign pol")
m30 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = foreign_policy)

#Social welfare
social_welfare <- expanded_df %>%
  filter(XL_AREA == "soc welfare")
m31 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = social_welfare)

#Economic Policy
economic_policy <- expanded_df %>%
  filter(XL_AREA == "econ & labor")
m32 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = economic_policy)


#Religious Issues
religious_issues <- expanded_df %>%
  filter(XL_AREA == "religious")
m33 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = religious_issues)

#Table 2
stargazer(m30, m31, m32, m33, title="Policy Responsiveness by Issue Area", align=TRUE, column.labels = c("Foreign Policy", "Social Welfare", "Economic Policy", "Religious Issues"), covariate.labels = c("Gender Difference", "Party Control", "Gender Difference * Party Control"))

# Bonus Model! Controlling for Number of Women in Congress
## Create a trend variable for each Congress session (e.g., cumulative female count)
gendered <- gendered %>%
  group_by(congress) %>%
  mutate(trend_female_representation = mean(house_demcount + senate_demcount + house_repcount + senate_repcount, na.rm = TRUE))

## regression model with new control
m2 <- glm(outcome ~ genderdiff_nonabs * party_control_scale + trend_female_representation,
          data = gendered, family = binomial(link = "logit"))

# Bonus Figure 5

## Define a grid of values for genderdiff_nonabs, party_control_scale, and trend_female_representation
pred_grid <- expand.grid(
  genderdiff_nonabs = seq(min(gendered$genderdiff_nonabs, na.rm = TRUE), 
                          max(gendered$genderdiff_nonabs, na.rm = TRUE), length.out = 100),
  party_control_scale = seq(0, 1, by = 0.25),
  trend_female_representation = mean(gendered$trend_female_representation, na.rm = TRUE)  # average trend
)

## Predict probabilities and confidence intervals
predictions <- predict(m2, newdata = pred_grid, type = "link", se.fit = TRUE)
pred_grid$predicted_prob <- plogis(predictions$fit)  # convert log-odds to probabilities
pred_grid$lower_ci <- plogis(predictions$fit - 1.96 * predictions$se.fit)  # 95% CI lower bound
pred_grid$upper_ci <- plogis(predictions$fit + 1.96 * predictions$se.fit)  # 95% CI upper bound


## Plot with confidence intervals
figure5 <- ggplot(pred_grid, aes(x = genderdiff_nonabs, y = predicted_prob, color = factor(party_control_scale))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = factor(party_control_scale)), 
              alpha = 0.2, color = NA) +
  labs(title = "Predicted Probability of Policy Passage by Gender Preferences and Party Control",
       x = "Gender Difference (Male - Female)",
       y = "Predicted Probability of Policy Passage",
       color = "Party Control Scale",
       fill = "Party Control Scale") +
  scale_color_manual(values = c("red", "orange", "purple", "green", "blue"), 
                     labels = c("Full Republican", "Mostly Republican", "Mixed",
                                "Mostly Democrat", "Full Democrat")) +
  scale_fill_manual(values = c("red", "orange", "purple", "green", "blue"), 
                    labels = c("Full Republican", "Mostly Republican", "Mixed",
                               "Mostly Democrat", "Full Democrat")) +
  theme_minimal()
ggsave("figures/figure_5.png", figure5, width = 16, height = 9, dpi = 300)

################################################################################
## Suggested Extensions 

## 1 Improving interpretation of coefficients by standardizing gender difference

# Standardize the gender difference variable
expanded_df <- expanded_df %>%
  mutate(genderdiff_nonabs_z = (genderdiff_nonabs - mean(genderdiff_nonabs, na.rm = TRUE)) / sd(genderdiff_nonabs, na.rm = TRUE))

# Run the regression model with partial standardization
m_partial_standardized <- glm(outcome ~ genderdiff_nonabs_z * party_control_scale,
                              data = expanded_df, family = binomial(link = "logit"))

# Display the model in a stargazer table
stargazer(m_partial_standardized, type = "text", 
          title = "Effect of Standardized Gender Difference and Non-Standardized Party Control on Policy Responsiveness",
          dep.var.labels = "Policy Outcome",
          covariate.labels = c("Standardized Gender Difference", 
                               "Party Control Scale", 
                               "Interaction: Gender Difference * Party Control"),
          omit.stat = c("f", "ser"),
          align = TRUE)

## 2 Recoding DV to not include row duplication, analyzing across time lags

library(dplyr)
library(stargazer)

expanded_df <- main %>%
  mutate(
    OUTCMYEAR = floor(OUTCMYEAR),      # Floor OUTCMYEAR to remove partial years
    time_lag = OUTCMYEAR - YEAR        # Calculate the time lag
  ) %>%
  filter(!is.na(time_lag))             # Remove rows with NA in time_lag

expanded_df <- expanded_df %>%
  mutate(
    outcome_dv_0 = ifelse(time_lag == 0, 1, 0),          # Only policies adopted within 0 years
    outcome_dv_1 = ifelse(time_lag <= 1, 1, 0),          # Policies adopted within 0–1 years
    outcome_dv_2 = ifelse(time_lag <= 2, 1, 0),          # Policies adopted within 0–2 years
    outcome_dv_3 = ifelse(time_lag <= 3, 1, 0),          # Policies adopted within 0–3 years
    outcome_dv_4 = ifelse(time_lag <= 4, 1, 0)           # Policies adopted within 0–4 years
  )

m_lag_0 <- lm(outcome_dv_0 ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
m_lag_1 <- lm(outcome_dv_1 ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
m_lag_2 <- lm(outcome_dv_2 ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
m_lag_3 <- lm(outcome_dv_3 ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
m_lag_4 <- lm(outcome_dv_4 ~ genderdiff_nonabs * party_control_scale, data = expanded_df)

stargazer(m_lag_0, m_lag_1, m_lag_2, m_lag_3, m_lag_4,
          title = "Effect of Party Control on Gender Responsiveness Across Different Time Lags",
          column.labels = c("Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 4"),
          dep.var.labels = "Policy Adoption",
          covariate.labels = c("Gender Difference", "Party Control", "Gender Difference * Party Control"),
          type = "text", align = TRUE)
