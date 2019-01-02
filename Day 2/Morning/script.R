library(tidyverse) # All hail the tidyverse
library(Hmisc) # Needed for correlation
library(broom) # Needed to convert stats objects into tibbles
library(car) # Needed for VIF calculations and DW test
library(psych) # Generating descriptives
library(afex) # Needed to run ANOVA model using Type III SS, contrast coding
library(emmeans) # Needed for adjusted means and pairwise comparisons
library(lme4)

dataset1 <- read_csv("dataset1.csv")

# Simple regression ####
# First we will build a scatterplot of points against investment
ggplot(dataset1, aes (x = investment, y = points)) + geom_point() 

# Let's add a regression line and a line of our outcome mean
ggplot(dataset1, aes(x = investment, y = points)) + 
  geom_point() + 
  geom_hline(yintercept = mean(dataset1$points), colour = "blue") + 
  geom_smooth(method = "lm", se = FALSE)

# Let’s calculate Pearson’s r
rcorr(dataset1$investment, dataset1$points)

# Let's do regression with just the one predictor
# We will first build a model where our outcome (points scored) is predicted 
#by the intercept of our line (i.e., the mean as a model of our data)
model0 <- lm(points ~ 1, data=dataset1)

# Now we will build a model where our outcome (points scored) is predicted by investment
model1 <- lm(points ~ investment, data=dataset1)

# Now we will compare the two models to each other
anova(model0, model1)

# Now let's get some parameter estimates
summary(model1)

# Now we're going to look at multiple regression ####
# First we're going to create the data
Region <- seq (1:250)

set.seed(1)
House_price <- rnorm(250, mean=200000, sd=10000)

set.seed(2)
Population <- rnorm (250, mean=50000, sd=2500)
Crime <- rnorm (250, mean=20, sd=5)
Average_age <- rnorm (250, mean=75, sd=5)
Household_income <- rnorm (250, mean=20000,sd=2000)
result1 <- tidy(rcorr(House_price, Population))
result2 <- tidy(rcorr(House_price, Crime))

# Find the first solution where House prices correlate (p < .01) with both Population and Crime
i <- 2
while (!(result1$p.value < .01 & result2$p.value < .01)) {
  set.seed(i)
  Population <- rnorm (250, mean=50000, sd=2500)
  Crime <- rnorm (250, mean=20, sd=5)
  result1 <- tidy(rcorr(House_price, Population))
  result2 <- tidy(rcorr(House_price, Crime))
  i <- i+1
}

data <- as.tibble(cbind(Region, as.integer(House_price), as.integer(Population), 
                        as.integer(Crime), Average_age, Household_income))

colnames(data) <- c("Region", "House_price", "Population", "Crime", 
                    "Average_age", "Household_income")

# Check the correlation structure
cor(data)

# Can house prices be predicted by (one or more of) Population, Crime (per 10000 people), 
# Average age, or Household income in a region?  We have data from 1,000 regions.
# First let's plot some individual graphs

ggplot(data, aes(x = House_price, y = Population)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = House_price, y = Crime)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = House_price, y = Average_age)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = House_price, y = Household_income)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# First let's build a null model
model0 <- lm (House_price ~ 1, data = data)

# First let's build a model with all predictors
model1 <- lm (House_price ~ Population + Crime + Average_age + Household_income, 
              data = data)
 
# Do we have any multi-colinearity issues?
vif(model1)

# Check to see if the full model is better than the null model
anova(model0, model1)

# Now get the summary of model1
summary(model1)

# Notice that Average_age and Household_income do not seem to predict house prices
# Let's drop them in model2
model2 <- lm (House_price ~ Population + Crime, data = data)

# Is model2 now better model1?
anova(model2, model1)

AIC(model1)
AIC(model2)

# Let's validate and look at some diagnostic plots
hist(residuals(model2))
qqnorm(residuals(model2))
plot(model2)

durbinWatsonTest(model2)

# Now let's do some stepwise regression to see what we end up with
steplimitsboth <- step(model0, scope=list (upper=model1), direction = "both")

summary(steplimitsboth)

confint(steplimitsboth, level = 0.95)

# Check we don't have collinearity issues
vif(steplimitsboth)

# ANOVA as regression ####
cond <- read_csv("cond.csv")
cond$Condition <- as.factor(cond$Condition)

ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + geom_point() 

# Separately by Condition
ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + 
  geom_point() + 
  facet_wrap(~ Condition) + 
  geom_smooth(method = 'lm') +
  guides(colour = FALSE)

# Run the ANOVA (i.e., without the covariate)- model is significant
model1 <- aov_4(Ability ~ Condition + (1 | Participant), data = cond)
anova(model1)

# Run the ANCOVA - when we add the effect of Gaming Frequency first,
# the model is now not significant
cond$Gaming <- scale(cond$Gaming)
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), 
                      data = cond, factorize = FALSE)
anova(model_ancova)

# Unadjusted means
describeBy(cond$Ability, group = cond$Condition)

# Report adjusted means
emmeans(model_ancova, pairwise ~ Condition, adjust = "none")

# ANCOVA as a special case of Regression 
ggplot(cond, aes(x = Condition, y = Ability, colour = Condition)) + geom_violin() + 
  geom_jitter(width = .1, alpha = .5) + stat_summary(fun.data = "mean_cl_boot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(colour = FALSE)

# Set up the Water level as the reference level and check the contrasts
cond$Condition <- relevel(cond$Condition, ref = 3)
contrasts(cond$Condition)

# Build a linear model just with Condition predicting Ability
lm1 <- lm(Ability ~ Condition, data = cond)
lm1

# Linear Mixed Models ####
# Start of random slopes and intercepts simulation ####
subject <- rep(1:10, each = 10)
condition <- rep(c("large", "small"), 50)
item <- rep(rep(1:5), 20)

data <- arrange(as.tibble(cbind(subject, condition, item)), -desc(condition))
data <- arrange(data, desc(item))

set.seed(1234)
rt <- c(rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100),
        rnorm(10, 800, 100), rnorm(10, 1000, 100))

fulldata <- arrange(cbind(data, rt), -desc(subject))

set.seed(1)
baseline <- c(rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5),
              rep(runif(1, 0, 500), 5))

fulldata <- fulldata %>% arrange(subject, desc(condition))

fulldata$rt <- fulldata$rt + baseline 

mixed_model <- lmer(rt ~ condition + (1 | subject) + (1 | item), data = fulldata)
summary(mixed_model)

mixed_model_null <- lmer(rt ~ (1 | subject) + (1 | item), data = fulldata)

anova(mixed_model, mixed_model_null)

coef(mixed_model)

mixed_model <- lmer(rt ~ condition + (1 + condition | subject) + 
                      (1 + condition | item), data = fulldata)
summary(mixed_model)

mixed_model_null <- lmer(rt ~ (1 + condition | subject) + (1 + condition | item), data = fulldata)
summary(mixed_model_null)

anova(mixed_model, mixed_model_null)

coef(mixed_model)

# Plot intercepts and slopes ####
# First by subjects ####
intercepts <- coef(mixed_model)$subject[1]
slopes<- coef(mixed_model)$subject[2]

large <- unlist(intercepts, recursive = TRUE, use.names = F)
small <- unlist(intercepts, recursive = TRUE, use.names = F) + 
  unlist(slopes, recursive = TRUE, use.names = F)
rt <- c(large, small)
condition <- c(rep("large", 10), rep("small", 10))

subject <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9, 1, 10, 2, 3, 4, 5, 6, 7, 8, 9)

data <- arrange(as.tibble(cbind(subject, condition, rt)), -desc(subject))

data$rt <- as.integer(data$rt)
data$condition <- as.factor(data$condition)
data$subject <- as.factor(data$subject)

data %>% group_by(condition) %>% summarise(mean(as.integer(rt)))

ggplot(data, aes (x = condition, y = rt, group = subject, label = subject)) + 
  geom_line(alpha = .5) +
  geom_text(check_overlap = T) +
  labs(x = "Condition", y = "RT in ms.", title = "Individual regression lines labelled per subject")

# Now by items ####
intercepts <- coef(mixed_model)$item[1]
slopes <- coef(mixed_model)$item[2]

large <- unlist(intercepts, recursive = TRUE, use.names = F)
small <- unlist(intercepts, recursive = TRUE, use.names = F) + 
  unlist(slopes, recursive = TRUE, use.names = F)
rt <- c(large, small)
condition <- c(rep("large", 5), rep("small", 5))

item <- c(1:5, 1:5)

data <- arrange(as.tibble(cbind(item, condition, rt)), -desc(item))

data$rt <- as.integer(data$rt)
data$condition <- as.factor(data$condition)
data$item <- as.factor(data$item)

data %>% group_by(condition) %>% summarise(mean(as.integer(rt)))

ggplot(data, aes (x = condition, y = rt, group = item, label = item)) + 
  geom_line(alpha = .5) +
  geom_text(check_overlap = T) +
  labs(x = "Condition", y = "RT in ms.", title = "Individual regression lines labelled per item")

# Illustration of partial pooling ####
# Inspired bt Tristan Mahr’s worked example: 
# https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/

subject <- rep(1:10, each = 5)
caffeine <- rep(rep(1:5), 10)

data <- arrange(as.tibble(cbind(subject, caffeine)), caffeine)

set.seed(1234)
rt <- c(rnorm(10, 1300, 50),
        rnorm(10, 1200, 55),
        rnorm(10, 1100, 55),
        rnorm(10, 1000, 60),
        rnorm(10, 1000, 80))

data_all <- arrange(cbind(data, rt), subject)

data_all[subject == 10 & caffeine != 1,]$rt <- NA

ggplot(data_all, aes (x = caffeine, y = rt)) + 
  geom_point() +
  geom_smooth(data = data_all, method = "lm", se = FALSE, colour = "#00BFC4", size = .75, linetype = 1) +
  facet_wrap(subject, nrow = 2) +
  labs(x = "Caffeine", y = "RT") + 
  theme(legend.position = "top")

no_pooling <- as.tibble(lmList(rt ~ caffeine | subject, data_all) %>% 
                          coef() %>% 
                          # Subject IDs are stored as row-names. Make them an explicit column
                          rownames_to_column("subject") %>% 
                          rename(Intercept = `(Intercept)`, Slope = caffeine) %>% 
                          add_column(Model = "No pooling"))

no_pooling$subject <- as.integer(no_pooling$subject)

model_pooled <- lm(rt ~ caffeine, data_all) 

# Repeat the intercept and slope terms for each participant
pooled <- data_frame(
  Model = "Complete pooling",
  subject = as.integer(unique(data_all$subject)),
  Intercept = coef(model_pooled)[1], 
  Slope = coef(model_pooled)[2])

# Join the raw data so we can use plot the points and the lines.
models <- bind_rows(pooled, no_pooling) %>% 
  left_join(data_all, by = "subject")

models$subject <- as.factor(models$subject)

p_model_comparison <- ggplot(models) + 
  aes(x = caffeine, y = rt) + 
  geom_abline(aes(intercept = Intercept, slope = Slope, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap(~ subject, nrow = 2) +
  labs(x = "Caffeine", y = "RT") + 
  theme(legend.position = "top")

p_model_comparison

m <- lmer(rt ~ caffeine + (1 + caffeine | subject), data_all)
# Just to note, the singularity message suggests our model may be over
# parameterised - we'll ignore that for the purposes of this illustration 

partial_pooling <- coef(m)[["subject"]] %>% 
  as_tibble() %>% 
  rownames_to_column("subject") %>% 
  rename(Intercept = `(Intercept)`, Slope = caffeine) %>% 
  add_column(Model = "Partial pooling")

partial_pooling$subject <- as.integer(partial_pooling$subject)

df_models <- bind_rows(pooled, no_pooling, partial_pooling) %>% 
  left_join(data_all, by = "subject")

# Replace the data-set of the last plot
ggplot(df_models) + 
  aes(x = caffeine, y = rt) + 
  geom_abline(aes(intercept = Intercept, slope = Slope, color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap(~ subject, nrow = 2) +
  labs(x = "Caffeine", y = "RT") + 
  theme(legend.position = "top")

# Linear mixed model (LMM) for 1-factor (repeated measures) with three levels ####
DV <- read_csv("DV_1factor.csv", 
               col_types = cols(Condition = col_factor(levels = c("Neutral", 
                                                                  "Positive", "Negative"))))
DV$Condition <- relevel(DV$Condition, ref=3)

ggplot(DV, aes(x = Condition, y = Gaze, colour = Condition)) + 
  geom_boxplot() + 
  guides(colour=FALSE)

ggplot(DV, aes(x = Condition, y = Gaze, colour = Condition)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .5) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + 
  guides(colour = FALSE)

model.null <- lmer(Gaze ~ (1 + Condition | Subject) + (1 + Condition | Item), 
                   data = DV, REML = TRUE)
model.full <- lmer(Gaze ~ Condition + (1 + Condition | Subject) + (1 + Condition | Item), 
                   data = DV, REML = TRUE)
anova(model.null, model.full)
summary(model.full)

# GLMM for binomial data - 1-factor with(repeated measures) with 3 levels 
RO <- read_csv("RO.csv", col_types = cols(Condition = col_factor(levels = c("Neutral", 
                                                                            "Positive", "Negative"))))

data_agg <- RO %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(DV), sd = sd(DV))

ggplot(data_agg, aes(x = Condition, y = mean, fill = Condition)) + 
  geom_col() +
  guides(fill = FALSE)

model.full <- glmer(DV ~ Condition + (1+Condition|Subject) + (1+Condition|Item), 
                    data=RO, family = binomial)

model.interceptonly <- glmer(DV ~ Condition + (1 | Subject) + (1 | Item), 
                             data = RO, family = binomial)

model.null <- glmer(DV ~  (1 | Subject) + (1 | Item), data = RO, family = binomial)
anova(model.interceptonly, model.null)

# LMM for 2x2 repeated measures factorial design
DV <- read_csv("DV.csv")

DV$Sentence <- as.factor(DV$Sentence)
DV$Context <- as.factor(DV$Context)

# Set up contrast coding
contrasts(DV$Sentence) <- matrix(c(.5, -.5))
contrasts(DV$Context) <- matrix(c(.5, -.5))

# Visualise
ggplot(DV, aes(x = Context:Sentence, y = RT, colour = Context:Sentence)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .1) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + 
  guides(colour = FALSE)

data_agg <- DV %>% 
  group_by(Context, Sentence) %>% 
  summarise_at("RT", funs(mean, sd), na.rm = TRUE)

data_agg$SE <- data_agg$sd/sqrt(60)

ggplot(data_agg, aes(x = Context, y = mean, fill = Context, group = Sentence, 
                     colour = Sentence)) +
  geom_line() + 
  geom_point() + 
  ylim(1200, 1800) + 
  labs(y = "Mean RT (ms.)") +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE, width = .1)) + 
  guides(fill = FALSE)

model.full <- lmer(RT ~ Context * Sentence + (1 + Context * Sentence | Subject) + 
                     (1 + Context * Sentence | Item), data = DV, REML = TRUE)
model.null <- lmer(RT ~ (1 + Context * Sentence | Subject) + (1 + Context * Sentence|Item), 
                   data = DV, REML = TRUE)
anova(model.full, model.null)
summary(model.full)

emmeans(model.full, pairwise ~ Context * Sentence, adjust = "none")

# Build and examine normality of residuals with data untransformed ####
model.full <- lmer(RT ~ Context * Sentence + (1 + Context * Sentence|Subject) + 
                     (1 + Context * Sentence | Item), data = DV, REML = TRUE)
qqnorm(residuals(model.full))
summary(model.full)

# Build and examine normality of residuals with data log transformed 
model.full <- lmer(log(RT) ~ Context * Sentence + (1 + Context * Sentence | Subject) + 
                     (1 + Context * Sentence | Item), data = DV, REML = TRUE)
qqnorm(residuals(model.full))
summary(model.full)

# Build and examine normality of residuals with GLMM under the Gamma distribution - 
# simplified random effects structure needed to converge
model.full <- glmer(RT ~ Context * Sentence + (1 | Subject) + 
                      (1 | Item), data = DV, family = Gamma)
qqnorm(residuals(model.full))
summary(model.full)
