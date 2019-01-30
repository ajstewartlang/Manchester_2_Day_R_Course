# Load the libraries we will be using ####
library(tidyverse)
library(psych)
library(NHANES)
library(viridisLite)
library(ggrepel)
library(visdat)

# Data Wrangling ####
# Create data for 10,000 people - each with measures of Working Memory (WM), 
# IQ, and Reading Comprehension (Comp)
set.seed(1234)
ID <- seq(1:10000)
WM <- as.integer(rnorm(10000, mean = 50, sd = 5))
IQ <- as.integer(rnorm(10000, mean = 100, sd = 15))
Comp <- as.integer(rnorm(10000, mean = 20, sd = 2))

data <- data.frame(ID, WM, IQ, Comp)

colnames(data) = c("ID", "WM", "IQ", "Comp")

# Create data for 48 participants (all present in data) taking part in an 
# experiment
set.seed(1234)
ID <- sample(ID, 48)
Simple <- as.integer(rnorm(48, mean = 2000, sd = 140))
Complex <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- data.frame(ID, Simple, Complex)
colnames(dataRT) = c("ID", "Simple_sentence", "Complex_sentence")

dataRT_all <- inner_join(data, dataRT, by = (c("ID")))

data_transformed <- mutate(dataRT_all, 
                           log_Simple = log(Simple_sentence), 
                           log_Complex = log(Complex_sentence))

filtered_data <- filter(data_transformed, ID != 2006)
filtered_data

data_long <- gather(dataRT, "Condition", "RT", c("Simple_sentence", "Complex_sentence"))

data_wide <- spread(data_long, "Condition", "RT")

# Use dplyr funcitons to get some summary statistics from the RT dataset 
# using the pipe operator
data_long %>% 
  group_by(Condition) %>% 
  summarise(Mean = mean(RT), Min = min(RT), Max = max(RT), SD = sd(RT))

# Use the psych::describeBy function to generate descriptives 
describeBy(data_long$RT, group=data_long$Condition)

# Revalue one column capturing 2x2 and then splitting
# First create the data set - 24 items each with one RT measure for each 
# of 4 conditions
Participant <- rep (1:24, each=4)
Condition <- rep (1:4, times=24)
set.seed(1234)
RT <- as.integer (rnorm (24*4, 1000,100))

data <- as.data.frame(cbind(Participant, Condition, RT))

# Recode Condition columns follows:
# Condition 1 = Prime A, Target A
# Condition 2 = Prime A, Target B
# Condition 3 = Prime B, Target A
# Condition 4 = Prime B, Target B
data$Condition <- recode (data$Condition, 
                          "1" = "PrimeA_TargetA", 
                          "2" = "PrimeA_TargetB",
                          "3" = "PrimeB_TargetA", 
                          "4" = "PrimeB_TargetB")

# Now separate the Condition column using "_" as our separator
data <- separate(data, col = "Condition", into = c("Prime", "Target"), sep = "_")

# Combine again
data <- unite(data, col = "Condition", c("Prime", "Target"), sep = "_")
wide_data <- spread(data, key = "Condition", value = "RT")

#or in tidyverse style with the %>% operator
data %>% 
  separate(col = "Condition", into = c("Prime", "Target"), sep = "_") %>% 
  unite(col = "Condition", c("Prime", "Target"), sep = "_") %>%
  spread(key = "Condition", value = "RT")

# NHANES data ####
colnames(NHANES)
head(NHANES)
nrow(NHANES)
length(unique(NHANES$ID))

NHANES_tidied <- NHANES %>% distinct(ID, .keep_all = TRUE)
nrow(NHANES_tidied)

vis_miss(NHANES_tidied)

NHANES_focused <- select(NHANES_tidied, ID, Gender, SleepHrsNight, 
                         AlcoholDay, Marijuana, BMI) 

vis_miss(NHANES_focused)

NHANES_focused %>%
  ggplot(aes(x = BMI, y = SleepHrsNight, colour = Gender)) + 
  geom_jitter(alpha = .25) +
  geom_smooth(method = "lm", colour = "blue") +
  guides(colour = FALSE) +
  facet_wrap(~ Gender)

NHANES_focused %>%
  ggplot(aes(x = BMI, fill = Gender)) + 
  geom_histogram(aes(y= ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = FALSE) +
  facet_wrap(~ Gender)

# Start of main data visualisation section ####
# Bar Graph
data_summ <- data_long %>% 
  group_by(Condition) %>% 
  summarise(Mean = mean(RT), sd = sd(RT))

ggplot (data_summ, aes (x=Condition, y=Mean, group=Condition, 
                        fill=Condition, ymin=Mean-sd, ymax=Mean+sd)) + 
  geom_bar(stat = "identity", width=.5) + 
  geom_errorbar(width=.25) +  
  ggtitle("Bar chart with Error Bars") + 
  guides(fill=FALSE) 

# When boxplots can mislead
Subject <- seq(1:80)
Group <- factor(rep(1,80))
set.seed(1234)
RT <- c(rnorm(40,500,200), rnorm(40,1900,200))
data1 <- tibble(Subject, Group, RT)

ggplot(data1, aes(x = Group, y = RT)) + 
  geom_boxplot()

ggplot(data1, aes(x = Group, y = RT)) + 
  geom_jitter(size = 2, width = .1, alpha = .25)

ggplot(data1, aes(x = Group, y = RT)) + 
  geom_boxplot() + 
  geom_jitter(size = 2, width = .1, alpha = .25)

ggplot(data1, aes(x = Group, y = RT)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .5)

# Violin Plot
ggplot (data_long, aes (x = Condition, y = RT, group = Condition, fill = Condition)) + 
  geom_violin() + 
  geom_jitter(alpha = .25, position = position_jitter(0.05)) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

# Raincloud plot ####
library(RColorBrewer)
library(plyr) # note, need to detach this after this plot as clashes with aspects of dplyr
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

set.seed(1234)
ID <- sample(ID, 48)
Simple <- as.integer(rnorm(48, mean = 2000, sd = 140))
Complex <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- data.frame(ID, Simple, Complex)
colnames(dataRT) <- c("ID", "Simple Sentence", "Complex Sentence")

dataRT <- gather(dataRT, key = "Condition", 
                 value = "RT", 
                 c("Simple Sentence", "Complex Sentence"))

raincloud_theme <- theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
  axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- ddply(dataRT, ~Condition, summarise, mean = mean(RT), median = median(RT), 
               lower = lb(RT), upper = ub(RT))

ggplot(data = dataRT, aes(y = RT, x = Condition, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = RT, color = Condition), position = position_jitter(width = .15), 
             size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL) +
  scale_y_continuous(breaks = seq(1500, 3000, by = 200))

# Recreate data for 10,000 people - each with measures of Working Memory (WM), IQ, and 
# Reading Comprehension (Comp)
set.seed(1234)
ID <- seq(1:10000)
WM <- as.integer(rnorm(10000, mean = 50, sd = 5))
IQ <- as.integer(rnorm(10000, mean = 100, sd = 15))
Comp <- as.integer(rnorm(10000, mean = 20, sd = 2))

data <- data.frame(ID, WM, IQ, Comp)

colnames(data) <- c("ID", "WM", "IQ", "Comp")

# Scatterplots
ggplot(data, aes(x = WM, y = IQ)) + 
  geom_point()

ggplot(data, aes(x = WM, y = IQ)) + 
  geom_jitter(alpha = .1, position = position_jitter(0.5)) 

ggplot(data, aes(x = WM, y = IQ)) + 
  geom_jitter(alpha = .1, position = position_jitter(0.5)) + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = WM, y = IQ)) + 
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) + 
  scale_fill_viridis() + 
  coord_cartesian(expand = FALSE) 

# More plotting using built in data - this time the mpg dataset ####
detach("package:plyr", unload = TRUE) #need to detach as some clashes with dplyr

# Find out about the mpg dataset and generate some descriptives ####
head(mpg)
unique(mpg$manufacturer)
length(unique(mpg$manufacturer))

mpg %>% 
  group_by(class) %>% 
  summarise (Mean=mean(hwy))

mpg %>% 
  group_by(cyl) %>% 
  summarise (Mean=mean(hwy))

# Build a violin plot with added descriptives
ggplot (mpg, aes(x = factor (cyl), y = cty, fill = factor (cyl))) + 
  geom_violin() +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = .5) +
  xlab("Number of Cylinders") + ylab("City Fuel Consumption (mpg)") +
  ggtitle ("City Fuel Consumption by Number of Cylinders")

# facet wrap by vehicle class with displacement instead of cylinder number
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour=FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class")

# now add a linear function to each
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "lm") + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class with Linear Regression Line")

# now with a non-linear function to each
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "loess") + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class with Non-Linear Regression Line")

# Focus on subcompact cars and use geom_repel() to label ####

# First without labels
ggplot(filter(mpg, class == "subcompact"), aes(x = displ, y = hwy, label = model)) + 
  geom_point() +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement")

# Now using ggrepel to add text labels
ggplot(filter(mpg, class == "subcompact"), aes(x = displ, y = hwy, colour = model, 
                                               label = model)) + 
  scale_colour_brewer(palette = "Dark2") +
  geom_label_repel(label.padding = 0.25, label.size = .25, 
                   min.segment.length = 100, size = 3.5) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement")

# plot basic histogram
ggplot(mpg, aes(displ)) + 
  geom_histogram(binwidth = .5) +
  guides(colour=FALSE) + 
  xlab("Displacement (litres)") + ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement")

# facet by class and include background histogram of all data in each facet
mpg1 <- select(mpg, -class)
ggplot(mpg, aes(x = displ)) + 
  geom_histogram(data = mpg1, fill = "grey", binwidth = .5) +
  geom_histogram(binwidth = .5) +
  guides(colour=FALSE) + 
  facet_wrap (~ class)  + 
  xlab("Displacement (litres)") + 
  ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement for Each \nVehicle Class")

# plot histogram of displacement, coloured by class
ggplot(mpg, aes(x = displ, colour = class, fill = class)) + 
  geom_histogram(binwidth = .5) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement Coloured By \nVehicle Class")

ggplot(mpg, aes(x = cty))  + 
  geom_density(aes(fill = factor(class)), alpha = 0.8) + 
  labs(title="City mpg Grouped by Vehicle Class",
       x="City Fuel Consumption (mpg)",
       fill="Vehicle Class")

# scatterplot with jitter 
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_jitter(width = 0.05, alpha = .2, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement")

ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement Grouped by Class")

ggplot(mpg, aes(x = displ, y = hwy, colour = cyl)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement Grouped by Cylinder Number")

# 2d histogram with density heatmap
ggplot(mpg, aes(x = displ, y = hwy)) +
  stat_bin2d(bins = 10, colour = "black") + 
  scale_fill_viridis() + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Density Heat Map of Highway Fuel Consumption against \nEngine Displacement")

# Time series plots using the 'economics' dataset
# First let's get the structure of the dataset and the first 10 rows
str(economics)
economics

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = pop)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Population Size from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Total Population (thousands)", x = "Date")

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = psavert)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Savings Rate from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Savings Rate (%)", x = "Date")

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = unemploy)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Unemployement Rate from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Unemployment (thousands)", x = "Date")

# Animated plots - first we need to tidy the data and aggregate by Year ####
# Need to load the lubridate package to extract the year information
library(lubridate)

economics.tidy <- economics %>% 
                        mutate (year = year (date)) %>% 
                        group_by(year) %>%
                        summarise (mean_pop = mean(pop), mean_un = mean(unemploy))

economics.tidy$mean_pop <- scale(economics.tidy$mean_pop)
economics.tidy$mean_un <- scale(economics.tidy$mean_un)

# first a static plot
data <- economics.tidy
ggplot(data, aes(x = mean_pop, y = mean_un)) + 
  ylim(-3, 3) + 
  xlim(-3, 3) +
  geom_point(size = 5, colour = "red") + 
  ggtitle("All Years") + 
  labs(x = "Population Size (Z-score)", y = "Unemployment Size (Z-score)")


# Using gganimate
# devtools::install_github('thomasp85/gganimate')
library(gganimate)

ggplot(data, aes(x = mean_pop, y = mean_un)) + 
  ylim(-3, 3) + 
  xlim(-3, 3) +
  geom_point(size = 5, colour = "red") + 
  labs (title = "Year: {as.integer(frame_time)}", 
        x = "Population (Z-score)", 
        y = "Unemployment (Z-score)") +
  transition_time(year) + 
  theme(title = element_text(size = 15)) +
  ease_aes("linear") + 
  shadow_trail()

# Animated plot using the 'gapminder' dataset
library(gapminder)

# gganimate code of life expectancy by GDP for each contintent over time
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Now vary each point according to population size
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# gganimate code faceted by continent
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Dot plot on life expectancy data
# Static faceted by year
df_Americas <- gapminder %>% filter(continent == "Americas")
ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) + 
  facet_wrap(~ year) + 
  theme(text = element_text(size = 8))

# Dynamic - separate frame per year
df_Americas <- gapminder %>% filter(continent == "Americas")
ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(title=element_text(size = 15)) +
  transition_time(year) +
  ease_aes("linear")

# animation of all countries and life expectancy
df_all <- gapminder
ggplot(df_all, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(title=element_text(size = 15)) +
  transition_time(year) +
  ease_aes("linear")

# Sample size and estimate variability around the mean ####
set.seed(1234)
max_sample_size <- 2500
data <- NULL

for (i in 2:max_sample_size){
  sample <- cbind(rnorm(i, 1000, 50), i)
  data <- rbind(data, sample)
}  

data <- as.tibble(data)
colnames(data) <- c("measure", "sample_size")

all <- NULL
for (i in 2:max_sample_size){
  each <- cbind(mean_cl_boot(filter(data, sample_size == i)$measure), i)
  all <- rbind(all, each)
}

colnames(all) <- c("mean", "ymin", "ymax", "sample_size")

ggplot(all, aes(x = sample_size, y = mean)) + 
  geom_point(colour = "red") + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  geom_hline(yintercept = 1000, colour = "blue") +
  ylim(925, 1075) +
  labs(x = "Sample Size", y = "Estimate of Mean") +
  theme_minimal() +
  theme(text = element_text(size = 15)) +
  transition_time(sample_size) + 
  #shadow_mark(size = 4, colour = "grey") +
  shadow_trail(distance = .025)

# Animated raincloud plots ####
library(plyr)
df <- NULL
set.seed(1111)
sample_size = 500  # Change sample size here to 20 or 500

for (i in 1:9) {
  a <- rnorm(sample_size, mean = 10, sd = 2)
  b <- rnorm(sample_size, mean = 10, sd = 2)
  a <- cbind(a, rep ("A", sample_size), rep (i, sample_size))
  b <- cbind(b, rep("B", sample_size), rep (i, sample_size))
  df <- rbind(df, (rbind(a, b)))
}

df <- as.tibble(df)

colnames(df) <- c("Score","Condition", "Sample")

df$Score <- as.numeric(df$Score)

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

dataRT <- df

sumld <- ddply(dataRT, ~Condition, summarise, mean = mean(Score), median = median(Score), lower = lb(Score), upper = ub(Score))

ggplot(data = dataRT, aes(y = Score, x = Condition, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = Score, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  transition_states(Sample, transition_length = 4, state_length = 8) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs (x = "Condition", y = "DV", title = paste0("Sample number {closest_state}
                                                  Sample size = ", sample_size))

# Correlation plots animated ####
library(MASS) # Needed to sample from multivariate distribution
library(broom)

# Simulating multivariate data with specific covariance structure
# Use the mvrnorm() function from the MASS package

# A vector of means of our two variables
mu <- c(1000, 2000) 

# Covariance of our 2 variables
# It is equal to Pearson's R * SD_var1 * SD_var2
# If we know the variance for each of our variables we can calculate the sd
# We can then use these values to work out the covariance we need for any 
# particular Pearson's r value
# For the below example to give us a Pearson's r of .5 
# we have covariance = .5 * sqrt(100) * sqrt(50) which gives us 35.35534
myr <- 35.35534

# The covariance matrix where we have the variance of variable 1, 
# the covariance of variables 1 and 2
# the covariance of variables 1 and 2 and the variance of variable 2
mysigma <- matrix(c(100, myr, myr, 50), 2, 2) 

# Animating sampling many times and illustrating varation in our regression line (and our correlation
# calculation) due to sampling error.

data <- NULL
sample_size <- 500

set.seed(3)
for (i in 2:9) {
  sample <- data.frame(mvrnorm(sample_size, mu, mysigma))
  sample$sample <- i
  data <- rbind(sample, data)
}

# Now set the first sample so the correlation is identical to the correlation in the multivariate
# population we're sampling from
# Need to reset empirical to TRUE for this to work
sample <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = TRUE))
sample$sample <- 1
data <- rbind(sample, data)

colnames(data) <- c("Var_1", "Var_2", "Sample")

ggplot(data, aes(x = Var_1, y = Var_2)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  transition_states(Sample, transition_length = 1, state_length = 2) + 
  labs(x = "Variable 1", y = "Variable 2", 
       title = "9 samples where each sample size = {sample_size}
       Sample number: {closest_state}") + 
  theme(title = element_text(size = 15)) +
  ease_aes("linear")

# Hockey game simluation ####
library(tidyverse)
library(gganimate)
set.seed(1234)

team_b_goals <- NULL

for(i in 1:100000) {
  score <- sum(sample(c(1, 0), size = 20, replace = TRUE, prob = c(0.055, 1-.055)))
  team_b_goals <- c(team_b_goals, score)}

team_a_goals <- rep(1, 100000)

all_games <- as_tibble(cbind(team_a_goals, team_b_goals))

# Calculate cumulative sum
all_games <- all_games %>% 
  mutate(team_a_wins = team_a_goals > team_b_goals,
         team_b_wins = team_b_goals > team_a_goals) 

all_games$game_number <- seq(1:100000)
all_games$team_a_wins <- cumsum(all_games$team_a_wins)
all_games$team_b_wins <- cumsum(all_games$team_b_wins)

small_data <- tibble(c(all_games$team_a_wins, all_games$team_b_wins), 
                     c(rep("Team_A", 100000), rep("Team_B", 100000)),
                     c(seq(1:100000), seq(1:100000)))

colnames(small_data) <- c("Points", "Team", "Game")

small_data %>% 
  filter(Game < 10000) %>% # remove this line to plot all 100000 games - crashes my Mac though!
  ggplot(aes(x = Points, y = Game, group = Team, colour = Team)) +
  geom_point() +
  geom_line(size = 2) + 
  coord_flip() +
  transition_reveal(Game) +
  shadow_mark(past = TRUE) +
  labs(title = "Wins of Team A vs. Team B", 
       x = "Running total of wins",
       y = "Game number") +
  theme(text = element_text(size = 15))

# Collecting Twitter data ####
# You will need to create a Twitter API access token to do this yourself
# Type vignette("auth") into the console to get information abot doing this
tweets <- search_tweets(q = "suicide", n = 1000, include_rts = FALSE, 
                        retryonratelimit = TRUE)

time <- tibble(Time = hour(tweets$created_at))

time %>%
  filter(!is.na(Time)) %>%
  group_by(Time) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

time %>%
  filter(!is.na(Time)) %>%
  ggplot(aes(x = Time)) + 
  geom_histogram(binwidth = 1)

# Doing a quick sentiment analysis on tweets mentioning suicide ####
# created between midnight and 6AM 
time_more <- mutate(tweets, Time = hour(created_at))

text <- filter(time_more, Time > 0 & Time < 6)

rt1 <- text %>%
  unnest_tokens(word, text)

sent <- get_sentiments("bing")   #nrc for details - bing for positive vs negatives sentiment

word_counts <- rt1 %>%
  inner_join(sent) %>%
  count(word, sentiment, sort = TRUE)

word_counts %>% 
  filter(n > 25 & word != "suicide") %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")  

# Creating a wordcloud ####
library(wordcloud)

word_counts_tidy <- filter(word_counts, word != "suicide")

set.seed(1234)

wordcloud(words = word_counts_tidy$word, 
          freq = word_counts_tidy$n, 
          min.freq = 5,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors=brewer.pal(8, "Dark2"))

# The End! ####


