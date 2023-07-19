library(tidyverse)
library(ggcorrplot)
library(gridExtra)
library(caTools)

df <- read_csv("D:/Internships/Technohack/Task - 1/data.csv")

View(df)
glimpse(df)
df %>% is.na() %>% sum()

# Count plots:
count_plot <- function(var){
  df %>% count({{var}}) %>% 
    ggplot(aes(x = as.factor({{var}}), y = n)) +
    geom_bar(stat = 'identity', position = position_dodge2(),
             fill = 'red', width = 0.4) +
    theme(axis.title = element_text(face = 'bold', size = 25),
          axis.text = element_text(face = 'bold', size = 15),
          axis.text.x = element_text(angle = 90)) +
    theme_minimal()
}

attach(df)
count_plot(bedrooms)
count_plot(bathrooms)
count_plot(floors)
count_plot(condition)
count_plot(grade)
count_plot(view)
count_plot(waterfront)
detach(df)

# price:

ggplot(df, aes(price)) + geom_histogram(aes(y = after_stat(density)),
                                        fill = 'yellow',
                                        colour = 'black',
                                        bins = 50) +
  labs(title = 'Distribution of price (dollar)',
       x = 'Price (Dollar)', y = 'Frequency density') +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 30),
        axis.title = element_text(face = 'bold', size = 25))

# Re-leveling the columns (to get an idea about the distribution
# of price)
df1 <- df


df1 %>% mutate(bedrooms = case_when(
  bedrooms <= 3 ~ "<=3",
  bedrooms > 3 ~ ">3"
)) %>% mutate(bedrooms = as.factor(bedrooms)) -> df1
df$bedrooms <- as.factor(df$bedrooms)


df1 %>% mutate(bathrooms = case_when(
  bathrooms <= 1.5 ~ "<=1.5",
  bathrooms > 1.5 & bathrooms <= 2.5 ~ "(1.5,2.5]",
  bathrooms > 2.5 ~ ">2.5"
)) %>% mutate(bathrooms = as.factor(bathrooms)) -> df1
df$bathrooms <- as.factor(df$bathrooms)


df1 %>% mutate(floors = case_when(
  floors < 2 ~ "<2",
  floors >= 2 ~ ">=2" 
)) %>% mutate(floors = as.factor(floors)) -> df1
df$floors <- as.factor(df$floors)


df1 %>% mutate(condition = case_when(
  condition <= 3 ~ '0',
  condition > 3 ~ '1'
)) %>% mutate(condition = as.factor(condition)) -> df1
df$condition <- as.factor(df$condition)


df1 %>% mutate(grade = case_when(
  grade <= 6 ~ '0',
  grade > 6 & grade <= 8 ~ '1',
  grade > 8 ~ '2'
)) %>% mutate(grade = as.factor(grade)) -> df1
df$grade <- as.factor(df$grade)


df1 %>% mutate(view = case_when(
  view == 0 ~ '0',
  view > 0 ~ '1'
)) %>% mutate(view = as.factor(view)) -> df1
df$view <- as.factor(df$view)


df1 %>% mutate(renovated = case_when(
  yr_renovated == 0 ~ '0',
  yr_renovated > 0 ~ '1'
)) %>% mutate(renovated = as.factor(renovated)) -> df1

df$waterfront <- as.factor(df$waterfront)


# For model
df2 <- df %>% select(-c(id,date,sqft_basement))
df1$waterfront <- as.factor(df1$waterfront)

# correlation:
df2 %>% select(where(is_double)) %>% 
  cor() %>% ggcorrplot(lab = T, type = 'upper',
                       ggtheme = ggplot2::theme_minimal)


attach(df)
l <- lm(price ~ sqft_lot15 + sqft_living15 + long + lat +
          zipcode + yr_renovated + yr_built + 
          sqft_above + sqft_lot + sqft_living, data = df2)
ols_vif_tol(l)

df2 <- df2 %>% select(-sqft_living)

l2 <- lm(price ~ sqft_lot15 + sqft_living15 + long + lat +
          zipcode + yr_renovated + yr_built + 
          sqft_above + sqft_lot, data = df2)
ols_vif_tol(l2)
summary(l2)



# price ~ categorical variables:
plot2 <- function(fill_var){
  cols <- c('yellow','blue','red')
  
  df1 %>% ggplot(aes(log(price), fill = {{fill_var}})) + 
    geom_density(colour = 'black', alpha = 0.4) +
    scale_x_continuous(n.breaks = 10) + 
    labs(x = '') + theme_minimal() +
    scale_fill_manual(values = cols) -> p1
  
  df1 %>% ggplot(aes(log(price), fill = {{fill_var}})) + 
    geom_boxplot(outlier.colour = 'orange',
                 outlier.size = 0.6) + 
    labs(x = '') +theme_minimal() +
    scale_fill_manual(values = cols) -> p2
  grid.arrange(p1,p2, ncol = 2)
}

attach(df1)
plot2(bedrooms)
plot2(bathrooms)
plot2(floors)
plot2(waterfront)
plot2(view)
plot2(condition)
plot2(grade)
plot2(renovated)
detach(df1)


glimpse(df1)
# Plot 1
name1 <- c('<2' = 'Floor < 2','>=2' = 'Floor >= 2')
df1 %>% ggplot(aes(x = bedrooms, y = log(price), fill = bathrooms)) +
  geom_boxplot(outlier.colour = 'orange') + 
  facet_wrap(.~floors, labeller = 
              labeller(floors = as_labeller(name1, label_context))) +
  theme_linedraw()

# plot 2
df1 %>% ggplot(aes(x = view, y = log(price), fill = waterfront)) +
  geom_boxplot() + facet_grid(renovated ~ grade) + 
  theme_bw()

# Fitting of model:===============================================

# Splitting
set.seed(123)
s <- sample.split(df2$price, SplitRatio = 3/4)
train_data <- df2[s,]
test_data <- df2[!s,]

f <- function(d)(paste(d[1], 'x', d[2]))
glue::glue("Dimension of train data: {d1}",
           "Dimension of test data: {d2}",
           d1 = f(dim(train_data)), d2 = f(dim(test_data)),
           .sep = '\n')


l <- lm(price ~ ., data = train_data)
summary(l)

train_data2 <- train_data %>% select(-bedrooms)
test_data2 <- test_data %>% select(-bedrooms)
l2 <- lm(price ~ ., data = train_data2)
summary(l2)



price_pred <- predict(l2, newdata = test_data2)
var(price_pred)/var(test_data2$price)


hist(test_data2$price)
hist(price_pred)


my_col = c('blue','orange')
data.frame('Actual' = test_data2$price,
           'Fitted' = price_pred) %>% 
  pivot_longer(Actual:Fitted, names_to = 'Type', values_to = 'Price') %>% 
  ggplot(aes(Price, fill = Type)) + geom_density(alpha = 0.4, colour = NA) +
  scale_fill_manual(values = my_col) + theme_minimal() + 
  labs(title = 'Distribution of price | Test data')+
  theme(plot.title = element_text(face = 'bold', size = 30),
      axis.title = element_text(face = 'bold', size = 25),
      legend.title = element_text(face = 'bold', size = 12),
      legend.text = element_text(face = 'bold', size = 10))




