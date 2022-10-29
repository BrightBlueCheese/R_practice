# libraries
library(dplyr)
library(tidyverse)
library(ggpubr)     # For placing multiple plots in one figure


# load data
df <- read_csv('./dataset/rodentData_for_assignment.csv')
df_origin <- read_csv('./dataset/rodentData_clean.csv')
df
glimpse(df)

# Convert columns to factor as needed
####################################
# Convert institutionCode to factor
df$institutionCode <- 
  as.factor(df$institutionCode)
# Convert countryCode to factor
df$countryCode <- 
  as.factor(df$countryCode)
# Convert states to factor
df$stateProvince <- 
  as.factor(df$stateProvince)
# Convert county to factor
df$county <- 
  as.factor(df$county)
#Convert genus to a factor
df$genus <- 
  as.factor(df$genus)
# Convert specificEpithet to a factor
df$specificEpithet <- 
  as.factor(df$species)
# Convert specificEpithet to a factor
df$species <- 
  as.factor(df$species)
# Convert sex to a factor
df$sex <- 
  as.factor(df$sex)

glimpse(df)
#####################################
df_origin$institutionCode <- 
  as.factor(df_origin$institutionCode)
# Convert countryCode to factor
df_origin$countryCode <- 
  as.factor(df_origin$countryCode)
# Convert states to factor
df_origin$stateProvince <- 
  as.factor(df_origin$stateProvince)
# Convert county to factor
df_origin$county <- 
  as.factor(df_origin$county)
#Convert genus to a factor
df_origin$genus <- 
  as.factor(df_origin$genus)
# Convert specificEpithet to a factor
df_origin$specificEpithet <- 
  as.factor(df_origin$species)
# Convert specificEpithet to a factor
df_origin$species <- 
  as.factor(df_origin$species)
# Convert sex to a factor
df_origin$sex <- 
  as.factor(df_origin$sex)

# HW
## length has been done, but not weight. fix it

# Create a dataframe to hold the lower/upper values for outlier identification
genusLimit = data.frame(genus = unique(df$genus))
genusLimit
# Calculate the lower/upper values for weight
genusLimit$lower = sapply(genusLimit$genus, 
                          function(x) quantile(df$weight[df$genus==x],0.25) 
                          - 1.5 * (quantile(df$weight[df$genus==x],0.75) 
                                   - quantile(df$weight[df$genus==x],0.25)) )
genusLimit$upper = sapply(genusLimit$genus, 
                          function(x) quantile(df$weight[df$genus==x],0.75) 
                          + 1.5 * (quantile(df$weight[df$genus==x],0.75) 
                                   - quantile(df$weight[df$genus==x],0.25)) )
genusLimit


# join lower/upper values  for weight to cleanData
df <- df %>% left_join(genusLimit, by='genus')

# set isInvalid based on lower/upper values for weight
df$isInvalid <- 
  ifelse(df$weight < df$lower | df$weight > df$upper,
         TRUE, df$isInvalid)

# remove lower/upper values for weight
df <- df %>% select(-lower, -upper)

# check on how many rows were marked invalid
summary(df$isInvalid)


## plot for before and after

  # plot base for df_origin
p_origin <- ggplot(data=df_origin)

  # plot base for df
p <- ggplot(data=df %>% filter(!isInvalid))



# density plot df vs df_origin
  # check the x axis range before set the xlim
summary(df_origin$weight)

p1_den <- p_origin + geom_density(aes(x = weight, fill = genus, alpha = .5))+
  xlim(0, 400) + ylim(0, 0.07) + ggtitle('df_origin (before)')
p2_den <- p + geom_density(aes(x = weight, fill = genus, alpha = .5)) +
  xlim(0, 400) + ylim(0, 0.07) + ggtitle('df (after)')


  # some parameters and functions to look better
ggarrange(p1_den, p2_den + rremove('ylab') + 
            theme(axis.ticks.y=element_blank(), axis.text.y=element_blank()),
          ncol = 2, nrow = 1, common.legend=TRUE, legend='bottom')

## @@ NOTE - density: 
## After clearing the invalid data, I can see all of the density plots by genus
## shortened their tails on the right side. Which means, the clearing reduce 
## the distortion of the data caused by invalid weight data
## Also, I can check the positions of the peak of each genus of the plot has been changed

# box plot df vs df_origin
p1_box <- p_origin + geom_boxplot(aes(x=genus, y=weight, fill=genus), outlier.color='red') + 
  ggtitle('df_origin (before)') + ylim(0, 400) + ggtitle('df_origin (before)')
p2_box <- p + geom_boxplot(aes(x=genus, y=weight, fill=genus), outlier.color='blue') + 
  ggtitle('df (after)') + ylim(0, 400) + ggtitle('df (after)')

ggarrange(p1_box + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()),
          p2_box + rremove('ylab') + 
            theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                  axis.ticks.x=element_blank(), axis.text.x=element_blank()),
          ncol = 2, nrow = 1, common.legend=TRUE, legend='bottom', align='v')

df %>% filter(weight <= 1) %>% .[c('weight', 'length')]
## @@ Note - box:
## After clearing the invalid data, I can see all min and max value of each genus 
## have been changed. And I also can see the numbers of outliers have been changed
## dramatically. I could see the min value (bottom tail) of microtus changed
## very little. So I used tidyverse to see which data why the min value for microtus
## changed only small amount. Then I realized that we still have some invalid data
## although we did some cleaning process


# scatter plot length vs weight of df & df_origin
p1_scatter <- df_origin %>% ggplot(aes(x = weight, y = length, color = genus)) + 
  geom_point() +
  facet_grid(~genus) +
  geom_smooth(color = "blue", se = FALSE) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlim(0, 400) +
  ylim(0, 500) +
  ggtitle('df_origin (before)')

p2_scatter <- df %>% filter(!isInvalid) %>% 
  ggplot(aes(x = weight, y = length, color = genus)) + 
  geom_point() +
  facet_grid(~genus) +
  geom_smooth(color = "blue", se = FALSE) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlim(0, 400) +
  ylim(0, 500) + 
  ggtitle('df (after)')

ggarrange(p1_scatter + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
          p2_scatter + rremove('ylab') + 
            theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
          ncol = 2, nrow = 1, common.legend=TRUE, legend='bottom', align='v')
## @@ Note - scatter:
## Since a lot of outliers (= invalid data) have been removed, I can check the 
## lengths of both losses line and linear regression line are reduced.
## Also, the curvature of losses lines (blue) have been reduce dramatically.
## So both lines does not have extreme difference comparing origial data
## However, it seems like tamias's data still have some data that affect to
## the rest of the data dramatically by checking the extreme curve on the front side
## of the losses line (blue).
## 

## save file
df %>%
  filter(!isInvalid) %>%
  write_csv("./dataset/rodentData_consistent.csv")


## check the additional work for graduate students
  # Answer in short: Because the density plot for df(after) calculated 
  # Q1, Q3, and IQR to find the outliers with the data which already has been
  # cleaned (the outliers)
  # We actually extracted the outliers TWICE!! <<<< Super short version @@@@@@@

  # Example with thomomys' quantile - density plot of df has been created 
  # based on this already cleared data
thomomys <- df %>% filter(!isInvalid) %>% filter(genus == 'thomomys')
quantile(thomomys$weight)
    # quantile(thomomys$weight)
    # 0%   25%   50%   75%  100% 
    # 34.0  77.0  93.0 110.0 166.3 
  # However, the actually extracted the outliers before with
  # Q1 = 24.125, and Q3 = 166.325 



