#The effect of intervention with a probiotic on abundance the bacterium Ruminococcus gnavus in human stool samples.
#SET UP----

usethis::use_git_config(user.name = "nnu22jbu", user.email = "nnu22jbu@uea.ac.uk")

#________________________----

#PACKAGES ----
#install.packages("tidyverse")
#install.packages("performance")
#install.packages("ggplot2")
#install.packages("janitor")
#install.packages("lubridate")
#install.packages("patchwork")
#install.packages("dplyr")
#install.packages("skimr")
#install.packages("see")
#install.packages("car")
#install.packages("lmtest")
#install.packages("usethis")
#install.packages("gitcreds")
library(gitcreds)
library(usethis)
library(skimr)
library(tidyverse)
library(ggplot2)
library(performance)
library(see)
library(janitor)
library(lubridate)
library(patchwork)
library(dplyr)
library(car)
library(lmtest)



#_______________________----
#IMPORT DATA----

probiotic <- read_csv ("data/probiotic.csv")
head(probiotic)

colnames(probiotic)

probiotic <-janitor::clean_names(probiotic) #clean column names

probiotic <- rename(probiotic, "r.gnavus_abund"="ruminococcus_gnavus_abund") #renaming columns

probiotic%>% 
  
  duplicated() %>%
  
  sum() #checking for duplicates.

probiotic%>%
  
  group_by(gender) %>%
  
  summarise(n=n()) #number of individuals per gender, also N/A


probiotic%>%
  
  group_by(group) %>%
  
  summarise(n=n()) #if individual had treatment or placebo


glimpse(probiotic)
tibble(probiotic)

#combining each individuals before and after treatment measurments
# Pivot the data to wide format
probiotic_wide <- probiotic %>% 
  pivot_wider(
    names_from =time,
    values_from =r.gnavus_abund,
    id_cols = c(subject, gender, group)
  )

# Display the pivoted data
head(probiotic_wide)
nrow(probiotic_wide)

probiotic_wide <- rename(probiotic_wide, "pretreatment"="1") #renaming columns
probiotic_wide <- rename(probiotic_wide, "posttreatment"="2") #renaming columns

# Add a column for the difference between posttreatment and prtreatment
probiotic_wide <- probiotic_wide %>%
  mutate(difference = posttreatment - pretreatment)
#____________________----
#SAVE NEW DATA SET----

# Save the pivoted data to a new CSV file
write_csv(probiotic_wide, "data/probiotic_wide.csv")


#____________________----
#CLEAN DATA----


missing_data <- probiotic_wide %>% 
  summarise_all(~ sum(is.na(.)))

print(missing_data) #checking for missing data

probiotic_wide %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all the TRUE statements


#removing n/a and unknown inputs from data
data_figure <- probiotic_wide[probiotic_wide$group %in% c("LGG","Placebo"),]
data_figure <- data_figure[data_figure$gender %in% c("M","F"),]

head(data_figure)
glimpse(data_figure)
tibble(data_figure)

#_________________________----
#EXPLORATION----

probiotic_wide %>%
  group_by(gender) %>%
  summarise(n=n())

probiotic_wide %>%
  group_by(group) %>%
  summarise(n=n())

skimr:: skim (probiotic_wide)

# Making a histogram
# Call the ggplot function and direct it to your data and define your x axis
pretreatment_abundance_histogram <- ggplot(data = probiotic_wide,aes(x = pretreatment)) + 
  geom_histogram(bins = 6) # Tell ggplot that you want it to build a histogram with 6 equal sized bins
print(pretreatment_abundance_histogram) # Print your new figure

# Call the ggplot function and direct it to your data and define your x axis
posttreatment_abundance_histogram <- ggplot(data = probiotic_wide,aes(x = posttreatment)) + 
  geom_histogram(bins = 6) # Tell ggplot that you want it to build a histogram with 6 equal sized bins
print(posttreatment_abundance_histogram) # Print your new figure


#___________________________________----
#GROUP DATA----

pretreatment_abundance_data <- probiotic_wide[c("pretreatment","gender")]
summary(pretreatment_abundance_data) 
head(pretreatment_abundance_data)

posttreatment_abundance_data <- probiotic_wide[c("posttreatment","gender")]
summary(posttreatment_abundance_data) 

difference_by_treatment_data <- probiotic_wide[c("difference", "gender", "group")]
summary(difference_by_treatment_data)
head(difference_by_treatment_data)


# Pipe your data set to the filter function, this will pull out the Placebo group and store then in a new object.
placebo_group <- probiotic_wide %>%
  filter(group == "Placebo")

# Pipe your data set to the filter function, this will pull out the LGG group and store then in a new object.
lgg_group <- probiotic_wide %>%
  filter(group == "LGG")




#__________________________________----
#MAKING PLOTS----

# Instruct R to treat gender variable as a factor 
probiotic_wide$gender <- as.factor(probiotic_wide$gender) # Note this will edit your data frame

# Making a box plot 
# Call the ggplot function and direct it to your data and define your x axis and y axis
pretreatment_abundance_differences_by_gender_boxplot <- ggplot(data = probiotic_wide,aes(x = gender, y = pretreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance Pre-treatment", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(pretreatment_abundance_differences_by_gender_boxplot) # Print your new figure

ggsave("figures/pretreatment_abundance_differences_by_gender_boxplot.pdf", # Give R a path to save to and a file name
       plot = pretreatment_abundance_differences_by_gender_boxplot, width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type


# Call the ggplot function and direct it to your data and define your x axis and y axis
posttreatment_abundance_differences_by_gender_boxplot <- ggplot(data = probiotic_wide,aes(x = gender, y = posttreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance Post-treatment", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(posttreatment_abundance_differences_by_gender_boxplot) # Print your new figure

ggsave("figures/posttreatment_abundance_differences_by_gender_boxplot.pdf", # Give R a path to save to and a file name
       plot = posttreatment_abundance_differences_by_gender_boxplot, width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type


# Making a scatter plot and saving it in an object
# Call the ggplot function and direct it to your data and define your x axis and y axis
gender_vs_pretreatment_abundance_point <- ggplot(data = probiotic_wide,aes(x = gender , y = pretreatment )) + 
  geom_point(aes (colour = group), size= 1.5, shape= 16) + # geom_point is ggplots scatter plot
  geom_smooth(method="lm", colour = "navy", fill = "lightblue1", linewidth = 0.5, linetype = "solid") +
  labs(x = "Gender", y = "Initial Parasite Burden", fill = "group") + # x and y axis labels 
  scale_color_manual(name = "Treatment", labels = c("Placebo", "LGG"), values = c("chocolate1", "cadetblue3")) +
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 10), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12),) # Changes the size of the legend title  
print(gender_vs_pretreatment_abundance_point)

ggsave("figures/gender_vs_pretreatment_abundance_point.pdf", # Give R a path to save to and a file name
       plot = gender_vs_pretreatment_abundance_point,  width = 10 , height = 10 , units = "cm", # Tell R what to save, and what size to save the pdf
       device = "pdf") # file type pdf

# Call the ggplot function and direct it to your data and define your x axis and y axis
placebo_group_abundance_pretreatment <- ggplot(data = placebo_group,aes(x = gender, y = pretreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  ggtitle("Placebo Group Pre-Treatment") + # Add title
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(placebo_group_abundance_pretreatment ) # Print your new figure

ggsave("figures/placebo_group_abundance_pretreatment.pdf", # Give R a path to save to and a file name
       plot = placebo_group_abundance_pretreatment , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type

# Call the ggplot function and direct it to your data and define your x axis and y axis
placebo_group_abundance_posttreatment <- ggplot(data = placebo_group,aes(x = gender, y = posttreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  ggtitle("Placebo Group Post-Treatment") + # Add title
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(placebo_group_abundance_posttreatment ) # Print your new figure

ggsave("figures/placebo_group_abundance_posttreatment.pdf", # Give R a path to save to and a file name
       plot = placebo_group_abundance_posttreatment , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type

# Call the ggplot function and direct it to your data and define your x axis and y axis
lgg_group_abundance_pretreatment <- ggplot(data = lgg_group,aes(x = gender, y = pretreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  ggtitle("LGG Group Pre-Treatment") + # Add title
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(lgg_group_abundance_pretreatment ) # Print your new figure

ggsave("figures/lgg_group_abundance_pretreatment.pdf", # Give R a path to save to and a file name
       plot = lgg_group_abundance_pretreatment , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type

# Call the ggplot function and direct it to your data and define your x axis and y axis
lgg_group_abundance_posttreatment <- ggplot(data = lgg_group,aes(x = gender, y = posttreatment)) + 
  geom_boxplot(aes(fill = gender), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Gender", y = "R.gnavus Abundance", fill = "Gender") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("F","M")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Gender"), labels = c("F", "M"), values = c("chocolate1", "cadetblue3")) + # Rename your labels
  ggtitle("LGG Group Post-Treatment") + # Add title
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(lgg_group_abundance_posttreatment ) # Print your new figure

ggsave("figures/lgg_group_abundance_posttreatment.pdf", # Give R a path to save to and a file name
       plot = lgg_group_abundance_posttreatment , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type

# Call the ggplot function and direct it to your data and define your x axis and y axis
difference_by_treatment_plot <- ggplot(data = difference_by_treatment_data,aes(x = group, y = difference)) + 
  geom_boxplot(aes(fill = group), width = 2,) + # Tell ggplot that you want it to build a box plot coloured by gender
  labs(x = "Group", y = "Change in R.gnavus Abundance", fill = "Group") + # Adjust your x and y axis labels 
  scale_x_discrete(labels = c("Placebo","LGG")) + # Rename the categories on the x axis
  scale_fill_manual(name= ("Treatment"), labels = c("Placebo", "LGG"), values = c("darkorange" , "darkorchid")) + # Rename your labels
  ggtitle("Change in R.gnavus abundance") + # Add title
  theme_bw() +
  theme(panel.border = element_rect(color="black"), # Specifies that the plot boarder is coloured black
        panel.grid.minor = element_blank(), # Removes minor grid lines 
        panel.grid.major = element_blank(), # Removes major grid lines 
        axis.text = element_text(size = 8), # Changes the size of text on both axis 
        axis.title = element_text(size = 10), # Changes size of your axis labels 
        legend.text = element_text(size = 10), # Changes the size of text within your legend
        legend.title = element_text(size = 12)) # Changes the size of the legend title
print(difference_by_treatment_plot ) # Print your new figure

ggsave("figures/difference_by_treatment_plot.pdf", # Give R a path to save to and a file name
       plot = difference_by_treatment_plot , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type

difference_by_treatment_plot2 <- difference_by_treatment_data %>% 
  ggplot(aes(x=group, 
             y=difference,
             colour=group))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.5)+
  theme_bw()

ggsave("figures/difference_by_treatment_plot2.pdf", # Give R a path to save to and a file name
       plot = difference_by_treatment_plot2 , width = 15 , height = 10 , units = "cm", # what size of pdf
       device = "pdf") # file type
#removed 2 values outside scale range 
#_________________________________________________________________----
#MAKING PATCHWORK PLOTS----


# Patchwork together plots
multiplot <- (placebo_group_abundance_pretreatment | placebo_group_abundance_posttreatment) /
  (lgg_group_abundance_pretreatment | lgg_group_abundance_posttreatment) + 
  patchwork::plot_layout(guides ='collect')+ 
  plot_annotation(
    title = "Abundance of R.gnavus in stool samples")

print(multiplot)


#save the multi-panel figure
ggsave("figures/multiplot.pdf", # Give R a path to save to and a file name
       plot = multiplot, width= 20, height= 20, units= "cm", # Tell R what to save - in this case your object
       device = "pdf") # file type


#___________________________________________________________----
#MODEL----


model1 <- lm(pretreatment ~ gender, data = pretreatment_abundance_data)
summary(model1)

plot(model1)


#not significant- gender accounted for 0.0036 of varience

model2 <- lm(difference ~ group + gender + difference:gender,
             data = difference_by_treatment_data)
summary(model2)#looking to analyse difference in R.gnavus abundance as a function of treatment group
#also looking at if there is enough evidence to support a difference in R.gnavus abundance change depending on gender

# issues- the small sample size relative to number of predictors causing overfitting of data

model3 <- lm(difference ~ group + gender ,
             data = difference_by_treatment_data)
summary(model3)
plot(model3)

#issues- non liniarity
#issues- outlier (cooks distance >1)

performance::check_model(model3, check="outliers")

model4 <- lm(difference ~ group + gender,
             data = difference_by_treatment_data [-21,])
summary(model4)
plot(model4)
#explains ~27% variance

#issues- non linearity

performance::check_model(model4, check="homogeneity")
performance::check_model(model4, check=c("normality","qq"))
performance::check_model(model4, check="outliers")

lmtest::bptest(model4) #test of normality

shapiro.test(residuals(model4)) #residual test

qqPlot(model4)
#confidence interval test

#______________________________________----
#IMPROVING MODEL?----

# Adding a constant to all values to ensure they are positive
constant <- abs(min(difference_by_treatment_data$difference)) + 1
difference_by_treatment_data$difference_adjusted <- difference_by_treatment_data$difference + constant

# Apply log transformation
difference_by_treatment_data$log_difference <- log(difference_by_treatment_data$difference_adjusted)

# Fit a linear model with the log-transformed response variable
model5_log <- lm(log_difference ~ group + gender, data = difference_by_treatment_data[-21,])

# Check the Box-Cox transformation for the log-transformed model
boxcox_results_log <- car::boxCox(model5_log)
print(boxcox_results_log)
summary(model5_log)
#explains ~8% varience

b <- car::boxCox(model5_log)
lambda <- b$x[which.max(b$y)]
print(lambda)


model6 <- lm((difference^lambda-1/lambda) ~ group + gender, data = difference_by_treatment_data[-21,])
summary(model6)
#explains ~13% varience




