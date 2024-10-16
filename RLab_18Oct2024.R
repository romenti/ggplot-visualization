##### Lab 18 October 2024 ####

#### Clean the environment ####

rm(list=ls())

# Remember to create a working directory to save everything!!!

#### Install Packages ####

#install.packages('ggthemes')
#install.packages('RColorBrewer')
library('RColorBrewer') # various qualitative color palettes 
library('ggthemes') # various themes in ggplot
library("tidyverse") # ggplot2


#### Upload data ####

# data on violence

gpi_data <- read.table('Data/gpi_data.txt',header=T)

# data on life expectancy from UN

life_exp_data <- read.table('Data/life_exp_data.txt',header=T)


# combine the two data sets 

data <- inner_join(gpi_data,life_exp_data,by=c('iso3','Year'))

#### Question 1 ####

# violence by region during period 2008-2022

data_violence_region <- data |>
  group_by(Year,area) |>
  summarize(GPI=weighted.mean(x=GPI,w=pop,na.rm = FALSE)) 

# plot the trend

plot1 <- ggplot(data=data_violence_region, # data input
               mapping=aes(x=Year,y=GPI,color=area))+ # relationships
  geom_line()+ # add a line for each country
  geom_point()+ # add points 
  theme_clean()+ # background
  scale_color_brewer(name = "Region", palette = "Set1")+ # color palette 
  xlab('Year')+ #label for title of x-axis
  ylab('Level of Violence') + # label for title of y-axis
  ggtitle('Region-specific Violence over Time')  # title for the plot


#### Question 2 ####

# Life expectancy by region during period 2008-2022

data_life_exp_region <- data |>
  group_by(Year,area) |>
  summarize(e0=weighted.mean(x=e0,w=pop,na.rm = FALSE))

# plot the trend

plot2 <- ggplot(data=data_life_exp_region, # data input
               mapping=aes(x=Year,y=e0,color=area))+ # relationships
  geom_line()+ # add a line for each country
  geom_point()+ # add points 
  annotate("text",x=2020,y=70,label="COVID-19")+ # add text
  theme_clean()+ # specify a theme
  xlab('Year')+ #label for title of x-axis
  ylab('Life Expectancy') + # label for title of y-axis
  scale_color_brewer(name = "Region", palette = "Set1")+ # change color palette
  ggtitle('Region-specific Life Expectancy over Time')  # title


#### Question 3 ####

# data for scatter plot

data_scatter <- data |>
  filter(Year %in% c(2008,2022))

# relationship between violence and life expectancy

plot3 <- ggplot(data=data_scatter, # data input
               mapping=aes(x=GPI,y=e0))+ # relationships
  geom_point()+  # scatter plot
  xlab('Level of Violence')+ # x-axis label
  ylab('Life Expectancy')+ # y-axis label
  theme_economist()+ # pick a background
  geom_smooth(method = "lm", se = FALSE,color='red')+ # regression line
  facet_wrap(~Year) + # seperate (sub)plot by year
  theme(axis.text.y = element_text(size=15,face="bold"), # font of y-axis text
        axis.title.y = element_text(size=15,face="bold"), # font of y-axis title
        axis.text.x = element_text(size=15,face="bold"),  
        axis.title.x = element_text(size=15,face="bold")) # font of x-axis title


#### Question 4 ####

# top 20 most violent countries in 2022

data_most_violence <- data |>
  filter(Year==2022) |>
  slice_max(GPI,n=20) |> 
  mutate(label='Most Violent')

# top 20 most peaceful countries in 2022

data_most_peaceful <- data |>
  filter(Year==2022) |>
  slice_min(GPI,n=20) |>
  mutate(label='Most Peaceful')

# data for the plot

data_plot <- rbind(data_most_violence,data_most_peaceful)

plot4 <- ggplot(data=data_plot, # data
             mapping=aes(x=label,y=e0,fill=label))+ # relationships
  geom_boxplot()+ # boxplot
  coord_cartesian(ylim=c(30,90))+ # fix y-axis limits
  scale_y_continuous(breaks=seq(30,90,10),
                     labels=seq(30,90,10))+ # fix y-axis labels
  theme_tufte()+ # pick background
  scale_fill_discrete(name='')+ # drop name from legend
  xlab('')+ # no name for x-axis title
  ylab('Life Expectancy')+ # y-axis title
  theme(axis.text.y = element_text(size=15,face="bold"), # font of y-axis text
        legend.text = element_text(size=15,face="bold"), # legend text details
        axis.title.y = element_text(size=15,face="bold"), # font of y-axis title
        axis.text.x = element_text(size=15,face="bold"),  # text of x-axis title
        axis.title.x = element_text(size=15,face="bold")) # font of x-axis title

#### Question 5 ####

# different way to display the distribution

plot5 <- ggplot(data=data_plot,  # data input
                aes(x=e0,color=label,fill=label))+ # relationships
  geom_density(alpha = 0.2, na.rm = TRUE) + # density
  theme_stata()+ # background
  scale_fill_discrete(name='')+ # no title to legend
  scale_color_discrete(name='')+ # no title to legend
  coord_cartesian(xlim=c(30,90))+ # fix x-axis limits
  scale_x_continuous(breaks=seq(30,90,10),
                     labels=seq(30,90,10))+ # control x-axis labels
  ylab('Density')+ # y-axis title
  xlab('Life Expectancy')+ # x-axis title
  theme(axis.text.y = element_text(size=15,face="bold"), # font of y-axis text
        legend.position = "bottom", # legend position
        axis.title.y = element_text(size=15,face="bold"), # font of y-axis title
        axis.text.x = element_text(size=15,face="bold"),  # font of x-axis text
        axis.title.x = element_text(size=15,face="bold")) # font of x-axis title


##### Question 6 ####

# scatter plot for 2022 with additional features

plot6 <- ggplot(data=data_scatter, # data input
                mapping=aes(x=GPI,y=e0))+ # relationships
  geom_point(aes(shape=area,color=area,size=pop))+ # scatter plot
  xlab('Level of Violence')+ # label x-axis
  ylab('Life Expectancy')+ # label y-axis
  theme_minimal()+ # plot them
  geom_smooth(method = "lm", se = FALSE,color='red')+ # regression line
  labs(size = 'Population Size')+ # legend label
  facet_wrap(~Year) + # seperate by year
  theme(axis.text.y = element_text(size=12,face="bold"), # font of y-axis text
        axis.title.y = element_text(size=15,face="bold"), # font of y-axis title
        legend.title = element_text(size=15,face="bold"), # font of legend title
        legend.text = element_text(size=15,face="bold"),  # font of legend text
        axis.text.x = element_text(size=12,face="bold"),  
        axis.title.x = element_text(size=15,face="bold")) # font of x-axis title


#### Question 7 ####

# Top 5 countries with the highest increase in Violence

data_high <- data |>
  filter(Year %in% c(2008,2022)) |>
  select(Year,Country,area,GPI) |>
  pivot_wider(names_from='Year',values_from='GPI') |>
  mutate(var=(`2022`-`2008`)/`2008`) |>
  group_by(area)|>
  slice_max(var,n=5) |>
  mutate(label='Highest Violence Increase')

# Top 5 countries with the highest decrease in Violence
data_low <- data |>
  filter(Year %in% c(2008,2022)) |>
  select(Year,Country,area,GPI) |>
  pivot_wider(names_from='Year',values_from='GPI') |>
  mutate(var=(`2022`-`2008`)/`2008`) |>
  group_by(area)|>
  slice_min(var,n=5) |>
  mutate(label='Highest Violence Decrease')


# Create a bar chart

plot7 <- rbind(data_high,data_low) |>
  filter(area=='Europe, Central Asia & North America') |>
  mutate(Country=factor(Country,levels=c(data_high$Country,
                                         data_low$Country))) |>
  ggplot(aes(x=Country,y=var,fill=label))+ # data
  geom_bar(stat='identity') + # bar plot
  theme_clean()+ # background
  xlab('Country')+ # x-axis title
  ylab('Variation in Violence')+ # y-axis title
  scale_fill_brewer(name='',palette='Set2')+ # color palette
  scale_y_continuous(labels = scales::percent)+ # y-axis labels
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1, size = 12, face = "bold"), # x-axis text
        legend.position = 'bottom', # legend position 
        axis.text.y = element_text(size = 12, face = "bold"), # y-axis text
        axis.title.x = element_text(size = 15, face = "bold"), # x-axis title
        axis.title.y = element_text(size = 15, face = "bold"), # y-axis title
        legend.text = element_text(size = 15, face = "bold")) # legend text

#### Saving your plots ####

ggsave(filename = "Results/plot7.pdf", # name of the file
       plot = plot7, # name of the plot
       height = 20,  # height of the figure
       width = 40, # width of the figure
       units = "cm", # unit of measurement
       dpi = 400) # resolution of picture 