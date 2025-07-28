# SPS data analysis task.
# Author: Levin Wilkinson
# Date: 24/07/2025
# Purpose: To wrangle data and produce charts for the SPS Senior Analyst -
# Justice Sector data task.


# Part 0: Housekeeping ---------------------------------------------------------

# Clear environment
rm(list=ls())

# Import libraries
library(tidyverse)
library(hablar)
library(openxlsx)
library(ggplot2)



# Part 1: Import data ----------------------------------------------------------

# Scottish Prison Population Statistics 2023/24 Supplementary Tables:
population_filepath <- paste0(getwd(), "/data/Prison_Population_Stats_2023.xlsx")

# I will need data from sheets M2 and B2 to create visualisations
prison_M2 <- read.xlsx(population_filepath, sheet = "M2", startRow = 3, sep.names = "_")
prison_B2 <- read.xlsx(population_filepath, sheet = "B2", startRow = 3, sep.names = "_")


# NRS Drug-Related Deaths 2023 data:
deaths_filepath <- paste0(getwd(), "/data/NRS_drug-related-deaths-23-data.xlsx")

# I will need data from Table 4 only.
NRS_Table_4 <- read.xlsx(deaths_filepath, sheet = "Table_4", startRow = 5, sep.names = "_")


rm(population_filepath, deaths_filepath)


# Part 2: Specify global graph features ----------------------------------------

size_plot_title <- 16
size_legend_title <- 14
size_x_title <- 14
size_y_title <- 14

size_legend_text <- 11
size_x_labels <- 11
size_y_labels <- 12



# Part 3: Graphs ---------------------------------------------------------------

## Proportion of ADP with each index offence group per year --------------------

# Look at the Index Offence Groups in the data (M2):
unique(prison_M2$Index_Offence_Group)
# 12 groups, including "Missing".

# This is quite a large number of groups to visualise, so I will group these
# further by collecting categories 6, 7, 8, 77, 88 and 98 together into "Other".
# I will also rename groups 1, 4 and 9 to look better in the plot legend.

# Select the correct rows and reformat the data for plotting
graph1_data <- prison_M2 |> 
  filter(Index_Offence == "All",
         Age == "All",
         Gender == "All",
         Establishment == "All",
         Ethnicity == "All",
         Measurement == "prop. of all (av%)") |> 
  pivot_longer(cols = `2009-10`:`2023-24`, names_to = "FY") |> 
  group_by(Index_Offence_Group, FY) |> 
  summarise(total_percentage = sum(as.numeric(value))) |> 
  mutate(Index_Offence_Group_2 = case_when(Index_Offence_Group == "1: Non-sexual crimes of violence" ~ "1: Violence",
                                           Index_Offence_Group == "4: Damage and reckless behaviour" ~ "4: Damage/reckless behaviour",
                                           grepl("6", Index_Offence_Group) | grepl("7", Index_Offence_Group) | grepl("8", Index_Offence_Group) ~ "Other offences",
                                           Index_Offence_Group == "99: (Missing)" ~ "Data missing",
                                           TRUE ~ Index_Offence_Group))

# Check that the percentages add to 100% as expected:
graph1_data |> 
  group_by(FY) |> 
  summarise(sum(as.numeric(total_percentage)))
# Some are slightly lower - this is just due to rounding in the provided data.
# If I had more time, I would take the time to calculate the percentages myself
# here to avoid this discrepancy.


# Reorder bars for percentage plot
graph1_data$Index_Offence_Group_2 <- factor(graph1_data$Index_Offence_Group_2, levels = c('Data missing', 'Other offences', '5: Crimes against society', '4: Damage/reckless behaviour', '3: Crimes of dishonesty', '2: Sexual crimes', '1: Violence'))

# Manually define colours for plot (using colour-blind friendly palette)
graph1_colours = c('1: Violence' = "#2E0A73",
                   '2: Sexual crimes' = "#A121BB",
                   '3: Crimes of dishonesty' = "#E29916",
                   '4: Damage/reckless behaviour' = "#EFC957",
                   '5: Crimes against society' = "#0BBD84",
                   'Other offences' = "#0082F7",
                   'Data missing' = "#A5A5A5")


# Plot graph
ggplot(graph1_data, aes(fill = Index_Offence_Group_2, y = total_percentage, x = FY)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Proportion of total average daily population (ADP) by index\noffence group, from 2009-10 to 2023-24") +
  theme_classic() +
  theme(plot.title = element_text(size = size_plot_title),
        
        axis.text.x = element_text(size = size_x_labels, colour = 'black'),
        axis.text.y = element_text(size = size_y_labels, colour = 'black'),
        axis.title.x = element_text(size = size_x_title, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = size_y_title, face = "bold"),
        
        legend.title = element_text(size = size_legend_title),
        legend.position="right",
        legend.key.size = unit(x = 0.3, "cm"),
        legend.text = element_text(size = size_legend_text),
        legend.direction = "vertical") +
  scale_fill_discrete(name = "Index Offence Group",
                      type = graph1_colours,
                      guide = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = c("2009-10", "", "2011-12", "", "2013-14", "", "2015-16", "", "2017-18", "", "2019-20", "", "2021-22", "", "2023-24")) +
  scale_y_continuous(name ="Share\nof ADP",
                     breaks = seq(0, 100, by = 10),
                     labels = function(x) paste0(x, "%"),
                     expand=c(0,0)) +
  labs(x = "Financial Year") +
  theme(axis.ticks.x = element_line(color = c(rep(NA, 1 - 1), rep("black", 1))))


# Save a png of the chart
ggsave(filename = paste0(getwd(), "/output/graph1.png"), width = 10, height = 7)




## Comparing average age of ADP with drug death cohort -------------------------

# Wrangle NRS drug-related deaths data
death_data <- NRS_Table_4 |> 
  filter(Sex == "Persons") |> 
  select(Year, Average_age) |> 
  rename(DrugRelatedDeaths = Average_age)

# Wrangle prison population data
prison_age_data <- prison_B2 |> 
  filter(Label == "Total",
         Measurement == "average age") |> 
  mutate(across(contains("20"), as.numeric)) |> 
  pivot_longer(cols = `2009-10`:`2023-24`, names_to = "Year") |> 
  select(Year, value) |> 
  rename(Prison = value) |> 
  mutate(Year = as.numeric(str_sub(Year, 1, 4)))
# Note: we have cropped the FY so that we can join onto the DRDs data. After
# joining, I will add 0.25 years to each prison data point to represent FY.


# Join yearly data together
graph2_data <- left_join(x = prison_age_data,
                         y = death_data,
                         by = "Year") |> 
  pivot_longer(cols = c(DrugRelatedDeaths, Prison), names_to = "Source", values_to = "Age") |> 
  mutate(Source = case_when(Source == "DrugRelatedDeaths" ~ "Drug-related\ndeaths",
                            Source == "Prison" ~ "Prison ADP",
                            TRUE ~ Source),
         Year = case_when(Source == "Prison ADP" ~ Year + 0.5, # shift FY data
                          TRUE ~ Year))


# Manually define plot colours
graph2_colours = c('Drug-related\ndeaths' = "#2E0A73",
                   'Prison ADP' = "#0BBD84")


# Plot line graph
ggplot(graph2_data, aes(color = Source, x = Year, y = Age, fontface = "bold")) +
  geom_point(size = 4) +
  geom_line(linewidth = 2) +
  ggtitle("Average age of daily prison population (2009-10 - 2023-24)\nand drug-related death cohort (2009 - 2023)") + 
  theme_classic() +
  theme(plot.title = element_text(size = size_plot_title),
        panel.grid.major.y = element_line(colour="grey", linewidth = 0.3),
        panel.grid.major.x = element_line(colour="grey", linewidth = 0.2),
        
        axis.text.x = element_text(size = size_x_labels + 1, colour = 'black'),
        axis.text.y = element_text(size = size_y_labels, colour = 'black'),
        axis.title.x = element_text(size = size_x_title, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = size_y_title, face = "bold"),
        
        #legend.position = "top",
        legend.key.size = unit(x = 0.4, "cm"),
        legend.title = element_text(size = size_legend_title),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(name = "Cohort",
                       type = graph2_colours) +
  scale_x_continuous(breaks = seq(2009, 2023, by = 1),
                     limits = c(2009, 2023.5),
                     labels = c("2009", "", "2011", "", "2013", "", "2015", "", "2017", "", "2019", "", "2021", "", "2023")) +
  scale_y_continuous(name ="Average\nage of\ncohort",
                     breaks = seq(25, 50, by = 5),
                     limits = c(27, 50), 
                     expand=c(0,0)) +
  labs(x = "Year")


# Save a png of the chart
ggsave(filename = paste0(getwd(), "/output/graph2.png"), width = 10, height = 7)



## Aging cohort: add linear fit ------------------------------------------------

# Generate linear model fits
linear_fits <- graph2_data |> 
  group_by(Source) |> 
  do(broom::tidy(lm(Age ~ Year, data = .))) |> 
  filter(term == "Year") |>  
  rename(Gradient = estimate) |> 
  select(-term)

# Define the graph labels and their positions
label_positions <- graph2_data |> 
  select(Source, Year, Age) |> 
  group_by(Source) |> 
  filter(Year == max(Year)) |> 
  left_join(linear_fits, by = "Source") |> 
  
  # Manually set labels and their locations
  mutate(label = paste0("Gradient = ", round(Gradient, 2), " \u00B1 ", round(std.error, 3)),
         Year = 2020,
         Age = Age + 1.25)

# We will use graph2_colours for this graph.

# Plot linear trend graph
ggplot(graph2_data, aes(color = Source, x = Year, y = Age, fontface = "bold")) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.5) +
  ggtitle("Average age of daily prison population (2009-10 - 2023-24)\nand drug-related death cohort (2009 - 2023)") + 
  geom_text(data = label_positions,
            size = 4.2,
            aes(label = label),
            show.legend = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(size = size_plot_title),
        panel.grid.major.y = element_line(colour="grey", size=0.3),
        panel.grid.major.x = element_line(colour="grey", size=0.2),
        
        axis.text.x = element_text(size = size_x_labels + 1, colour = 'black'),
        axis.text.y = element_text(size = size_y_labels, colour = 'black'),
        axis.title.x = element_text(size = size_x_title, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = size_y_title, face = "bold"),
        legend.title = element_text(size = size_legend_title),
        legend.key.size = unit(x = 0.4, "cm"),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(name = "Cohort",
                       type = graph2_colours) +
  scale_x_continuous(breaks = seq(2009, 2023, by = 1),
                     limits = c(2009, 2023.5),
                     labels = c("2009", "", "2011", "", "2013", "", "2015", "", "2017", "", "2019", "", "2021", "", "2023")) +
  scale_y_continuous(name ="Average\nage of\ncohort",
                     breaks = seq(25, 50, by = 5),
                     limits = c(27, 50), 
                     expand=c(0,0)) +
  labs(x = "Year")


# Save a png of the chart
ggsave(filename = paste0(getwd(), "/output/graph3.png"), width = 10, height = 7)
