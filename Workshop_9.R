#19/03/2024
#Workshop 9 - Data Visualisation II

#Labelling data points and then look into manipulating the optics of your graphic using them e(), colour palettes and ways to arrange several plots in one figure

#[2] Annotating data points
#Highlighting specific data points by labelling them individually
#In the example shown there are differently expressed geene with top changers individually labelled

install.packages("palmerpenguins")
install.packages("tidyverse")
library(palmerpenguins)
library(tidyverse)

install.packages('dplyr')
library('dplyr')

#We want to label the five biggest Gentoo penguins with names in a scatterplot
#We can subset our dataframe to the five heaviest Gentoo penguins and add a colum with names
#We can then use that datagrame to label the dots
# Subset penguins dataframe to the the five heaviest penguins
big_penguins <- penguins %>%
  filter(species == "Gentoo",!is.na(body_mass_g)) %>%
  arrange(body_mass_g) %>% 
  tail(n = 5L) #arranges the data in ascending order
#"tail(n = 5l)" will choose the last five rows of the data frame, and this will therefore be the 5 heaviest Gentoo penguins
#'L' tells R to treat the number as a integer (whole number)

# Add a column with names to big_penguins
big_penguins$names <- c("Dwayne", "Hulk", "Giant", "Gwendoline", "Usain")

# Plot all Gentoo penguins and use big_penguins dataframe for labels
penguins %>% filter(species == "Gentoo") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(colour = flipper_length_mm)) + #this adds a scatter plot layer to the plot
#the colour of the points is mapped to "flipper_length_mm" which means the points will be coloured differently based on the flipper length of the penguins and this creates a gradient effect that can help with visualisation between flipper length and colour scale
  geom_text(
    data = big_penguins,
    mapping = aes(label = names),
    nudge_x = -1.5,
    nudge_y = -0.5,
#the "nudge" arguments slightly offset the labels from their corresponding points (by -1.5 units in the x-direction and -0.5 units in the y-direction)
#the text is coloured red to stand out 
    colour = "red"
  ) +
  xlim(3900, 6400) #this sets the limits for x-axis to range from 3900 to 6400 
#this is likely done to focus the plot on a specific range of body mass values where the data is most interesting or relevant

#(1) geom_text() switches to different data, it still uses the position mappings from ggplot() and that is how it knows where to put the labels

#(2) Nudge parameters push the labels down and left a bit 

#(3) The x-axis has been made longer so that the names don't get cut off 

#If your desired labels are already in your dataframe you can filter within the data argument to geom_text
#In this case we want to highlight the home islands of Adelie penguins with flipper lengths over 200 mm
penguins %>% filter(species == "Adelie") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_text(
    data = filter(penguins, species == "Adelie" &
                    flipper_length_mm > 200),
    aes(label = island),
    nudge_y = -0.7
  )

#if we don't use geom_text() it won't add labels to show which values are greater than 200

#[3] FACETS
#faceting produces several small plots separated by categorical variables
#facet_wrap() takes a number of plots and "wraps" them into a panel
#facet_grid()

# Reading in data
modeltab <- read.table("wmr_modelling.txt",sep="\t",header=T)

# Subsetting to the first half or so for readability
modeltab_short <- head(modeltab, n = 506L)

# Plotting deaths in years 2019-2021 faceted by country
modeltab_short %>% drop_na() %>% #removes the data points with NA regardless of what column (column hasn't been specified)
filter(year >2018) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~country, ncol = 5, dir = "v")
# '~' determines the variable by which we want to split our data into separate plots
#we can choose the number of rows and columns with ncol or nrow 
#dir controls the direction of the wrap

##Copy the code above and play around with different options. What does the facet_wrap() argument as.table do? What hapened if you set the argumnet scales to "free"?

?facet_wrap
modeltab <- read.table("wmr_modelling.txt",sep="\t",header=T)

modeltab_short <- head(modeltab, n = 506L)

modeltab_short %>% drop_na() %>% filter(year >2018) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~country, ncol = 5, dir = "v")#, as.table = TRUE)
#'as.table = TRUE' arranges the panels (facets) in the plot like a table, following the convention in R where the first level of the factor appears in the bottom left and subsequent levels fill up the grid in column-major order

#facet_grid() lays out the plots in a 2D grid
#this is often used to separate plots by two categorical variables like so:
penguins %>% drop_na() %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() +
  facet_grid(sex ~ species)

#the formula in facet_grid() determines first the rows, then columns
#you can also use this to control how you want plots laid out that are separated by just one variable:
p_plot <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point()

p_plot + facet_grid(. ~ species)

#here the species are on the x axis
p_plot + facet_grid(species ~ .)


#(4) PATCHWORK
#When we publish papers or produce figures for a presentation we often need to combine several panels into a larger figure and the package called patchwork can do this
install.packages("patchwork")
library(patchwork)

#Let's produce some plots:
p1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() + facet_grid(. ~ species)
p1

p2 <- penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity")
p2

p3 <- penguins %>% drop_na() %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex))
p3

#Patchwork stitches these plots together using an intuitive formula which is:
p1/(p2+p3)

#We can also say that we want only one plot on the left, and the other two stacked on the right:
p2 | (p1/p3)

#We can annotate patchwork using the plot_annotation() function
p1/(p2+p3) + plot_annotation(tag_levels = "a",
                             title = "Plenty of penguin plots")

#Patchwork is useful in aligning plots with the same x- or y-axis 
p_deaths <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = deaths, colour = country)) +
  geom_point() +
  geom_line() +
  xlim(1999,2022)

p_pop <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = population, fill = country)) +
  geom_col(position = "dodge") +
  xlim(1999,2022)

p_deaths/p_pop

#'%in%' is useful for subsetting, in the above example it is used with a vector written on the fly, but you can also use a variable that contains a vector made previously

#(5) COLOURS

#Discrete colour scales
#There are 657 built-in colour names in R and you can get their bames using colours()
#Alternatively you can use hexadecimal colour codes

#Here is an example of how to change discrete colours manually:
s_counts <- penguins %>% ggplot(aes(x = species, fill = species)) +
  geom_bar()

s_counts + scale_fill_manual(values = c("yellow2", "magenta", "darkblue"))

#You can also put together a vector of your favourite colours and use that instead
#However, it is a good idea to use established colour palettes to make sure the colours are well-balanced and colour-blind friendly
#ggplot2 has several built-in colour palettes that are based on colour packages
#For discrete colours we can use scales with palettes from "ColorBrewer"

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

#ColorBrewer has three types of palettes:
#1. suitable for ranked discrete graphs e.g. a series of years
#2. best for unranked categorical data 
#3. used when you have discrete diverging data going from low to high through 0

brew_1 <- s_counts + scale_fill_brewer(palette = "Set1")
brew_2 <- s_counts + scale_fill_brewer(palette = "Dark2", direction = -1)

brew_1 + brew_2

#The first graph isn't great for people with colour-blindness as it will be hard to distinguish between red and green 
#The virdis scales are designed to be colour-blind friendly
viri_1 <- s_counts + scale_fill_viridis_d() #Uses default option viridis
viri_2 <- s_counts + scale_fill_viridis_d(option = "plasma")

viri_1 + viri_2

#CONTINUOUS COLOUR SCALES

#Similar to discreet scales you can manipulatw continuous scales as well
#the function for continuous virdis scales is scale_colour_virdis_c() and for ColorBrewer scale_colour_distiller()

con_plot_1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(size = body_mass_g, colour = body_mass_g))

con_plot_2 <- con_plot_1 + scale_colour_viridis_c(option = "magma")

con_plot_1 + con_plot_2

#NA VALUES
#If you are displaying NA values in your graoh it is recommended to give them a colour
#some palette functions have grey set as default for NA whereas others don't
#in the ase where the colour of NA isn't set to grey it can sometimes be set to the background of the plot. This can have unintended results, for example if you remvoe the default grey plot background 

penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  scale_fill_brewer(palette = "Set2", na.value = "yellow2")

#(6) THEMES
#ggplot2 has a default theme which is theme_grey() and this determines the overall look of the plot
#it sets the plot panel to grey, grid lines and axes to white, determines where the legend goes, etc 
#there is a number of other complete themes available, such as theme_minimal(), theme_classic(), etc

#we can change frim the default to another like so:
con_plot_3 <- con_plot_1 + theme_classic()

con_plot_1 + con_plot_3 + plot_annotation(title = "Default theme on the left, theme_classic() on the right")

#theme() allows us to change each element of the plot

?theme

#elements of a plot are divided into three broad types: lines, text and rectangles
#element_line()
#element_text()
#element_blank()

penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(colour = body_mass_g)) +
  labs(title = "My pretty plot") +
  scale_colour_viridis_c(option = "magma") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(colour = "red", size = 14, hjust = 1),
        axis.title.y = element_blank(),
        axis.line.y = element_line(colour = "cornflowerblue", size = 4),
        axis.text.y = element_text(size = 20, angle = 45),
        panel.background = element_rect(colour = "green", fill = "yellow", size = 10),
        plot.title = element_text(family = "Times", face = "italic",  hjust = 0.5, size = 18))

#theme() is handy for adjusting the position of the legend 
penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity") +
  theme(legend.position = c(0.9,0.85),
        legend.background = element_blank())

#(7) EXERCISES

#[1] Labels:Produce the following plot. Plotted here are only the penguins resident on the island Biscoe
biscoe_penguins <- penguins %>% 
  filter(island == 'Biscoe') %>%
  filter(!species == 'Chinstrap') %>%
  arrange(bill_depth_mm) %>%
  ggplot(mapping = aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(mapping = aes(colour = species)) +
  geom_text(data = filter(penguins, species == 'Gentoo' & bill_length_mm > 54),
            aes(label = sex)) +
  geom_text(data = filter(penguins, species == 'Adelie' & bill_depth_mm > 22),
            aes(label = sex)) +
  xlim(13,25)

biscoe_penguins

#[2] Facets: Produce this plot using the dataset in the file called wmr_cases_deaths_modelling_summaries.txt
death_model <- read.table('wmr_cases_deaths_modelling_summaries.txt',
                          sep = '\t',
                          header = T)

death_model %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col() +
  facet_wrap(~region, ncol = 3, dir = 'h', as.table = TRUE, scales = 'free') 
#chosen variable to get information from is 'region'
#3 columns present
#direction of plots are horizontal i.e. alphabetical order going horizontally
#'scales = FREE' means that each plot can have their own individual axis limits

#[3] Patchwork: Using the datasets in wmr_modelling.txt and wmr_cases_deaths_modelling_summaries.txt produce a publication-style figure. It should contain at least three plots, one with faceting, arranged with patchwork

model_summary <- read.table('wmr_modelling.txt',
                            sep = '\t',
                            header = T)

f1 <- model_summary %>%
  ggplot(mapping = aes(x = year, y = population)) +
  geom_point()
f1

p1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() + facet_grid(. ~ species)
p1

p2 <- penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity")
p2

p3 <- penguins %>% drop_na() %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex))
p3

 