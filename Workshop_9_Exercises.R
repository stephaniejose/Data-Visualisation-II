#(7) EXERCISES

install.packages('tidyverse')
install.packages("palmerpenguins")

#[1] Labels:Produce the following plot. Plotted here are only the penguins resident on the island Biscoe



biscoe_penguins <- penguins %>%
  filter(island == 'Biscoe') &>&
  filter(!species == 'Chinstrap') %>%
  ggplot(mapping = aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(mapping = aes(colour = species))