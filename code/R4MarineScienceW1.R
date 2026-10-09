# Memory allocation and environment hygiene 
## Establishing a baseline workflow at the top of every fresh script file

# Housekeeping
objects() # inventory every active object currently residing in session RAM
rm(list = ls()) # purge global environment
objects() #confirm that global session memory now completely vacant
unlink("~/.RData") # remove any .RData file that may have been created in the home directory

# Ingesting data into R
## Practice reading varied inputs
# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)
# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/workshop1/reef_cover_log.csv"))
# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/workshop1/acoustic_telemetry_stream.txt"))
# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/workshop1/fish_catch_data.xlsx"), sheet = "Commercial_2026")
# Load in mangrove_data
mangrove_data <- read_csv(file = here::here("data/workshop1/mangrove_survey_raw.csv"))
# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5, # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank")) # COnvert known text alts to true NA

# Data frame architectures: Tibbles versus legacy tables 
## When you import tabular assets the resulting object is stored as a structure called a tibble
benthic_cover_df <- as.data.frame(benthic_cover) # force a modern tibble to degrade into a legacy base R data frame structure
print(benthic_cover_df) # print the old-style dataframe structure to view
print(benthic_cover) # compare with tibble alternative

# Wrangling out ecological signals using Palmer Penguins
## Load the package data into active memory
library(palmerpenguins) 
data("penguins")
## Examine the structure of the dataset - always do this when loading a new dataset!
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version
summary(penguins) # generate an exploratory summary matrix

# Foundational grammar: Slicing, filtering, sorting and transforming
## Isolating attributes with select(), targeting variables vertically, isolating or dropping columns based on varaible names
morphology_metrics <- select(penguins, species, bill_length_mm, 
                             bill_depth_mm, body_mass_g) # vertically slice specific morphometric variables by explicit name
glimpse(morphology_metrics)
spatial_block <- select(penguins, species:island) # retain a continuous block of attributes using the colon operator
clean_scientific_fields <- select(penguins, -year) # discard logistics tracking attributes while preserving everything else using the minus sign

## Sifting rows with filter(), targeting variables horizontally retaining entries that evaluate to TRUE and dropping those FALSE or NA
adelie_cohort <- filter(penguins, species == "Adelie") # isolate observations belonging to a single categorical target group
## Sift out individuals using continuous numerical boundary thresholds
heavy_penguins <- filter(penguins, body_mass_g > 4500) # preserves only large penguins whose mass exceeds 4500 grams
## Combine multiple conditional parameters across separate attributes
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe") # preserves records matching Gentoo penguins sampled explicity on Biscoe Island
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

## Ordering sequences with arrange()
## arrange() alters the sorting configuration of rows within your data frame without changing individual cell values, essential for size hierarchies or chronologicallt ordering
lightest_first <- arrange(penguins, body_mass_g) # sort by ascending body mass
heaviest_first <- arrange(penguins, desc(body_mass_g)) # sort by descending body mass
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm)) # nested sorting criteria: group by species, then by descending bill length

## Introducing the Pipe (|>)
## cleaner way to chain operations: the pipe, passing your data through a series of transformations in a single, linear flow
## Example, instead of:
penguins_subset <- mutate(penguins, bill_ratio = bill_length_mm / bill_depth_mm)
penguins_final <- filter(penguins_subset, species == "Adelie")
## Simply write:
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")
penguins_final <- penguins |>
  drop_na(species, island, body_mass_g)

## Computing new attributes with mutate()
## use it to modify existing attributes or append entirely new vectors to the data frame
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  ) # calculate a new morphological ratio in our environment
glimpse(penguin_ratios) # view your newly engineered variables 

# Data aggregation and ecological summarisation
## Extract meaningful biological conclusions by compressing individual observations into explicity summary metrics using group_by() and summarise()
grouped_penguins <- group_by(penguins, species) # grouping our active memory penguins by species
print(grouped_penguins) # table looks identical, but metadata notes 'Groups: species [3]'
species_mass_summary <- summarise(grouped_penguins, mean_mass_g = mean(body_mass_g)) # collapsing the buckets into explicit summary metrics
print(species_mass_summary)
## Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(biological_signal)

# Integrating data grammar with visual diagnostics in qmd
## Wrangling and Plotting in Parallel
penguins_final |>
  group_by(species, island) |>
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),
    .groups = "drop"
  )

penguins_final |>
  ggplot(aes(x = species,
             y = body_mass_g,
             fill = island)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Dream" = "turquoise3", "Torgersen" = "olivedrab", "Biscoe" = "sienna2")) +
  labs(
    title = "Penguin Body Mass Distribution by Species and Island",
    x = "Species",
    y = "Body Mass (g)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

## Piping directly to visualisation
## You can extend your pipeline directly into ggplot2
## Example: Visualising Mean Trends with Uncertainty, you can pipe your grouped data straight into a plot and use geom_errorbar() to represent the spread of your data
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean_mass - sd_mass,
        ymax = mean_mass + sd_mass),
    width = 0.2
  ) +
  scale_colour_manual(
    values = c(
      "Biscoe" = "#319795",
      "Dream" = "#D98B78",
      "Torgersen" = "#9982BA"
    )
  ) +
  labs(
    x = "Penguin species",
    y = "Mean body mass (g)",
    colour = "Island"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )

mass_compare_plot # view plot

# try creating a new plot experiment with swapping sd_mass (standard deviation) for standard error you will need to divide your sd by the square root of n (sd/sqrt(n))
mass_compare_plot2 <- penguins |> 
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  mutate(
    se_mass = sd_mass / sqrt(n)
  )

mass_compare_plot2

mass_compare_plotSE <- mass_compare_plot2 |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_mass - se_mass,
      ymax = mean_mass + se_mass
    ),
    width = 0.2,
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Biscoe" = "#319795",
      "Dream" = "#D98B78",
      "Torgersen" = "#9982BA"
    )
  ) +
  labs(
    x = "Penguin species",
    y = "Mean body mass (g)",
    colour = "Island"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )

mass_compare_plotSE

# Exporting our collapsed summary table as a universal flat text file
# write_csv(biological_signal, "outputs/tables/penguin_species_mass_summary.csv")

# Saving as a native R binary file
# saveRDS(biological_signal, "Rdata/penguin_species_mass_summary.rds")

# Save as a figure
# ggsave("outputs/figures/mass_compare_plot.png", 
#       plot = mass_compare_plot, 
#       width = 120, height = 120, 
#       units = "mm", dpi = 300)

