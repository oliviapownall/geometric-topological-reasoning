### Importing libraries
library(readr) 
library(psych)
library(dplyr)

### Reading in dataframe
df <- read_csv("data/DataForPaper.csv")

### PARTICIPANTS

## Assessing age of  participants
describe(df$Age[df$Group == "Adult"])
describe(df$Age[df$Group == "Children"])

### MATERIALS AND PROCEDURE

## Assessing averages of the initial circle attempted
describe(df['TimesInitialCirclesAttempted']) # overall average attempts
describe(df$TimesInitialCirclesAttempted[df$Group == "Adult"])
describe(df$TimesInitialCirclesAttempted[df$Group == "Children"])

### DATA ANALYSIS

## Calculating n and % of participants that translated and scales circles

df %>%
  filter(InferenceType %in% c("IV5", "IV6")) %>% # Filtering for IV5 and IV6- what is done in all the transformation trials
  group_by(Group) %>%
  summarise(
    n_eligible = sum(!is.na(TranslatedOrScaled)), # counting all IV5 and IV6 trials
    n_both     = sum(TranslatedOrScaled == "Both", na.rm = TRUE), # counting all trials where both circles were transformed
    percentage        = round(100 * n_both / n_eligible, 2) # working out percentage
  )

## Calculating n and % of participants that trasnformed both circles or that started with circles the same size

df %>%
  group_by(Group) %>%
  summarise(
    n_eligible = sum(!is.na(CircleChanged)), # counting all valid trials in the dataset (includes valid and invalid)
    n_both     = sum(CircleChanged == "Both", na.rm = TRUE), # counting the number of both
    percentage_both   = round(100 * n_both / n_eligible, 2), # calculating the percentage for both
    n_same     = sum(CircleChanged == "Circles same size", na.rm = TRUE), # counting the number for drawing the circles the same size
    percentage_same   = round(100 * n_same / n_eligible, 2) # calculating the percentage
  )
