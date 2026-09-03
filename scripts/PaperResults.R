### Importing libraries
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(ggrepel)

### Reading in data
df <- read_csv("data/DataForPaper.csv")

################################# ACCURACY ON VALID VS INVALID INFERENCES ##########################################

### Overall accuracy on all inferences

## Creating cohort for analysis grouped by participant
participant_acc <- df %>%
  group_by(Group, ParticipantID) %>%
  summarise(p_acc = mean(CorrectJudgement, na.rm = TRUE) * 100, .groups = "drop")

## Calculating accuracy
Accuracy <- participant_acc %>%
  filter(!is.na(p_acc)) %>%
  group_by(Group) %>%
  summarise(
    mean_acc = mean(p_acc),
    sd_acc   = sd(p_acc),
    n_subj   = n(),
    sem      = sd_acc / sqrt(n_subj)
  )
Accuracy

### Accuracy on invalid and valid inferences separately

## Creating cohort for analysis grouped by participant
participant_acc_invalidvalid <- df %>%
  group_by(Group, ParticipantID, InvalidValid) %>%
  summarise(p_acc = mean(CorrectJudgement, na.rm = TRUE) * 100, .groups = "drop")

## Caluclating accuracy
AccuracyInvalidValid <- participant_acc_invalidvalid %>%
  filter(!is.na(p_acc)) %>%
  group_by(Group, InvalidValid) %>% # separating invalid and valid
  summarise(
    mean_acc = mean(p_acc),
    sd_acc   = sd(p_acc),
    n_subj   = n(),
    sem      = sd_acc / sqrt(n_subj)
  )
AccuracyInvalidValid

### Binomial test to assess accuracy against chance

## Filtering dataset so only children are included
KidsData <- df %>%
  filter(Group == "Children",
         !is.na(CorrectJudgement),
         !is.na(InvalidValid))

## Computing p values and confidence intervals of binomial test for valid and invalid inferences
ChildrenBinomial <- KidsData %>%
  group_by(InvalidValid) %>%
  summarise(
    TotalCorrect = sum(CorrectJudgement == 1),
    TotalTrials = n(),
    ProportionCorrect = TotalCorrect / TotalTrials,
    PValue = binom.test(TotalCorrect, TotalTrials, p = 0.5)$p.value,
    ConfidenceIntervals = list(binom.test(TotalCorrect, TotalTrials)$conf.int)
  ) %>%
  rowwise() %>% # separating confidence intervals into two cells to reflect upper and lower
  mutate(
    ConfidenceIntervalLower = ConfidenceIntervals[1],
    ConfidenceIntervalUpper = ConfidenceIntervals[2]
  ) %>%
  select(-ConfidenceIntervals) # removing the og confidence intervals column now that it has been separated

ChildrenBinomial

## Filtering dataset so only adults are included
AdultsData <- df %>%
  filter(Group == "Adult",
         !is.na(CorrectJudgement),
         !is.na(InvalidValid))

## Computing p values and confidence intervals of binomial test for valid and invalid inferences
AdultsBinomial <- AdultsData %>%
  group_by(InvalidValid) %>%
  summarise(
    TotalCorrect = sum(CorrectJudgement == 1),
    TotalTrials = n(),
    ProportionCorrect = TotalCorrect / TotalTrials,
    PValue = binom.test(TotalCorrect, TotalTrials, p = 0.5)$p.value,
    ConfidenceIntervals = list(binom.test(TotalCorrect, TotalTrials)$conf.int)
  ) %>%
  rowwise() %>% # separating confidence intervals into two cells to reflect upper and lower
  mutate(
    ConfidenceIntervalLower = ConfidenceIntervals[1],
    ConfidenceIntervalUpper = ConfidenceIntervals[2]
  ) %>%
  select(-ConfidenceIntervals) # removing orignal confidence interval column

AdultsBinomial

################################# SIMILARITY BETWEEN CHILDREN AND ADULTS ##########################################

## Spearman's Rank Correlation

# Summarizing data per inference
MeanAccuracyPerInference <- df %>% 
  group_by(InferenceType, Group) %>%
  summarise(
    MeanAccuracy = mean((CorrectJudgement) * 100, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Group,
    values_from = MeanAccuracy 
  )

# Tests normality functions: both violate so spearman's is used
shapiro.test(MeanAccuracyPerInference$Adult)
shapiro.test(MeanAccuracyPerInference$Children)

# Spearman's rank correlation
correlation_test <- cor.test(MeanAccuracyPerInference$Children, MeanAccuracyPerInference$Adult, method= "spearman", exact = FALSE) 
print(correlation_test)

################################# EVIDENCE OF COUNTEREXAMPLE SEARCH ##########################################

## Children

# Keep correct judgements

KidsData <- df %>%
  filter(Group == "Children") %>%
  mutate(CounterexampleGiven = ifelse(InitialFigTopology == CounterexampleTopology, 0, 1))

# One % per participant within each inference type
participant_ce <- KidsData %>%
  group_by(InvalidValid, CorrectJudgement, ParticipantID) %>%
  summarise(p_ce = mean(CounterexampleGiven, na.rm = TRUE) * 100, .groups = "drop")

# Group-level descriptives
CounterexampleDescriptives <- participant_ce %>%
  filter(!is.na(p_ce) & !is.na(CorrectJudgement)) %>%
  group_by(InvalidValid, CorrectJudgement) %>%
  summarise(
    mean_ce = mean(p_ce),
    sd_ce   = sd(p_ce),
    n_subj  = n(),
    sem     = sd_ce / sqrt(n_subj),
    .groups = "drop"
  )
print(CounterexampleDescriptives)

## Adults

# Keep correct judgements
AdultsData <- df %>%
  filter(Group == "Adult") %>%
  mutate(CounterexampleGiven = ifelse(InitialFigTopology == CounterexampleTopology, 0, 1))

# One % per participant within each inference type
participant_ce <- AdultsData %>%
  group_by(InvalidValid, CorrectJudgement, ParticipantID) %>%
  summarise(p_ce = mean(CounterexampleGiven, na.rm = TRUE) * 100, .groups = "drop")

# Group-level descriptives
CounterexampleDescriptives <- participant_ce %>%
  filter(!is.na(p_ce) & !is.na(CorrectJudgement)) %>%
  group_by(InvalidValid, CorrectJudgement) %>%
  summarise(
    mean_ce = mean(p_ce),
    sd_ce   = sd(p_ce),
    n_subj  = n(),
    sem     = sd_ce / sqrt(n_subj),
    .groups = "drop"
  )
print(CounterexampleDescriptives)

## Looking at percentage of correct answers

participant_correct <- AdultsData %>%
  group_by(InvalidValid, ParticipantID) %>%
  summarise(p_correct = mean(CorrectJudgement, na.rm = TRUE) * 100, .groups = "drop")

CorrectSummary <- participant_correct %>%
  group_by(InvalidValid) %>%
  summarise(
    mean_correct = mean(p_correct, na.rm = TRUE),
    sd_correct   = sd(p_correct, na.rm = TRUE),
    n_subj       = n_distinct(ParticipantID),
    sem          = sd_correct / sqrt(n_subj),
    .groups      = "drop"
  )

print(CorrectSummary)

################################# GENERAL TEST OF INFLUENCE OF GEOMETRY VS TOPOLOGY ##########################################

# Creating a column for whether it was a geometric inference (IV1 , IV2, IV3)
df$geometric_inference <- df$InferenceType %in% c("IV1", "IV2", "IV3")

# Creating a column for whether it was a topological inference (IV1 , IV2, IV4)
df$topological_inference <- df$InferenceType %in% c("IV1", "IV2", "IV4")

## Kids
KidsData <- df %>%
  filter(Group == "Children") 

# Logistic regression- Accuracy Answering Question
InfluenceOfConstraintsKids <- glm(CorrectJudgement ~  geometric_inference * topological_inference , 
                                      data = KidsData, 
                                      family = binomial)
summary(InfluenceOfConstraintsKids)
exp(coef(InfluenceOfConstraintsKids))

## Posthoc pairwise comparisons- estimated marginal means

# Simple effect of geometry WITHIN each level of topology
emmeans(InfluenceOfConstraintsKids,
        ~ geometric_inference | topological_inference) %>%
  pairs()

# Simple effect of topology WITHIN each level of geometry
emmeans(InfluenceOfConstraintsKids,
        ~ topological_inference | geometric_inference) %>%
  pairs()

## Adults
AdultsData <- df %>%
  filter(Group == "Adult") 

# Logistic regression- Accuracy Answering Question
InfluenceOfConstraintsAdults <- glm(CorrectJudgement ~  geometric_inference * topological_inference, 
                                    data = AdultsData, 
                                    family = binomial)

summary(InfluenceOfConstraintsAdults)
exp(coef(InfluenceOfConstraintsAdults))

# Simple effect of geometry WITHIN each level of topology
emmeans(InfluenceOfConstraintsAdults,
        ~ geometric_inference | topological_inference) %>%
  pairs()

# Simple effect of topology WITHIN each level of geometry
emmeans(InfluenceOfConstraintsAdults,
        ~ topological_inference | geometric_inference) %>%
  pairs()

################################# FOCUS ON TOPOLOGICAL CONSTRAINTS ##########################################

## Binomial test for inital configurations: children

KidsData <- df %>%
  filter(Group == "Children",
         InferenceType %in% c("IV5", "IV6"),
         !is.na(InitialFigTopology))

ChildrenInitialBinomial <- KidsData %>%
  group_by(InitialFigTopology) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    TotalTrials = sum(Count),
    ProportionChosen = Count / TotalTrials
  ) %>%
  rowwise() %>%
  mutate(
    PValue = binom.test(Count, TotalTrials, p = 1/3)$p.value,
    ConfidenceIntervalLower = binom.test(Count, TotalTrials, p = 1/3)$conf.int[1],
    ConfidenceIntervalUpper = binom.test(Count, TotalTrials, p = 1/3)$conf.int[2]
  ) %>%
  ungroup()

# Printing test results
print(ChildrenInitialBinomial)

## Binomial test for inital configurations: adults

AdultsData <- df %>%
  filter(Group == "Adult",
         InferenceType %in% c("IV5", "IV6"),
         !is.na(InitialFigTopology))

AdultsInitialBinomial <- AdultsData %>%
  group_by(InitialFigTopology) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    TotalTrials = sum(Count),
    ProportionChosen = Count / TotalTrials
  ) %>%
  rowwise() %>%
  mutate(
    PValue = binom.test(Count, TotalTrials, p = 1/3)$p.value,
    ConfidenceIntervalLower = binom.test(Count, TotalTrials, p = 1/3)$conf.int[1],
    ConfidenceIntervalUpper = binom.test(Count, TotalTrials, p = 1/3)$conf.int[2]
  ) %>%
  ungroup()

# Printing test results
print(AdultsInitialBinomial)

### Participants preferences after an apart initial config

# Filtering for apart initial topologies for IV5 and IV6
apart <- df %>%
  filter(InferenceType %in% c("IV5", "IV6"),
         InitialFigTopology == "Apart",
         !is.na(CounterexampleTopology),
         InitialFigTopology != CounterexampleTopology) %>% # needs to be excluded otherwise ones that stay apart the whole time will remain the same
  # Creating new binary columns for crossing and fully inside (1 for crossing, 0 for fully inside)
  mutate(
    Crossing = as.integer(CounterexampleTopology == "Crossing"),
    PID      = factor(ParticipantID)
  )

# Running logistic regression from the apart initial topologies
regression <- glm(Crossing ~ Group, data = apart, family = binomial)
summary(regression)
broom.mixed::tidy(regression, exponentiate = TRUE, conf.int = TRUE)
emmeans(regression, ~ Group, type = "response")

# Re-levelled so Children is the reference → intercept tests children vs. chance
regression_kids_ref <- glm(Crossing ~ relevel(factor(Group), ref = "Children"),
                           data = apart, family = binomial)
summary(regression_kids_ref)
broom.mixed::tidy(regression_kids_ref, exponentiate = TRUE, conf.int = TRUE)

### Participants preferences after a crossing/fully inside initial config

# Filtering for non-apart initial topologies for IV5 and IV6
nonapart <- df %>%
  filter(InferenceType %in% c("IV5", "IV6"),
         InitialFigTopology %in% c("Crossing", "Fully inside"),
         !is.na(CounterexampleTopology),
         InitialFigTopology != CounterexampleTopology) %>%   # <- excludes Crossing→Crossing, Inside→Inside
  mutate(
    ToApart = as.integer(CounterexampleTopology == "Apart"),
    InitialFigTopology = factor(InitialFigTopology),
    Group   = factor(Group, levels = c("Adult", "Children")),
    PID     = factor(ParticipantID)
  )

# Running logistic regression from the non-apart initial topologies
regression <- glm(ToApart ~ InitialFigTopology + Group, data = nonapart, family = binomial)
summary(regression)
broom.mixed::tidy(regression, exponentiate = TRUE, conf.int = TRUE)
emmeans(regression, ~ InitialFigTopology + Group, type = "response")

###################### FOCUS ON GEOMETRIC INFLUENCES ##########################################

# Filtering for needed rows
geo <- df %>%
  filter(InferenceType %in% c("IV5", "IV6"),
         TranslatedOrScaled %in% c("Translated", "Scaled")) %>%  
  mutate(Translation = as.integer(TranslatedOrScaled == "Translated"))

# Getting observed proportions of translation
geo %>%
  group_by(Group) %>%
  summarise(n_translation = sum(Translation),
            n_total       = n(),
            prop_translation = mean(Translation),
            .groups = "drop")

# binomial tests vs chance (.50), children and adults separately
kids   <- geo %>% filter(Group == "Children")
adults <- geo %>% filter(Group == "Adult")

binom.test(sum(kids$Translation),   nrow(kids),   p = 0.5)
binom.test(sum(adults$Translation), nrow(adults), p = 0.5)

# Fisher exact test: do the two groups differ in translation vs scaling?
fisher.test(table(geo$Group, geo$Translation))

###################################  PREFERENCE TO CHANGE SMALLER VS LARGER CIRCLE ################################

# Creating cohort for analysis
size <- df %>%
  filter(CircleChanged %in% c("Big", "Small")) %>%  
  mutate(
    Smaller = as.integer(CircleChanged == "Small"), 
    Group   = factor(Group, levels = c("Adult", "Children")),
    PID     = factor(ParticipantID)
  )

size %>%
  group_by(Group) %>%
  summarise(
    n_smaller    = sum(Smaller),
    n_total      = n(),
    prop_smaller = mean(Smaller),
    .groups = "drop"
  )


regression <- glm(Smaller ~ Group, data = size, family = binomial)
summary(regression)
broom::tidy(regression, exponentiate = TRUE, conf.int = TRUE)
emmeans(regression, ~ Group, type = "response")

size_kids_ref <- glm(Smaller ~ relevel(Group, ref = "Children"), data = size, family = binomial)
summary(size_kids_ref)
broom::tidy(size_kids_ref, exponentiate = TRUE, conf.int = TRUE)
###################################  PREFERENCE TO CHANGE LEAST CONSTRAINED CIRCLE ################################

# Filtering for needed rows
topo <- df %>%
  filter(CircleChangedConstraints %in% c("Least", "Most")) %>% 
  mutate(
    Least = as.integer(CircleChangedConstraints == "Least"),     # 1 = least constrained, 0 = most
    Group = factor(Group, levels = c("Adult", "Children")),
    PID   = factor(ParticipantID)
  )

topo %>%
  group_by(Group) %>%
  summarise(
    n_least    = sum(Least),
    n_total    = n(),
    prop_least = mean(Least),
    .groups = "drop"
  )

regression <- glm(Least ~ Group, data = topo, family = binomial)

summary(regression)
broom.mixed::tidy(regression, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) 
emmeans(regression, ~ Group, type = "response") 

topo_kids_ref <- glm(Least ~ relevel(Group, ref = "Children"),
                       data = topo, family = binomial)
broom.mixed::tidy(topo_kids_ref, effects = "fixed", exponentiate = TRUE, conf.int = TRUE)
summary(topo_kids_ref)$coefficients

################################### CORRELATION BETWEEN SMALLER AND LEAST CONSTRAINED CIRCLE ################################


# Creating df with both geometrical and topological changes
df_filtered <- df %>%
  filter(CircleChanged %in% c("Small", "Big"),
         CircleChangedConstraints %in% c("Least", "Most")) %>%
  mutate(
    Smaller       = as.integer(CircleChanged == "Small"),
    Least         = as.integer(CircleChangedConstraints == "Least"),
    SmallerIsLeast = as.integer(Smaller == Least),   # 1 = smaller circle is also least-constrained
    Group = factor(Group, levels = c("Adult", "Children"))
  )

# overall: % of trials where smaller == least-constrained, binomial test vs .5
df_filtered %>% summarise(n_coincide = sum(SmallerIsLeast),
                   n_total    = n(),
                   prop       = mean(SmallerIsLeast))

binom.test(sum(df_filtered$SmallerIsLeast), nrow(df_filtered), p = 0.5)

# Looking for both children and adults
df_filtered %>% group_by(Group) %>%
  summarise(n_coincide = sum(SmallerIsLeast),
            n_total    = n(),
            prop       = mean(SmallerIsLeast),
            .groups = "drop")


########################################### FIGURES ##########################################

## Figure 2

# Summarizing data per inference
MeanAccuracyPerInference <- df %>% 
  group_by(InferenceType, Group) %>%
  summarise(
    MeanAccuracy = mean((CorrectJudgement) * 100, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Group,
    values_from = MeanAccuracy 
  )

# Spearman's rank correlation
correlation_test <- cor.test(MeanAccuracyPerInference$Children, MeanAccuracyPerInference$Adult, method= "spearman", exact = FALSE) 
print(correlation_test)

# Extracts rho value in a presentable way
RhoValue <- round(correlation_test$estimate, 2)

# Extract p value
PValue <- correlation_test$p.value

# Function that formulates p value to look presentable in graph
if (PValue < 0.001) {
  PValueFormatted <- " < 0.001"
} else {
  PValueFormatted <- paste0("= ", round(PValue, 3))
}

# Creating graph limits
lims <- range(c(MeanAccuracyPerInference$Adult, MeanAccuracyPerInference$Children))

# Plotting Figure
ggplot(MeanAccuracyPerInference, aes(x = Adult, y = Children)) +
  geom_point(
    shape = 21,
    size = 2,
    fill = NA, 
    color = "black",                    
    stroke = 0.8
  ) +
  geom_text_repel(aes(label = InferenceType), size = 3.5,
                  box.padding = 0.4, segment.colour = NA) +
  scale_x_continuous(
    limits = c(30, 100),
    breaks = c(30, 40, 50, 60, 70, 80, 90, 100)
  ) +
  scale_y_continuous(
    limits = c(30, 100),
    breaks = c(30, 40, 50, 60, 70, 80, 90, 100)
  ) +
  geom_smooth(method = "lm", fullrange = TRUE, color = "black", se = FALSE, linewidth = 0.6) +
  labs(title = "",
       x = "Adult's Accuracy (%)",
       y = "") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
  )


############# FIGURE 3 ###################################################################

LIGHT <- "#C9C9C9"; DARK <- "#595959"           # Children/Adults
group_cols <- c(Children = LIGHT, Adults = DARK)
geo_cols   <- c(Absent  = LIGHT, Present = DARK)

# Setting theme for whole figure
theme_panel <- theme_classic(base_size = 9, base_family = "") +
  theme(axis.text  = element_text(colour = "black", size = 8),
        axis.line  = element_line(linewidth = 0.4),
        axis.ticks = element_line(linewidth = 0.4, colour = "black"),
        legend.position = "top", legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), legend.key.size = unit(3.2, "mm"),
        strip.background = element_blank(), strip.text = element_text(size = 9, face = "bold"))
theme_set(theme_panel)

# Y-axis scale used across all bar panels
scale_pct <- scale_y_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                                expand = expansion(mult = c(0, 0)))

# Relabels "Adult" to "Adults" for display and fixes group order (Children before Adults) in all plots
disp_group <- function(x) factor(recode(as.character(x), Adult = "Adults"),
                                 levels = c("Children", "Adults"))

# Draws a significance bracket with a label (e.g. "*", "**", "n.s.") between two group bars
bracket <- function(label, y = 106)
  list(annotate("segment", x = 1, xend = 2, y = y, yend = y, linewidth = 0.3),
       annotate("segment", x = 1, xend = 1, y = y, yend = y - 2, linewidth = 0.3),
       annotate("segment", x = 2, xend = 2, y = y, yend = y - 2, linewidth = 0.3),
       annotate("text", x = 1.5, y = y + 4, label = label, size = 3))

# Builds a standard group-comparison bar chart (Children vs Adults) with error bars and significance bracket
group_bar <- function(d, ylab, sig = NULL)
  ggplot(d, aes(Group, value, fill = Group)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.14, linewidth = 0.4) +
  scale_fill_manual(values = group_cols, guide = "none") +
  scale_pct + labs(x = NULL, y = ylab) +
  (if (!is.null(sig)) bracket(sig) else NULL)

# Extracts model-estimated group proportions (%) and CIs from a fitted glm
emm_pct <- function(m) {
  d <- as.data.frame(emmeans(m, ~ Group, type = "response"))
  names(d)[names(d) %in% c("prob", "response")] <- "est"
  transmute(d, Group = disp_group(Group),
            value = est * 100, lo = asymp.LCL * 100, hi = asymp.UCL * 100)
}


## SUBFIGURE A — accuracy (geometric x topological)

df <- df |>
  mutate(geometric_inference   = InferenceType %in% c("IV1","IV2","IV3"),
         topological_inference = InferenceType %in% c("IV1","IV2","IV4"))

acc_pct <- function(g) {
  m <- glm(CorrectJudgement ~ geometric_inference * topological_inference,
             data = filter(df, Group == g), family = binomial)
  as.data.frame(emmeans(m, ~ geometric_inference * topological_inference, type = "response")) |>
    mutate(Group = g)
}
emmA <- bind_rows(acc_pct("Children"), acc_pct("Adult")) |>
  mutate(value = prob * 100, lo = asymp.LCL * 100, hi = asymp.UCL * 100,
         Geometric   = factor(ifelse(geometric_inference,   "Present", "Absent"), c("Absent","Present")),
         Topological = factor(ifelse(topological_inference, "Present", "Absent"), c("Absent","Present")),
         Group = disp_group(Group))

p_acc <- ggplot(emmA, aes(Topological, value, fill = Geometric)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  geom_col(position = position_dodge(.75), width = .68, colour = "black", linewidth = .3) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(.75), width = .18, linewidth = .4) +
  facet_wrap(~ Group) +
  scale_fill_manual(values = geo_cols, name = "Geometric constraint") +
  scale_pct +
  labs(x = "Topological constraint", y = "Judgements correct (%)")

print(p_acc)

## SUBFIGURE C — translation vs scaling     

datT <- df |>
  filter(InferenceType %in% c("IV5","IV6"), TranslatedOrScaled %in% c("Translated","Scaled")) |>
  mutate(t = as.integer(TranslatedOrScaled == "Translated")) |>
  group_by(Group) |>
  summarise(k = sum(t), n = n(),
            value = 100 * k / n,
            lo = 100 * binom.test(k, n)$conf.int[1],
            hi = 100 * binom.test(k, n)$conf.int[2], .groups = "drop") |>
  mutate(Group = disp_group(Group))

p_trans <- group_bar(datT, "Chose translation (% of trials)", sig = "n.s.")

print(p_trans)

## SUBFIGURE D — smaller vs larger circle                                   

mS <- glm(as.integer(CircleChanged == "Small") ~ Group,
            data = filter(df, CircleChanged %in% c("Big","Small")), family = binomial)
p_small <- group_bar(emm_pct(mS), "Transformed smaller circle (% of trials)", sig = "*")

print(p_small)
## SUBFIGURE E — least vs most constrained circle                            

mL <- glm(as.integer(CircleChangedConstraints == "Least") ~ Group,
            data = filter(df, CircleChangedConstraints %in% c("Least","Most")), family = binomial)
p_least <- group_bar(emm_pct(mL), "Transformed least-constrained circle (% of trials)", sig = "**")

print(p_least)

## Probabilities for diagram ##
df %>%
  filter(InferenceType %in% c("IV5","IV6"),
         !is.na(InitialFigTopology), !is.na(CounterexampleTopology),
         InitialFigTopology != CounterexampleTopology) %>%
  count(Group, InitialFigTopology, CounterexampleTopology, name = "n") %>%
  group_by(Group, InitialFigTopology) %>%
  mutate(Probability = round(n / sum(n), 2)) %>%
  ungroup() %>%
  print(n = Inf)


