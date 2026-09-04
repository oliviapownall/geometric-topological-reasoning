# geometric-topological-reasoning

Data and analysis code examining how children and adults reason about geometric and topological  constraints in a counterexample-drawing task. This study is a developmental follow-up to Hamami & Amalric (2024).

## Overview

Participants (children, n = 46; adults, n = 38) completed a counterexample-drawing task across 12 trials spanning six invalid (IV1–IV6) and six valid (V1-V6) inference types. Geometric inferences (IV1–IV3) and topological inferences (IV1, IV2, IV4) were assessed separately, with  IV5/IV6 used for counterexample and transition analyses. Three topological states (Apart, Crossing, Fully Inside) form the basis of a transition analysis examining how participants' drawings shift between states.

## Repository Structure

```
├── data/
│   └── DataForPaper.csv        # Trial-level data, one row per trial (12 per participant)
├── scripts/
│   ├── PaperMethods.R           # Analysis for Methods section (descriptives)
│   └── PaperResults.R           # Analysis for Results section (mixed-effects models, figures)
├── README.md
└── LICENSE
```

## Data Dictionary

| Variable | Description |
|---|---|
| ParticipantID | Unique participant identifier |
| Group | Age group (Children / Adult) |
| Age | Participant age |
| Gender | Participant gender |
| PairOrder | Order in which the paired trials were presented |
| TimesInitialCirclesAttempted | Number of attempts at the initial circle configuration |
| InferenceType | Inference type (IV1–IV6; V1-V6) |
| InvalidValid | Whether the trial's inference was valid or invalid |
| InitialFigTopology | Topological state of the initial figure (Apart / Crossing / Fully inside) |
| CorrectJudgement | Whether the participant's judgement was correct |
| CounterexampleTopology | Topological state of the drawn counterexample |
| TranslatedOrScaled | Whether the second configuration was translated or scaled |
| CircleChanged | Which circle was changed when drawing the counterexample (Small / Big / Both / Circles same size) |
| CircleChangedConstraints | Whether the circle changed was the least- or most-constrained circle (Least / Most) |

## Session Info

Analysis run with:
- R version 4.5.2 (2025-10-31)
- Platform: aarch64-apple-darwin20 (macOS Tahoe 26.5.2)

Key packages:
- lme4_2.0-6
- emmeans_2.0.4
- ggplot2_4.0.3
- ggrepel_0.9.8
- psych_2.6.5
- broom_1.0.13
- broom.mixed_0.2.9.7
- dplyr_1.2.1
- tidyr_1.3.2
- readr_2.2.0

## Citation

If you use this data or code, please cite: X

Related work:
Hamami, Y., & Amalric, M. (2024). Going round in circles. *Open Mind*. 
https://pmc.ncbi.nlm.nih.gov/articles/PMC11627530/

## License

Code: MIT License  
Data: available on request

## Contact

[Olivia Pownall / olivia.pownall25@imperial.ac.uk]
[Co-Supervisor: Yacin Hamami/ yacin.hamami@univ-lorraine.fr]
[Co-Supervisor: Elizabeth Spelke/ spelke@wjh.harvard.edu]
[Supervisor: Marie Amalric/ marie.amalric@inserm.fr]
