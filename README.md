UV radiation affects anti-predatory defense traits in *Daphnia pulex*
================

This repository contains the raw data and R scripts for “UV radiation
affects anti-predatory defense traits in *Daphnia pulex*” by
Eshun-Wilson *et al.*, 2020, Ecology and Evolution (A link to the final
version of the article will be added here in the future.) All data is
also available on Dryad: <https://doi.org/10.5061/dryad.kprr4xh3c>

The `data` folder contains the raw data used for the analyses, and the
`scripts` folder contains the R scripts which were written for the
analyses. The fitted models are in the `models` folder, and all graphics
are in the `figures` folder.

The following R package needs to be installed to run the analysis:

``` r
install.packages("brms")
```

Additionally, all graphics require the `tidyverse` package collection,
especially `ggplot2`, and `patchwork`. They can be installed as follows:

``` r
install.packages(c("tidyverse", "patchwork"))
```

## 1\. Data files, definitions of column headers, explanations of indeces used, and units if applicable

### Information to data file `Data_Experiment_Effectivity of kairomones.csv`

**`Treatment`**: Three treatment levels. `UVR`: media-filled jars with
added kairomone extract exposed to ultraviolet radiation, `PAR`:
media-filled jars with added kairomone extract exposed to
photosynthetically active radiation, `CONTROL`: media-filled jars
without kairomone extract kept in the dark.

**`Mother_ID`**: Identifier of individual *Daphnia pulex* females
carrying brood of neonates (mothers). *Daphnia pulex* of a single clonal
line, called `UNI`.

**`Neonate_ID`**: Identifier of individual second instar *Daphnia pulex*
juveniles (neonates) per *Daphnia pulex* female (nested within
`Mother_ID`).

**`Hour`**: Time in hours media-filled jars with added kairomone extract
have been exposed to UVR and PAR before *Daphnia pulex* females carrying
brood of neonates have been placed into the jars.

**`Neckteeth_count`**: Number of neckteeth counted on individual 2nd
instar *Daphnia pulex* juveniles.

**`Bump_keel`**: Pedestal score on individual 2nd instar *Daphnia pulex*
juveniles. `0`: no pedestal, `1`: small pedestal, `2`: large pedestal.

**`Pedestal_score`**: Pedestal score recorded on individual 2nd instar
*Daphnia pulex* juveniles. `A`: no pedestal, `B`: small pedestal, `C`:
large pedestal.

**`Induction_score`**: Neckteeth induction score (in %) recorded on
individual 2nd instar *Daphnia pulex* juveniles.

Further information on methods can be found below in section 2 & 4.

### Information to data file `Data_Experiment_Direct effect on Daphnia.csv`

**`Clone`**: *Daphnia pulex* of two clonal lines were used, called `P5`
and `UNI`.

**`Treatment`**: Eight treatment levels. `P5_Control`: no UVR and no
kairomone exposure of clone P5, `P5_Kairomone`: no UVR but kairomone
exposure of clone P5, `P5_UVR`: UVR but no kairomone exposure of clone
P5, `P5_Both`: UVR and kairomone exposure of clone P5, `UNI_Control`: no
UVR and no kairomone exposure of clone UNI, `UNI_Kairomone`: no UVR but
kairomone exposure of clone UNI, `UNI_UVR`: UVR but no kairomone
exposure of clone UNI, `UNI_Both`: UVR and kairomone exposure of clone
UNI.

**`Kairomone`**: Two levels. `+Kairomone`: kairomone exposure,
`-Kairomone`: no kairomone exposure.

**`UVR`**: Two levels. `+UVR`: exposure to ultraviolet radiation,
`-UVR`: no exposure to ultraviolet radiation.

**`Mother_ID`**: Identifier of individual *Daphnia pulex* females
carrying brood of neonates (mothers).

**`Sex`**: Sex of individual *Daphnia pulex* juveniles (`female`,
`male`, or `NA`: not available).

**`Instar`**: Instar of individual *Daphnia pulex* juveniles
(`instar 1`, `instar 2`, or NA: not available).

**`Neck_teeth_count`**: Number of neckteeth counted on individual
*Daphnia pulex* juveniles.

**`Bump_score`**: Pedestal score recorded on individual *Daphnia pulex*
juveniles. `A`: no pedestal, `B`: small pedestal, `C`: large pedestal.

**`Induction`**: Neckteeth induction score (in %) recorded on individual
*Daphnia pulex* juveniles.

**`Body_length`**: Body length (in mm) of individual *Daphnia pulex*
juveniles (`NA`: not available).

**`Body_width`**: Body width (in mm) of individual *Daphnia pulex*
juveniles (`NA`: not available).

**`Spina_length`**: Tail spine (spina) length (in mm) of individual
*Daphnia pulex* juveniles (`NA`: not available).

Further information on methods can be found below in section 3 & 4.

## 2\. Experiment testing the effect of UVR on kairomone effectivity

### Further information to data collection of data file `Data_Experiment_Effectivity of kairomones.csv`

We first conducted an experiment to (i) demonstrate that our kairomone
extract effectively induced neckteeth formation as well as to (ii)
investigate the hypothesis that UVR may limit neckteeth induction by UVR
making the kairomone ineffective. In our study, the integrity of
kairomone suspensions was tested for different time intervals with the
following three treatments (with 8 replicates/jars per treatment):
Medium with kairomones exposed to UVR (340-400 nm), medium with
kairomones exposed to PAR (400-700 nm), and a control with medium that
contained no kairomones and was kept in the dark. Kairomone solutions
were prepared by adding 60 µL of the kairomone extract into 50 mL glass
jars containing 40 mL medium and food algae. The jars containing the
kairomone solutions and the control were subjected to the respective
treatments for 2, 4, 6, and 8 hours with no daphnids present. After each
time period, females of the UNI clone with developing offspring in their
brood pouch were placed individually in jars of the different treatment
groups (UVR, PAR, and control treatments, respectively), with 2 mother
individuals for each group and time interval. Five to ten released
offspring juveniles per mother were inspected under the microscope at
their 2nd instar to count the number of neckteeth and score the pedestal
(see section 4 below).

## 3\. Experiment testing the direct effect of UVR on *Daphnia*

### Further information to data collection of data file `Data_Experiment_Direct effect on Daphnia.csv`

In another experiment we tested whether exposure of egg-bearing mothers
and offspring to UVR would affect kairomone-induced neckteeth formation
in the juveniles. *D. pulex* clones P5 and UNI were exposed to the
following four treatments in a full factorial design: without UVR or
kairomone exposure (control), kairomone exposure without UVR exposure,
UVR exposure without kairomone exposure, and UVR and kairomone exposure.
D. pulex females of both clones carrying the 4th clutch in their brood
pouch were used for the experiment (5-7 mothers per treatment). These
mother individuals were placed individually in transparent 50 mL open
glass jars filled with 40 mL medium and food algae and were exposed to
UVR and kairomones depending on treatment. Kairomone treatments were
prepared by adding 60 µL of the kairomone extract to the jars. UVR and
non-UVR treatment groups were exposed to UVR and PAR light,
respectively, in 16:8 hours light:dark cycles. Mother individuals of all
treatment groups were transferred daily to freshly prepared kairomone
and food suspensions until release of their 4th clutch juveniles. The
mothers were removed, and juvenile clutch groups were kept in the same
treatments until reaching the 2nd instar. Juveniles were inspected daily
alive using a microscope for counting neckteeth, scoring pedestals, and
taking photographs of the full body using a computer-aided camera for
later length measurements (see section 4 below).

## 4\. Scoring of morphological defense traits and length measurements

### 4.1. Neckteeth number, pedestal score, and neckteeth induction score

Neckteeth, i.e., small spines at the dorsal head margin, were counted on
live individuals of *D. pulex* juveniles in the 1st and 2nd instar using
a microscope (Nikon Eclipse E200) with 100x magnification. At the base
of the neckteeth, a pedestal of varying size can develop and was scored
in a categorial way with `A` when absent, `B` when small, and `C` when
large. Individuals were then photographed at 40x magnification for later
length measurements (see below) with a microscope-mounted Nikon camera
(DS-5M). From the neckteeth counts and pedestal score, a neckteeth
induction score has been calculated: neckteeth were scored 10% each and
the pedestal score was translated to `A`: 0%, `B`: 30%, `C`: 50%; the
induction score per individual juvenile is the sum of the neckteeth and
pedestal scores.

### 4.2. Measurements of body length, body width and tail spine (spina) length

Body length, body width, and tail spine length of *D. pulex* juveniles
were measured from the photographs using ImageJ and a landmark approach.
Body length was calculated as the distance between the top of the head
and the base of tail spine, body width between the ventral midpoint and
dorsal midpoint, and tail spine length between the base and the tip of
the tail spine. Technical difficulties caused by image file corruption
limited the number of measurements in some treatment groups of instar 2
juveniles.

## 5\. Statistics and data analyses

Statistical analysis of **2. Experiment testing the effect of UVR on
kairomone effectivity** with the associated data file
`Data_Experiment_Effectivity of kairomones.csv` can be found in the R
script `R_script_Model_kairomone_efficacy.R`.

Statistical analysis of **3. Experiment testing the direct effect of UVR
on *Daphnia*** with the associated data file `Data_Experiment_Direct
effect on Daphnia.csv` can be found in the R scripts
`R_script_Model_body_length_body_width_and_spina_length.R`,
`R_script_Model_neck_teeth_count_and_bump_score_instar_2.R`,
`R_script_Model_neck_teeth_count_and_bump_score_instar_1.R`.

The figures of the paper can be reproduced with the R scripts
`R_script_figure_1.R`, `R_script_figure_2.R`, `R_script_figure_3.R`,
`R_script_figure_4.R`, `R_script_figure_5.R`, `R_script_figure_S1.R`,
`R_script_figure_S2.R`, `R_script_figure_S3.R`.
