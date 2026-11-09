# Is Populism an Issue of Social Integration?
### Evaluating subjective social status in Germany

Replication code for the master thesis *"Is Populism an Issue of Social Integration? Evaluating Subjective Social Status in Germany"* by Alexander Ott (University of Mannheim, MA Political Science, 2025).

Building on Gidron and Hall (2019), the thesis examines subjective social status (SSS) in Germany: how economic and cultural factors shape it, whether it captures the three dimensions of social integration, how it developed across occupational groups over time, and how it relates to political alienation and to voting for the AfD and Die Linke.

---

## Repository structure

```
.
├── README.md
├── analysis.R          # complete analysis: data preparation, figures, regressions
├── data/               # place the ALLBUS file here (not included, see below)
└── output/             # created by the script: figures and regression tables
```

## Data

| Data set | Study number | Version | File |
|---|---|---|---|
| ALLBUS cumulation 1980–2018 | ZA5274 | 1.1.0 | `ZA5274_v1-1-0.dta` |

**The data are not included in this repository.** ALLBUS data may not be redistributed. They are free for academic use after registration at the [GESIS Data Archive](https://search.gesis.org/). Download the file and place it in the `data/` folder.

Subjective social status (`id03`) is not included in ALLBUS 2021, so all analyses cover 1980–2018.

## Requirements

- R (≥ 4.1) — developed with RStudio
- R packages:

```r
install.packages(c("tidyverse", "haven", "sandwich", "lmtest"))
```

## How to run

1. Clone the repository and open the folder as an RStudio project.
2. Put `ZA5274_v1-1-0.dta` into `data/`.
3. Run `analysis.R`. All results are written to `output/`.

| Output | Content |
|---|---|
| `fig1_sss_by_occupation.png` | Average SSS by occupational class, 1992–2018 |
| `fig2_trust_by_sss.png` | Trust in people by SSS |
| `fig3_respect_by_sss.png` | Respect for others by SSS |
| `fig4_predicted_sss_social_activity.png` | Predicted SSS by frequency of social meetings |
| `fig5_predicted_alienation.png` | Predicted political trust and satisfaction with democracy by SSS |
| `fig6_sss_by_party.png` | Average SSS by vote intention, 1980–2018 |
| `tab1_predictors_of_sss.csv` | Tab. 1: predictors of SSS (OLS) |
| `tab2_sss_social_integration.csv` | Tab. 2: SSS and social integration (OLS) |
| `tab3_sss_political_alienation.csv` | Tab. 3: SSS and political alienation (OLS / LPM) |
| `tab4_sss_radical_voting.csv` | Tab. 4: SSS and voting for AfD and Die Linke (LPM) |

All regression tables report heteroscedasticity-robust (HC1) standard errors. Descriptive figures are weighted with the east-west weight `wghtpew`, because East Germans are oversampled in ALLBUS.

## Variables

### Dependent variables

| Variable | ALLBUS | Coding |
|---|---|---|
| `sss` | `id03` | Subjective social status, 1 = bottom ... 10 = top |
| `afd`, `die_linke` | `pv01` | 1 = vote intention AfD / Die Linke (PDS), 0 = CDU/CSU, SPD, FDP, Greens |
| `no_vote` | `pv01` | 1 = would not vote |
| `sat_democracy` | `ps03` | Satisfaction with democracy, reversed (6 = very satisfied) |
| `trust_parties`, `trust_parliament` | `pt15`, `pt03` | 1 = no trust ... 7 = great trust |

### Independent variables

| Variable | ALLBUS | Coding |
|---|---|---|
| `Occupation` | `isco88`, `dw03` | Six occupational classes following Oesch (2006); not employed respondents split into unemployed, retired and other not employed. Reference: socio-cultural (semi-)professionals |
| `income_decile` | `di08` | Equivalized household income, deciles within each survey year |
| `econ_situation` | `ep03` | Own current economic situation: very good (reference) ... very bad |
| `materialist` | `ingle` | 1 = materialist, 0 = post-materialist (mixed types excluded) |
| `distrust` | `st01` | 1 = "one should always be cautious", 0 = "most people can be trusted" |
| `no_care` | `lp06` | 1 = "most people don't care about others" |
| `SocialActivity` | `si03` | Time spent with friends, never (reference) ... daily |
| `immigration` | `pa09` | Immigrants should adapt, reversed (5 = strongly agree) |
| `redistribution` | `pa16` | Redistribution in favour of ordinary people, reversed |
| `UrbanRural` | `gs01` | City (reference), suburb, small city, village, countryside |
| `higher_education` | `de01` | 1 = vocational school, technical school, master craftsman, university of applied sciences or university |
| `female`, `union`, `church`, `foreign_born`, `east` | `sex`, `sm01`, `rp01`, `dn07`, `eastwest` | Dummies (church = attendance several times a year or more) |
| `age_div10`, `year_index` | `age`, `year` | Age / 10; survey year (1980 = 1) |

All negative values are ALLBUS missing codes and are set to `NA`.

## Notes

- The immigration and redistribution items are only available in 2008 and 2018. AfD model 3 therefore uses 2018 only.
- SSS was asked of only part of the sample in 2000 and 2002, which explains the outlier in these years.

## References

- Gidron, N., & Hall, P. A. (2019). Populism as a Problem of Social Integration. *Comparative Political Studies*, 53(7), 1027–1059.
- Oesch, D. (2006). *Redrawing the Class Map: Stratification and Institutions in Britain, Germany, Sweden and Switzerland*. Palgrave Macmillan.
- GESIS – Leibniz Institute for the Social Sciences. ALLBUS cumulation 1980–2018 (ZA5274).

## Author

Alexander Ott
