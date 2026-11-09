# =============================================================================
# Is Populism an Issue of Social Integration?
# Evaluating Subjective Social Status in Germany (Master thesis)
#
# Replication script. Run from the repository root.
# Data (not included): place the ALLBUS cumulation in ./data/
#   - ZA5274_v1-1-0.dta  (ALLBUS cumulation 1980-2018)
# Subjective social status (id03) is not part of ALLBUS 2021, so the 2021
# survey is not needed for any of the analyses.
# =============================================================================

library(tidyverse)
library(haven)
library(sandwich)
library(lmtest)

dir.create("output", showWarnings = FALSE)
theme_set(theme_minimal())


# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------

vars <- c("year", "id03", "isco88", "dw03", "de01", "di08", "ep03", "ingle",
          "lp06", "st01", "si03", "gs01", "rp01", "dn07", "sm01", "sex", "age",
          "eastwest", "pa09", "pa16", "pt03", "pt15", "ps03", "pv01", "wghtpew")

alldata <- read_dta("data/ZA5274_v1-1-0.dta", col_select = all_of(vars)) %>%
  zap_labels() %>%
  # isco88 uses -10 for "not employed": keep it before recoding missing values
  mutate(not_employed = isco88 == -10) %>%
  # All negative codes are ALLBUS missing codes (no answer, not asked, ...)
  mutate(across(-c(year, wghtpew, not_employed), ~ if_else(.x < 0, NA_real_, as.numeric(.x))))


# -----------------------------------------------------------------------------
# 2. Occupational classes (Oesch 2006, based on ISCO-88)
# -----------------------------------------------------------------------------

technical_codes <- c(
  2100, 2110, 2111, 2112, 2113, 2114, 2120, 2121, 2122, 2130, 2131, 2139, 2140,
  2141, 2142, 2143, 2144, 2145, 2146, 2147, 2148, 2149, 2200, 2210, 2211, 2212,
  2213, 3100, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119, 3120,
  3121, 3122, 3123, 3130, 3131, 3132, 3133, 3139, 3140, 3141, 3142, 3143, 3144,
  3145, 3150, 3151, 3152, 3210, 3211, 3212, 3213, 3434, 7900)

production_codes <- c(
  6100, 6110, 6111, 6112, 6120, 6121, 6122, 6129, 6130, 6140, 6141, 6142, 6150,
  6151, 6152, 6153, 6154, 6200, 6210, 7100, 7110, 7111, 7112, 7113, 7120, 7121,
  7122, 7123, 7124, 7129, 7130, 7131, 7132, 7133, 7134, 7135, 7136, 7137, 7139,
  7140, 7141, 7143, 7200, 7210, 7211, 7212, 7213, 7214, 7215, 7216, 7220, 7221,
  7222, 7223, 7224, 7230, 7231, 7232, 7233, 7240, 7241, 7242, 7244, 7245, 7300,
  7310, 7311, 7312, 7313, 7320, 7321, 7322, 7323, 7324, 7330, 7331, 7332, 7340,
  7341, 7342, 7343, 7344, 7345, 7346, 7400, 7410, 7411, 7412, 7413, 7414, 7415,
  7416, 7420, 7421, 7422, 7423, 7424, 7430, 7431, 7432, 7433, 7434, 7435, 7436,
  7437, 7440, 7441, 7442, 7500, 7510, 8310, 8311, 8312, 8324, 8330, 8332, 8333,
  8334, 8340, 8100, 8110, 8111, 8112, 8113, 8120, 8121, 8122, 8123, 8124, 8130,
  8131, 8139, 8140, 8141, 8142, 8143, 8150, 8151, 8152, 8153, 8154, 8155, 8159,
  8160, 8161, 8162, 8163, 8170, 8200, 8210, 8211, 8212, 8220, 8221, 8222, 8223,
  8224, 8229, 8230, 8231, 8232, 8240, 8250, 8251, 8252, 8253, 8260, 8261, 8262,
  8263, 8264, 8265, 8266, 8269, 8270, 8271, 8272, 8273, 8274, 8275, 8276, 8277,
  8278, 8279, 8280, 8281, 8282, 8283, 8284, 8285, 8286, 8287, 8290, 8300, 8320,
  8321, 9153, 9160, 9161, 9162, 9200, 9210, 9211, 9212, 9213, 9300, 9310, 9311,
  9312, 9313, 9320, 9330, 9331, 9332, 9333)

manager_codes <- c(
  1100, 1110, 1140, 1141, 1142, 1143, 1200, 1210, 1220, 1221, 1222, 1223, 1224,
  1225, 1226, 1227, 1228, 1229, 1230, 1231, 1232, 1233, 1234, 1235, 1236, 1237,
  1239, 2400, 2410, 2411, 2412, 2419, 2420, 2421, 2422, 2429, 2441, 2470, 1300,
  1310, 1311, 1312, 1313, 1314, 1315, 1316, 1317, 1318, 1319, 3400, 3410, 3411,
  3412, 3413, 3414, 3415, 3416, 3417, 3419, 3420, 3421, 3422, 3423, 3429, 3430,
  3431, 3432, 3433, 3440, 3441, 3442, 3443, 3444, 3449, 3450)

clerk_codes <- c(
  4100, 4110, 4111, 4112, 4114, 4115, 4120, 4121, 4122, 4130, 4131, 4132, 4133,
  4140, 4141, 4142, 4143, 4144, 4190, 4200, 4210, 4212, 4213, 4214, 4215, 4220,
  4221, 4222, 4113, 4211, 4223)

sociocultural_codes <- c(
  2220, 2221, 2222, 2223, 2224, 2229, 2230, 2300, 2310, 2320, 2340, 2350, 2351,
  2352, 2359, 2430, 2431, 2432, 2440, 2442, 2443, 2445, 2451, 2460, 2330, 2331,
  2332, 2444, 2446, 2450, 2452, 2453, 2454, 2455, 3200, 3220, 3221, 3222, 3223,
  3224, 3226, 3229, 3230, 3231, 3232, 3300, 3310, 3320, 3330, 3340, 3460, 3470,
  3471, 3472, 3480)

service_codes <- c(
  3225, 3227, 3228, 3473, 3474, 3475, 5100, 5110, 5111, 5112, 5113, 5122, 5131,
  5132, 5140, 5141, 5143, 5160, 5161, 5162, 5163, 5169, 5200, 5210, 5220, 8323,
  5120, 5121, 5123, 5130, 5133, 5139, 5142, 5149, 5230, 8322, 9100, 9110, 9111,
  9113, 9120, 9130, 9131, 9132, 9133, 9140, 9141, 9142, 9150, 9151, 9152)

occupation_levels <- c("Socio-cultural (semi-)professionals", "(Associate) managers",
                       "Clerks", "Service workers", "Technical (semi-)professionals",
                       "Production workers")

alldata <- alldata %>%
  mutate(
    occupation_class = case_when(
      isco88 %in% technical_codes     ~ "Technical (semi-)professionals",
      isco88 %in% production_codes    ~ "Production workers",
      isco88 %in% manager_codes       ~ "(Associate) managers",
      isco88 %in% clerk_codes         ~ "Clerks",
      isco88 %in% sociocultural_codes ~ "Socio-cultural (semi-)professionals",
      isco88 %in% service_codes       ~ "Service workers"
    ),
    # isco88 = -10 means "not employed" (retired, students, homemakers,
    # unemployed, ...). Only dw03 = 3 are actually unemployed.
    Occupation = case_when(
      !is.na(occupation_class)  ~ occupation_class,
      not_employed & dw03 == 3  ~ "Unemployed",
      not_employed & dw03 == 2  ~ "Retired",
      not_employed & !is.na(dw03) ~ "Other not employed"
    ),
    Occupation = factor(Occupation, levels = c(occupation_levels, "Unemployed",
                                               "Retired", "Other not employed")),
    occupation_class = factor(occupation_class, levels = occupation_levels)
  )


# -----------------------------------------------------------------------------
# 3. Other variables
# -----------------------------------------------------------------------------

alldata <- alldata %>%
  mutate(
    # Subjective social status (1 = bottom ... 10 = top)
    sss = id03,

    # Vote intention: 1 = AfD / Die Linke, 0 = mainstream party
    afd       = case_when(pv01 == 42 ~ 1, pv01 %in% 1:4 ~ 0),
    die_linke = case_when(pv01 == 6  ~ 1, pv01 %in% 1:4 ~ 0),
    no_vote   = case_when(pv01 == 91 ~ 1, !is.na(pv01) ~ 0),

    # Political alienation (higher = more trust / satisfaction)
    sat_democracy     = 7 - ps03,
    trust_parties     = pt15,
    trust_parliament  = pt03,

    # Social integration
    distrust      = case_when(st01 == 2 ~ 1, st01 == 1 ~ 0),   # "always be cautious"
    no_care       = case_when(lp06 == 1 ~ 1, lp06 == 2 ~ 0),   # "most people don't care about others"
    SocialActivity = factor(case_when(si03 == 1 ~ "Daily", si03 == 2 ~ "Once every week",
                                      si03 == 3 ~ "Once every month", si03 == 4 ~ "Seldom",
                                      si03 == 5 ~ "Never"),
                            levels = c("Never", "Seldom", "Once every month",
                                       "Once every week", "Daily")),

    # Economic variables
    econ_situation = factor(case_when(ep03 == 1 ~ "Very good", ep03 == 2 ~ "Good",
                                      ep03 == 3 ~ "Partly good/partly bad",
                                      ep03 == 4 ~ "Bad", ep03 == 5 ~ "Very bad"),
                            levels = c("Very good", "Good", "Partly good/partly bad",
                                       "Bad", "Very bad")),

    # Cultural variable: 1 = materialist, 0 = post-materialist (mixed types excluded)
    materialist = case_when(ingle == 4 ~ 1, ingle == 1 ~ 0),

    # Key issues (reversed: 5 = strongly agree)
    immigration    = 6 - pa09,   # immigrants should adapt
    redistribution = 6 - pa16,   # redistribution in favour of ordinary people

    # Place of residence
    UrbanRural = factor(case_when(gs01 == 1 ~ "City", gs01 == 2 ~ "Suburb",
                                  gs01 == 3 ~ "Small City", gs01 == 4 ~ "Village",
                                  gs01 == 5 ~ "Countryside"),
                        levels = c("City", "Suburb", "Small City", "Village", "Countryside")),

    # Controls
    female           = case_when(sex == 2 ~ 1, sex == 1 ~ 0),
    age_div10        = age / 10,
    union            = case_when(sm01 == 1 ~ 1, sm01 == 2 ~ 0),
    church           = case_when(rp01 <= 4 ~ 1, rp01 >= 5 ~ 0),   # several times a year or more
    foreign_born     = case_when(dn07 == 2 ~ 1, dn07 == 1 ~ 0),
    east             = case_when(eastwest == 2 ~ 1, eastwest == 1 ~ 0),
    # Higher vocational/academic degree: vocational school, technical school,
    # master craftsman, university of applied sciences, university (de01 = 7-11)
    higher_education = case_when(de01 %in% 7:11 ~ 1, de01 %in% 1:6 ~ 0),
    year_index       = year - 1980 + 1
  ) %>%
  # Income deciles WITHIN each survey year (equivalized income rises with
  # inflation; deciles over all years would mostly measure the survey year)
  group_by(year) %>%
  mutate(income_decile = ntile(di08, 10)) %>%
  ungroup()


# -----------------------------------------------------------------------------
# 4. Helpers: robust standard errors, tables, average predictions
# -----------------------------------------------------------------------------

robust <- function(model) coeftest(model, vcov = vcovHC(model, type = "HC1"))

stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")

# Regression table with HC1 robust standard errors -> CSV
regression_table <- function(models, file) {
  rows <- imap(models, function(m, name) {
    r <- robust(m)
    tibble(term = rownames(r),
           value = sprintf("%.3f%s (%.3f)", r[, 1], stars(r[, 4]), r[, 2]),
           model = name)
  }) %>% bind_rows()
  fit <- imap(models, ~ tibble(term = c("Observations", "R2"),
                               value = c(format(nobs(.x), big.mark = ","),
                                         sprintf("%.3f", summary(.x)$r.squared)),
                               model = .y)) %>% bind_rows()
  tab <- bind_rows(rows, fit) %>%
    pivot_wider(names_from = model, values_from = value, values_fill = "")
  write_csv(tab, file)
  tab
}

# Average predicted value of the outcome for chosen values of one variable
# (all other variables kept at their observed values), with robust 95% CI
average_prediction <- function(model, var, values) {
  mf <- model.frame(model)
  V <- vcovHC(model, type = "HC1")
  map_dfr(values, function(v) {
    d <- mf
    d[[var]] <- if (is.factor(mf[[var]])) factor(v, levels = levels(mf[[var]])) else v
    X <- colMeans(model.matrix(terms(model), d))
    tibble(value = v, fit = sum(X * coef(model)), se = sqrt(drop(t(X) %*% V %*% X)))
  }) %>%
    mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se)
}


# -----------------------------------------------------------------------------
# 5. SSS by occupation over time (Fig. 1) and Tab. 1
# -----------------------------------------------------------------------------
# Descriptive means are weighted with the east-west weight (wghtpew), because
# East Germans are oversampled in ALLBUS since 1991.

fig1 <- alldata %>%
  filter(!is.na(sss), !is.na(occupation_class)) %>%
  group_by(year, occupation_class) %>%
  summarise(mean = weighted.mean(sss, wghtpew), n = n(), .groups = "drop") %>%
  ggplot(aes(year, mean, colour = occupation_class)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(x = "Year", y = "Average SSS", colour = "Occupation",
       title = "Average SSS by occupation")
ggsave("output/fig1_sss_by_occupation.png", fig1, width = 10, height = 5.5, bg = "white")

controls <- "female + age_div10 + union + church + higher_education + UrbanRural + foreign_born + year_index + east"

tab1 <- list(
  `(1)` = lm(sss ~ income_decile + Occupation + higher_education + year_index + east, data = alldata),
  `(2)` = lm(as.formula(paste("sss ~ income_decile + econ_situation + Occupation +", controls)), data = alldata),
  `(3)` = lm(as.formula(paste("sss ~ income_decile + materialist + Occupation +", controls)), data = alldata)
)
regression_table(tab1, "output/tab1_predictors_of_sss.csv")


# -----------------------------------------------------------------------------
# 6. SSS and social integration (Fig. 2-4, Tab. 2)
# -----------------------------------------------------------------------------

share_plot <- function(var, labels, title, file) {
  p <- alldata %>%
    filter(!is.na(sss), !is.na(.data[[var]])) %>%
    group_by(sss, group = factor(.data[[var]], 0:1, labels)) %>%
    summarise(w = sum(wghtpew), .groups = "drop_last") %>%
    mutate(share = w / sum(w)) %>%
    ggplot(aes(factor(sss), share, fill = group)) +
    geom_col(colour = "black") +
    scale_fill_manual(values = c("lightgreen", "powderblue")) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = "SSS", y = NULL, fill = NULL, title = title)
  ggsave(file, p, width = 8, height = 5, bg = "white")
}

share_plot("distrust", c("Most people can be trusted", "Always be cautious"),
           "Trust in people by SSS", "output/fig2_trust_by_sss.png")
share_plot("no_care", c("People care about others", "People don't care about others"),
           "Respect for others by SSS", "output/fig3_respect_by_sss.png")

tab2 <- list(
  Respect  = lm(as.formula(paste("sss ~ no_care + income_decile + Occupation +", controls)), data = alldata),
  Trust    = lm(as.formula(paste("sss ~ distrust + income_decile + Occupation +", controls)), data = alldata),
  Activity = lm(as.formula(paste("sss ~ SocialActivity + income_decile + Occupation +", controls)), data = alldata)
)
regression_table(tab2, "output/tab2_sss_social_integration.csv")

fig4 <- average_prediction(tab2$Activity, "SocialActivity", levels(alldata$SocialActivity)) %>%
  mutate(value = factor(value, levels(alldata$SocialActivity))) %>%
  ggplot(aes(value, fit)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), colour = "red") +
  labs(x = "Social activity", y = "Predicted SSS", title = "Predicted SSS by social activity")
ggsave("output/fig4_predicted_sss_social_activity.png", fig4, width = 8, height = 5, bg = "white")


# -----------------------------------------------------------------------------
# 7. SSS and political alienation (Fig. 5, Tab. 3)
# -----------------------------------------------------------------------------

alienation_controls <- paste("sss + income_decile + Occupation +", controls)
tab3 <- list(
  `Sat. with democracy`   = lm(as.formula(paste("sat_democracy ~", alienation_controls)), data = alldata),
  `Trust in pol. parties` = lm(as.formula(paste("trust_parties ~", alienation_controls)), data = alldata),
  `Trust in parliament`   = lm(as.formula(paste("trust_parliament ~", alienation_controls)), data = alldata),
  `No vote`               = lm(as.formula(paste("no_vote ~", alienation_controls)), data = alldata)
)
regression_table(tab3, "output/tab3_sss_political_alienation.csv")

fig5 <- imap_dfr(tab3[1:3], ~ average_prediction(.x, "sss", 1:10) %>% mutate(outcome = .y)) %>%
  ggplot(aes(value, fit, colour = outcome, fill = outcome)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Subjective social status", y = "Predicted value", colour = NULL, fill = NULL,
       title = "Predicted political trust and satisfaction with democracy by SSS")
ggsave("output/fig5_predicted_alienation.png", fig5, width = 9, height = 5, bg = "white")


# -----------------------------------------------------------------------------
# 8. SSS and voting for radical parties (Fig. 6, Tab. 4)
# -----------------------------------------------------------------------------

party_labels <- c(`1` = "CDU/CSU", `2` = "SPD", `3` = "FDP", `4` = "Greens",
                  `6` = "Die Linke (PDS)", `42` = "AfD")

fig6 <- alldata %>%
  filter(!is.na(sss), pv01 %in% c(1, 2, 3, 4, 6, 42)) %>%
  group_by(year, party = party_labels[as.character(pv01)]) %>%
  summarise(mean = weighted.mean(sss, wghtpew), n = n(), .groups = "drop") %>%
  ggplot(aes(year, mean, colour = party)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = c(`CDU/CSU` = "#6E6E6E", SPD = "#E78D6C", FDP = "#EAC985",
                                 Greens = "#9EBC6A", `Die Linke (PDS)` = "#A585C5", AfD = "#5C8FB8")) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(x = "Year", y = "Average SSS", colour = "Party", title = "Average SSS by vote intention")
ggsave("output/fig6_sss_by_party.png", fig6, width = 10, height = 5.5, bg = "white")

vote_controls <- "Occupation + female + age_div10 + union + church + higher_education + UrbanRural + foreign_born + east"
since_2014 <- filter(alldata, year >= 2014)   # AfD founded in 2013
since_1991 <- filter(alldata, year >= 1991)   # PDS / Die Linke
only_2018  <- filter(alldata, year == 2018)   # immigration and redistribution items

tab4 <- list(
  `AfD (1)`   = lm(afd ~ sss + year_index, data = since_2014),
  `AfD (2)`   = lm(as.formula(paste("afd ~ sss + income_decile +", vote_controls, "+ year_index")), data = since_2014),
  `AfD (3)`   = lm(as.formula(paste("afd ~ sss + redistribution + immigration + income_decile +", vote_controls)), data = only_2018),
  `AfD (4)`   = lm(as.formula(paste("afd ~ sss + econ_situation +", vote_controls, "+ year_index")), data = since_2014),
  `Linke (5)` = lm(die_linke ~ sss + year_index, data = since_1991),
  `Linke (6)` = lm(as.formula(paste("die_linke ~ sss + income_decile +", vote_controls, "+ year_index")), data = since_1991),
  `Linke (7)` = lm(as.formula(paste("die_linke ~ sss + redistribution + immigration + income_decile +", vote_controls, "+ year_index")), data = since_1991),
  `Linke (8)` = lm(as.formula(paste("die_linke ~ sss + econ_situation +", vote_controls, "+ year_index")), data = since_1991)
)
regression_table(tab4, "output/tab4_sss_radical_voting.csv")
