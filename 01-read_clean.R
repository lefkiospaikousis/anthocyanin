
#' Script to read the raw data and clean
#' 
#' 

library(corrr)
library(haven)
library(skimr)
library(labelled)
library(tidyverse)

source("000-variable_info.R")
source("000-helper_functions.R")


set_greek_locale()

dta <- read_sav("Data/anthocyans_raw.sav")

glimpse(dta)

dta <- dta %>% 
  mutate(
    across(where(haven::is.labelled), ~ as_factor(.))
  )

# vtable::vtable(dta, out = "browser")
# 
# dta_summary <- summarytools::dfSummary(dta)
# 
# dta_summary
# 
# view(dta_summary)


# High correlation between weather variables
dta %>% 
  select(vars_weather) %>% 
  correlate() %>% 
  shave()


res_fa <- psych::fa(dta[vars_weather] , nfactors = 1, rotate = "oblimin", fm = "minres")


fa_scores <- as.numeric(res_fa$scores)


dta$weather <- fa_scores

var_label(dta$weather) <- "Overall Weather Condition (FA score)"

saveRDS(dta, "Data/anthocyans-clean.rds")
haven::write_sav(dta, "Data/anthocyans-clean.sav")
write.csv(dta, "Data/anthocyans-clean.csv", row.names = FALSE)
