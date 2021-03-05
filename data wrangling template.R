## ----data:attach_packages, warning=FALSE, message=FALSE------------------
# Load required packages from local library into R session.

library(rattle)       # normVarNames().
library(readr)        # Efficient reading of CSV data.
library(dplyr)        # Data wrangling, glimpse() and tbl_df().
library(tidyr)        # Prepare a tidy dataset, gather().
library(magrittr)     # Pipes %>% and %T>% and equals().
library(glue)         # Format strings.
library(lubridate)    # Dates and time.
library(FSelector)    # Feature selection, information.gain().
library(stringi)      # String concat operator %s+%.
library(stringr)      # String operations.
library(randomForest) # Impute missing values with na.roughfix().
library(ggplot2)      # Visualise data.

## ----data:rattle_csv_togaware--------------------------------------------
# Note the source location of a dataset to ingest into R.

dspath <- "http://rattle.togaware.com/weatherAUS.csv"

## ----data:display_dspath-------------------------------------------------
dspath

## ----data:weather_dataset_load-------------------------------------------
# Ingest the dataset.

weather <- read_csv(file=dspath)

## ----data:weather_print, out.lines=NULL----------------------------------
# Print the dataset in a human useful way.

weather

## ----data:weather_glimpse, out.lines=NULL--------------------------------
# A quick view of the contents of the dataset.

glimpse(weather)

## ----data:alternative_dataset_assignment, eval=FALSE---------------------
## # Take a copy of the dataset into a generic variable.
## 
## ds <- weather

## ----data:prepare_dataset------------------------------------------------
# Prepare for a templated analysis and processing.

dsname <- "weather"
ds     <- get(dsname)
glimpse(ds)

## ----data:changes_change_cell--------------------------------------------
weather[1,3] <- 5

## ----data:changes_changed------------------------------------------------
location(weather)
location(ds)

## ----data:changed_changes, out.lines=NULL--------------------------------
changes(weather, ds)

## ----data:review_variable_names, message=FALSE---------------------------
# Review the variables to consider normalising their names.

names(ds)

## ----data:normalise_variable_names, message=FALSE------------------------
# Normalise the variable names.

names(ds) %<>% normVarNames() %T>% print() 

## ----data:norm_changes---------------------------------------------------
location(weather)
location(ds)

## ----data:string_process_variable_names, eval=FALSE----------------------
## names(ds) %<>% str_replace("^[^_]*_", "")

## ----data:string_proces_prefix_variable_name-----------------------------
str_replace("ab123_tax_payable", "^[^_]*_", "") 

## ----data:glimpse_ds, out.lines=NULL-------------------------------------
# Review the dataset.

glimpse(ds)

## ----data:head_ds, out.lines=11------------------------------------------
# Review the first few observations.

head(ds) %>% print.data.frame()

## ----data:tail_ds, out.lines=11------------------------------------------
# Review the last few observations.

tail(ds) %>% print.data.frame()

## ----data:sample_ds, out.lines=11----------------------------------------
# Review a random sample of observations.

sample_n(ds, size=6) %>% print.data.frame()

## ----data:char_vars------------------------------------------------------
# Identify the character variables by index.

ds %>%
  sapply(is.character) %>%
  which() %T>%
  print() ->
  chari

# Identify the chracter variables by name.

ds %>% 
  names() %>% 
  '['(chari) %T>% 
  print() ->
  charc

## ----data:character_levels, out.lines=25---------------------------------
# Observe the unique levels.

ds[charc] %>% sapply(unique)

## ----data:character_to_factor, eval=FALSE--------------------------------
## # Convert all chracter variables to be factors.
## 
## ds[charc] %<>% map(factor)

## ----data:single_location------------------------------------------------
# How many locations are represented in the dataset.

ds$location %>% 
  unique() %>%
  length()

## ----data:list_locations, out.lines=24-----------------------------------
ds$location %>%
  table()

## ----data:cvars_na, out.lines=NULL---------------------------------------
# Note the character remaining variables to be dealt with.

head(ds$evaporation)
head(ds$sunshine)

# Review other random values.

sample(ds$evaporation, 8)
sample(ds$sunshine, 8)

## ----data:cvars_convert--------------------------------------------------
# Identify the vairables to process.

cvars <- c("evaporation", "sunshine")

# Check the current class of the variables.

ds[cvars] %>% sapply(class)

# Convert to numeric.

ds[cvars] %<>% sapply(as.numeric)

# Review some random values.

sample(ds$evaporation, 10)
sample(ds$sunshine, 10)

## ----data:wind_gust_dirs, out.lines=20-----------------------------------
# Review the distribution of observations across levels.

ds %>%
  select(contains("_dir")) %>%
  sapply(table)

## ----data:wind_dir_to_factor---------------------------------------------
# Levels of wind direction are ordered compas directions.

compass <- c("N", "NNE", "NE", "ENE",
             "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW",
             "W", "WNW", "NW", "NNW")

## ----data:wind_dir_ordered_factor----------------------------------------
# Note the names of the wind direction variables.

ds %>% 
  select(contains("_dir")) %>% 
  names() %T>%
  print() ->
  vnames

# Convert these variables from character to factor.

ds[vnames] %<>% 
  lapply(factor, levels=compass, ordered=TRUE) %>% 
  data.frame() %>% 
  tbl_df()
# Confirm they are now factors.

ds[vnames] %>% sapply(class)

## ----data:wind_variables_table_factor, out.lines=12----------------------
# Verify the distribution has not changed.

ds %>%
  select(contains("_dir")) %>%
  sapply(table)

## ----data:rain_variables_table, out.lines=NULL---------------------------
# Review the distribution of observations across levels.

ds %>%
  select(starts_with("rain_")) %>%
  sapply(table)

## ----data:rain_variables_to_factor---------------------------------------
# Note the names of the rain variables.

ds %>% 
  select(starts_with("rain_")) %>% 
  names() ->
  vnames

# Confirm these are currently character variables.

ds[vnames] %>% sapply(class)

# Convert these variables from character to factor.

ds[vnames] %<>% 
  lapply(factor) %>% 
  data.frame() %>% 
  tbl_df()

# Confirm they are now factors.

ds[vnames] %>% sapply(class)

## ----data:summary_numeric, out.lines=16----------------------------------
ds %>%
  sapply(is.numeric) %>%
  which() %>%
  names %T>%
  print() ->
  numi


ds[numi] %>% 
  summary()

## ----data:glimpse_summary_factor-----------------------------------------
ds %>% 
  select(rain_today, rain_tomorrow) %>%
  summary()

## ----data:vars, out.lines=NULL-------------------------------------------
# Note the available variables.

vars <- names(ds) %T>% print()

## ----data:target---------------------------------------------------------
# Note the target variable.

target <- "rain_tomorrow"

# Place the target variable at the beginning of the vars.

vars <- c(target, vars) %>% unique() %T>% print()

## ----data:risk-----------------------------------------------------------
# Note the risk variable - measures the severity of the outcome.

risk <- "risk_mm"

## ----data:no_target_no_risk, out.lines=NULL------------------------------
# Review the distribution of the risk variable for non-targets.

ds %>%
  filter(rain_tomorrow == "No") %>%
  select(risk_mm) %>%
  summary()

## ----data:yes_target_no_risk, out.lines=NULL-----------------------------
# Review the distribution of the risk variable for targets.

ds %>%
  filter(rain_tomorrow == "Yes") %>%
  select(risk_mm) %>%
  summary()

## ----data:id-------------------------------------------------------------
# Note any identifiers.

id <- c("date", "location")

## ----data:id_distributions-----------------------------------------------
ds[id] %>%
  group_by(location) %>%
  count() %>%
  rename(days=n) %>%
  mutate(years=round(days/365)) %>%
  as.data.frame() %>%
  sample_n(10)

## ----data:id_years_distributions, out.lines=8----------------------------
ds[id] %>%
  group_by(location) %>%
  count() %>%
  rename(days=n) %>%
  mutate(years=round(days/365)) %>%
  ungroup() %>%
  select(years) %>%
  summary()

## ----data:ignore---------------------------------------------------------
# Initialise ignored variables: identifiers and risk.

ignore <- union(id, risk) %T>% print()

## ----data:ids------------------------------------------------------------
# Heuristic for candidate indentifiers to possibly ignore.

ds[vars] %>%
  sapply(function(x) x %>% unique() %>% length()) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  ids

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, ids) %T>% print()

## ----data:ignore_missing_variables---------------------------------------
# Identify variables with only missing values.

ds[vars] %>%
  sapply(function(x) x %>% is.na %>% sum) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  missing

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, missing) %T>% print()

## ----ignore_mostly_missing_variables-------------------------------------
# Identify a threshold above which proportion missing is fatal.

missing.threshold <- 0.7

# Identify variables that are mostly missing.

ds[vars] %>%
  sapply(function(x) x %>% is.na() %>% sum()) %>%
  '>'(missing.threshold*nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  mostly

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, mostly) %T>% print()

## ----ignore_factors_with_many_levels-------------------------------------
# Identify a threshold above which we have too many levels.

levels.threshold <- 20

# Identify variables that have too many levels.

ds[vars] %>%
  sapply(is.factor) %>%
  which() %>%
  names() %>%
  sapply(function(x) ds %>% extract2(x) %>% levels() %>% length()) %>%
  '>='(levels.threshold) %>%
  which() %>%
  names() %T>%
  print() ->
  too.many

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, too.many) %T>% print()

## ----ignore_variables_constant_values------------------------------------
# Identify variables that have a single value.

ds[vars] %>%
  sapply(function(x) all(x == x[1L])) %>%
  which() %>%
  names() %T>%
  print() ->
  constants

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, constants) %T>% print()

## ----out.lines=10--------------------------------------------------------
# Note which variables are numeric.

vars %>%
  setdiff(ignore) %>%
  extract(ds, .) %>%
  sapply(is.numeric) %>% 
  which() %>%
  names() %T>%
  print() ->
  numc

## ----data:calculate_correlations-----------------------------------------
# For the numeric variables generate a table of correlations

ds[numc] %>%
  cor(use="complete.obs") %>%
  ifelse(upper.tri(., diag=TRUE), NA, .) %>% 
  abs %>% 
  data.frame %>%
  tbl_df %>%
  set_colnames(numc) %>%
  mutate(var1=numc) %>% 
  gather(var2, cor, -var1) %>% 
  na.omit %>%
  arrange(-abs(cor)) %T>%
  print() ->
  mc

## ----data:initialise_ignore----------------------------------------------
# Note the correlated variables that are redundant.

correlated <- c("temp_3pm", "pressure_3pm", "temp_9am")

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, correlated) %T>% print()


## ----data:remove_variables-----------------------------------------------
# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore)

# Confirm they are now ignored.

length(vars)

## ----data:Fselector, out.lines=NULL--------------------------------------
# Construct the formulation of the modelling we plan to do.

form <- formula(target %s+% " ~ .") %T>% print()

# Use correlation search to identify key variables.

cfs(form, ds[vars])

# Use information gain to identify variable importance.

information.gain(form, ds[vars])

## ----remove_missing_target-----------------------------------------------
# Check the dimensions to start with.

dim(ds)

# Identify observations with a missing target.

missing.target <- ds %>% extract2(target) %>% is.na()

# Check how many are found.

sum(missing.target)

# Remove observations with a missing target.

ds %<>% filter(!missing.target)

# Confirm the filter delivered the expected dataset.

dim(ds)

## ----data:copy_original--------------------------------------------------
# Backup the dataset so we can restore it as required.

ods <- ds

## ----impute_missing_values-----------------------------------------------
# Count the number of missing values.

ds[vars] %>%  is.na() %>% sum()

# Impute missing values.

ds[vars] %<>% na.roughfix()

# Confirm that no missing values remain.

ds[vars] %>%  is.na() %>% sum()

## ----data:resotre_original-----------------------------------------------
# Restore the original dataset.

ds <- ods

## ----data:initialise_omit------------------------------------------------
# Backup the dataset so we can restore it as required.

ods <- ds

# Initialise the list of observations to be removed.

omit <- NULL

## ----remove_missing_values-----------------------------------------------
# Review the current dataset.

ds[vars] %>% nrow()
ds[vars] %>% is.na() %>% sum()

# Identify any observations with missing values.

mo <- attr(na.omit(ds[vars]), "na.action")

# Record the observations to omit.

omit <- union(omit, mo)

# If there are observations to omit then remove them.

if (length(omit)) ds <- ds[-omit,]

# Confirm the observations have been removed.

ds[vars] %>% nrow()
ds[vars] %>% is.na() %>% sum()

## ----data:restore_omit---------------------------------------------------
# Restore the original dataset.

ds <- ods

## ----normalise_factors---------------------------------------------------
# Note which variables are categoric.

ds %>%
  sapply(is.factor) %>%
  which() ->
  catc

# Normalise the levels of all categoric variables.

for (v in catc) 
  levels(ds[[v]]) %<>% normVarNames()

## ----ensure_target_is_categoric------------------------------------------
# Ensure the target is categoric.

ds[[target]] %<>% as.factor()

# Confirm the distribution.

ds[target] %>% table()

## ----data:plot_target_distribution, fig.pos='h', fig.height=3, fig.cap="Target variable distribution. Plotting the distribution is useful to gain an insight into the number of observations in each category. As is the case here we often see a skewed distribution."----
ds %>%
  ggplot(aes_string(x=target)) +
  geom_bar(width=0.2, fill="grey") +
  theme(text=element_text(size=14))

## ----identify_variables_inputs-------------------------------------------
inputs <- setdiff(vars, target) %T>% print()

## ----identify_variables_inputi-------------------------------------------
inputi <- sapply(inputs, 
                 function(x) which(x == names(ds)), 
                 USE.NAMES=FALSE)
inputi

## ----number_of_observations----------------------------------------------
nobs <- nrow(ds) %T>% print()

## ----dimensions----------------------------------------------------------
dim(ds)
dim(ds[vars])
dim(ds[inputs])

## ----identify_variable_types---------------------------------------------
# Identify the numeric variables by index.

ds %>%
  sapply(is.numeric) %>%
  which() %>%
  intersect(inputi) %T>%
  print() ->
  numi

# Identify the numeric variables by name.

ds %>% 
  names() %>% 
  extract(numi) %T>% 
  print() ->
  numc


# Identify the categoric variables by index and then name.

ds %>%
  sapply(is.factor) %>%
  which() %>%
  intersect(inputi) %T>%
  print() ->
  cati

ds %>% 
  names() %>% 
  extract(cati) %T>% 
  print() ->
  catc

## ----data:save_dataset---------------------------------------------------
# Timestamp for the dataset.

dsdate  <- "_" %s+% format(Sys.Date(), "%y%m%d") %T>% print()

# Filename for the saved dataset

dsrdata <- dsname %s+% dsdate %s+% ".RData" %T>% print()

# Save relevant R objects to binary RData file.

save(ds, dsname, dspath, dsdate, nobs,
     vars, target, risk, id, ignore, omit, 
     inputi, inputs, numi, numc, cati, catc, 
     file=dsrdata)

## ----data:load_dsrdata---------------------------------------------------
load(dsrdata) %>% print()

## ----echo=FALSE, eval=FALSE----------------------------------------------
## ds <- read_csv("http://HandsOnDataScience.com/data/ferry.csv")
## 
## # Some possible questions from Tony
## 
## # is there a difference between years, and seasons, or when they travel ,
## # even questions like, do they travel later in winter than in summer, etc
## 
## # plus we can factor in weather, and location, etc  so do more people travel one way, than return, etc
## 
## # do people in the eastern suburbs travel more than people in the western suburbs,
## 
## # also some ideas about the boats , cause they are different types of ferrysw
## 
## # on the greenwhich run, they change ferries after the first run, etc
