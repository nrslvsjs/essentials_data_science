# Welcome to the Togaware Data Science Data Template ----
# XX
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# General Setup.
#
# File: 00_setup.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# Before our own journey into literate programming we can
# make use of these R scripts as our templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Data Science in R from CRC Press (pending).
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

# Load required packages from local library into R.

library(tidyverse)    # ggplot2, tibble, tidyr, readr, purr, dplyr
library(rattle)       # comcat(), weatherAUS, normVarNames().
library(magrittr)     # Pipe operator %>% %<>% %T>% equals().
library(lubridate)    # Dates and time.
library(stringi)      # String concat operator %s+%.
library(stringr)      # String manipulation: str_replace().
library(randomForest) # Impute missing values with na.roughfix()
library(FSelector)    # Feature selection: information.gain().
library(scales)       # Include commas in numbers.
library(xtable)       # Generate LaTeX tables.

# Welcome to the Togaware Data Science Data Template
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# Data Ingestion.
#
# File: 10_ingest.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# If you are not familiar yet with literate programming you can
# make use of these R scripts as templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Literate Data Science Using R from CRC Press.
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

# Name of the dataset.

dsname <- "weatherAUS"
dspath <- "data/."

# Name of the dataset.

dsname <- "weatherAUS"

# Identify the source location of the dataset.

dsloc <- "data"

# Alternatives might include:
#
# dsloc <- "C:/Users/graham/Projects"
# dsloc <- "~/projects"
# dsloc <- "http://rattle.togaware.com"

# Construct the path to the dataset and display some if it.

dsname %s+% ".csv" %>%
  file.path(dsloc, .) %T>%
  cat("Dataset:", ., "\n\n") %T>%
  {
    paste("head", .) %>%
      system(intern=TRUE) %>%
      sub("\r", "\n", .) %>%
      print()
  } ->
  dspath

# Ingest the dataset.

dspath %>%
  read_csv() ->
  weatherAUS

weatherAUS

# Prepare the dataset for usage with our template.

ds <- get(dsname)

# Save the dataset to disk as a binary R Data for backup.

fpath <- "data"

dsname %s+% ".RData" %>%
  file.path(fpath, .) %T>%
  cat("Saving:", ., "\n\n") ->
  fname

if (! dir.exists(fpath))  dir.create(fpath)
if (! file.exists(fname)) save(weatherAUS, file=fname)

# Remove the original dataset to save on memory.

rm(weatherAUS)

# Test the loading of the saved dataset.

load(fname) %>% print()

# Cleanup to save on memory.

rm(weatherAUS)

# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# Data Observation.
#
# File: 20_observe.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# Before our own journey into literate programming we can
# make use of these R scripts as our templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Data Science in R from CRC Press (pending).
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

# A glimpse into the dataset.

glimpse(ds)

# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# Data Wrangling.
#
# File: 30_prepare.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# Before our own journey into literate programming we can
# make use of these R scripts as our templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Data Science in R from CRC Press (pending).
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

# Review the variables to optionally normalise their names.

names(ds)

# Capture the original variable names for use later on.

vnames <- names(ds)

# Normalise the variable names.

names(ds) %<>% normVarNames() %T>% print()

# Index the original variable names by the new names.

names(vnames) <- names(ds)

# We can then obtain the original variable name.

cat(vnames["min_temp"])

dspath %>%
  read_csv() %>%
  set_names(names(.) %>% normVarNames()) ->
  weatherAUS

weatherAUS

# Glimpse the dataset.

glimpse(ds)

# Review the first few observations.

head(ds) %>% print.data.frame()

# Review the last few observations.

tail(ds) %>% print.data.frame()

# Review a random sample of observations.

sample_n(ds, size=6) %>% print.data.frame()

# Traditional dataset summary to get started.

summary(ds)

# Date data type conversion (if required). Set the appropriate date format.

head(ds$date) %>% as.character() %>% class()
ds$date %<>% as.character() %>% ymd() %>% as.Date()
class(ds$date)
head(ds$date)

# How many locations are represented in the dataset.

ds$location %>% 
  unique() %>%
  length()

# Review the distribution of observations across levels.

ds %>%
  select(starts_with("rain_")) %>%
  sapply(table)

# Note the  names of the rain variables.

ds %>% 
  select(starts_with("rain_")) %>% 
  names() %T>%
  print() ->
  vrain

# Confirm these are currently character variables.

ds[vrain] %>% sapply(class)

# Choose to convert these variables from character to factor.

ds[vrain] %<>% 
  lapply(factor) %>% 
  data.frame() %>% 
  tbl_df()

# Confirm they are now factors.

ds[vrain] %>% sapply(class)

# Verify the distribution has not changed.

ds %>%
  select(starts_with("rain_")) %>%
  sapply(table)

ds %>% 
  select(contains("_dir")) %>%
  names() %>%
  paste(collapse="|, \\verb|") %>%
  paste0("\\verb|", . , "|") %>%
  str_replace(", (\\\\\\verb[^,]+)$", ", and \\1") ->
  wgvars

# Review the distribution of observations across levels.

ds %>%
  select(contains("_dir")) %>%
  sapply(table)

# Levels of wind direction are ordered compas directions.

compass <- c("N", "NNE", "NE", "ENE",
             "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW",
             "W", "WNW", "NW", "NNW")

# Note the names of the wind direction variables.

ds %>% 
  select(contains("_dir")) %>% 
  names() %T>%
  print() ->
  vdir

# Confirm these are currently character variables.

ds[vdir] %>% sapply(class)

# Convert these variables from character to factor.

ds[vdir] %<>% 
  lapply(factor, levels=compass, ordered=TRUE) %>% 
  data.frame() %>% 
  tbl_df()

# Confirm they are now factors.

ds[vdir] %>% sapply(class)

# Verify the distribution has not changed.

ds %>%
  select(contains("_dir")) %>%
  sapply(table)

# Note the character remaining variables to be dealt with.

cvars <- c("evaporation", "sunshine")

# Review the values.

head(ds[cvars])
tail(ds[cvars])
sample_n(ds[cvars], 6)

# Check the current class of the variables.

ds[cvars] %>% sapply(class)

# Convert to numeric.

ds[cvars] %<>% sapply(as.numeric)

# Confirm the conversion.

ds[cvars] %>% sapply(class)

## Identifiers and Targets ----------------

# Note the available variables.

ds %>%
  names() %T>%
  print() ->
  vars

# Note the target variable.

target <- "rain_tomorrow"

# Place the target variable at the beginning of the vars.

c(target, vars) %>%
  unique() %T>%
  print() ->
  vars

# Note the risk variable - measures the severity of the outcome.

risk <- "risk_mm"

# Note any identifiers.

id <- c("date", "location")

## Generic Data Wrangling ----------------

# Initialise ignored variables: identifiers and risk.

ignore <- union(id, if (exists("risk")) risk) %T>% print()

# Heuristic for indentifiers to possibly ignore.

ds[vars] %>%
  sapply(function(x) x %>% unique() %>% length()) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  ids

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, ids) %T>% print()

# Identify variables with only missing values.

ds[vars] %>%
  sapply(function(x) x %>% is.na %>% sum) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  missing

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, missing) %T>% print()

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

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, mostly) %T>% print()

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

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, too.many) %T>% print()

# Identify variables that have a single value.

ds[vars] %>%
  sapply(function(x) all(x == x[1L])) %>%
  which() %>%
  names() %T>%
  print() ->
  constants 

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, constants) %T>% print()

# Note which variables are numeric.

vars %>%
  setdiff(ignore) %>%
  '['(ds, .) %>%
  sapply(is.numeric) %>% 
  which() %>%
  names() %T>%
  print() ->
  numc

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

# Any variables could be removed because highly correlated?

correlated <- c("temp_3pm", "pressure_3pm", "temp_9am")

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, correlated) %T>% print()

# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore) %T>% print()

# Confirm they are now ignored.

length(vars)

## Variable Selection ----------------

# Formula for modelling.

form <- formula(target %s+% " ~ .") %T>% print()

# Use correlation search to identify key variables.
# Could be useful to decide which variables to retain.

cfs(form, ds[vars])

# Any variables to remove because not useful?

vars %<>% setdiff(NULL) %T>% print()

# Use information gain to identify variable importance.

information.gain(form, ds[vars]) %>%
  rownames_to_column() %>%
  arrange(attr_importance)

# Any variables to remove because not useful?

vars %<>% setdiff(NULL) %T>% print()

## Further Wrangling ----------------

# Check the dimensions to start with.

dim(ds) %>% comcat()

# Identify observations with a missing target.

ds %>% 
  extract2(target) %>% 
  is.na() %T>%
  {sum(.) %>% comcat()} ->
  missing.target 

# Remove observations with a missing target.

ds %<>% filter(!missing.target)

# Confirm the filter delivered the expected dataset.

dim(ds) %>% comcat()

## Optional: Missing Value Imputation ----------------

# Count the number of missing values.

ds[vars] %>%  is.na() %>% sum() %>% comcat()

# Impute missing values.

ds[vars] %<>% na.roughfix()

# Confirm that no missing values remain.

ds[vars] %>%  is.na() %>% sum() %>% comcat()

## Optional: Remove Observations With Missing Values ----------------

# Initialise the list of observations to be removed.

omit <- NULL

# Review the current dataset.

ds[vars] %>% nrow() %>% comcat()
ds[vars] %>% is.na() %>% sum() %>% comcat()

# Identify any observations with missing values.

ds[vars] %>%
  na.omit() %>%
  attr("na.action") %T>%
  print() ->
  mo

# Record the observations to omit.

omit <- union(omit, mo) %T>% {length(.) %>% print()}

# If there are observations to omit then remove them.

if (length(omit)) ds <- ds[-omit,]

# Confirm the observations have been removed.

ds[vars] %>% nrow() %>% comcat()
ds[vars] %>% is.na() %>% sum() %>% comcat()

## Normalise Factors ----------------

# Note which variables are categoric.

ds[vars] %>%
  sapply(is.factor) %>%
  which() %>%
  names() %T>%
  print() ->
  catc

# Check the levels.

ds[catc] %>% sapply(levels)

# Normalise the levels of all categoric variables.

for (v in catc) 
  levels(ds[[v]]) %<>% normVarNames()

# Review the levels.

ds[catc] %>% sapply(levels)

## Categoric Target ----------------

# Ensure the target is categoric.

class(ds[[target]])

ds[[target]] %<>% as.factor()

# Confirm the distribution.

ds[target] %>% table()

ds %>%
  ggplot(aes_string(x=target)) +
  geom_bar(width=0.2, fill="grey") +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=14))

## Numeric Target - Alternative ----------------

# Ensure the target is numeric.

class(ds[[target]])

ds[[target]] %<>% as.numeric()

# Confirm the distribution.

ds[target] %>% summary()

ds %>%
  ggplot(aes_string(x=target)) +
  geom_histogram(fill="grey", col="black", binwidth=20) +
  theme(text=element_text(size=14))

# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# Data Preparation.
#
# File: 40_meta.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# Before our own journey into literate programming we can
# make use of these R scripts as our templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Data Science in R from CRC Press (pending).
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

#### META DATA --------------------------------

# Identify the input variables by name.

inputs <- setdiff(vars, target) %T>% print()

# Identify the input variables by index.

inputi <- sapply(inputs, 
                 function(x) which(x == names(ds)), 
                 USE.NAMES=FALSE) %T>% print()

# Record the number of observations.

nobs <- nrow(ds) %T>% comcat()

# Confirm various subset sizes.

dim(ds)         %>% comcat()
dim(ds[vars])   %>% comcat()
dim(ds[inputs]) %>% comcat()
dim(ds[inputi]) %>% comcat()

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
  '['(numi) %T>% 
  print() ->
  numc

# Identify the categoric variables by index.

ds %>%
  sapply(is.factor) %>%
  which() %>%
  intersect(inputi) %T>%
  print() ->
  cati

# Identify the categoric variables by name.

ds %>% 
  names() %>% 
  '['(cati) %T>% 
  print() ->
  catc

# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Australian Weather Dataset.
# Cache the Data to Disk.
#
# File: 50_save.R
#
# This template provides a starting point for the 
# data scientist exploring a new dataset. By no means
# is it the end point of the data science journey.
# 
# This R script is automatically extracted from a knitr
# file with a .Rnw extension. That file includes a broader 
# narrative and explanation of the journey through our data.
# Before our own journey into literate programming we can
# make use of these R scripts as our templates for data science.
# 
# The template is under regular revision and improvement
# and is provided as is. It is published as an appendix to the 
# book, Quick Start Data Science in R from CRC Press (pending).
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION

#### SAVE THE DATASET --------------------------------

# Timestamp for the dataset - this is the general approach.

dsdate  <- "_" %s+% format(Sys.Date(), "%Y%m%d") %T>% print()

# We will use a fixed timestamp to identify our file for convenience.

dsdate <- "_20180702"

# Filename for the saved dataset.

dsrdata <- 
  file.path(fpath, dsname %s+% dsdate %s+% ".RData") %T>% 
  print()

# Save relevant R objects to the binary RData file.

save(ds, dsname, dspath, dsdate, nobs,
     vars, target, risk, id, ignore, omit, 
     inputi, inputs, numi, numc, cati, catc, 
     file=dsrdata)
