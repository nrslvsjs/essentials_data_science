library(FSelector)
library(dplyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(rattle)
library(readr)
library(scales)
library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(magrittr)


##DATA INGESTION

#identify source location of dataset
dspath <- "http://rattle.togaware.com/weatherAUS.csv"

#ingest data set
weatherAUS <- read_csv(file = dspath)

#copy data set into generic variable
ds <- weatherAUS

#prepare dataset for usage with template
dsname <- "weatherAUS"
ds <- get(dsname)

#print dataset
ds

#basic size information
dim(ds) %>% comcat()
nrow(ds) %>% comcat()
ncol(ds) %>% comcat()

#quickview
glimpse(ds)

#identify variables of dataset
names(ds)

#review variables before noramalizing names
names(ds)

#normalize names
names(ds) %<>% normVarNames()
#confirm
names(ds)

##DATA REVIEW

#review dataset
glimpse(ds)

#review first few observations
head(ds)
#review last few
tail(ds)

#review random sample of observations
set.seed(2)
sample_n(ds, size = 6)


##DATA CLEANING

#how many locations represented in dataset?
ds$location %>%
  unique() %>%
  length()
#convert to factor
ds$location %<>% as.factor()
##review table of distribution of locations
table(ds$location)

###reclassify logical vectors as numeric
#check variable types
#glimpse(ds)
#ds$evaporation <- as.numeric(ds$evaporation)
#ds$sunshine <- as.numeric(ds$sunshine)

#review distribution of observations across levels
ds %>%
  select(starts_with("rain_")) %>%
  sapply(table)
#note name of rain variables
ds %>%
  select(starts_with("rain_")) %>%
  names() %T>%
  print() ->
  vnames
#confirm they are character variables
ds[vnames] %>% sapply(class)
#convert from character to factor
ds[vnames] %<>%
  lapply(factor) %>%
  data.frame() %>%
  as_tibble() %T>%
  {sapply(., class) %>% print()}
#verify distribution has not changed
ds %>%
  select(starts_with("rain_")) %>%
  sapply(table)

glimpse(ds)

#review distribution of observations across levels
ds %>%
  select(contains("_dir")) %>%
  sapply(table)
#levels of wind direction are ordered compass directions
compass <- c("N", "NNE", "NE", "ENE",
             "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW",
             "W", "WNW", "NW", "NNW")
#note name of wind direction variables
ds %>%
  select(contains("_dir")) %>%
  names() %T>%
  print() ->
  vnames
#confirm they are character variables
ds[vnames] %>% sapply(class)
#convert from character to vector
ds[vnames] %<>%
  lapply(factor, levels = compass, ordered = TRUE) %>%
  data.frame() %>%
  as_tibble() %T>%
  {sapply(., class) %>% print()}
#verify distribution has not changed
ds %>%
  select(contains("_dir")) %>%
  sapply(table)

#note remaining character variables to be dealt with
cvars <- c("evaporation", "sunshine")
#review their values
head(ds[cvars])
sample_n(ds[cvars], 6)
#check current class of variables
ds[cvars] %>% sapply(class)
#convert to numeric
ds[cvars] %<>% sapply(as.numeric)
#confirm conversion
ds[cvars] %>% sapply(class)
###evaporation and sunshine are logical so not converted to numeric

#normalize factors
#note which variables are categorical
ds %>%
  sapply(is.factor) %>%
  which() %T>%
  print() ->
  catc
#normalize levels of all categorical variables
for(v in catc) levels(ds[[v]]) %<>% normVarNames()
#confirm
glimpse(ds[catc])

#note the target variable
target <- "rain_tomorrow"
#ensure target is categorical
ds[[target]] %<>% as.factor()

glimpse(ds)

#confirm distribution
ds[target] %>% table()
#visualize distribution of the target
ds %>%
  ggplot(aes_string(x = target)) +
  geom_bar(width = 0.2, fill = "grey") +
  theme(text = element_text(size = 14)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of rain tomorrow",
       x = "Rain tomorrow",
       y = "Count",
       caption = "source:  weatherAUS")

##VARIABLE ROLES
#note available variables
ds %>%
  names() %T>%
  print() ->
  vars
#place target variable first in vars
c(target, vars) %>%
  unique() %T>%
  print() ->
  vars
#note the risk variable - measures the severity of the outcome
risk <- "risk_mm"
#note any identifiers
id <- c("date", "location")

##FEATURE SELECTION
#initialize ignored variables:  identifiers and risk
id %>%
  union(if (exists("risk")) risk) %T>%
  print() ->
  ignore
#helper function to count number of distinct values
count_unique <- function(x)
{
  x %>% unique() %>% length()
}
#heuristic candidate identifiers to possibly ignore
ds[vars] %>%
  sapply(count_unique) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  ids
#add ids to variables ignored for modelling
ignore <- union(ignore, ids) %T>% print()

#engineer data to illustrate identifier selection
ods <- ds #take backup copy of original dataset
ds %<>% filter(location == "sydney")

ds[vars] %>%
  sapply(count_unique) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names()

ds <- ods #resore original dataset

#remove all missing
#helper function to count missing values
count_na <- function(x)
{
  x %>% is.na() %>% sum()
}
#identify variables with missing values
ds[vars] %>%
  sapply(count_na) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  missing
#add to variables to be ignored for modelling
ignore %<>% union(missing) %T>% print()

#engineer dataset to illustrate missing columns
ods <- ds #backup copy of dataset
ds %<>% filter(location == "albury")
ds[vars] %>%
  sapply(count_na) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names()
ds <- ods #restore dataset

#many missing values
#identify proportion above which proportion missing is fatal
missing.threshold <- 0.8
#identify variables that are mostly missing
ds[vars] %>%
  sapply(count_na) %>%
  '>' (missing.threshold * nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  mostly
#add variables to ignored ### did not do this because evaporation and sunshine are important for further analysis
ignore <- union(ignore, mostly) %T>% print()

#too many levels
#helper function
count_levels <- function(x)
{
  ds %>% extract2(x) %>% levels() %>% length()
}
#identify threshold 
levels.threshold <- 20
#identify variables with too many levels
ds[vars] %>%
  sapply(is.factor) %>%
  which() %>%
  names() %>%
  sapply(count_levels) %>%
  '>' (levels.threshold) %>%
  which() %>%
  names() %T>%
  print() ->
  too.many
#add to ignored
ignore <- union(ignore, too.many) %T>% print()

#ignore variable with constants
#helper function
all_same <- function(x)
{
  all(x == x[1L])
}
#identify variables
ds[vars] %>%
  sapply(all_same) %>%
  which() %>%
  names() %T>%
  print() ->
  constants
#add to ignored
ignore <- union(ignore, constants) %T>% print()

#correlated variables
#note numeric variables
vars %>%
  setdiff(ignore) %>%
  magrittr::extract(ds, .) %>%
  sapply(is.numeric) %>%
  which() %>%
  names() %T>%
  print() ->
  numc
#generate table of correlations
ds[numc] %>%
  cor(use = "complete.obs") %>%
  ifelse(upper.tri(., diag = TRUE), NA, .) %>%
  abs() %>%
  data.frame() %>%
  as_tibble() %>%
  set_colnames(numc) %>%
  mutate(var1 = numc) %>%
  gather(var2, cor, -var1) %>%
  na.omit() %>%
  arrange(-abs(cor)) %T>%
  print() ->
  mc
#note correlated variables that are redundant
correlated <- c("temp_3pm", "pressure_3pm", "temp_9am")
#add to ignored
ignore <- union(ignore, correlated) %T>% print()

#removing variables
#check number of variables currently
length(vars)
#remove variables to ignore
vars %<>% setdiff(ignore) %T>% print()
#confirm
length(vars)

#algorithmic feature selection
#construct formulation of modelling to undertake
form <- formula(target %s+% " ~ .") %T>% print()
#use correlation to identify key variables
cfs(form, ds[vars])
#use information gain to identify variable importance
information.gain(form, ds[vars]) %>%
  rownames_to_column("variable") %>%
  arrange(-attr_importance)

##missing data
#check dimensions
dim(ds)
#identify observations with a missing target
missing_target <- ds %>% extract2(target) %>% is.na()
#check how many
sum(missing_target)
#remove
ds %<>% filter(!missing_target)
#confirm filter delivered expected dataset
dim(ds)


#impute missing values
#backup dataset
ods <- ds
#count number missing
ds[vars] %>% is.na() %>% sum() %>% comcat()
#impute missing values
ds[vars] %<>% na.roughfix() 
#confirm that no missing values remain
ds[vars] %>% is.na() %>% sum() %>% comcat()
#restore dataset
ds <- ods

#remove observations with missing values
#backup dataset
ods <- ds
#initialize list of observations to remove
omit <- NULL
#review dataset
ds[vars] %>% nrow()
ds[vars] %>% is.na() %>% sum() %>% comcat()
#identify observations with missing values
mo <- attr(na.omit(ds[vars]), "na.action")
#record observations to omit
omit <- union(omit, mo)
#if there are observations to omit then remove
if(length(omit)) ds <- ds[-omit, ]
#confirm observations removed
ds[vars] %>% nrow() %>% comcat()
ds[vars] %>% is.na() %>% sum()
#restore dataset
ds <- ods
omit <- NULL


##feature creation
#derived features
#create year and season
ds %<>%
  mutate(year = factor(format(date, "%Y")),
         season = format(ds$date, "%m") %>%
           as.integer() %>%
           sapply(function(x)
             switch(x,
                    "summer", "summer", "autumn",
                    "autumn", "autumn", "winter",
                    "winter", "winter", "spring",
                    "spring", "spring", "summer")) %>%
           as.factor()) %T>%
  {select(., date, year, season) %>% sample_n(10) %>% print()}
#introduce variables to vars and id
vars %<>% c("season")
vars
id %<>% c("year")
id

glimpse(ds)

#model generated features
#cluster analysis:  group locations; within group, similar values for numeric variables; between groups = dissimilar
#reset random number generator for repeatability
set.seed(7465)
#cluster numeric data per location
NCLUST <- 5 #number of clusters to create

ds[c("location", numc)] %>%
  group_by(location) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %T>%
  {locations <<- .$location} %>% #store locations for later
  select(-location) %>%
  sapply(function(x) ifelse(is.nan(x), 0, x)) %>%
  as.data.frame() %>%
  sapply(scale) %>%
  kmeans(NCLUST) %>%
  print() %>%
  extract2("cluster") ->
  cluster

head(cluster)

#index cluster vector by appropriate locations
names(cluster) <- locations
#add cluster to dataset
ds %<>% mutate(cluster = "area" %>%
                 paste0(cluster[ds$location]) %>%
                 as.factor)
#check clusters
ds %>% select(location, cluster) %>% sample_n(10)
#introduce variables role to the modelling
vars %<>% c("cluster")
#check that clustering looks okay
cluster[levels(ds$location)] %>% sort()

##preparing the metadata
#variable types
#identify variables that are model inputes, i.e. independent variables, record as vector of characters
vars %>%
  setdiff(target) %T>%
  print() ->
  inputs
#record as vector of integers (variable indices)
inputs %>%
  sapply(function(x) which(x == names(ds)), USE.NAMES = FALSE) %T>%
  print() ->
  inputi
#record number of observations
ds %>%
  nrow() %T>%
  comcat() ->
  nobs
#confirm subsets
dim(ds) %>% comcat()
dim(ds[vars]) %>% comcat()
dim(ds[inputs]) %>% comcat()
dim(ds[inputi]) %>% comcat()

#numeric and categorical variables
#identify numeric variables by index
ds %>%
  sapply(is.numeric) %>%
  which() %>%
  intersect(inputi) %T>%
  print() ->
  numi
#identify numeric variables by name
ds %>%
  names() %>%
  extract(numi) %T>%
  print() ->
  numc
#identify categorical variables by index
ds %>%
  sapply(is.factor) %>%
  which() %>%
  intersect(inputi) %T>%
  print() ->
  cati
#identify categoric variables by name
ds %>%
  names() %>%
  extract(cati) %T>%
  print() ->
  catc

##prepare for model building
ds[vars] %>%
  formula() %>%
  print() ->
  form

#initialize random numbers for repeatable results
seed <- 42
set.seed(seed)
#record index of observations belonging to each subset, i.e. partition full dataset into three
nobs %>%
  sample(0.70*nobs) %T>%
  {length(.) %>% comcat()} %T>%
  {sort(.) %>% head(30) %>% print()} ->
  train

nobs %>%
  seq_len() %>%
  setdiff(train) %>%
  sample(0.15 * nobs) %T>%
  {length(.) %>% comcat()} %T>%
  {sort(.) %>% head(15) %>% print()} ->
  validate

nobs %>%
  seq_len() %>%
  setdiff(union(train, validate)) %T>%
  {length(.) %>% comcat()} %T>%
  {head(.) %>% print(15)} ->
  test

#cache various actual values for target and risk
tr_target <- ds[train, ][[target]] %T>%
  {head(., 20) %>% print()}
tr_risk <- ds[train, ][[risk]] %T>%
  {head(., 20) %>% print()}
va_target <- ds[validate, ][[target]] %T>%
  {head(., 20) %>% print()}
va_risk <- ds[validate, ][[risk]] %>%
  {head(., 20) %>% print()}
te_target <- ds[test, ][[target]] %T>%
  {head(., 20) %>% print()}
te_risk <- ds[test, ][[risk]] %T>%
  {head(., 20) %>% print()}

##save the dataset
fpath <- "data"
#timestamp for dataset
dsdate <- "_" %s+% format(Sys.Date(), "%Y%m%d") %T>% print()
#use fixed timestamp to name file for convenience
dsdate <- "_20210228"
#filename for saved dataset
dsfile <- dsname %s+% dsdate %s+% ".RData"
#full path to dataset
fpath %>%
  file.path(dsfile) %T>%
  print() ->
  dsrdata
#save relevant r objects to binary RData file
save(ds, dsname, dspath, dsdate, nobs, vars, target, risk, id, ignore, omit, inputi, inputs, numi, numc, cati, catc, form, seed, train, validate, test, tr_target, tr_risk, va_target, va_risk, te_target, te_risk, file = dsrdata)
#check resulting file size in bytes
file.size(dsrdata) %>% comma()

load(dsrdata) %>% print()
