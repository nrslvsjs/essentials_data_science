\documentclass[a4paper]{article}
\usepackage[british]{babel}
\usepackage[colorlinks=true]{hyperref}
\usepackage{geometry}
\usepackage[parfill]{parskip}          % No indent. Add space between paragraphs.
\usepackage{xcolor}
<<dtmpl:module, echo=FALSE, results="asis", purl=FALSE>>=
  Module <- sub(".Rnw", "", current_input())
  cat(paste0("\\newcommand{\\Module}{", Module, "}"))
  @ 
    \begin{document}
  
  <<dtmpl:00_setup, echo=FALSE>>=
    # Welcome to the Togaware Data Science Data Template ----
  #
  # Refer to the book, The Essentials of Data Science available
  # from Amazon at https://amzn.to/2L35wPe, and the web site
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
  @ 
    
    <<dtmpl:setup, child="mycourse.Rnw", purl=FALSE>>=
    @ 
    
    <<dtmpl:attach_packages, echo=FALSE, message=FALSE, warning=FALSE>>=
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
  @ 
    
    % We actually set up the dataset here so that within the text that
  % follows we can refer to the dataset name. We insert the actual code
  % into the produced document below.
  
  <<dtmpl:10_ingest, echo=FALSE>>=
    # Welcome to the Togaware Data Science Data Template
    #
    # Refer to the book, The Essentials of Data Science available
    # from Amazon at https://amzn.to/2L35wPe, and the web site
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
  @ 
    
    <<dtmpl:dataset_setup, echo=FALSE, message=FALSE>>=
    # Name of the dataset.
    
    dsname <- "weatherAUS"
  
  # Identify the source location of the dataset.
  
  dsloc <- "data"
  
  # Other common alternatives include:
  #
  # dsloc <- "C:/Users/graham/Projects"
  # dsloc <- "~/projects"
  # dsloc <- "http://rattle.togaware.com"
  
  # Construct the path to the dataset and display some if it.
  
  dsname %s+% ".csv" %>%
    file.path(dsloc, .) %T>%
    cat("\n\n") %T>%
    {
      paste("head", .) %>%
        system(intern=TRUE) %>%
        sub("\r", "\n", .) %>%
        print()
    } ->
    dspath
  
  # Ingest the dataset.
  
  dspath %>%
    read_csv() %T>%
    glimpse() %>%
    assign(dsname, ., .GlobalEnv)
  @ 
    
    \title{Data Preparation Template\\
      The \textbf{\Sexpr{dsname}} Dataset}
  \author{Graham Williams}
  \date{2 July 2018}
  \maketitle\thispagestyle{empty}
  
  \section{Introduction}
  
  This template provides a guide to the typical sequence of steps a data
  scientist begins with in analyzing a new dataset. See
  \href{https://amzn.to/2L35wPe}{The Essentials of Data Science With R}
  (2017) for details. The sequence is extracted into separate files,
  corresponding to the logical steps along the way. Each can be run
  sequentially.
  
  We collect here the packages used throughout this document.
  
  <<dtmpl:attach_packages, eval=FALSE, purl=FALSE>>=
    @ 
    
    \section{Data Sources}
  
  In this section we identify the data sources and discuss access with
  the data owners. Be sure to document the data sources, integrity,
  providence, and dates. This is generally meta information setting up
  our expectations of what we expect to see from the data.
  
  The dataset we use is the \textit{\Sexpr{dsname}} dataset containing
  Australian weather observations. It covers observations from weather
  stations around Australia. The data is scraped from the Australian
  Bureau of
  Meteorology\footnote{\url{http://www.bom.gov.au/climate/dwo/}} and
  has been collected daily since 2007.
  
  The data is scraped regularly for multiple locations and includes most
  of the weather station's measurements. The raw source consists of
multiple files which are collated into a single CSV file to be made
available for analysis.

The source dataset for analysis is available from
\url{\Sexpr{dspath}}. We will use this location to access the dataset
whilst noting that the dataset is also directly available as a sample
dataset with the \texttt{rattle} package. The data from the provided
URL may be more up-to-date than that available from \texttt{rattle}.

\section{Data Ingestion}

In this section we ingest the data into R and identify any particular
issues with doing so. We begin by naming the dataset, identifying its
location, and then identifying the complete path to the dataset. A
function appropriate to the source dataset format is then used to
ingest and store the dataset into a variable naming the dataset
(\Rvariable{dsname}).

<<dtmpl:dataset_setup, eval=FALSE, purl=FALSE>>=
@ 

A generic variable (\Rvariable{ds}) will be used to reference the
storage in memory of this dataset. All of our following activity will
then refer to this generic variable rather than the name of the
original dataset. We keep the original dataset available so that we
can restart our wrangling process as data mistakes are made. That way
we do not need to download the source dataset again. This is good
practise even though in our case here it takes only a few seconds to
download (depending on our Internet connection) and little time to
parse to data.

<<dtmpl:set_template_variable>>=
# Prepare the dataset for usage with our template.

ds <- get(dsname)
@ 

It is useful to store the original dataset as an R Data file on disk
and to then remove it to free up memory for processing. Saving to disk
for this dataset takes just a second.

<<dtmpl:store_dataset>>=
# Save the dataset to disk as a binary R Data for backup.

fpath <- "data"

# Other common alternatives include:
#
# fpath <- dsloc

dsname %s+% ".RData" %>%
  file.path(fpath, .) %T>%
  cat("\n\n") ->
fname

if (! dir.exists(fpath))  dir.create(fpath)
if (! file.exists(fname)) save(weatherAUS, file=fname)

# Remove the original dataset to save on memory.

rm(weatherAUS)
@ 

Later, we can more quickly load the dataset which for our dataset
takes a fraction of a second generally, compared to the several
seconds to download and parse from the Internet, even for this
relatively small dataset.

<<dtmpl:load_dataset>>=
# Test the loading of the saved dataset.

load(fname) %>% print()
@ 

The result returned by \texttt{load()} is printed as the name of the
object(s) loaded from the file. That worked, so we clean up.

<<dtmpl:remove_dataset, echo=FALSE>>=
# Cleanup to save on memory.

rm(weatherAUS)
@ 

\section{Data Observation and Preparation}

<<dtmpl:20_observe, echo=FALSE>>=
# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available
# from Amazon at https://amzn.to/2L35wPe, and the web site
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
# Copyright (c) 2014-2017 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION
@ 

Often we will iterate over these simple steps observing and preparing
our dataset for the more serious data wrangling.

We can observe with some simple commands that the \Sexpr{dsname}
dataset has \Sexpr{format(nrow(ds),big.mark=",")} observations of
\Sexpr{ncol(ds)} variables.

The \Sexpr{ncol(ds)} variables observed in the dataset can be readily
observed using \Sexpr{Rfunction(glimpse)}.

<<dtmpl:glimpse>>=
# A glimpse into the dataset.

glimpse(ds)
@ 

We first note that the variable names do not conform to our standards
and so we change them to suit. The advantage is that we then have a
well defined and consistent naming scheme. The disadvantage is that
the names now differ from the original, though the mapping is simple
enough.

<<dtmpl:30_prepare, echo=FALSE>>=
# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available
# from Amazon at https://amzn.to/2L35wPe, and the web site
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
# Copyright (c) 2014-2017 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION
@ 

<<dtmpl:variable_normalisation>>=
# Review the variables to optionally normalise their names.

names(ds)

# Normalise the variable names.

names(ds) %<>% normVarNames() %T>% print()

# Confirm the results are as expected.

glimpse(ds)
@ 

If we are reading the data from a CSV file then we can include the
process within a pipeline, converting the column names, and saving the
result into both the dataset named variable and the generic variable.

<<dtmpl:readr_norm_var_names_pipeline>>=
dspath %>%
  read_csv() %T>%
  set_names(names(.) %>% normVarNames()) ->
weatherAUS -> ds
@ 

We can also then observe the top, the bottom and a random sample of
data. Often this will be instructive as it is here. We note that
\texttt{evaporation} and \texttt{sunshine} are often but not always
missing.

<<dtmpl:review>>=
# Review the first few observations.

head(ds) %>% print.data.frame()

# Review the last few observations.

tail(ds) %>% print.data.frame()

# Review a random sample of observations.

sample_n(ds, size=6) %>% print.data.frame()
@ 

\section{Data Preparation}

Load the data into R{} and perform various operations on the data to
shape it for analysis. Notice we comment sections which are specific
to the weatherAUS dataset.

<<dtmpl:wrangle>>=
# Traditional dataset summary to get started.

summary(ds)

## Data Wrangling weatherAUS

# Date data type conversion (if required). Set the appropriate date format.

class(ds$date)
head(ds$date)
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
  names() %>%
  print() ->
vnames

# Confirm these are currently character variables.

ds[vnames] %>% sapply(class)

# Choose to convert these variables from character to factor.

ds[vnames] %<>% 
  lapply(factor) %>% 
  data.frame() %>% 
  tbl_df()

# Confirm they are now factors.

ds[vnames] %>% sapply(class)

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
  names() %>%
  print() ->
vnames

# Confirm these are currently character variables.

ds[vnames] %>% sapply(class)

# Convert these variables from character to factor.

ds[vnames] %<>% 
  lapply(factor, levels=compass, ordered=TRUE) %>% 
  data.frame() %>% 
  tbl_df()

# Confirm they are now factors.

ds[vnames] %>% sapply(class)

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

vars <- names(ds) %>% print()

# Note the target variable.

target <- "rain_tomorrow"

# Place the target variable at the beginning of the vars.

vars <- c(target, vars) %>% unique() %>% print()

# Note the risk variable - measures the severity of the outcome.

risk <- "risk_mm"

# Note any identifiers.

id <- c("date", "location")

## Generic Data Wrangling ----------------

# Initialise ignored variables: identifiers and risk.

ignore <- union(id, if (exists("risk")) risk) %>% print()

# Heuristic for indentifiers to possibly ignore.

ds[vars] %>%
  sapply(function(x) x %>% unique() %>% length()) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %>%
  print() ->
ids

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, ids) %>% print()

# Identify variables with only missing values.

ds[vars] %>%
  sapply(function(x) x %>% is.na %>% sum) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %>%
  print() ->
missing

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, missing) %>% print()

# Identify a threshold above which proportion missing is fatal.

missing.threshold <- 0.7

# Identify variables that are mostly missing.

ds[vars] %>%
  sapply(function(x) x %>% is.na() %>% sum()) %>%
  '>'(missing.threshold*nrow(ds)) %>%
  which() %>%
  names() %>%
  print() ->
mostly

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, mostly) %>% print()

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
  names() %>%
  print() ->
too.many

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, too.many) %>% print()

# Identify variables that have a single value.

ds[vars] %>%
  sapply(function(x) all(x == x[1L])) %>%
  which() %>%
  names() %>%
  print() ->
constants 

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, constants) %>% print()

# Note which variables are numeric.

vars %>%
  setdiff(ignore) %>%
  '['(ds, .) %>%
  sapply(is.numeric) %>% 
  which() %>%
  names() %>%
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
  arrange(-abs(cor)) %>%
  print() ->
mc

# Any variables could be removed because highly correlated?

correlated <- c("temp_3pm", "pressure_3pm", "temp_9am")

# Add them if any to the variables to be ignored for modelling.

ignore <- union(ignore, correlated) %>% print()

# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore) %>% print()

# Confirm they are now ignored.

length(vars)

## Variable Selection ----------------

# Formula for modelling.

form <- formula(target %s+% " ~ .") %>% print()

# Use correlation search to identify key variables.
# Could be useful to decide which variables to retain.

cfs(form, ds[vars])

# Any variables to remove because not useful?

vars %<>% setdiff(NULL) %>% print()

# Use information gain to identify variable importance.

information.gain(form, ds[vars]) %>%
  rownames_to_column() %>%
  arrange(attr_importance)

# Any variables to remove because not useful?

vars %<>% setdiff(NULL) %>% print()

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
  attr("na.action") %>%
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
  names() %>%
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
@ 

<<dtmpl:40_meta, echo=FALSE>>=
# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available
# from Amazon at https://amzn.to/2L35wPe, and the web site
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
# Copyright (c) 2014-2017 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION
@ 

<<>>=
#### META DATA --------------------------------

# Identify the input variables by name.

inputs <- setdiff(vars, target) %>% print()

# Identify the input variables by index.

inputi <- sapply(inputs, 
                 function(x) which(x == names(ds)), 
                 USE.NAMES=FALSE) %>% print()

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
  intersect(inputi) %>%
  print() ->
numi

# Identify the numeric variables by name.

ds %>% 
  names() %>% 
  '['(numi) %>% 
  print() ->
numc

# Identify the categoric variables by index.

ds %>%
  sapply(is.factor) %>%
  which() %>%
  intersect(inputi) %>%
  print() ->
cati

# Identify the categoric variables by name.

ds %>% 
  names() %>% 
  '['(cati) %>% 
  print() ->
catc
@ 

<<dtmpl:50_save, echo=FALSE>>=
# Welcome to the Togaware Data Science Data Template ----
#
# Refer to the book, The Essentials of Data Science available
# from Amazon at https://amzn.to/2L35wPe, and the web site
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
# Copyright (c) 2014-2017 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA 
#
# DOCVERSION
@ 

<<>>=
#### SAVE THE DATASET --------------------------------

# Timestamp for the dataset - this is the general approach.

dsdate  <- "_" %s+% format(Sys.Date(), "%Y%m%d") %>% print()

# We will use a fixed timestamp to identify our file for convenience.

dsdate <- "_20170702"

# Filename for the saved dataset.

dsrdata <- 
  file.path(fpath, dsname %s+% dsdate %s+% ".RData") %>% 
  print()

# Save relevant R objects to the binary RData file.

save(ds, dsname, dspath, dsdate, nobs,
     vars, target, risk, id, ignore, omit, 
     inputi, inputs, numi, numc, cati, catc, 
     file=dsrdata)
@ 

\section{Data Exploration}

We should always understand our data by exploring it in various
ways. Include data summaries and various plots that give insights.

\end{document}
