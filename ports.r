## ----ports:setup, echo=FALSE---------------------------------------------
# Welcome to the Togaware Data Science End-to-End Template Ports ----
#
# Refer to the book, The Essentials of Data Science available from
# Amazon at http://bit.ly/essentials_data_science, and the web site
# https://essentials.togaware.com for more details.
#
# Ports Dataset.
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
# book, Essentials of Data Science in R from CRC Press.
#
# Copyright (c) 2014-2018 Togaware.com
# Authored by and feedback to Graham.Williams@togaware.com
# License: Creative Commons Attribution-ShareAlike CC BY-SA
#
# DOCVERSION

## ----ports:attach_packages, echo=FALSE, message=FALSE, warning=FALSE-----
# Load required packages from local library into R.

library(directlabels) # Dodging labels for ggplot2.
library(grid)         # Layout of plots: viewport().
library(magrittr)     # Pipe operator %>% %<>% %T>% equals().
library(rattle)       # normVarNames().
library(readxl)       # read_excel().
library(scales)       # Include commas in numbers.
library(stringi)      # String concat operator %s+%.
library(tidyverse)    # ggplot2, tibble, tidyr, readr, purr, dplyr, stringr

## ----ports:data_source, message=FALSE------------------------------------
# Name of the dataset.

dsname <- "ports"

# Identify the Essentials location of the dataset.

dsloc  <- "https://essentials.togaware.com"
dspath <- dsname %s+% ".xlsx"
dsurl  <- file.path(dsloc, dspath) %T>% print()

## ----ports:data_ingest, message=FALSE------------------------------------
# Download the file locally.

download.file(dsurl, destfile=dspath, mode="wb")

# Ingest the dataset.

dspath %>% 
  read_xlsx(sheet=1, col_names=FALSE) %>% 
  assign(dsname, ., envir=.GlobalEnv)

get(dsname)

## ----ports:set_template_variable-----------------------------------------
# Prepare the dataset for usage with template.

ds <- get(dsname)

## ----ports:norm_var_names------------------------------------------------
# Normalise the variable names.

names(ds) %<>% normVarNames() %T>% print()

## ----ports:glimpse, out.lines=NULL---------------------------------------
# A glimpse into the dataset.

glimpse(ds)

## ----ports_value_dataset, out.lines=NULL---------------------------------
# Confirm the row and column span for the table of interest.

ds[72:93, 1:4]

## ----ports:value_names, out.lines=10-------------------------------------
# Wrangle the dataset: Rename columns informatively.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import"))

## ----ports:value_numerics, out.lines=10----------------------------------
# Wrangle the dataset: Numeric variable conversion.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  mutate(
    export = as.numeric(export),
    import = as.numeric(import)
  )

## ----ports:value_replicate_period_generate_index, out.lines=3------------
# Generate indicies that will be useful for indexing the data.

seq(1,21,2) %>% rep(2) %>% sort()

# Confirm this achieves the desired outcome.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  extract2("period") %>%
  extract(seq(1,21,2) %>% rep(2) %>% sort())

## ----ports:value_replicate_period, out.lines=9---------------------------
# Wrangle the dataset: Repair the period column.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  mutate(
    export = as.numeric(export),
    import = as.numeric(import),
    period = period[seq(1, 21, 2) %>% rep(2) %>% sort()]
  )

## ----ports:value_gather, out.lines=9-------------------------------------
# Wrangle the dataset: Reshape the datset.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  mutate(
    export = as.numeric(export),
    import = as.numeric(import),
    period = period[seq(1, 21, 2) %>% rep(2) %>% sort()]
  ) %>%
  gather(type, value, -c(period, location))

## ----ports:theme_bitre---------------------------------------------------
# Identify specific colors required for the organisational style.

cols <- c('#F6A01A', # Primary Yellow
          '#0065A4', # Primary Blue
          '#455560', # Primary Accent Grey
          '#B2BB1E', # Secondary Green
          '#7581BF', # Secondary Purple
          '#BBB0A3', # Secondary Light Grey
          '#E31B23', # Secondary Red
          '#C1D2E8') # Variant Grey

# Create a ggplot2 theme using these colours.

theme_bitre <- scale_fill_manual(values=cols)

## ----ports:value_plot, fig.width=9, fig.height=4, out.width="\\textwidth"----
# Facetted bar plot coparing import/export value across years.

ds[72:93, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  mutate(
    export = as.numeric(export),
    import = as.numeric(import),
    period = period[seq(1, 21, 2) %>% rep(2) %>% sort()]
  ) %>%
  gather(type, value, -c(period, location)) %>%
  ggplot(aes(x=location, y=value/1000, fill=type)) +
  geom_bar(stat="identity", position=position_dodge(width=1)) +
  facet_grid(~period) +
  labs(y="Billion dollars", x="", fill="") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10)) +
  theme_bitre

## ----ports:weight_plot, fig.width=9, fig.height=4, out.width="\\textwidth"----
# Facetted bar plot coparing import/export weight across years.

ds[96:117, 1:4] %>%
  set_names(c("period", "location", "export", "import")) %>%
  mutate(
    export = as.numeric(export),
    import = as.numeric(import),
    period = period[seq(1, 21, 2) %>% rep(2) %>% sort()]
  ) %>%
  gather(type, value, -c(period, location)) %>%
  ggplot(aes(x=location, y=value/1000, fill=type)) +
  geom_bar(stat="identity",position=position_dodge(width = 1)) +
  facet_grid(~period) +
  labs(y="Million tonnes", x="", fill="") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=10)) +
  theme_bitre

## ----ports:growth_data_frame, out.lines=NULL-----------------------------
# Confirm the table of interest from the dataset.

ds[2:4, 2:18]

## ----ports:growth_data_frame_process-------------------------------------
# Wrangle the dataset: Transpose and retain as a dataset.

ds[2:4, 2:18] %>% 
  t() %>% 
  data.frame(row.names=NULL, stringsAsFactors=FALSE) %>%
  tbl_df()

## ----ports:growth_data_frame_names---------------------------------------
# Wrangle the dataset: Renaming columns informatively.

ds[2:4, 2:18] %>% 
  t() %>% 
  data.frame(row.names=NULL, stringsAsFactors=FALSE) %>%
  tbl_df() %>%
  set_names(c("port", "weight", "rate"))

## ----ports:growth_data_frame_mutate--------------------------------------
# Wrangle the dataset: Numeric variable conversion.

ds[2:4, 2:18] %>% 
  t() %>% 
  data.frame(row.names=NULL, stringsAsFactors=FALSE) %>%
  tbl_df() %>%
  set_names(c("port", "weight", "rate")) %>%
  mutate(
    weight = as.numeric(weight),
    rate   = as.numeric(rate)
  )

## ----ports:types---------------------------------------------------------
# Identify port types from appropriate data in the sheet.

ds[6, 1:2]
ds[7:17, 1:2]

## ----ports_types_data_frame, out.lines=NULL------------------------------
# Construct a port type table.

ds[7:17, 1:2] %>%
  set_names(ds[6, 1:2])

## ----ports:gather_types, warning=FALSE, out.lines=20---------------------
# Tidy the dataset into a more useful structure.

ds[7:17, 1:2] %>%
  set_names(ds[6, 1:2]) %>%
  gather(type, port) %>%
  na.omit()

## ----ports:growth_join_types_display, out.lines=6------------------------
#  Wrangle the dataset: Join to port type.

ds[2:4, 2:18] %>% 
  t() %>% 
  data.frame(row.names=NULL, stringsAsFactors=FALSE) %>%
  tbl_df() %>%
  set_names(c("port", "weight", "rate")) %>%
  mutate(
    weight = as.numeric(weight),
    rate   = as.numeric(rate)
  ) %>%
  left_join(ds[7:17, 1:2] %>%
              set_names(ds[6, 1:2]) %>%
              gather(type, port) %>%
              na.omit(), 
            by="port")

## ----ports:ports_scatter, fig.width=10, fig.height=5, out.height="0.35\\textheight"----
# Labelled scatter plot with inset.

ds[2:4, 2:18] %>% 
  t() %>% 
  data.frame(row.names=NULL, stringsAsFactors=FALSE) %>%
  tbl_df() %>%
  set_names(c("port", "weight", "rate")) %>%
  mutate(weight = as.numeric(weight),
         rate   = as.numeric(rate)) %>%
  left_join(ds[7:17, 1:2] %>%
              set_names(ds[6, 1:2]) %>%
              gather(type, port) %>%
              na.omit(), 
            by="port") %>% 
  mutate(type=factor(type, levels=c("Mixed", "Bulk"))) %>%
  filter(port != "Darwin") ->
  tds

tds %>%
  ggplot(aes(x=weight, y=rate)) +
  geom_point(aes(colour=type, shape=type), size=4) +
  xlim(0, 300) + ylim(0, 13) +
  labs(shape="Port Type", colour="Port Type",
       x="Throughput 2011-12 (million tonnes)",
       y="Throughput average annual growth rate") +
  geom_text(data=filter(tds, type=="Bulk"), 
            aes(label=port), vjust=2) +
  annotate("rect", xmin=0, xmax=45, ymin=3, ymax=6.5, alpha = .1) +
  annotate("text", label="See inset", x=28, y=3.3, size=4) +
  theme(legend.position="bottom")

## ----ports:ports_scatter_inset, fig.width=7, fig.height=5----------------
# Labelled scatter plot - the inset.

above <- c("Townsville", "Fremantle")

tds %<>% filter(port != "Darwin" & type == "Mixed")

tds %>%
  ggplot(aes(x=weight, y=rate, label=port)) +
  geom_point(aes(colour=type, shape=type), size=4) +
  labs(shape="Port Type", colour="Port Type") +
  xlim(0, 40) + ylim(3, 6) +
  labs(x="Throughput 2011-12 (million tonnes)",
       y="Throughput average annual growth rate") +
  geom_text(data=filter(tds, !port%in%above), vjust= 2.0) +
  geom_text(data=filter(tds,  port%in%above), vjust=-1.0) +
  theme(legend.position="bottom")

## ----ports:calls_dataset-------------------------------------------------
# Wrangle the dataset: Name columns informatively.

ds[20:36, 1:13] %>% set_names(c("port", ds[19, 2:13]))

## ----ports:calls_gather--------------------------------------------------
# Wrangle the dataset: Dataset reshape and convert integer.

ds[20:36, 1:13] %>%
  set_names(c("port", ds[19, 2:13])) %>% 
  gather(period, calls, -port) %>%
  mutate(calls=as.integer(calls))

## ----ports:annual--------------------------------------------------------
# Wrangle the dataset: Avg calculation.

ds[20:36, 1:13] %>%
  set_names(c("port", ds[19, 2:13])) %>%
  select(port, 2, 13) %>%
  set_names(c('port', 'start', 'end')) %>%
  mutate(
    start = as.integer(start),
    end   = as.integer(end),
    avg   = 100*(exp(log(end/start)/11)-1)
  )

## ----ports:facet_plot----------------------------------------------------
# Build the main faceted plot.

p1 <- 
  ds[20:36, 1:13] %>%
  set_names(c("port", ds[19, 2:13])) %>% 
  gather(period, calls, -port) %>%
  mutate(calls=as.integer(calls)) %>%
  ggplot(aes(x=period, y=calls)) +
  geom_bar(stat="identity", position="dodge", fill="#6AADD6") +
  facet_wrap(~port) +
  labs(x="", y="Number of Calls") +
  theme(axis.text.x=element_text(angle=90, hjust=1, size=8)) +
  scale_y_continuous(labels=comma)

## ----ports:facet_plot_extra----------------------------------------------
# Generate the second plot.

p2 <- 
  ds[20:36, 1:13] %>%
  set_names(c("port", ds[19, 2:13])) %>%
  select(port, 2, 13) %>%
  set_names(c('port', 'start', 'end')) %>%
  mutate(
    start = as.integer(start),
    end   = as.integer(end),
    avg   = 100*(exp(log(end/start)/11)-1)
  ) %>%
  ggplot(aes(x=port, y=avg)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#6AADD6") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8), 
        axis.text.y=element_text(size=8), 
        axis.title=element_text(size=10),
        plot.title=element_text(size=8),
        plot.background = element_blank()) +
  labs(x="", 
       y="Per cent",
       title="Average Annual Growth, 2001-02 to 2012-13")

## ----ports:facet_print, fig.width=8, fig.height=6------------------------
# Combine the plots into a single faceted bar plot with embedded bar plot.

print(p1)
print(p2, vp=viewport(x=0.72, y=0.13, height=0.28, width=0.54))

## ----ports:horiz_bar_chart_plot, fig.width=7, fig.height=3---------------
# Horizontal bar chart.

ds[48:56, 1:2] %>%
  set_names(c("occupation", "percent")) %>%
  mutate(percent    = as.numeric(percent),
         occupation = factor(occupation, 
                             levels=occupation[order(percent)])) %>%
  ggplot(aes(x=occupation, y=percent)) + 
  geom_bar(stat="identity", fill="#6AADD6", width=0.8) + 
  theme(axis.title.x=element_text(size=10)) + 
  labs(x="", y="Per cent") + 
  coord_flip()

## ----ports:occ_data------------------------------------------------------
tds <- 
  ds[39:40, 2:9] %>%
  set_names(ds[38, 2:9]) %>%
  mutate(type=c("Mixed Ports", "Bulk Ports")) %>%
  gather(occupation, percent, -type) %>%
  mutate(
    percent    = as.numeric(percent),
    occupation = factor(occupation,
                        levels=c("Managers", 
                                 "Professionals", 
                                 "Technicians and Trades Workers", 
                                 "Community and Personal Service Workers", 
                                 "Clerical and Administrative Workers", 
                                 "Sales Workers", 
                                 "Machinery Operators and Drivers", 
                                 "Labourers"))
  ) %T>%
  print()

## ----ports:occ_positions-------------------------------------------------
mv <- 
  tds %>% 
  filter(type=="Mixed Ports") %>% 
  extract2("percent") %>%
  rev()

my <- (mv/2) + c(0, head(cumsum(mv), -1))

bv <- 
  tds %>% 
  filter(type=="Bulk Ports") %>% 
  extract2("percent") %>%
  rev()

by <- (bv/2) + c(0, head(cumsum(bv), -1))

lbls <- 
  data.frame(x=c(rep(1, length(mv)), rep(2, length(bv))),
             y=c(by, my),
             v=round(c(bv, mv))) %T>%
  print()

## ----ports:occ_bar, fig.width=8, fig.height=3----------------------------
# Horizontal bar chart with multiple stacks.

tds %>%
  ggplot(aes(x=type, y=percent, fill=occupation)) +
  geom_bar(stat="identity", width=0.5) +
  labs(x="", y="Per cent", fill="") +
  annotate("text", 
           x=lbls$x, 
           y=lbls$y, 
           label=lbls$v, 
           colour="white") +
  coord_flip() +
  scale_y_reverse() +
  theme_bitre

## ----ports:workers, fig.width=7, fig.height=5----------------------------
# Simple bar chart with dodged and labelled bars.

ds[43:45, 1:3] %>%
  set_names(c("type", ds[42, 2:3])) %>%
  gather(var, count, -type) %>% 
  mutate(count = as.integer(count),
         type  = factor(type, 
                        levels=c("Bulk", "Mixed", "Australia"))) ->
  tds

lbls <- data.frame(x=c(.7, 1, 1.3, 1.7, 2, 2.3),
                   y=tds$count-3,
                   lbl=round(tds$count))

tds %>%
  ggplot(aes(x=var, y=count)) +
  geom_bar(stat="identity", position="dodge", aes(fill=type)) +
  labs(x="", y="Per cent", fill="") + ylim(0, 100) +
  geom_text(data=lbls, aes(x=x, y=y, label=lbl), colour="white") +
  theme_bitre
