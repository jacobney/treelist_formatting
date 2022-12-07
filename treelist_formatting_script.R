#This script will help convert the treelists into the format expected by the QUIC-Fire Script

library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)

###This is the script to see changes for one csv at a time################################################################################
  #prepares JZ treelist to appropriate format

OG_data <- read.csv("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\prerun1treelist.csv")
crown <- filter(OG_data, output == ".TRUE.")
crown_measures <- mutate(crown, cr = cw / 2, dbh = 0, x_new = x - 500)
crown_vars <- subset(crown_measures, select = c(x_new, y, dbh, ht, cbh, cr))
csv_ready <- unite(crown_vars, x.y.dbh.ht.cbh.cr, x_new, y, dbh, ht, cbh, cr,  sep = " ")
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_prerun1treelist.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)

  #Pulls, max, min, and mean of treelist and writes csv

find_max <- sapply(crown_vars, max)
max <- data.frame(t(find_max))
find_min <- sapply(crown_vars, min)
min <- data.frame(t(find_min))
find_mean <- sapply(crown_vars, mean)
mean <- data.frame(t(find_mean))
treelist_upper <- max %>%
  full_join(mean, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
treelist_sum <- treelist_upper %>%
  full_join(min, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
write_csv(treelist_sum, paste("C:\\Users\\neyja\\data_desktop\\summary_prerun1treelist.csv"))

###This is the batch process to run for an entire directory##################################################################################

setwd("C:\\Users\\neyja\\data_desktop\\OG_treelist_csvs\\preruns")
raw_files <- Sys.glob( "*.csv" )
for( i in raw_files )
{
  OG_data <- read.csv( i )
  crown <- filter(OG_data, output == ".TRUE.")
  crown_measures <- mutate(crown, cr = cw / 2, dbh = 0, x_new = x - 500)
  crown_vars <- subset(crown_measures, select = c(x_new, y, dbh, ht, cbh, cr))
  csv_ready <- unite(crown_vars, x.y.dbh.ht.cbh.cr, x_new, y, dbh, ht, cbh, cr,  sep = " ")
  write.table(csv_ready, paste( "C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\outputs\\output_", i, sep = ""), 
        row.names = FALSE, col.names = FALSE, quote = FALSE)
 
  find_max <- sapply(crown_vars, max)
  max <- data.frame(t(find_max))
  find_min <- sapply(crown_vars, min)
  min <- data.frame(t(find_min))
  find_mean <- sapply(crown_vars, mean)
  mean <- data.frame(t(find_mean))
  treelist_upper <- max %>%
    full_join(mean, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
  treelist_sum <- treelist_upper %>%
    full_join(min, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
  write_csv(treelist_sum, paste("C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\summaries\\summary_", i, sep = " "))
}

###This is the batch process to summarize summaries for overall domain specs########################################################

library(data.table)
setwd("C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\summaries")
summary_files  <- list.files(pattern = '.+\\.csv$')
summary_combined <- rbindlist(lapply(summary_files, fread))

  #Makes overall summary csv

find_max <- sapply(summary_combined, max)
max <- data.frame(t(find_max))
find_min <- sapply(summary_combined, min)
min <- data.frame(t(find_min))
find_mean <- sapply(summary_combined, mean)
mean <- data.frame(t(find_mean))
treelist_upper <- max %>%
  full_join(mean, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
treelist_sum <- treelist_upper %>%
  full_join(min, by = c("x_new", "y", "dbh", "ht", "cbh", "cr"))
metric <- c("Max", "Mean", "Min")
grand_summary <- cbind(treelist_sum, metric)
write_csv(grand_summary, paste("C:\\Users\\neyja\\data_desktop\\grand_summary.csv"))