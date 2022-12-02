#This script will help convert the treelists into the format expected by the QUIC-Fire Script

library(readxl)
library(tidyverse)

#This is the script to see changes for one csv at a time
  
  #prepares JZ treelist to appropriate format

OG_data <- read.csv("C:\\Users\\neyja\\OneDrive\\Documents\\GitHub\\treelist_formatting\\prerun1treelist.csv")
crown <- filter(OG_data, output == ".TRUE.")
crown_measures <- mutate(crown, cr = cw / 2, dbh = 0)
crown_vars <- subset(crown_measures, select = -c(output, id, cw))
csv_ready <- unite(crown_vars, x, x, y, dbh, ht, cbh, cr,  sep = " ")
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive\\Documents\\GitHub\\treelist_formatting\\treelist.csv"), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

  #Pulls, max, min, and mean of treelist and writes table

find_max <- sapply(crown_vars, max)
max <- data.frame(t(find_max))
find_min <- sapply(crown_vars, min)
min <- data.frame(t(find_min))
find_mean <- sapply(crown_vars, mean)
mean <- data.frame(t(find_mean))
treelist_upper <- max %>%
  full_join(mean, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
treelist_sum <- treelist_upper %>%
  full_join(min, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
write.table(treelist_sum, paste("C:\\Users\\neyja\\OneDrive\\Documents\\GitHub\\treelist_formatting\\prerun1treelist_sum.csv"), 
            row.names = FALSE, col.names = TRUE, quote = FALSE)

#This is the batch process to run for an entire directory

setwd("C:\\Users\\neyja\\OneDrive\\Documents\\Bitvise_SSH\\Temp4Servers\\OG_treelist_csvs\\prerun_treelists")
raw_files <- Sys.glob( "*.csv" )
for( i in raw_files )
{
  OG_data <- read.csv( i )
  crown <- filter(OG_data, output == ".TRUE.")
  crown_measures <- mutate(crown, cr = cw / 2, dbh = 0)
  crown_vars <- subset(crown_measures, select = -c(output, id, cw))
  csv_ready <- unite(crown_vars, x, x, y, dbh, ht, cbh, cr,  sep = " ")
  write.table(csv_ready, paste( "C:\\Users\\neyja\\OneDrive\\Documents\\Bitvise_SSH\\Temp4Servers\\formatted_treelists\\prerun\\Outputs\\Output_", i, sep = ""), 
        row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  find_max <- sapply(crown_vars, max)
  max <- data.frame(t(find_max))
  find_min <- sapply(crown_vars, min)
  min <- data.frame(t(find_min))
  find_mean <- sapply(crown_vars, mean)
  mean <- data.frame(t(find_mean))
  treelist_upper <- max %>%
    full_join(mean, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
  treelist_sum <- treelist_upper %>%
    full_join(min, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
  write.table(treelist_sum, paste("C:\\Users\\neyja\\OneDrive\\Documents\\Bitvise_SSH\\Temp4Servers\\formatted_treelists\\prerun\\Summaries\\Summary_", i, sep = ""), 
              row.names = FALSE, col.names = TRUE, quote = FALSE)
}