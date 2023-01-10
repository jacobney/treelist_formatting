#This script will help convert the treelists into the format expected by the QUIC-Fire Script

library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(stringr)

###Prepares Datasets################################################################################
  
  #prepares treelist from txt file for 56 dataset
Txt <- read.delim("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\Heil_pre_2ms.txt",
      col.names = "Lines")
Tree_Lines <- Txt %>% filter(grepl('&TREE',Lines)) 
sep_vars <- Tree_Lines %>% 
      separate(Lines, into = c("x", "y", "z", "PART_ID", "FUEL_GEOM", "cw", "cbh", "ht", "OUTPUT_TREE", "LABEL"), sep = ",")
XYZ <- sep_vars %>%
      mutate_at("x", str_replace, "&TREE XYZ=", "")
CW <- XYZ %>%
      mutate_at("cw", str_replace, "CROWN_WIDTH=", "")
CBH <- CW %>%
      mutate_at("cbh", str_replace, "CROWN_BASE_HEIGHT=", "")
TH <- CBH %>%
      mutate_at("ht", str_replace, "TREE_HEIGHT=", "")
Trees_data <- TH %>%
      mutate_at("OUTPUT_TREE", str_replace, "OUTPUT_TREE=", "")
relevant <- subset(Trees_data, select = c(x, y, cw, ht, cbh))
numeric <- relevant %>% mutate_if(is.character, as.numeric)
measures <- mutate(numeric, cr = cw / 2, dbh = 0)
domain_treelist <- subset(measures, select = c(x, y, dbh, ht, cbh))
csv_ready_56 <- unite(domain_treelist, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
write.table(csv_ready_56, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_Heil_pre2.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)

  #241 dataset
OG_data <- read.csv("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\prerun1treelist.csv")
measures <- mutate(OG_data, cr = cw / 2, dbh = 0)
domain_treelist <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
csv_ready_241 <- unite(domain_treelist, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_prerun1treelist.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)

###Gets Summary Stats################################################################################

 #241 dataset has output == ".TRUE." and 36 hectares

 #filter to AOI 
AOI <- filter(Trees_data, OUTPUT_TREE == ".TRUE.")
#AOI_crown_vars <- subset(AOI, select = c(x, y, dbh, ht, cbh, cr))
AOI_trees <- count(AOI)
domain_trees <- count(numeric)

  #get key params
TPH <- domain_trees/40
ninetieth_ht <- quantile(numeric$ht, probs = 0.9)
nine_perc <- data.frame(ninetieth_ht)
median_cbh <- quantile(numeric$cbh, probs = 0.5)
med_cbh <- data.frame(t(median_cbh))

  #Pulls, max, min, and median of AOI
find_max <- sapply(crown_vars, max)
max <- data.frame(t(find_max))
find_min <- sapply(crown_vars, min)
min <- data.frame(t(find_min))
find_median <- sapply(crown_vars, median)
mean <- data.frame(t(find_median))
treelist_upper <- max %>%
  full_join(mean, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
treelist_sum <- treelist_upper %>%
  full_join(min, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
params <- mutate(treelist_sum, med_cbh = med_cbh, nine_perc = nine_perc, TPH = TPH)

  #241 write summary CSV
write_csv(params, paste("C:\\Users\\neyja\\data_desktop\\summary_prerun1treelist.csv"))
  #56 write summary CSV
write_csv(params, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\summary_Heil_pre2.csv"))

###This is the batch process to run for an entire directory##################################################################################

  #241 dataset
setwd("C:\\Users\\neyja\\data_desktop\\OG_treelist_csvs\\preruns")
raw_files <- Sys.glob( "*.csv" )
for( i in raw_files )
{
  #Get dataframe
  OG_data <- read.csv(i)
  measures <- mutate(OG_data, cr = cw / 2, dbh = 0)
  crown_vars <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
  csv_ready <- unite(crown_vars, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
  write.table(csv_ready, paste( "C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\outputs\\output_", i, sep = ""), 
        row.names = FALSE, col.names = FALSE, quote = FALSE)
  #write_csv(params, paste("C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\summaries\\summary_", i, sep = " "))
}

  #56 dataset
setwd("C:\\Users\\neyja\\data_desktop\\wfds_simulations\\57 restoration_sims\\restoration_sims_input_files")
raw_files <- Sys.glob( "*.txt" )
for( i in raw_files )
{
  Txt <- read.delim(i, col.names = "Lines")
  Tree_Lines <- Txt %>% filter(grepl('&TREE',Lines)) 
  sep_vars <- Tree_Lines %>% 
    separate(Lines, into = c("x", "y", "z", "PART_ID", "FUEL_GEOM", "cw", "cbh", "ht", "OUTPUT_TREE", "LABEL"), sep = ",")
  XYZ <- sep_vars %>%
    mutate_at("x", str_replace, "&TREE XYZ=", "")
  CW <- XYZ %>%
    mutate_at("cw", str_replace, "CROWN_WIDTH=", "")
  CBH <- CW %>%
    mutate_at("cbh", str_replace, "CROWN_BASE_HEIGHT=", "")
  TH <- CBH %>%
    mutate_at("ht", str_replace, "TREE_HEIGHT=", "")
  Trees_data <- TH %>%
    mutate_at("OUTPUT_TREE", str_replace, "OUTPUT_TREE=", "")
  Trees_num <- transform(Trees_data, cw = as.numeric(cw))
  measures <- mutate(Trees_num, cr = cw / 2, dbh = 0)
  domain <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
  csv_ready <- unite(domain, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
  write.table(csv_ready, paste( "C:\\Users\\neyja\\data_desktop\\wfds_simulations\\57 restoration_sims\\treelists\\output_", i, sep = ""), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
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
  full_join(mean, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
treelist_sum <- treelist_upper %>%
  full_join(min, by = c("x", "y", "dbh", "ht", "cbh", "cr"))
metric <- c("Max", "Mean", "Min")
grand_summary <- cbind(treelist_sum, metric)
write_csv(grand_summary, paste("C:\\Users\\neyja\\data_desktop\\grand_summary.csv"))