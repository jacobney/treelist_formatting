#This script will help convert the treelists into the format expected by the QUIC-Fire Script

library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(stringr)

###Prepares Datasets################################################################################
  
  #241 cutting dataset
data_241 <- read.csv("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\prerun1treelist.csv")
measures <- mutate(data_241, cr = cw / 2, dbh = 0)
domain_treelist <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
csv_ready_241 <- unite(domain_treelist, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_prerun1treelist.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE) 

  # 57 restoration txt file
Txt <- read.delim("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\Heil_pre_2ms.txt",
      col.names = "Lines")
  # 36 sierra txt file
Txt <- read.delim("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\9_1929_6_het.txt",
                  col.names = "Lines")

  #prepares treelist from txt file
Tree_Lines <- Txt %>% filter(grepl('LABEL=', Lines)) 
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
  #go to AOI?
relevant <- subset(Trees_data, select = c(x, y, cw, ht, cbh))
numeric <- relevant %>% mutate_if(is.character, as.numeric)
measures <- mutate(numeric, cr = cw / 2, dbh = 0)
domain_treelist <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
csv_ready <- unite(domain_treelist, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
  
  #write 57 CSV
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_Heil_pre2.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)
  #write 36 CSV
write.table(csv_ready, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\output_9_1929_6_het.csv"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)

###Gets Summary Stats################################################################################

  #241 dataset has output == ".TRUE." and 36 hectares

  #56 filter to AOI 
AOI <- filter(Trees_data, OUTPUT_TREE == ".TRUE.")
AOI_count <- count(AOI)
domain_count <- count(domain_treelist)
AOI_relevant <- subset(AOI, select = c(x, y, cw, ht, cbh))
AOI_numeric <- relevant %>% mutate_if(is.character, as.numeric)
AOI_measures <- mutate(AOI_numeric, cr = cw / 2, dbh = 0)
AOI_treelist <- subset(AOI_measures, select = c(x, y, dbh, ht, cbh, cr))
AOI_csv_ready_56 <- unite(AOI_treelist, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")

  #get key params
TPH <- domain_trees/40
ninetieth_HT <- quantile(domain_treelist$ht, probs = 0.9)
nine_perc <- data.frame(ninetieth_HT)
median_CBH <- quantile(domain_treelist$cbh, probs = 0.5)
med_CBH <- data.frame(t(median_CBH))
mean_CBH <- mean(domain_treelist$cbh)
mn_CBH <- data.frame(t(mean_CBH))

  #Below this needs work

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

  #sum_stats <- mutate(mn_CBH, med_cbh = X50., nine_perc = nine_perc, TPH = TPH)

  #241 write summary CSV
write_csv(params, paste("C:\\Users\\neyja\\data_desktop\\summary_prerun1treelist.csv"))
  #56 write summary CSV
write_csv(params, paste("C:\\Users\\neyja\\OneDrive - Colostate\\Documents\\GitHub\\treelist_formatting\\summary_Heil_pre2.csv"))

###Batch process to create treelists from directory##################################################################################

  #241 cutting
setwd("C:\\Users\\neyja\\data_desktop\\OG_treelist_csvs\\preruns")
raw_files <- Sys.glob( "*.csv" )
for( i in raw_files )
{
  data_241 <- read.csv(i)
  measures <- mutate(data_241, cr = cw / 2, dbh = 0)
  crown_vars <- subset(measures, select = c(x, y, dbh, ht, cbh, cr))
  csv_ready <- unite(crown_vars, x.y.dbh.ht.cbh.cr, x, y, dbh, ht, cbh, cr,  sep = " ")
  write.table(csv_ready, paste( "C:\\Users\\neyja\\data_desktop\\fomatted_treelists\\preruns\\outputs\\output_", i, sep = ""), 
        row.names = FALSE, col.names = FALSE, quote = FALSE)
}

  #57 restoration
setwd("C:\\Users\\neyja\\data_desktop\\wfds_simulations\\57 restoration_sims\\restoration_sims_input_files")
raw_files <- Sys.glob( "*.txt" )
for( i in raw_files )
{
  Txt <- read.delim(i, col.names = "Lines")
  Tree_Lines <- Txt %>% filter(grepl('LABEL=',Lines)) 
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

#36 sierra
setwd("C:\\Users\\neyja\\data_desktop\\wfds_simulations\\sierra_sims\\inputfiles")
raw_files <- Sys.glob( "*.txt" )
for( i in raw_files )
{
  Txt <- read.delim(i, col.names = "Lines")
  Tree_Lines <- Txt %>% filter(grepl('LABEL=',Lines)) 
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
  write.table(csv_ready, paste( "C:\\Users\\neyja\\data_desktop\\wfds_simulations\\sierra_sims\\treelists\\output_", i, sep = ""), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

###This is the batch process to summarize 241 summaries for overall domain specs########################################################

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