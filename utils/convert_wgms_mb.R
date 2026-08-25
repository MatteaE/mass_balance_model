###################################################################################################
# This program takes the mass_balance_points.csv file from WGMS and extracts annual and winter    #
# mass balance points for the selected glacier, in a format suitable for DMBSim.                  #
# It only includes points which do have coordinates (many do not).                                #
# Author: Enrico Mattea (University of Fribourg)                                                  #
# Latest change: 2026/08/25                                                                       #
###################################################################################################

library(stringr)

# Define input and output here --------------------------------------------------------------------
tgt_p    <- "mass_balance_point.csv"
gl_sel   <- ""
name_out <- ""

df <- read.csv(tgt_p)

ids_sel <- which((df$glacier_name == gl_sel) & (!is.na(df$latitude)) & (!is.na(df$longitude)))
df_sel <- df[ids_sel,]


# Prepare winter file -----------------------------------------------------------------------------
df_winter <- df_sel[which(df_sel$balance_code == "winter"),]
df_winter <- df_winter[order(df_winter$begin_date),]

if (nrow(df_winter) > 0) {
  df_winter_out <- data.frame(id         = str_pad(df_winter$original_id, width = max(2, nchar(df_winter$original_id), na.rm = T), side = "right"),
                              date_start = format(as.Date(df_winter$begin_date), format = "%d.%m.%Y"),
                              date_end   = format(as.Date(df_winter$end_date), format = "%d.%m.%Y"),
                              lon        = sprintf("%.7f", df_winter$longitude),
                              lat        = sprintf("%.7f", df_winter$latitude),
                              ele        = str_pad(as.character(round(df_winter$elevation)), width = 5, side = "left"),
                              massbal    = str_pad(sprintf("%.1f", df_winter$balance*100), width = 6, side = "left"),
                              density    = 1)
  write.table(df_winter_out,
              paste0("mb_", name_out, "_winter.dat"),
              quote = F,
              row.names = F)
  
}


# Prepare annual (incl. summer) file --------------------------------------------------------------
df_annual <- df_sel[which(df_sel$balance_code %in% c("summer", "annual")),]
df_annual <- df_annual[order(df_annual$begin_date),]

if (nrow(df_annual) > 0) {
  df_annual_out <- data.frame(id         = str_pad(df_annual$original_id, width = max(2, nchar(df_annual$original_id), na.rm = T), side = "right"),
                              date_start = format(as.Date(df_annual$begin_date), format = "%d.%m.%Y"),
                              date_end   = format(as.Date(df_annual$end_date), format = "%d.%m.%Y"),
                              lon        = sprintf("%.7f", df_annual$longitude),
                              lat        = sprintf("%.7f", df_annual$latitude),
                              ele        = str_pad(as.character(round(df_annual$elevation)), width = 5, side = "left"),
                              massbal    = str_pad(sprintf("%.1f", df_annual$balance*100), width = 6, side = "left"),
                              density    = 1)
  write.table(df_annual_out,
              paste0("mb_", name_out, "_annual.dat"),
              quote = F,
              row.names = F)
  
}

message("All done. Now make sure to remove the first line (header) from the output file(s), as the model does not expect it.")
