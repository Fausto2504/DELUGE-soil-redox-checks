# Deluge_Soil_redox_checks_adjusting_csvs_merging_dataframes_graphs
# This code clean csvs generated in the field by comparing Redox values
# from Loggernet and using manual measurements

# 1. set up code -----------------------------------------------------------------

# packages
library(tidyverse)
library(readr)
library(stringr)
library(ggpmisc)

# set working directory
files <- list.files(
  "C:/Users/fsilva/OneDrive - University of Toledo/ESE Lab Stuff/People/Research Assistant Professors/Fausto/DELUGE/Soil Redox Checks/DELUGE Redox measurements (1)",
  pattern = "\\.csv$",
  full.names = TRUE
)


# 2. function to adjust CSVs in a single dataframe ------------------------------------------

read_redox <- function(file){
  
  # Read raw file
  raw <- read_csv(
    file,
    col_names = FALSE,
    show_col_types = FALSE
  )
  
  # Site is in A2
  site <- raw[[1]][2] 
  
  # Find the row containing Loggernet header
  header_row <- which(
    str_detect(
      tolower(raw[[2]]),
      "loggernet"
    )
  )[1]
  
  # Read again using that row as header
  dat <- read_csv(
    file,
    skip = header_row - 1,
    show_col_types = FALSE
  )
  
  names(dat)[1:3] <- c("Sensor","Loggernet","Manual")
  
  dat <- dat %>%
    select(Sensor, Loggernet, Manual) %>%
    
    # keep only actual sensors
    filter(str_detect(Sensor, "^RedoxRa\\(")) %>%
    
    mutate(
      Site = site,
      Loggernet = str_extract(Loggernet, "-?[0-9]+[\\.,]?[0-9]*"),
      Loggernet = str_replace(Loggernet, ",", "."),
      Loggernet = as.numeric(Loggernet),
      Manual = as.numeric(Manual)
    ) %>%
    select(Site, Sensor, Loggernet, Manual)
  
  dat
}

# making dataframe and check
redox_df <- map_dfr(files, read_redox)
redox_df



# 3. Graphs: -----------------------------------------------------

# F1
redox_df %>%
  filter(Site == "F1") %>%
  ggplot(aes(x = Loggernet, y = Manual)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw() +
  labs(
    title = "F1",
    x = "LoggerNet ORP (mV)",
    y = "Manual ORP (mV)"
  )

# F2
redox_df %>%
  filter(Site == "F2") %>%
  ggplot(aes(x = Loggernet, y = Manual)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw() +
  labs(
    title = "F2",
    x = "LoggerNet ORP (mV)",
    y = "Manual ORP (mV)"
  )

# F3
redox_df %>%
  filter(Site == "F3") %>%
  ggplot(aes(x = Loggernet, y = Manual)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw() +
  labs(
    title = "F3",
    x = "LoggerNet ORP (mV)",
    y = "Manual ORP (mV)"
  )


# F4
redox_df %>%
  filter(Site == "F4") %>%
  ggplot(aes(x = Loggernet, y = Manual)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_bw() +
  labs(
    title = "F4",
    x = "LoggerNet ORP (mV)",
    y = "Manual ORP (mV)"
  )


# 4. Single graph --------------------------------------------------

redox_df %>%
  ggplot(aes(Loggernet, Manual)) +
  geom_point(size = 2.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "blue"
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  facet_wrap(~Site, ncol = 2) +
  theme_bw() +
  labs(
    x = "LoggerNet ORP (mV)",
    y = "Manual ORP (mV)",
    title = "DELUGE Redox Sensor Evaluation"
  )
