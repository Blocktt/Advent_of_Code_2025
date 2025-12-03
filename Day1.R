# Libraries needed
library(dplyr)
library(readr)

# Declare directories ####
wd <- getwd()
input.dir <- "Input_Data"

# specify input files
fn.data1 <- "Input_Day1_Puzzle1.txt"

# Read data files ####
df_input <- read_csv(file.path(wd, input.dir, fn.data1)
                     , na = c("NA",""), trim_ws = TRUE, skip = 0
                     , col_names = FALSE, guess_max = 100000)

# cleanup
rm(fn.data1, input.dir)

## Format Data ####
df_input_v2 <- df_input %>% 
  rename(Original = X1) %>% 
  mutate(Direction = substr(Original, 1, 1) # First character
         , Number = as.numeric(substr(Original, 2, nchar(Original)))
         , Num_New = case_when((Direction == "L") ~ (0-Number)
                               , TRUE ~ Number))

rotations <- df_input_v2 %>% 
  pull(Num_New)

# cleanup
rm(df_input, df_input_v2)

# Define starting location
start <- 50

# Compute the sequence
result <- (start + cumsum(rotations)) %% 100
result[result == 100] <- 0
print(rotations)
print(result)
min(result)
max(result)

num_zeros <- sum(result == 0)

# cleanup
rm(result)

# Checking for passing zero
wrapped_vals <- numeric(length(rotations))
wrap_flags <- character(length(rotations))
wrap_counts <- integer(length(rotations))

current <- start

for (i in seq_along(rotations)) {
  rotation <- rotations[i]
  raw <- current + rotation
  
  # Calculate base wraps
  wraps <- abs(rotation) %/% 100
  
  # Check if we cross zero from current position
  if (rotation < 0 && (current + rotation) < 0) {
    wrap_flags[i] <- "NegativeWrap"
    wraps <- wraps + 1
  } else if (rotation > 0 && (current + rotation) > 99) {
    wrap_flags[i] <- "OverflowWrap"
    wraps <- wraps + 1
  } else {
    wrap_flags[i] <- ""
  }
  
  wrapped <- ((raw %% 100) + 100) %% 100
  wrapped_vals[i] <- wrapped
  wrap_counts[i] <- wraps
  
  current <- wrapped
}


result_df <- data.frame(
  Step = seq_along(rotations),
  Rotation = rotations,
  RawValue = start + cumsum(rotations), # for reference
  Wrapped = wrapped_vals,
  WrapType = wrap_flags,
  WrapCount = wrap_counts
)

