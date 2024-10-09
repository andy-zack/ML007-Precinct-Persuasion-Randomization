
# Summary -----------------------------------------------------------------
# randomization plan is here: 
# https://docs.google.com/document/d/1BSxVhuH9QBo7fpsLSRE1X0AlBIj0ad8ggcXUGEhowxU/edit

# It will be 90/10 treatment/control
# Florida will be done separately and will be 50/50

# Load Pacakges -----------------------------------------------------------
library(tidyverse)
library(here)
library(randomizr)
library(purrr)
library(broom)
library(blockTools)


# Make blocking function --------------------------------------------------
make_blocks <- function(data, matching_covars, id_covars, group_size) {
  out <- blockTools::block(
    data, 
    n.tr = group_size, 
    id.vars = id_covars, 
    block.vars = matching_covars  # Directly pass the vector of column names
  )
  
  return(createBlockIDs(out, data, id.var = id_covars))
}


# Remove Memory Limit -----------------------------------------------------
mem.maxVSize(vsize = Inf)


# Set Covariates to Match on for Blocking ---------------------------------
matching_covars = c("reg_voters_count",
                    "pct_female",
                    "avg_tsmart_urbanicity",
                    "avg_tsmart_ideology_score",
                    "avg_tsmart_presidential_general_turnout_score",
                    "pct_reg_white",
                    "pct_reg_black",
                    "pct_reg_latinx",
                    "avg_tsmart_college_graduate_score",
                    "vf_reg_under_40",
                    "vf_reg_over_65",
                    "ts_partisan_under_15",
                    "ts_partisan_over_15",
                    "d_two_way_vote_share_pres_16",
                    "d_two_way_vote_share_cong_18",
                    "d_two_way_vote_share_pres_20",
                    "d_two_way_vote_share_cong_22")

# Load Data ---------------------------------------------------------------
nm_df <- read_csv(here("NM_Precinct_data - NM_Precinct_Data.csv"))
other_states_df <- read_csv(here("bq-results-20241001-165358-1727801739528.csv"))


# Make Two Datasets for Randomization -------------------------------------
main_df <- bind_rows(nm_df, other_states_df) %>%
  filter(state != "FL")
fl_df <- other_states_df %>%
  filter(state == "FL")


# Main Randomization ------------------------------------------------------
main_group_size <- 10

nrow(main_df)
# drop rows with missing matching covariates
main_df <- main_df %>%
  filter(if_all(all_of(matching_covars), ~ !is.na(.)))
nrow(main_df)


# Drop Rows if there is not a multiple of 10 total
rows_to_drop <- nrow(main_df) %% main_group_size
main_df <- head(main_df, nrow(main_df) - rows_to_drop)
nrow(main_df)

# this is commented out to just test on a smaller dataset if necessary
#small_df <- main_df %>% slice_sample(n=5000)
set.seed(84489)

main_randomized_df <- main_df %>%
  mutate(randomization_block = make_blocks(
    data = ., 
    matching_covars = matching_covars, 
    id_covars = "vb_vf_national_precinct_code",
    group_size = main_group_size)) %>%
  mutate(treatment = block_ra(blocks = .$randomization_block,
                              num_arms = 2,
                              prob = 1 / main_group_size,
                              conditions = c(1, 0))) %>% 
  mutate(randomization_block = as.factor(randomization_block))

count(main_randomized_df, treatment)
count(main_randomized_df, randomization_block, name = "group_size") %>% count(group_size)



# Florida Randomization ------------------------------------------------------
fl_group_size <- 2

nrow(fl_df)
# drop rows with missing matching covariates
fl_df <- fl_df %>%
  filter(if_all(all_of(matching_covars), ~ !is.na(.)))
nrow(fl_df)


# Drop Rows if there is not a multiple of 10 total
rows_to_drop <- nrow(fl_df) %% fl_group_size
fl_df <- head(fl_df, nrow(fl_df) - rows_to_drop)
nrow(fl_df)

set.seed(230)

fl_randomized_df <- fl_df %>%
  mutate(randomization_block = make_blocks(
    data = ., 
    matching_covars = matching_covars, 
    id_covars = "vb_vf_national_precinct_code",
    group_size = fl_group_size)) %>%
  mutate(treatment = block_ra(blocks = .$randomization_block,
                              num_arms = 2,
                              prob = 1/fl_group_size,
                              conditions = c(1, 0))) %>% 
  mutate(randomization_block = as.factor(randomization_block))

count(fl_randomized_df, treatment)
count(fl_randomized_df, randomization_block, name = "group_size") %>% count(group_size)


# Check Balance -----------------------------------------------------------
main_randomized_df %>%
  group_by(treatment) %>%
  summarize(n = n(),
            reg_voters_count = mean(reg_voters_count),
            urbanicity = mean(avg_tsmart_urbanicity),
            two_way_pres_20 = mean(d_two_way_vote_share_pres_20),
            two_way_cong_18 = mean(d_two_way_vote_share_cong_18),
            ts_partisan_under_15 = mean(ts_partisan_under_15),
            pct_reg_white = mean(pct_reg_white))

main_randomized_df %>%
  group_by(treatment) %>%
  summarize(n = n(),
            across(all_of(matching_covars), \(x) mean(x, na.rm = TRUE)))

fl_randomized_df %>%
  group_by(treatment) %>%
  summarize(n = n(),
            across(all_of(matching_covars), \(x) mean(x, na.rm = TRUE)))


# Plots to show how groups clustered --------------------------------------
main_randomized_df %>%
  filter(randomization_block %in% sample(main_randomized_df$randomization_block, 7)) %>%
  ggplot(aes(x = avg_tsmart_presidential_general_turnout_score,
             y = pct_vap_white,
             color = randomization_block)) +
  geom_point()

main_randomized_df %>%
  filter(randomization_block %in% sample(main_randomized_df$randomization_block, 7)) %>%
  ggplot(aes(x = d_two_way_vote_share_cong_22,
             y = household_count,
             color = randomization_block)) +
  geom_point()

main_randomized_df %>%
  filter(randomization_block %in% sample(main_randomized_df$randomization_block, 7)) %>%
  ggplot(aes(x = avg_tsmart_urbanicity,
             y = pct_female,
             color = randomization_block)) +
  geom_point()

# Save Results ------------------------------------------------------------
main_randomized_df %>%
  select(vb_vf_national_precinct_code, randomization_block, treatment) %>%
  write_csv("main_randomized_precincts.csv")

fl_randomized_df %>%
  select(vb_vf_national_precinct_code, randomization_block, treatment) %>%
  write_csv("fl_randomized_precincts.csv")

