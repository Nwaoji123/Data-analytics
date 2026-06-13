# install.packages(c("labelled", "haven", "naniar", "openxlsx", "stringr"))
options(warn =-1)
options(repr.plot.width = 14, repr.plot.height =10, digits = 3)
install.packages("expss", dependencies = TRUE)
install.packages("labelled")
install.packages("dplyr")
install.packages(c(
  "ggplot2",
  "survey",
  "factoextra",
  "tidyverse",
  "FactoMineR",
  "likert",
  "expss",
  "haven",
  "here",
  "labelled",
  "naniar",
  "openxlsx",
  "stringr"
))
library(tidyverse)
women_data= read_csv("MCSS_KD_WOMEN_MERGED.csv")
childfever_data= read_csv("MCSS_KD_CHILDREN_MERGED.csv")
main_data= read_csv("MCSS_KD_MAIN.csv")
main_data_25= read_csv("MCSS_2025_HOUSEHOLD.csv")
MCSS_2025_HOUSEHOLD_MERGED= read_csv("MCSS_2025_HOUSEHOLD_MERGED.csv")
MCSS_KD_MEMBERS_MERGED= read_csv("MCSS_KD_MEMBERS_MERGED.csv")

#rural_urban classification
lga_classification <- read.csv("KD_rural_urban.csv")
women_data <- women_data %>%
  left_join(
    lga_classification,
    by = "lga"
  )
library(dplyr)
library(stringr)

# Clean LGA names in both datasets
MCSS_KD_MEMBERS_MERGED <- MCSS_KD_MEMBERS_MERGED%>%
  mutate(
    lga = str_to_lower(trimws(lga))
  )

lga_classification <- lga_classification %>%
  mutate(
    lga = str_to_lower(trimws(lga))
  )

# Join rural/urban classification
main_data <- main_data %>%
  left_join(
    lga_classification,
    by = "lga"
  )

MCSS_KD_MEMBERS_MERGED <- MCSS_KD_MEMBERS_MERGED %>%
  left_join(
    lga_classification,
    by = "lga"
  )

library(dplyr)

# Create lookup table from main_data
wealth_lookup <- main_data %>%
  select(
    hhid,
    wealth_score,
    wealth_quintile_num,
    wealth_quintile
  ) %>%
  distinct()

# Join wealth variables to other dataset
women_data <- women_data %>%
  left_join(
    wealth_lookup,
    by = "hhid"
  )
wealth_lookup_unique <- wealth_lookup %>%
  distinct(hhid, .keep_all = TRUE)
childfever_data <- childfever_data %>%
  left_join(
    wealth_lookup,
    by = "hhid"
  )
MCSS_KD_MEMBERS_MERGED <- MCSS_KD_MEMBERS_MERGED %>%
  left_join(
    wealth_lookup,
    by = "hhid"
  )

library(dplyr)
library(stringr)

# Clean LGA names in both datasets
main_data <- main_data %>%
  mutate(
    lga = str_to_lower(trimws(lga))
  )

lga_classification <- lga_classification %>%
  mutate(
    lga = str_to_lower(trimws(lga))
  )

# Join rural/urban classification
main_data <- main_data %>%
  left_join(
    lga_classification,
    by = "lga"
  )
names(main_data)
discordant_names <- function(names1, names2){
  
  different_names <- setdiff(names1, names2)
  
  return(print(different_names))
  
}
# check for names that don't have a match between datasets
discordant_names(main_data$lga, lga_classification$lga)
# Households with nets
library(dplyr)
library(tidyr)
library(expss)

lga_nmis = c(
  "chikun",
  "giwa",
  "igabi",
  "jemaa",
  "kubau",
  "kachia",
  "kagarko",
  "kaduna_north",
  "kaduna_south",
  "kauru",
  "lere",
  "makarfi",
  "sanga",
  "soba",
  "sabon_gari",
  "zaria",
  "zangon_kataf"
)
tab_for_hh_have_nets <- main_data %>%
  filter(lga %in% lga_nmis)%>%
  tab_cols(hh_have_nets == "Yes", total()) %>% 
  tab_rows(
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Households with at Least One ITN"
  )

tab_for_hh_have_nets
tab_for_hh_have_nets <- main_data %>%
  
  filter(!is.na(Urban_rural.classification)) %>%
  tab_cols(hh_have_nets == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Households with at Least One ITN"
  )

tab_for_hh_have_nets

# Average number of nets per household
library(expss)
library(dplyr)

table_AveNetNum <- main_data %>%
  
  filter(!is.na(Urban_rural.classification)) %>%
  
  tab_cells(nets_num) %>%
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>% 
  
  tab_weight(weights) %>% 
  
  tab_stat_fun(
    Mean = w_mean,
    "Weighted N" = w_n,
    method = list
  ) %>%
  
  tab_pivot() %>% 
  
  set_caption(
    "Average number of ITNs per household"
  )

table_AveNetNum

#average household size
library(dplyr)
library(expss)

table_AvgHHSize <- main_data %>%
  
  filter(!is.na(urban_rural_classification)) %>%
  
  tab_cells(hh_member) %>%
  
  tab_rows(
    urban_rural_classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>% 
  
  tab_weight(weights) %>% 
  
  tab_stat_fun(
    Mean = w_mean,
    "Weighted N" = w_n,
    method = list
  ) %>%
  
  tab_pivot() %>% 
  
  set_caption(
    "Average Household Size per Household"
  )

table_AvgHHSize


# Households with at least 1 ITN per 2 members
library(dplyr)
library(tidyr)
library(expss)

MCSS_KD_MEMBERS_MERGED <- MCSS_KD_MEMBERS_MERGED %>%
  mutate(
    itn_every_2_persons = case_when(
      !is.na(nets_num) & !is.na(hh_member) & nets_num * 2 >= hh_member ~ "Yes",
      TRUE ~ NA_character_
    )
  )
tab_for_HHITN2member <- MCSS_KD_MEMBERS_MERGED %>%
  filter(!is.na(Urban_rural.classification.x)) %>%
  tab_cols(itn_every_2_persons == "Yes", total()) %>% 
  tab_rows(
    zone,
    Urban_rural.classification.x,
    wealth_quintile,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Households with at Least One ITN for Every Two Persons"
  )

tab_for_HHITN2member

#ITN access calculation
library(dplyr)

# 1. Create ITN access numerator
# Assumption: dataset is one row per household

main_data <- main_data %>%
  mutate(
    num_nets = if_else(is.na(num_nets), 0, num_nets)
  )%>%
  mutate(
    itn_access_people = case_when(
      !is.na(num_nets) & !is.na(de_facto_population) & de_facto_population > 0 ~
        pmin(num_nets * 2, de_facto_population),
      TRUE ~ NA_real_
    )
  )

# 2. ITN access by each grouping variable

make_itn_access_table <- function(data, group_var) {
  
  data %>%
    filter(
      !is.na(.data[[group_var]]),
      !is.na(itn_access_people),
      !is.na(de_facto_population),
      de_facto_population > 0
    ) %>%
    group_by(row_labels = .data[[group_var]]) %>%
    summarise(
      `ITN Access|Proportion` = round(
        100 * sum(weights * itn_access_people, na.rm = TRUE) /
          sum(weights * de_facto_population, na.rm = TRUE),
        1
      ),
      `ITN Access|Weighted N` = round(
        sum(weights * itn_access_people, na.rm = TRUE),
        0
      ),
      `#Total|Weighted N` = round(
        sum(weights * de_facto_population, na.rm = TRUE),
        0
      ),
      .groups = "drop"
    ) %>%
    mutate(
      row_labels = paste0(group_var, "|", row_labels)
    ) %>%
    select(row_labels, everything())
}

# 3. Create row tables

itn_access_by_zone <- make_itn_access_table(
  main_data,
  "zone"
)

itn_access_by_urban_rural <- make_itn_access_table(
  main_data,
  "Urban_rural.classification"
)

itn_access_by_wealth <- make_itn_access_table(
  main_data,
  "wealth_quintile"
)

itn_access_by_lga <- make_itn_access_table(
  main_data,
  "lga"
)

# 4. Create total row

itn_access_total <- main_data %>%
  filter(
    !is.na(itn_access_people),
    !is.na(de_facto_population),
    hh_member > 0
  ) %>%
  summarise(
    `ITN Access|Proportion` = round(
      100 * sum(weights * itn_access_people, na.rm = TRUE) /
        sum(weights * de_facto_population, na.rm = TRUE),
      1
    ),
    `ITN Access|Weighted N` = round(
      sum(weights * itn_access_people, na.rm = TRUE),
      0
    ),
    `#Total|Weighted N` = round(
      sum(weights * de_facto_population, na.rm = TRUE),
      0
    )
  ) %>%
  mutate(
    row_labels = "Total|Total"
  ) %>%
  select(row_labels, everything())

# 5. Combine final table

tab_for_ITNAccess <- bind_rows(
  itn_access_by_zone,
  itn_access_by_urban_rural,
  itn_access_by_wealth,
  itn_access_by_lga,
  itn_access_total
)

tab_for_ITNAccess
write.csv(
  tab_for_ITNAccess,
  "Malaria_SNT_Output/ITN_Access.csv",
  row.names = FALSE
  
)
#ROUND 1 ITN ACCESS
library(dplyr)
main_data_25 <- main_data_25 %>%
  mutate(
    num_nets = if_else(is.na(num_nets), 0, num_nets)
  )%>%
  mutate(
    itn_access_people = case_when(
      !is.na(num_nets) & !is.na(de_facto_population) & de_facto_population > 0 ~
        pmin(num_nets * 2, de_facto_population),
      TRUE ~ NA_real_
    )
  )

# 2. ITN access by each grouping variable

make_itn_access_table <- function(data, group_var) {
  
  data %>%
    filter(
      !is.na(.data[[group_var]]),
      !is.na(itn_access_people),
      !is.na(de_facto_population),
      de_facto_population > 0
    ) %>%
    group_by(row_labels = .data[[group_var]]) %>%
    summarise(
      `ITN Access|Proportion` = round(
        100 * sum(weights * itn_access_people, na.rm = TRUE) /
          sum(weights * de_facto_population, na.rm = TRUE),
        1
      ),
      `ITN Access|Weighted N` = round(
        sum(weights * itn_access_people, na.rm = TRUE),
        0
      ),
      `#Total|Weighted N` = round(
        sum(weights * de_facto_population, na.rm = TRUE),
        0
      ),
      .groups = "drop"
    ) %>%
    mutate(
      row_labels = paste0(group_var, "|", row_labels)
    ) %>%
    select(row_labels, everything())
}

# 3. Create row tables

itn_access_by_zone <- make_itn_access_table(
  main_data_25,
  "zone"
)

itn_access_by_urban_rural <- make_itn_access_table(
  main_data_25,
  "urban_rural"
)

itn_access_by_lga <- make_itn_access_table(
  main_data_25,
  "lga"
)

# 4. Create total row

itn_access_total <- main_data_25 %>%
  filter(
    !is.na(itn_access_people),
    !is.na(de_facto_population),
    de_facto_population > 0
  ) %>%
  summarise(
    `ITN Access|Proportion` = round(
      100 * sum(weights * itn_access_people, na.rm = TRUE) /
        sum(weights * de_facto_population, na.rm = TRUE),
      1
    ),
    `ITN Access|Weighted N` = round(
      sum(weights * itn_access_people, na.rm = TRUE),
      0
    ),
    `#Total|Weighted N` = round(
      sum(weights * de_facto_population, na.rm = TRUE),
      0
    )
  ) %>%
  mutate(
    row_labels = "Total|Total"
  ) %>%
  select(row_labels, everything())

# 5. Combine final table

tab_for_ITNAccess <- bind_rows(
  itn_access_by_zone,
  itn_access_by_urban_rural,
  itn_access_by_lga,
  itn_access_total
)

tab_for_ITNAccess
write.csv(
  tab_for_ITNAccess,
  "Malaria_SNT_Output/ITN_Access_R1.csv",
  row.names = FALSE
  
)

#creating de-facto population column
de_facto_pop1 <- MCSS_2025_HOUSEHOLD_MERGED %>%
  group_by(hhid) %>%
  summarise(
    de_facto_population = sum(
      sleep_here_last_night == "Yes",
      na.rm = TRUE
    ),
    .groups = "drop"
  )
#merge de-facto pop into main data set - ROUND 1
library(dplyr)
library(tidyr)
library(expss)
main_data_25 <- main_data_25 %>%
  left_join(
    de_facto_pop1,
    by = "hhid"
  )
#merge de-facto pop into main data set
main_data <- main_data %>%
  left_join(
    de_facto_pop,
    by = "hhid"
  )
#source of nets
library(dplyr)
library(expss)

TableOnSourceNets <- main_data %>%
  
  filter(
    hh_have_nets == "Yes",
    !is.na(Urban_rural.classification)
  ) %>%
  
  mutate(
    net_source = case_when(
      get_through_campaign == "Yes_Campaign" ~ "Campaign",
      get_through_campaign == "Yes_ANC" ~ "ANC",
      get_through_campaign == "Yes_Immunization_Visit" ~ "Immunization Visit",
      get_through_campaign == "No" & !is.na(get_net) & get_net != "" ~ get_net,
      TRUE ~ "Source not recorded"
    )
  ) %>%
  
  cross_rpct(
    cell_vars = list(
      Urban_rural.classification,
      zone,
      lga,
      total()
    ),
    col_vars = list(net_source),
    weight = weights,
    total_label = "N",
    total_statistic = "w_cases",
    expss_digits(digits = 1)
  ) %>%
  
  set_caption("Sources of ITN")

TableOnSourceNets

#slept under net round 1
MCSS_2025_HOUSEHOLD_MERGED <- MCSS_2025_HOUSEHOLD_MERGED %>%
  left_join(
    de_facto_pop1,
    by = "hhid"
  )
urban_rural_lookup <- main_data_25 %>%
  select(lga, urban_rural) %>%
  distinct(lga, .keep_all = TRUE)
MCSS_2025_HOUSEHOLD_MERGED <- MCSS_2025_HOUSEHOLD_MERGED %>%
  left_join(
    urban_rural_lookup,
    by = "lga"
  )
weights_lookup <- main_data_25 %>%
  select(cluster_id, weights) %>%
  distinct(cluster_id, .keep_all = TRUE)

MCSS_2025_HOUSEHOLD_MERGED <- MCSS_2025_HOUSEHOLD_MERGED %>%
  left_join(
    weights_lookup,
    by = "cluster_id"
  )
library(dplyr)
library(tidyr)
library(expss)

tab_for_slept_under_itn <- MCSS_KD_MEMBERS_MERGED %>%
  filter(
    sleep_here_last_night == "Yes",
    !is.na(Urban_rural.classification)
  ) %>%
  tab_cols(slept_ubder_net_mem == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  set_caption(
    "Slept under an ITN last night among households with at least 1 ITN"
  )

tab_for_slept_under_itn
write.csv(
  tab_for_slept_under_itn,
  "Malaria_SNT_Output/slept_under_net_R2.csv",
  row.names = FALSE
  
)

members_KD_25= read_csv("members_KD_2025.csv")
sleep_under_net_lookup <- members_KD_25 %>%
  select(hhid, sleep_under_net_mm) %>%
  distinct(hhid, .keep_all = TRUE)
MCSS_2025_HOUSEHOLD_MERGED <- MCSS_2025_HOUSEHOLD_MERGED %>%
  left_join(
    sleep_under_net_lookup,
    by = "hhid"
  )
#slept under net
library(dplyr)
library(tidyr)
library(expss)

tab_for_slept_under_itn <- MCSS_2025_HOUSEHOLD_MERGED %>%
  filter(
    sleep_here_last_night == "Yes",
    !is.na(urban_rural)
  ) %>%
  tab_cols(sleep_under_net_mm == "Yes", total()) %>% 
  tab_rows(
    urban_rural,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  set_caption(
    "Slept under an ITN last night among households with at least 1 ITN"
  )

tab_for_slept_under_itn
write.csv(
  tab_for_slept_under_itn,
  "Malaria_SNT_Output/slept_under_net_r1(new).csv",
  row.names = FALSE
  
)

#reasons for not using net
library(dplyr)

ReasonsForNotUsingITN <- main_data %>%
  filter(
    hh_have_nets == "Yes",
    sleep_under_net == "No"
  ) %>%
  group_by(zone, lga) %>%
  summarise(
    `Bed bugs` = round(
      sum(reason_net_wasnt_used_membed_bug * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Don't like smell` = round(
      sum(reason_net_wasnt_used_memdont_li * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Net not available` = round(
      sum(reason_net_wasnt_used_memnet_not * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Net too hot` = round(
      sum(reason_net_wasnt_used_memnet_too * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `No mosquitoes` = round(
      sum(reason_net_wasnt_used_memno_mosq * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Other reason` = round(
      sum(reason_net_wasnt_used_memothers * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Slept outside` = round(
      sum(reason_net_wasnt_used_memslept_o * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Unable to hang net` = round(
      sum(reason_net_wasnt_used_memunable_ * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Usually don't use nets` = round(
      sum(reason_net_wasnt_used_memusual_u * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Weather condition` = round(
      sum(reason_net_wasnt_used_memweather * weights, na.rm = TRUE) /
        sum(weights, na.rm = TRUE) * 100, 1
    ),
    `Weighted N` = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

zone_totals <- main_data %>%
  filter(
    hh_have_nets == "Yes",
    sleep_under_net == "No"
  ) %>%
  group_by(zone) %>%
  summarise(
    lga = "Total",
    `Bed bugs` = round(sum(reason_net_wasnt_used_membed_bug * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Don't like smell` = round(sum(reason_net_wasnt_used_memdont_li * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Net not available` = round(sum(reason_net_wasnt_used_memnet_not * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Net too hot` = round(sum(reason_net_wasnt_used_memnet_too * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `No mosquitoes` = round(sum(reason_net_wasnt_used_memno_mosq * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Other reason` = round(sum(reason_net_wasnt_used_memothers * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Slept outside` = round(sum(reason_net_wasnt_used_memslept_o * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Unable to hang net` = round(sum(reason_net_wasnt_used_memunable_ * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Usually don't use nets` = round(sum(reason_net_wasnt_used_memusual_u * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Weather condition` = round(sum(reason_net_wasnt_used_memweather * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Weighted N` = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

overall_total <- main_data %>%
  filter(
    hh_have_nets == "Yes",
    sleep_under_net == "No"
  ) %>%
  summarise(
    zone = "Total",
    lga = "Total",
    `Bed bugs` = round(sum(reason_net_wasnt_used_membed_bug * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Don't like smell` = round(sum(reason_net_wasnt_used_memdont_li * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Net not available` = round(sum(reason_net_wasnt_used_memnet_not * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Net too hot` = round(sum(reason_net_wasnt_used_memnet_too * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `No mosquitoes` = round(sum(reason_net_wasnt_used_memno_mosq * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Other reason` = round(sum(reason_net_wasnt_used_memothers * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Slept outside` = round(sum(reason_net_wasnt_used_memslept_o * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Unable to hang net` = round(sum(reason_net_wasnt_used_memunable_ * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Usually don't use nets` = round(sum(reason_net_wasnt_used_memusual_u * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Weather condition` = round(sum(reason_net_wasnt_used_memweather * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE) * 100, 1),
    `Weighted N` = sum(weights, na.rm = TRUE)
  )

ReasonsForNotUsingITN_final <- bind_rows(
  ReasonsForNotUsingITN,
  zone_totals,
  overall_total
) %>%
  arrange(zone, lga)

ReasonsForNotUsingITN_final

#children U5 who slept under net
library(dplyr)
library(tidyr)
library(readr)
library(expss)

MCSS_KD_MEMBERS_MERGED <- read_csv("MCSS_KD_MEMBERS_MERGED.csv")

tab_for_u5_sleep_under_net <- MCSS_KD_MEMBERS_MERGED %>%
  filter(child_u5 == 1) %>%
  tab_cols(sleep_under_net == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Children Under 5 Who Slept Under an ITN Last Night"
  )

tab_for_u5_sleep_under_net
write.csv(
  tab_for_u5_sleep_under_net,
  "Malaria_SNT_Output/U5_slept_under_net.csv",
  row.names = FALSE
  
)

#pregnant women who slept under net
library(dplyr)
library(expss)

tab_for_pregnant_sleep_under_net <- MCSS_KD_MEMBERS_MERGED %>%
  filter(Pregnancy == "Yes") %>%
  tab_cols(sleep_under_net == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Pregnant Women Who Slept Under an ITN Last Night"
  )

tab_for_pregnant_sleep_under_net

#saving to computer
# Create a folder for all outputs (optional)
dir.create("Malaria_SNT_Output", showWarnings = FALSE)

# Save all tables as CSV files

write.csv(
  ReasonsForNotUsingITN_final,
  "Malaria_SNT_Output/ReasonsForNotUsingITN.csv",
  row.names = FALSE
)

write.csv(
  tab_for_pregnant_sleep_under_net,
  "Malaria_SNT_Output/pw_slept_undernet.csv",
  row.names = FALSE
)

write.csv(
  tab_for_slept_under_itn,
  "Malaria_SNT_Output/sleptunderITN.csv",
  row.names = FALSE
)

write.csv(
  table_AveNetNum,
  "Malaria_SNT_Output/avgnetperHH.csv",
  row.names = FALSE
)

write.csv(
  table_AvgHHSize,
  "Malaria_SNT_Output/avgHHsize.csv",
  row.names = FALSE
)

write.csv(
  tab_for_hh_have_nets,
  "Malaria_SNT_Output/HHwithITN.csv",
  row.names = FALSE
)

write.csv(
  tab_for_HHITN2member,
  "Malaria_SNT_Output/HHITN2members.csv",
  row.names = FALSE
)

write.csv(
  TableOnSourceNets,
  "Malaria_SNT_Output/sourceofnets.csv",
  row.names = FALSE
)

#child care seeking
childfever_data = read_csv("MCSS_KD_CHILDREN_MERGED.csv")

# % of children with fever 2 weeks befofe survey
library(dplyr)
library(tidyr)
library(expss)

tab_for_child_ill <- childfever_data %>%
  mutate(
    child_ill = factor(
      child_ill,
      levels = c("Yes", "No")
    )
  ) %>%
  tab_cols(child_ill == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot()

tab_for_child_ill
write.csv(
  tab_for_child_ill,
  "Malaria_SNT_Output/childrenwithfever.csv",
  row.names = FALSE
)

#children tested for malaria
library(dplyr)
library(tidyr)

tab_for_ch_bloodtaken <- childfever_data %>%
  filter(child_ill=="Yes") %>%
  tab_cols(blood_taken=="Yes", total()) %>% 
  tab_rows(Urban_rural.classification.y,zone, lga, total()) %>%
  tab_weight(weights)%>% 
  tab_stat_rpct(label = "Proportion Yes", total_statistic = "w_cases") %>% 
  # tab_stat_cases(label = "Total Cases") %>% 
  tab_pivot()
write.csv(
  tab_for_ch_bloodtaken,
  "Malaria_SNT_Output/childrenwithfevertested.csv",
  row.names = FALSE
)

#children with confirmed malaria
library(dplyr)
library(tidyr)
library(expss)

tab_for_confirmed_malaria <- childfever_data %>%
  filter(
    child_ill == "Yes",
    blood_taken == "Yes"
  ) %>%
  tab_cols(confirm_malaria == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot()

tab_for_confirmed_malaria
write.csv(
  tab_for_confirmed_malaria,
  "Malaria_SNT_Output/childrenwithfevertestedpositive.csv",
  row.names = FALSE
)

#children who sought treatment
library(dplyr)
library(tidyr)
library(expss)

tab_for_sought_treatment <- childfever_data %>%
  filter(child_ill == "Yes") %>%
  tab_cols(seek_treatment == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot()

tab_for_sought_treatment
write.csv(
  tab_for_sought_treatment,
  "Malaria_SNT_Output/children_seektreatment.csv",
  row.names = FALSE
)

#treatment place
library(dplyr)
library(tidyr)
library(expss)

tab_for_treatment_place <- childfever_data %>%
  filter(child_ill == "Yes") %>%
  mutate(
    treatment_place_clean = case_when(
      is.na(treatment_place)~ NA_character_,
      TRUE ~ treatment_place
    )
  ) %>%
  tab_cols(treatment_place_clean, total()) %>% 
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot()

tab_for_treatment_place
write.csv(
  tab_for_treatment_place,
  "Malaria_SNT_Output/children_sourceofcare.csv",
  row.names = FALSE
)

#children tested by source of care
library(dplyr)
library(expss)

tab_for_blood_taken_by_source <- childfever_data %>%
  
  filter(child_ill == "Yes") %>%
  
  mutate(
    treatment_place_clean = case_when(
      is.na(treatment_place) ~ NA_character_,
      TRUE ~ treatment_place
    ),
    
    blood_taken = factor(
      blood_taken,
      levels = c("Yes", "No")
    )
  ) %>%
  tab_cells(blood_taken) %>%
  tab_rows(
    treatment_place_clean, 
    total()
  ) %>%
  
  
  tab_weight(weights) %>%
  
  tab_stat_cpct() %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "Blood Taken by Source of Care"
  )

tab_for_blood_taken_by_source
write.csv(
  tab_for_blood_taken_by_source,
  "Malaria_SNT_Output/childrentested_sourceofcare.csv",
  row.names = FALSE
)

#children who took medicine
library(dplyr)
library(expss)

tab_for_child_take_medicine <- childfever_data %>%
  filter(confirm_malaria == "Yes") %>%
  tab_cols(take_medicine == "Yes", total()) %>% 
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>% 
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  tab_pivot() %>%
  tab_caption(
    "Children with Confirmed Malaria Who Took Medicine"
  )

tab_for_child_take_medicine
write.csv(
  tab_for_child_take_medicine,
  "Malaria_SNT_Output/children_take_medicine.csv",
  row.names = FALSE
)

#medicine type
library(dplyr)
library(expss)

tab_for_medicines_type <- childfever_data %>%
  filter(confirm_malaria == "Yes") %>%
  tab_cols(
    type_medicineamodiaquine == 1,
    type_medicineantibiotic == 1,
    type_medicineartemisinin_combina == 1,
    type_medicineartesunate_injectio == 1,
    type_medicinechloroquine == 1,
    type_medicineibuprofen == 1,
    type_medicineother_injection == 1,
    type_medicineother_pill_syrup == 1,
    type_medicineparacetamol_panadol == 1,
    type_medicinequinine == 1,
    type_medicinerectal_artesunate == 1,
    type_medicinesp_fansidar == 1,
    total()
  ) %>%
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  tab_weight(weights) %>%
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  tab_pivot() %>%
  tab_caption(
    "Proportion of Medicine Types Taken Among Confirmed Malaria Cases by Rural/Urban, Zone and LGA"
  )

tab_for_medicines_type
write.csv(
  tab_for_medicines_type,
  "Malaria_SNT_Output/children_medicine_type.csv",
  row.names = FALSE
)

#medicines taken (sum to 100)
library(dplyr)
library(tidyr)
library(expss)

medicine_long <- childfever_data %>%
  
  filter(confirm_malaria == "Yes") %>%
  
  select(
    weights,
    Urban_rural.classification.y,
    zone,
    lga,
    
    type_medicineamodiaquine,
    type_medicineantibiotic,
    type_medicineartemisinin_combina,
    type_medicineartesunate_injectio,
    type_medicinechloroquine,
    type_medicineibuprofen,
    type_medicineother_injection,
    type_medicineother_pill_syrup,
    type_medicineparacetamol_panadol,
    type_medicinequinine,
    type_medicinerectal_artesunate,
    type_medicinesp_fansidar
  ) %>%
  
  pivot_longer(
    cols = starts_with("type_medicine"),
    names_to = "medicine_type",
    values_to = "taken"
  ) %>%
  
  filter(taken == 1) %>%
  
  mutate(
    medicine_type = case_when(
      
      medicine_type == "type_medicineamodiaquine" ~ "Amodiaquine",
      
      medicine_type == "type_medicineantibiotic" ~ "Antibiotic",
      
      medicine_type == "type_medicineartemisinin_combina" ~ "ACT",
      
      medicine_type == "type_medicineartesunate_injectio" ~ "Artesunate Injection",
      
      medicine_type == "type_medicinechloroquine" ~ "Chloroquine",
      
      medicine_type == "type_medicineibuprofen" ~ "Ibuprofen",
      
      medicine_type == "type_medicineother_injection" ~ "Other Injection",
      
      medicine_type == "type_medicineother_pill_syrup" ~ "Other Pill/Syrup",
      
      medicine_type == "type_medicineparacetamol_panadol" ~ "Paracetamol",
      
      medicine_type == "type_medicinequinine" ~ "Quinine",
      
      medicine_type == "type_medicinerectal_artesunate" ~ "Rectal Artesunate",
      
      medicine_type == "type_medicinesp_fansidar" ~ "SP/Fansidar",
      
      TRUE ~ medicine_type
    )
  )
tab_for_medicines_taken <- medicine_long %>%
  
  tab_cols(
    medicine_type,
    total()
  ) %>%
  
  tab_rows(
    Urban_rural.classification.y,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  
  tab_pivot() %>%
  
  tab_caption(
    "Distribution of Medicines Taken Among Confirmed Malaria Cases"
  )

tab_for_medicines_taken
write.csv(
  tab_for_medicines_taken,
  "Malaria_SNT_Output/distribution_medicine_type.csv",
  row.names = FALSE
)

#load women dataset
MCSS_KD_WOMEN_MERGED <- read_csv("MCSS_KD_WOMEN_MERGED.csv")

# Merge by LGA
MCSS_KD_WOMEN_MERGED <- MCSS_KD_WOMEN_MERGED %>%
  left_join(
    lga_classification,
    by = "lga"
  )


#women receiving ANC
library(dplyr)
library(expss)

tab_for_receive_anc <- MCSS_KD_WOMEN_MERGED %>%
  
  tab_cols(receive_anc == "Yes", total()) %>% 
  
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    women_hightest_educ,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Proportion of Women Attending ANC"
  )

tab_for_receive_anc
write.csv(
  tab_for_receive_anc,
  "Malaria_SNT_Output/women_receiving_ANC.csv",
  row.names = FALSE
)

#source of ANC
library(dplyr)
library(expss)

tab_for_where_receive_anc <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(receive_anc == "Yes") %>%
  
  mutate(
    where_receive_anc = case_when(
      is.na(where_receive_anc) | where_receive_anc == "" ~ "Dont_know",
      TRUE ~ where_receive_anc
    )
  ) %>%
  
  tab_cols(
    where_receive_anc,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Where Women Receive ANC"
  )

tab_for_where_receive_anc
write.csv(
  tab_for_where_receive_anc,
  "Malaria_SNT_Output/source_ANC.csv",
  row.names = FALSE
)

#whom seen for ANC
library(dplyr)
library(expss)

tab_for_whom_see_anc <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(receive_anc == "Yes") %>%
  
  mutate(
    whom_see = case_when(
      is.na(whom_see) | whom_see == "" ~ "Dont_know",
      TRUE ~ whom_see
    )
  ) %>%
  
  tab_cols(
    whom_see,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Who Women See for ANC"
  )

tab_for_whom_see_anc
write.csv(
  tab_for_whom_see_anc,
  "Malaria_SNT_Output/whom_seen_ANC.csv",
  row.names = FALSE
)

#how many times women attend anc
library(dplyr)
library(expss)

tab_for_time_anc <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(receive_anc == "Yes") %>%
  
  mutate(
    time_anc = case_when(
      is.na(time_anc) | time_anc == "" ~ "Dont_know",
      TRUE ~ as.character(time_anc)
    )
  ) %>%
  
  tab_cols(
    time_anc,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Number of Times Women Attended ANC"
  )

tab_for_time_anc
write.csv(
  tab_for_time_anc,
  "Malaria_SNT_Output/frequency_of_ANC.csv",
  row.names = FALSE
)

#trimester of first anc visit
library(dplyr)
library(expss)

tab_for_first_anc_trimester <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(
    receive_anc == "Yes",
    !is.na(weeks_anc)
  ) %>%
  
  mutate(
    first_anc_trimester = case_when(
      weeks_anc <= 12 ~ "1st Trimester",
      weeks_anc >= 13 & weeks_anc <= 27 ~ "2nd Trimester",
      weeks_anc >= 28 ~ "3rd Trimester",
      TRUE ~ "Dont_know"
    )
  ) %>%
  
  tab_cols(
    first_anc_trimester,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Trimester in Which Women Attended First ANC"
  )

tab_for_first_anc_trimester
write.csv(
  tab_for_first_anc_trimester,
  "Malaria_SNT_Output/first_ANC_trimester.csv",
  row.names = FALSE
)

#women currently pregnant receiving IPTp
library(dplyr)
library(expss)

tab_for_current_preg_iptp <- MCSS_KD_WOMEN_MERGED %>%
  
  tab_cols(current_preg == "Yes", total()) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Proportion of Women Who Received IPTp"
  )

tab_for_current_preg_iptp
write.csv(
  tab_for_current_preg_iptp,
  "Malaria_SNT_Output/current_preg_IPTp.csv",
  row.names = FALSE
)

#currently pregnant number of IPTp doses
library(dplyr)
library(expss)

tab_for_time_taken_fansidar <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(current_preg == "Yes") %>%
  
  mutate(
    time_taken_fansidar = case_when(
      is.na(time_taken_fansidar) | time_taken_fansidar == "" ~ "Dont_know",
      TRUE ~ as.character(time_taken_fansidar)
    )
  ) %>%
  
  tab_cols(
    time_taken_fansidar,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Number of Times Currently Pregnant Women Received IPTp/Fansidar"
  )

tab_for_time_taken_fansidar
write.csv(
  tab_for_time_taken_fansidar,
  "Malaria_SNT_Output/current_preg_IPTp_uptake.csv",
  row.names = FALSE
)

#women receiving IPTp in their last pregnancy
library(dplyr)
library(expss)

tab_for_last_preg_IPTp <- MCSS_KD_WOMEN_MERGED %>%
  filter(had_live_birth == "Yes") %>%
  
  tab_cols(last_preg == "Yes", total()) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Women Who Received IPTp in Their Last Pregnancy"
  )

tab_for_last_preg_IPTp
write.csv(
  tab_for_last_preg_IPTp,
  "Malaria_SNT_Output/last_preg_IPTp.csv",
  row.names = FALSE
)

#number of times women received IPTp in last pregnancy
library(dplyr)
library(expss)

tab_for_last_preg_iptp_times <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(had_live_birth == "Yes") %>%
  
  mutate(
    time_taken_fansidar_num = as.numeric(time_taken_fansidar),
    
    IPTp1_plus = ifelse(time_taken_fansidar_num >= 1, "Yes", "No"),
    IPTp2_plus = ifelse(time_taken_fansidar_num >= 2, "Yes", "No"),
    IPTp3_plus = ifelse(time_taken_fansidar_num >= 3, "Yes", "No")
  ) %>%
  
  tab_cols(
    IPTp1_plus,
    IPTp2_plus,
    IPTp3_plus,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Women 15-49 with a recent pregnancy who received IPTp1+, IPTp2+, and IPTp3+"
  )

tab_for_last_preg_iptp_times
write.csv(
  tab_for_last_preg_iptp_times,
  "Malaria_SNT_Output/last_preg_IPTp_uptake.csv",
  row.names = FALSE
)
library(dplyr)
library(tidyr)

# Keep only women with last pregnancy
iptp_data <- MCSS_KD_WOMEN_MERGED %>%
  filter(last_preg == "Yes") %>%
  mutate(
    time_taken_fansidar = as.numeric(time_taken_fansidar),
    weights = as.numeric(weights),
    
    `IPTp1+` = ifelse(time_taken_fansidar >= 1, 1, 0),
    `IPTp2+` = ifelse(time_taken_fansidar >= 2, 1, 0),
    `IPTp3+` = ifelse(time_taken_fansidar >= 3, 1, 0),
    `IPTp4+` = ifelse(time_taken_fansidar >= 4, 1, 0)
  )

# Function to calculate weighted percentage
weighted_pct <- function(x, w) {
  round(sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) * 100, 1)
}

# Function to create table by grouping variable
make_iptp_table <- function(data, group_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      `IPTp1+` = weighted_pct(`IPTp1+`, weights),
      `IPTp2+` = weighted_pct(`IPTp2+`, weights),
      `IPTp3+` = weighted_pct(`IPTp3+`, weights),
      `IPTp4+` = weighted_pct(`IPTp4+`, weights),
      `Weighted N` = round(sum(weights, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    rename(Category = .data[[group_var]]) %>%
    mutate(Group = group_var) %>%
    select(Group, Category, `IPTp1+`, `IPTp2+`, `IPTp3+`, `IPTp4+`, `Weighted N`)
}

# Build tables
IPTp_table <- bind_rows(
  make_iptp_table(iptp_data, "lga"),
  make_iptp_table(iptp_data, "zone"),
  make_iptp_table(iptp_data, "wealth_quintile"),
  make_iptp_table(iptp_data, "Urban_rural.classification"),
  iptp_data %>%
    summarise(
      Group = "Total",
      Category = "Total",
      `IPTp1+` = weighted_pct(`IPTp1+`, weights),
      `IPTp2+` = weighted_pct(`IPTp2+`, weights),
      `IPTp3+` = weighted_pct(`IPTp3+`, weights),
      `IPTp4+` = weighted_pct(`IPTp4+`, weights),
      `Weighted N` = round(sum(weights, na.rm = TRUE), 0)
    )
)

IPTp_table
write.csv(
  IPTp_table,
  "Malaria_SNT_Output/IPTp_table.csv",
  row.names = FALSE
)

#women hearing malaria messages
library(dplyr)
library(expss)

tab_for_malaria_messages <- MCSS_KD_WOMEN_MERGED %>%
  
  tab_cols(malaria_msg == "Yes", total()) %>% 
  
  tab_rows(
    Urban_rural.classification,
    women_hightest_educ,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Proportion of Women Who Heard Malaria Messages"
  )

tab_for_malaria_messages
write.csv(
  tab_for_malaria_messages,
  "Malaria_SNT_Output/women_malaria_msg.csv",
  row.names = FALSE
)

#source of malaria messages
library(dplyr)
library(tidyr)
library(expss)

message_long <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(malaria_msg == "Yes") %>%
  
  select(
    weights,
    Urban_rural.classification,
    zone,
    lga,
    
    msg_heard_seenanywhereelse,
    msg_heard_seencommunityeventoutr,
    msg_heard_seencommunityhealthwor,
    msg_heard_seenposterbillboard,
    msg_heard_seenradio,
    msg_heard_seentelevision
  ) %>%
  
  pivot_longer(
    cols = starts_with("msg_heard"),
    names_to = "message_source",
    values_to = "heard"
  ) %>%
  
  filter(heard == 1) %>%
  
  mutate(
    message_source = case_when(
      
      message_source == "msg_heard_seenanywhereelse" ~ "Anywhere Else",
      
      message_source == "msg_heard_seencommunityeventoutr" ~ "Community Event/Outreach",
      
      message_source == "msg_heard_seencommunityhealthwor" ~ "Community Health Worker",
      
      message_source == "msg_heard_seenposterbillboard" ~ "Poster/Billboard",
      
      message_source == "msg_heard_seenradio" ~ "Radio",
      
      message_source == "msg_heard_seentelevision" ~ "Television",
      
      TRUE ~ message_source
    )
  )

tab_for_message_sources <- message_long %>%
  
  tab_cols(
    message_source,
    total()
  ) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Sources of Malaria Messages Among Women Who Heard Malaria Messages"
  )

tab_for_message_sources
write.csv(
  tab_for_message_sources,
  "Malaria_SNT_Output/source_malaria_msg.csv",
  row.names = FALSE
)

#women who know ways to avoid malaria
library(dplyr)
library(expss)

tab_for_way_to_avoid_malaria <- MCSS_KD_WOMEN_MERGED %>%
  
  tab_cols(way_to_avoid_malaria == "Yes", total()) %>% 
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>% 
  
  tab_stat_rpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>% 
  
  tab_pivot() %>%
  
  tab_caption(
    "Proportion of Women Who Know Ways to Prevent Malaria"
  )

tab_for_way_to_avoid_malaria
write.csv(
  tab_for_way_to_avoid_malaria,
  "Malaria_SNT_Output/women_avoid_malaria.csv",
  row.names = FALSE
)

#how women prevent malaria
library(dplyr)
library(tidyr)
library(expss)

prevention_long <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(way_to_avoid_malaria == "Yes") %>%
  
  select(
    weights,
    Urban_rural.classification,
    zone,
    lga,
    
    malaria_preventionavoid_stagnant,
    malaria_preventiondont_know,
    malaria_preventionkeep_surroundi,
    malaria_preventionothers,
    malaria_preventionput_mosquito_s,
    malaria_preventionsleep_inside_a,
    malaria_preventionspray_house_wi,
    malaria_preventiontake_preventat,
    malaria_preventionuse_mosquito_r
  ) %>%
  
  pivot_longer(
    cols = starts_with("malaria_prevention"),
    names_to = "prevention_method",
    values_to = "mentioned"
  ) %>%
  
  mutate(
    mentioned = as.numeric(mentioned),
    
    prevention_method = case_when(
      prevention_method == "malaria_preventionavoid_stagnant" ~ "Avoid Stagnant Water",
      prevention_method == "malaria_preventiondont_know" ~ "Don't Know",
      prevention_method == "malaria_preventionkeep_surroundi" ~ "Keep Surroundings Clean",
      prevention_method == "malaria_preventionothers" ~ "Others",
      prevention_method == "malaria_preventionput_mosquito_s" ~ "Put Mosquito Screens",
      prevention_method == "malaria_preventionsleep_inside_a" ~ "Sleep Inside a Net",
      prevention_method == "malaria_preventionspray_house_wi" ~ "Spray House with Insecticide",
      prevention_method == "malaria_preventiontake_preventat" ~ "Take Preventive Treatment",
      prevention_method == "malaria_preventionuse_mosquito_r" ~ "Use Mosquito Repellent",
      TRUE ~ prevention_method
    )
  )

tab_for_malaria_prevention_methods <- prevention_long %>%
  
  group_by(
    Urban_rural.classification,
    zone,
    lga,
    prevention_method
  ) %>%
  
  summarise(
    weighted_yes = sum(weights * mentioned, na.rm = TRUE),
    weighted_total = sum(weights, na.rm = TRUE),
    proportion = 100 * weighted_yes / weighted_total,
    .groups = "drop"
  )

tab_for_malaria_prevention_methods
tab_for_malaria_prevention_methods_total <- prevention_long %>%
  
  group_by(prevention_method) %>%
  
  summarise(
    weighted_yes = sum(weights * mentioned, na.rm = TRUE),
    weighted_total = sum(weights, na.rm = TRUE),
    proportion = 100 * weighted_yes / weighted_total,
    .groups = "drop"
  )

tab_for_malaria_prevention_methods_total
write.csv(
  tab_for_malaria_prevention_methods_total,
  "Malaria_SNT_Output/women_prevent_methods_total.csv",
  row.names = FALSE
)

# question statements
library(dplyr)
library(tidyr)
library(expss)

attitude_long <- MCSS_KD_WOMEN_MERGED %>%
  select(
    weights,
    pple_get_malaria_during_rainy,
    worry_malaria,
    malaria_can_be_treated,
    weak_child_can_die_frommalaria,
    sleep_entire_night_lots,
    sleep_entire_night_few,
    donotlike_sleep_inside,
    best_start_taking_medicine_at_ho,
    full_dose_medicine,
    take_healthcare_provider,
    comm_sleep_inside_net
  ) %>%
  pivot_longer(
    cols = -weights,
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(
    response = case_when(
      response %in% c("Agree", "agree", "AGREE") ~ "Agree",
      
      response %in% c("Neutral", "neutral", "NEUTRAL") ~ "Neutral",
      
      response %in% c(
        "Don't Know/Uncertain",
        "Dont Know/Uncertain",
        "DON'T KNOW/UNCERTAIN"
      ) ~ "Neutral",
      
      response %in% c("Disagree", "disagree", "DISAGREE") ~ "Disagree",
      
      is.na(response) | response == "" ~ "Neutral",
      
      TRUE ~ "Neutral"
    ),
    
    response = factor(
      response,
      levels = c("Agree", "Neutral", "Disagree")
    ),
    
    question = case_when(
      question == "pple_get_malaria_during_rainy" ~ "People get malaria during rainy season",
      question == "worry_malaria" ~ "Worry about malaria",
      question == "malaria_can_be_treated" ~ "Malaria can be treated",
      question == "weak_child_can_die_frommalaria" ~ "Weak child can die from malaria",
      question == "sleep_entire_night_lots" ~ "Sleep entire night when mosquitoes are many",
      question == "sleep_entire_night_few" ~ "Sleep entire night when mosquitoes are few",
      question == "donotlike_sleep_inside" ~ "Do not like sleeping inside net",
      question == "best_start_taking_medicine_at_ho" ~ "Best to start taking medicine at home",
      question == "full_dose_medicine" ~ "Take full dose of medicine",
      question == "take_healthcare_provider" ~ "Take child to healthcare provider",
      question == "comm_sleep_inside_net" ~ "Community sleeps inside net",
      TRUE ~ question
    )
  )

tab_for_attitude_state_total <- attitude_long %>%
  tab_cols(response, total()) %>%
  tab_rows(question) %>%
  tab_weight(weights) %>%
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  tab_pivot() %>%
  tab_caption(
    "State Total Responses to Malaria Knowledge and Attitude Questions"
  )

tab_for_attitude_state_total
write.csv(
  tab_for_attitude_state_total,
  "Malaria_SNT_Output/women_statement_responses.csv",
  row.names = FALSE
)

names(main_data)
write.csv(
  main_data,
  "Malaria_SNT_Output/MCSS_main_updated.csv",
  row.names = FALSE
)

#wealth quantile analysis
library(dplyr)
library(tidyr)

wealth_vars <- c(
  "source_drinking_water",
  "toilet_facility",
  "share_toilet_facility",
  "hh_use_toilet",
  "toilet_facility_location",
  "type_cookstove",
  "hh_room_sleeping",
  "owns_livestock",
  "cows",
  "other_cattle",
  "horses_donkeys",
  "goats",
  "sheep",
  "chickens",
  "hh_assetsbed",
  "hh_assetschair",
  "hh_assetscomputer",
  "hh_assetselectricity",
  "hh_assetsfan",
  "hh_assetsgenerator",
  "hh_assetsnon_mobile_telephone",
  "hh_assetsradio",
  "hh_assetsrefrigerator",
  "hh_assetssofa",
  "hh_assetstable",
  "hh_assetstelevision",
  "floor",
  "roof",
  "walls"
)

wealth_pca_data <- main_data %>%
  
  select(all_of(wealth_vars)) %>%
  
  mutate(
    across(
      everything(),
      ~ as.numeric(as.factor(.))
    )
  ) %>%
  
  mutate(
    across(
      everything(),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

wealth_pca <- prcomp(
  wealth_pca_data,
  center = TRUE,
  scale. = TRUE
)

main_data <- main_data %>%
  
  mutate(
    wealth_score = wealth_pca$x[, 1],
    
    wealth_quintile_num = ntile(
      wealth_score,
      5
    ),
    
    wealth_quintile = case_when(
      wealth_quintile_num == 1 ~ "Poorest",
      wealth_quintile_num == 2 ~ "Second",
      wealth_quintile_num == 3 ~ "Middle",
      wealth_quintile_num == 4 ~ "Fourth",
      wealth_quintile_num == 5 ~ "Highest"
    )
  )

main_data %>%
  count(wealth_quintile)
write.csv(
  main_data,
  "Malaria_SNT_Output/MCSS_main_with_wealthquint.csv",
  row.names = FALSE
)
names(main_data)
#number of households interviewed
library(dplyr)
library(expss)

weighted_table <- main_data %>%
  
  tab_rows(
    urban_rural_classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  )

unweighted_table <- main_data %>%
  
  tab_rows(
    urban_rural_classification,
    wealth_quintile,
    zone,
    lga,
    total()
  ) %>%
  
  tab_stat_cases(
    label = "Unweighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  )

tab_for_households_interviewed <- weighted_table %>%
  left_join(
    unweighted_table,
    by = "row_labels"
  )

tab_for_households_interviewed
write.csv(
  tab_for_households_interviewed,
  "Malaria_SNT_Output/number_households_interviewed.csv",
  row.names = FALSE
)

#distribution of women interviewed
library(dplyr)
library(expss)

weighted_table <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(
    !is.na(woman_age_1549_id),
    woman_age_1549_id != "",
    !is.na(women_hightest_educ)
  ) %>%
  
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    women_hightest_educ,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  )

unweighted_table <- MCSS_KD_WOMEN_MERGED %>%
  
  filter(
    !is.na(woman_age_1549_id),
    woman_age_1549_id != "",
    !is.na(women_hightest_educ)
  ) %>%
  
  tab_rows(
    Urban_rural.classification,
    wealth_quintile,
    women_hightest_educ,
    zone,
    lga,
    total()
  ) %>%
  
  tab_stat_cases(
    label = "Unweighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  )

tab_for_women_15_49 <- weighted_table %>%
  
  left_join(
    unweighted_table,
    by = "row_labels"
  )

tab_for_women_15_49
write.csv(
  tab_for_women_15_49,
  "Malaria_SNT_Output/women_age_15-49.csv",
  row.names = FALSE
)

#number of children U5
library(dplyr)
library(expss)

make_children_u5_count <- function(data, group_var) {
  
  data %>%
    mutate(
      children_u5 = as.numeric(children_u5)
    ) %>%
    filter(
      !is.na(children_u5)
    ) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      `Unweighted Count` = sum(children_u5, na.rm = TRUE),
      `Weighted Count` = sum(children_u5 * weights, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      row_labels = paste(group_var, .data[[group_var]], sep = "|")
    ) %>%
    select(row_labels, `Weighted Count`, `Unweighted Count`)
}

tab_for_children_u5 <- bind_rows(
  make_children_u5_count(main_data, "Urban_rural.classification"),
  make_children_u5_count(main_data, "zone"),
  make_children_u5_count(main_data, "lga"),
  
  main_data %>%
    mutate(
      children_u5 = as.numeric(children_u5)
    ) %>%
    summarise(
      row_labels = "#Total",
      `Weighted Count` = sum(children_u5 * weights, na.rm = TRUE),
      `Unweighted Count` = sum(children_u5, na.rm = TRUE)
    )
)

tab_for_children_u5
write.csv(
  tab_for_children_u5,
  "Malaria_SNT_Output/children_u5.csv",
  row.names = FALSE
)

#household average income
library(dplyr)

make_average_income <- function(data, group_var) {
  
  data %>%
    mutate(
      average_income = as.numeric(average_income)
    ) %>%
    filter(
      !is.na(average_income)
    ) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      `Average Income` = round(
        weighted.mean(
          average_income,
          weights,
          na.rm = TRUE
        ),
        1
      ),
      
      `Weighted N` = round(
        sum(weights, na.rm = TRUE),
        1
      ),
      
      `Unweighted N` = n(),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      row_labels = paste(
        group_var,
        .data[[group_var]],
        sep = "|"
      )
    ) %>%
    
    select(
      row_labels,
      `Average Income`,
      `Weighted N`,
      `Unweighted N`
    )
}

tab_for_average_income <- bind_rows(
  
  make_average_income(
    main_data,
    "Urban_rural.classification"
  ),
  
  make_average_income(
    main_data,
    "zone"
  ),
  
  make_average_income(
    main_data,
    "lga"
  ),
  
  main_data %>%
    mutate(
      average_income = as.numeric(average_income)
    ) %>%
    filter(
      !is.na(average_income)
    ) %>%
    summarise(
      row_labels = "#Total",
      
      `Average Income` = round(
        weighted.mean(
          average_income,
          weights,
          na.rm = TRUE
        ),
        1
      ),
      
      `Weighted N` = round(
        sum(weights, na.rm = TRUE),
        1
      ),
      
      `Unweighted N` = n()
    )
)

tab_for_average_income
write.csv(
  tab_for_average_income,
  "Malaria_SNT_Output/average_income.csv",
  row.names = FALSE
)

#household head gender
library(dplyr)
library(expss)

tab_for_gender_distribution <- main_data %>%
  
  mutate(
    gender = case_when(
      is.na(gender) | gender == "" ~ "Dont_know",
      TRUE ~ as.character(gender)
    )
  ) %>%
  
  tab_cols(
    gender,
    total()
  ) %>%
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "Gender Distribution of Household Heads"
  )

tab_for_gender_distribution
write.csv(
  tab_for_gender_distribution,
  "Malaria_SNT_Output/hh_head_igender.csv",
  row.names = FALSE
)

#rural urban distribution of households
library(dplyr)
library(expss)

tab_for_rural_urban_households <- main_data %>%
  
  mutate(
    Urban_rural.classification = case_when(
      is.na(Urban_rural.classification) |
        Urban_rural.classification == "" ~ "Dont_know",
      
      TRUE ~ as.character(Urban_rural.classification)
    )
  ) %>%
  
  tab_cols(
    Urban_rural.classification,
    total()
  ) %>%
  
  tab_rows(
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "Number of Rural and Urban Households by LGA"
  )

tab_for_rural_urban_households
write.csv(
  tab_for_rural_urban_households,
  "Malaria_SNT_Output/hh_rural_urban_distro.csv",
  row.names = FALSE
)

#occupation of household heads
library(dplyr)
library(expss)

tab_for_occupation_hh_head <- main_data %>%
  
  mutate(
    occupation_hh_head = case_when(
      is.na(occupation_hh_head) |
        occupation_hh_head == "" ~ "Dont_know",
      
      TRUE ~ as.character(occupation_hh_head)
    )
  ) %>%
  
  tab_cols(
    occupation_hh_head,
    total()
  ) %>%
  
  tab_rows(
    Urban_rural.classification,
    zone,
    lga,
    total()
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "Distribution of Occupation of Household Heads"
  )

tab_for_occupation_hh_head
write.csv(
  tab_for_occupation_hh_head,
  "Malaria_SNT_Output/occupation_hh_head.csv",
  row.names = FALSE
)

#Distribution of the de jure household population by 5-year age groups, by sex
library(dplyr)
library(expss)

tab_for_age_gender_state_total <- MCSS_KD_MEMBERS_MERGED %>%
  
  mutate(
    age_diff = as.numeric(age_diff),
    
    age_group = case_when(
      age_diff < 5 ~ "<5",
      age_diff >= 5  & age_diff <= 9  ~ "5-9",
      age_diff >= 10 & age_diff <= 14 ~ "10-14",
      age_diff >= 15 & age_diff <= 19 ~ "15-19",
      age_diff >= 20 & age_diff <= 24 ~ "20-24",
      age_diff >= 25 & age_diff <= 29 ~ "25-29",
      age_diff >= 30 & age_diff <= 34 ~ "30-34",
      age_diff >= 35 & age_diff <= 39 ~ "35-39",
      age_diff >= 40 & age_diff <= 44 ~ "40-44",
      age_diff >= 45 & age_diff <= 49 ~ "45-49",
      age_diff >= 50 & age_diff <= 54 ~ "50-54",
      age_diff >= 55 & age_diff <= 59 ~ "55-59",
      age_diff >= 60 & age_diff <= 64 ~ "60-64",
      age_diff >= 65 & age_diff <= 69 ~ "65-69",
      age_diff >= 70 & age_diff <= 74 ~ "70-74",
      age_diff >= 75 & age_diff <= 79 ~ "75-79",
      age_diff >= 80 ~ "80+",
      TRUE ~ NA_character_
    ),
    
    age_group = factor(
      age_group,
      levels = c(
        "<5", "5-9", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65-69", "70-74",
        "75-79", "80+"
      )
    ),
    
    hh_mem_gender = case_when(
      hh_mem_gender %in% c("Male", "male", "M") ~ "Male",
      hh_mem_gender %in% c("Female", "female", "F") ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(age_group),
    !is.na(hh_mem_gender)
  ) %>%
  
  tab_cols(
    hh_mem_gender,
    total()
  ) %>%
  
  tab_rows(
    age_group
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "State Total Age Distribution of Household Members by Gender"
  )

tab_for_age_gender_state_total
write.csv(
  tab_for_age_gender_state_total,
  "Malaria_SNT_Output/dejure_age_distro.csv",
  row.names = FALSE
)
names(MCSS_KD_WOMEN_MERGED)
library(dplyr)

MCSS_KD_WOMEN_MERGED <- MCSS_KD_WOMEN_MERGED %>%
  
  mutate(
    Hhid_linenum = paste0(
      hhid,
      woman_age_1549_id
    )
  )
age_lookup <- MCSS_KD_MEMBERS_MERGED %>%
  
  select(
    Hhid_linenum,
    age_diff
  ) %>%
  
  distinct()

# Join age_diff into women dataset
MCSS_KD_WOMEN_MERGED <- MCSS_KD_WOMEN_MERGED %>%
  
  left_join(
    age_lookup,
    by = "Hhid_linenum"
  )
names(MCSS_KD_WOMEN_MERGED)
#age distribution of women
library(dplyr)
library(expss)

tab_for_women_age_distribution <- MCSS_KD_WOMEN_MERGED %>%
  
  mutate(
    age_diff = as.numeric(age_diff),
    
    age_group = case_when(
      age_diff >= 15 & age_diff <= 19 ~ "15-19",
      age_diff >= 20 & age_diff <= 24 ~ "20-24",
      age_diff >= 25 & age_diff <= 29 ~ "25-29",
      age_diff >= 30 & age_diff <= 34 ~ "30-34",
      age_diff >= 35 & age_diff <= 39 ~ "35-39",
      age_diff >= 40 & age_diff <= 44 ~ "40-44",
      age_diff >= 45 & age_diff <= 49 ~ "45-49",
      age_diff >= 50 ~ "50+",
      TRUE ~ NA_character_
    ),
    
    age_group = factor(
      age_group,
      levels = c(
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50+"
      )
    )
  ) %>%
  
  filter(
    !is.na(age_group)
  ) %>%
  
  tab_rows(
    age_group
  ) %>%
  
  tab_weight(weights) %>%
  
  tab_stat_cases(
    label = "Weighted N"
  ) %>%
  
  tab_pivot(
    stat_position = "inside_columns"
  ) %>%
  
  tab_caption(
    "Age Distribution of Women Based on age_diff"
  )

tab_for_women_age_distribution
write.csv(
  tab_for_women_age_distribution,
  "Malaria_SNT_Output/women_age_distro.csv",
  row.names = FALSE
)

#analysis of sources of drinking water
cols_with_Water <- names(main_data)[
  grepl(
    "source_drinking_water",
    names(main_data)
  )
]
install.packages("fastDummies")
install.packages("janitor")
library(dplyr)
library(fastDummies)
library(janitor)

# Create dummy columns from source_drinking_water
main_data <- main_data %>%
  
  mutate(
    source_drinking_water = as.character(source_drinking_water)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "source_drinking_water",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

names(main_data)
improved_Din = c("source_drinking_water_borehole", "source_drinking_water_bottled_sachet_water", "source_drinking_water_dug_well",
                 "source_drinking_water_piped_water","source_drinking_water_purchase_from_water_vendor", "source_drinking_water_rainwater")
unimproved_Din <- c("source_drinking_water_surface_water_river_dam")

main_data <- main_data %>%
  
  mutate(
    improved_wFac = if_else(
      rowSums(
        select(., all_of(improved_Din)),
        na.rm = TRUE
      ) >= 1,
      1,
      0
    ),
    
    unimproved_wFac = if_else(
      rowSums(
        select(., all_of(unimproved_Din)),
        na.rm = TRUE
      ) >= 1,
      1,
      0
    )
  )
#main data survey design
library(dplyr)
library(survey)

main_data_design <- main_data %>%
  filter(
    !is.na(cluster_id),
    !is.na(weights)
  ) %>%
  svydesign(
    data = .,
    strata = NULL,
    id = ~cluster_id,
    nest = TRUE,
    weights = ~weights
  )
var_water = c(cols_with_Water, "improved_wFac", "unimproved_wFac")

#function for multiple generation of tables
library(dplyr)
library(survey)

run_and_save_svyby <- function(
    var_list,
    by,
    design,
    bycol,
    env = .GlobalEnv,
    statistic = svymean,
    ...
) {
  
  all_results <- list()
  
  for (var in var_list) {
    
    formula <- as.formula(paste0("~", var))
    
    df <- suppressWarnings(
      svyby(
        formula = formula,
        by = by,
        design = design,
        FUN = statistic,
        na.rm = TRUE,
        vartype = c("se"),
        ...
      )
    )
    
    df$variable <- var
    
    assign(
      paste0("TablewithSE_", var, "_by_", bycol),
      df,
      envir = env
    )
    
    all_results[[var]] <- df
  }
  
  combined <- bind_rows(all_results)
  
  return(combined)
}

tab_of_DrinkingWaterSourcebyResidence <- run_and_save_svyby(
  var_list = var_water,
  by = ~urban_rural_classification,
  bycol = "urban_rural_classification",
  design = main_data_design
)
tab_of_DrinkingWaterSourceSenDist <- run_and_save_svyby(
  var_list = var_water,
  by = ~zone,
  bycol = "Zone",
  design = main_data_design
)
#table on improved and unimproved drinking water facilities
table_for_waterSource <- main_data %>%
  tab_cells(
    improved_wFac,
    source_drinking_water_borehole,
    source_drinking_water_bottled_sachet_water,
    source_drinking_water_dug_well,
    source_drinking_water_piped_water,
    source_drinking_water_purchase_from_water_vendor,
    source_drinking_water_rainwater,
    unimproved_wFac,
    source_drinking_water_surface_water_river_dam
  ) %>%
  tab_cols(
    urban_rural_classification,
    wealth_quintile,
    total()
  ) %>% 
  tab_weight(weights) %>% 
  tab_stat_cpct(
    label = "Proportion Yes",
    total_statistic = "w_cases"
  ) %>%
  tab_pivot()

table_for_waterSource
write.csv(
  table_for_waterSource,
  "Malaria_SNT_Output/imprpved_unimp_watersource.csv",
  row.names = FALSE
)

#Sanitation facility
cols_with_toilet <- names(main_data)[grepl("toilet_facility", names(main_data))]
library(dplyr)
library(fastDummies)
library(janitor)

main_data <- main_data %>%
  
  mutate(
    toilet_facility = as.character(toilet_facility)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "toilet_facility",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

improved_San_l <- c(
  "toilet_facility_composting_toilet",
  "toilet_facility_flush_or_pour_flush_toilet",
  "toilet_facility_pit_latrine"
)

unimproved_San_l <- c(
  "toilet_facility_bucket_toilet",
  "toilet_facility_no_facility_bush_field",
  "toilet_facility_open_pit"
)

main_data <- main_data %>%
  mutate(
    improved_Sanitation = if_else(
      rowSums(select(., all_of(improved_San_l)), na.rm = TRUE) >= 1,
      1,
      0
    ),
    
    unimproved_Sanitation = if_else(
      rowSums(select(., all_of(unimproved_San_l)), na.rm = TRUE) >= 1,
      1,
      0
    )
  )


# Specify the HHData design
main_data_design <- main_data %>%
  
  filter(
    !is.na(cluster_id),
    !is.na(weights)
  ) %>%
  
  svydesign(
    data = .,
    strata = NULL,
    id = ~cluster_id,
    nest = TRUE,
    weights = ~weights
  )
var_toilet <- c(
  improved_San_l,
  unimproved_San_l,
  "improved_Sanitation",
  "unimproved_Sanitation"
)
tab_of_Sanitation_Residence <- run_and_save_svyby(
  var_list = var_toilet,
  by = ~urban_rural_classification,
  bycol = "urban_rural_classification",
  design = main_data_design
)
tab_of_Sanitation_Zone <- run_and_save_svyby(
  var_list = var_toilet,
  by = ~zone,
  bycol = "zone",
  design = main_data_design
)

table_for_sanitation <- main_data %>%
  tab_cells(
    improved_Sanitation,
    toilet_facility_composting_toilet,
    toilet_facility_flush_or_pour_flush_toilet,
    toilet_facility_pit_latrine,
    toilet_facility_bucket_toilet,
    toilet_facility_no_facility_bush_field,
    toilet_facility_open_pit,
    unimproved_Sanitation
  ) %>%
  tab_cols(
    urban_rural_classification,
    wealth_quintile,
    lga,
    total()
  ) %>% 
  tab_weight(weights) %>% 
  tab_stat_cpct() %>%
  tab_pivot() %>%
  set_caption("table_for_Housing Sanitation")

table_for_sanitation
write.csv(
  table_for_sanitation,
  "Malaria_SNT_Output/imprpved_unimp_sanitation.csv",
  row.names = FALSE
)

#sanitation service ladder
library(dplyr)
library(survey)

main_data <- main_data %>%
  mutate(
    Sanitation_basic = case_when(
      share_toilet_facility == "No" & improved_Sanitation == 1 ~ 1,
      TRUE ~ 0
    ),
    
    Sanitation_limited = case_when(
      as.numeric(hh_use_toilet) >= 2 & improved_Sanitation == 1 ~ 1,
      TRUE ~ 0
    ),
    
    hh_oneRoom = case_when(
      as.numeric(hh_room_sleeping) == 1 ~ 1,
      TRUE ~ 0
    ),
    
    hh_twoRoom = case_when(
      as.numeric(hh_room_sleeping) == 2 ~ 1,
      TRUE ~ 0
    ),
    
    hh_threeorMRoom = case_when(
      as.numeric(hh_room_sleeping) >= 3 ~ 1,
      TRUE ~ 0
    ),
    
    clean_fuels_and_tech = case_when(
      type_cookstove %in% c(
        "Cooking Gas Stove",
        "Electric Stove",
        "Electric/Solar Cooker"
      ) ~ 1,
      TRUE ~ 0
    ),
    
    Other_fuels_and_technologies = case_when(
      type_cookstove %in% c(
        "Kerosene Stove",
        "Three Stone Open Fire"
      ) ~ 1,
      TRUE ~ 0
    )
  )

main_data_design <- main_data %>%
  filter(
    !is.na(cluster_id),
    !is.na(weights)
  ) %>%
  svydesign(
    data = .,
    strata = NULL,
    id = ~cluster_id,
    nest = TRUE,
    weights = ~weights
  )

var_SanLadder <- c(
  "Sanitation_basic",
  "Sanitation_limited"
)

main_data <- main_data %>%
  mutate(
    Sanitation_disagre = case_when(
      share_toilet_facility == "No" & improved_Sanitation == 1 ~ "Basic",
      as.numeric(hh_use_toilet) >= 2 & improved_Sanitation == 1 ~ "Limited",
      unimproved_Sanitation == 1 ~ "Unimproved",
      TRUE ~ "Unimproved"
    )
  )

tab_of_Sanitation_Residence <- run_and_save_svyby(
  var_list = var_SanLadder,
  by = ~urban_rural_classification,
  bycol = "urban_rural_classification",
  design = main_data_design
)

table_SanitationLadder <- main_data %>% 
  cross_rpct(
    cell_vars = list(
      urban_rural_classification,
      zone,
      wealth_quintile,
      lga,
      total()
    ),
    col_vars = list(
      Sanitation_basic,
      Sanitation_limited,
      unimproved_Sanitation,
      total()
    ),
    weight = weights,
    total_label = "Total",
    total_statistic = "w_cases",
    expss_digits(digits = 1)
  ) %>%
  set_caption("Sanitation Service Ladder")

table_SanitationLadder
#sanitation ladder disaggregation
table_Sanitation_disaggregation <- main_data %>% 
  cross_cpct(
    col_vars = list(
      urban_rural_classification,
      zone,
      wealth_quintile,
      lga,
      total()
    ),
    
    cell_vars = list(
      Sanitation_disagre,
      total()
    ),
    
    weight = weights,
    total_label = "Total",
    total_statistic = "w_cases",
    expss_digits(digits = 1)
  ) %>%
  
  set_caption(
    "Sanitation Service Ladder"
  )

table_Sanitation_disaggregation
write.csv(
  table_Sanitation_disaggregation,
  "Malaria_SNT_Output/sanitation_service_ladder.csv",
  row.names = FALSE
)

#housing construction materials
main_data <- main_data %>%
  
  mutate(
    floor = as.character(floor)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "floor",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

names(main_data)[grepl("^floor_", names(main_data))]

table_for_floor_construction <- main_data %>%
  tab_cells(
    floor_carpet,
    floor_cement,
    floor_ceramic_tiles,
    floor_earth_sanddung,
    floor_other,
    floor_palm_bamboo,
    floor_polished_wood,
    floor_wood_planks
  ) %>%
  tab_cols(
    urban_rural_classification,
    wealth_quintile,
    total()
  ) %>% 
  tab_weight(weights) %>% 
  tab_stat_cpct(
    total_statistic = "w_cases"
  ) %>%
  tab_pivot() %>%
  set_caption("table_for_floor_construction")

table_for_floor_construction
write.csv(
  table_for_floor_construction,
  "Malaria_SNT_Output/floor_construction.csv",
  row.names = FALSE
)

#roof materials
main_data <- main_data %>%
  
  mutate(
    roof = as.character(roof)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "roof",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

table_for_roof_construction <- main_data %>%
  tab_cells(
    roof_cement,
    roof_no_roof,
    roof_palm_bamboo,
    roof_roofing_shingles,
    roof_thatch_palm_leaf,
    roof_wood_wood_planks
  ) %>%
  tab_cols(
    urban_rural_classification,
    wealth_quintile,
    total()
  ) %>% 
  tab_weight(weights) %>%
  tab_stat_cpct() %>%
  tab_pivot() %>%
  set_caption("table_for_roof_construction")

table_for_roof_construction
write.csv(
  table_for_roof_construction,
  "Malaria_SNT_Output/roof_construction.csv",
  row.names = FALSE
)

#wall construction
var_walls <- names(main_data)[
  grepl(
    "walls",
    names(main_data)
  )
]

main_data <- main_data %>%
  
  mutate(
    walls = as.character(walls)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "walls",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

table_for_walls_construction <- main_data %>%
  tab_cells(
    walls_bamboo_with_mud,
    walls_bricks,
    walls_cane_palm_trunks,
    walls_cement_blocks,
    walls_covered_mud_adobe,
    walls_mud,
    walls_no_walls,
    walls_wood_planks
  ) %>%
  tab_cols(
    urban_rural_classification,
    wealth_quintile,
    total()
  ) %>% 
  tab_weight(weights) %>%
  tab_stat_cpct() %>%
  tab_pivot() %>%
  set_caption("table_for_walls_construction")

table_for_walls_construction
write.csv(
  table_for_walls_construction,
  "Malaria_SNT_Output/walls_construction.csv",
  row.names = FALSE
)

#rooms used for sleeping
main_data <- main_data %>%
  
  mutate(
    hh_room_sleeping = as.character(hh_room_sleeping)
  ) %>%
  
  fastDummies::dummy_cols(
    select_columns = "hh_room_sleeping",
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE
  ) %>%
  
  janitor::clean_names()

library(dplyr)

main_data <- main_data %>%
  mutate(
    hh_oneRoom = case_when(
      as.numeric(hh_room_sleeping) == 1 ~ 1,
      TRUE ~ 0
    ),
    
    hh_twoRoom = case_when(
      as.numeric(hh_room_sleeping) == 2 ~ 1,
      TRUE ~ 0
    ),
    
    hh_threeorMRoom = case_when(
      as.numeric(hh_room_sleeping) >= 3 ~ 1,
      TRUE ~ 0
    )
  )
table_for_rooms_sleeping <- main_data %>%
  tab_cells(
    hh_oneRoom,
    hh_twoRoom,
    hh_threeorMRoom
  ) %>%
  tab_cols(
    urban_rural_classification,
    zone,
    wealth_quintile,
    total()
  ) %>% 
  tab_weight(weights) %>% 
  tab_stat_cpct(
    total_statistic = "w_cases"
  ) %>%
  tab_pivot() %>%
  set_caption(
    "table_for_Rooms_sleep for Sleeping"
  )

table_for_rooms_sleeping
write.csv(
  table_for_rooms_sleeping,
  "Malaria_SNT_Output/rooms_for_sleeping.csv",
  row.names = FALSE
)

#wealth quintile distribution
library(dplyr)
library(expss)

table_for_wealth_distribution <- MCSS_KD_MEMBERS_MERGED %>%
  mutate(
    wealth_quintile = factor(
      wealth_quintile,
      levels = c("Poorest", "Second", "Middle", "Fourth", "Highest")
    )
  ) %>%
  tab_cols(
    wealth_quintile,
    total()
  ) %>%
  tab_rows(
    Urban_rural.classification,
    zone,
    total()
  ) %>%
  tab_weight(weights) %>%
  tab_stat_rpct(
    label = "Proportion",
    total_statistic = "w_cases"
  ) %>%
  tab_pivot() %>%
  set_caption(
    "Household Distribution of Wealth Quintile by Zone and Rural/Urban"
  )

table_for_wealth_distribution
write.csv(
  table_for_wealth_distribution,
  "Malaria_SNT_Output/wealth_quintile_distro.csv",
  row.names = FALSE
)
#women perceived severity, succeptibility to malaria
library(dplyr)

# Create indicators
MCSS_KD_WOMEN_MERGED <- MCSS_KD_WOMEN_MERGED %>%
  mutate(
    weights = as.numeric(weights),
    
    succeptibi_risk = ifelse(
      worry_malaria == "Agree" |
        pple_get_malaria_during_rainy == "Disagree",
      1, 0
    ),
    
    severe_mal = ifelse(
      malaria_can_be_treated == "Disagree" |
        weak_child_can_die_frommalaria == "Disagree",
      1, 0
    ),
    
    self_eff = ifelse(
      sleep_entire_night_lots == "Agree" |
        sleep_entire_night_few == "Agree",
      1, 0
    ),
    
    mal_related_behavi = ifelse(
      donotlike_sleep_inside == "Disagree" |
        best_start_taking_medicine_at_ho == "Disagree" |
        full_dose_medicine == "Agree",
      1, 0
    ),
    
    comm_norms = ifelse(
      take_healthcare_provider == "Agree" |
        comm_sleep_inside_net == "Agree",
      1, 0
    )
  )

# Weighted percentage function
weighted_pct <- function(x, w) {
  round(sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) * 100, 1)
}

# Function to summarise by group
make_ideation_table <- function(data, group_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      `Susceptibility/Risk` = weighted_pct(succeptibi_risk, weights),
      `Severe Malaria` = weighted_pct(severe_mal, weights),
      `Self-Efficacy` = weighted_pct(self_eff, weights),
      `Malaria-related Behaviour` = weighted_pct(mal_related_behavi, weights),
      `Community Norms` = weighted_pct(comm_norms, weights),
      `Weighted N` = round(sum(weights, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    rename(Category = .data[[group_var]]) %>%
    mutate(Group = group_var) %>%
    select(
      Group,
      Category,
      `Susceptibility/Risk`,
      `Severe Malaria`,
      `Self-Efficacy`,
      `Malaria-related Behaviour`,
      `Community Norms`,
      `Weighted N`
    )
}

# Final table
Ideation_table <- bind_rows(
  make_ideation_table(MCSS_KD_WOMEN_MERGED, "lga"),
  make_ideation_table(MCSS_KD_WOMEN_MERGED, "zone"),
  make_ideation_table(MCSS_KD_WOMEN_MERGED, "wealth_quintile"),
  make_ideation_table(MCSS_KD_WOMEN_MERGED, "Urban_rural.classification"),
  
  MCSS_KD_WOMEN_MERGED %>%
    summarise(
      Group = "Total",
      Category = "Total",
      `Susceptibility/Risk` = weighted_pct(succeptibi_risk, weights),
      `Severe Malaria` = weighted_pct(severe_mal, weights),
      `Self-Efficacy` = weighted_pct(self_eff, weights),
      `Malaria-related Behaviour` = weighted_pct(mal_related_behavi, weights),
      `Community Norms` = weighted_pct(comm_norms, weights),
      `Weighted N` = round(sum(weights, na.rm = TRUE), 0)
    )
)

Ideation_table
write.csv(
  Ideation_table,
  "Malaria_SNT_Output/women_succept_severity.csv",
  row.names = FALSE
)

library(dplyr)
library(tidyr)

medicine_summary <- childfever_data %>%
  
  filter(
    !is.na(type_medicine),
    type_medicine != ""
  ) %>%
  
  separate_rows(type_medicine, sep = "\\s+") %>%
  
  mutate(
    type_medicine = dplyr::recode(
      type_medicine,
      artemisinin_combina = "ACT",
      artemisinin_combination = "ACT",
      paracetamol_panadol = "Paracetamol/Panadol",
      other_pill_syrup = "Other Pill/Syrup",
      other_injection = "Other Injection",
      sp_fansidar = "SP/Fansidar",
      artesunate_injectio = "Artesunate Injection",
      artesunate_injection = "Artesunate Injection",
      rectal_artesunate = "Rectal Artesunate",
      amodiaquine = "Amodiaquine",
      antibiotic = "Antibiotic",
      chloroquine = "Chloroquine",
      ibuprofen = "Ibuprofen",
      quinine = "Quinine"
    )
  ) %>%
  
  count(type_medicine, name = "n") %>%
  
  mutate(
    proportion = round(
      100 * n /
        nrow(
          childfever_data %>%
            filter(
              !is.na(type_medicine),
              type_medicine != ""
            )
        ),
      1
    )
  ) %>%
  
  arrange(desc(proportion))

medicine_summary
write.csv(
  medicine_summary,
  "Malaria_SNT_Output/medine_summary.csv",
  row.names = FALSE
)

library(dplyr)
library(tidyr)
library(stringr)

#type of medicine taken
medicine_binary <- childfever_data %>%
  
  filter(
    !is.na(type_medicine),
    type_medicine != ""
  ) %>%
  
  mutate(
    type_medicine = str_squish(type_medicine),
    
    ACT = if_else(str_detect(type_medicine, "artemisinin_combina|artemisinin_combination"), 1, 0),
    `Paracetamol/Panadol` = if_else(str_detect(type_medicine, "paracetamol_panadol"), 1, 0),
    Antibiotic = if_else(str_detect(type_medicine, "antibiotic"), 1, 0),
    Ibuprofen = if_else(str_detect(type_medicine, "ibuprofen"), 1, 0),
    `SP/Fansidar` = if_else(str_detect(type_medicine, "sp_fansidar"), 1, 0),
    Chloroquine = if_else(str_detect(type_medicine, "chloroquine"), 1, 0),
    Quinine = if_else(str_detect(type_medicine, "quinine"), 1, 0),
    Amodiaquine = if_else(str_detect(type_medicine, "amodiaquine"), 1, 0),
    `Artesunate Injection` = if_else(str_detect(type_medicine, "artesunate_injectio|artesunate_injection"), 1, 0),
    `Rectal Artesunate` = if_else(str_detect(type_medicine, "rectal_artesunate"), 1, 0),
    `Other Injection` = if_else(str_detect(type_medicine, "other_injection"), 1, 0),
    `Other Pill/Syrup` = if_else(str_detect(type_medicine, "other_pill_syrup"), 1, 0)
  )

medicine_vars <- c(
  "ACT",
  "Paracetamol/Panadol",
  "Antibiotic",
  "Ibuprofen",
  "SP/Fansidar",
  "Chloroquine",
  "Quinine",
  "Amodiaquine",
  "Artesunate Injection",
  "Rectal Artesunate",
  "Other Injection",
  "Other Pill/Syrup"
)

# 2. Function to create grouped tables
make_medicine_table <- function(data, group_var) {
  
  data %>%
    group_by(row_labels = .data[[group_var]]) %>%
    summarise(
      across(
        all_of(medicine_vars),
        list(
          Proportion = ~ round(100 * sum(weights * .x, na.rm = TRUE) / sum(weights, na.rm = TRUE), 1),
          `Weighted N` = ~ round(sum(weights * .x, na.rm = TRUE), 0)
        ),
        .names = "{.col}|{.fn}"
      ),
      `#Total|Weighted N` = round(sum(weights, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    mutate(row_labels = paste0(group_var, "|", row_labels)) %>%
    select(row_labels, everything())
}

# 3. Create tables for each row group
medicine_by_urban_rural <- make_medicine_table(
  medicine_binary,
  "Urban_rural.classification"
)

medicine_by_zone <- make_medicine_table(
  medicine_binary,
  "zone"
)

medicine_by_lga <- make_medicine_table(
  medicine_binary,
  "lga"
)

medicine_by_wealth <- make_medicine_table(
  medicine_binary,
  "wealth_quintile"
)

# 4. Create total row
medicine_total <- medicine_binary %>%
  summarise(
    across(
      all_of(medicine_vars),
      list(
        Proportion = ~ round(100 * sum(weights * .x, na.rm = TRUE) / sum(weights, na.rm = TRUE), 1),
        `Weighted N` = ~ round(sum(weights * .x, na.rm = TRUE), 0)
      ),
      .names = "{.col}|{.fn}"
    ),
    `#Total|Weighted N` = round(sum(weights, na.rm = TRUE), 0)
  ) %>%
  mutate(row_labels = "Total|Total") %>%
  select(row_labels, everything())

# 5. Combine final output
tab_for_medicine_types_binary <- bind_rows(
  medicine_by_urban_rural,
  medicine_by_zone,
  medicine_by_lga,
  medicine_by_wealth,
  medicine_total
)

tab_for_medicine_types_binary
write.csv(
  tab_for_medicine_types_binary,
  "Malaria_SNT_Output/children_medicine_type.csv",
  row.names = FALSE
)

#type of medicine taken (grouped ACT)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Create binary medicine indicators from type_medicine
# Denominator = children with at least one medicine recorded

medicine_binary <- childfever_data %>%
  
  filter(
    confirm_malaria == "Yes",
    !is.na(type_medicine),
    type_medicine != ""
  ) %>%
  
  mutate(
    type_medicine = str_squish(type_medicine),
    
    ACT = if_else(
      str_detect(
        type_medicine,
        "artemisinin_combina|artemisinin_combination|amodiaquine|chloroquine|quinine|artesunate_injectio|artesunate_injection|rectal_artesunate|artesunate_injection|other_injection"
      ),
      1,
      0
    ),
    
    `Paracetamol/Panadol` = if_else(
      str_detect(type_medicine, "paracetamol_panadol"),
      1,
      0
    ),
    
    Antibiotic = if_else(
      str_detect(type_medicine, "antibiotic"),
      1,
      0
    ),
    
    Ibuprofen = if_else(
      str_detect(type_medicine, "ibuprofen"),
      1,
      0
    ),
    
    `SP/Fansidar` = if_else(
      str_detect(type_medicine, "sp_fansidar"),
      1,
      0
    ),
    
    `Other Pill/Syrup` = if_else(
      str_detect(type_medicine, "other_pill_syrup"),
      1,
      0
    )
  )

# 2. Medicine variables to include in the final table

medicine_vars <- c(
  "ACT",
  "Paracetamol/Panadol",
  "Antibiotic",
  "Ibuprofen",
  "SP/Fansidar",
  "Other Pill/Syrup"
)

# 3. Function to calculate Proportion and Weighted N by row group

make_medicine_table <- function(data, group_var) {
  
  data %>%
    
    group_by(row_labels = .data[[group_var]]) %>%
    
    summarise(
      across(
        all_of(medicine_vars),
        list(
          Proportion = ~ round(
            100 * sum(weights * .x, na.rm = TRUE) / sum(weights, na.rm = TRUE),
            1
          ),
          `Weighted N` = ~ round(
            sum(weights * .x, na.rm = TRUE),
            0
          )
        ),
        .names = "{.col}|{.fn}"
      ),
      
      `#Total|Weighted N` = round(sum(weights, na.rm = TRUE), 0),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      row_labels = paste0(group_var, "|", row_labels)
    ) %>%
    
    select(row_labels, everything())
}

# 4. Create row tables

medicine_by_urban_rural <- make_medicine_table(
  medicine_binary,
  "Urban_rural.classification"
)

medicine_by_zone <- make_medicine_table(
  medicine_binary,
  "zone"
)

medicine_by_lga <- make_medicine_table(
  medicine_binary,
  "lga"
)

medicine_by_wealth <- make_medicine_table(
  medicine_binary,
  "wealth_quintile"
)

# 5. Create total row

medicine_total <- medicine_binary %>%
  
  summarise(
    across(
      all_of(medicine_vars),
      list(
        Proportion = ~ round(
          100 * sum(weights * .x, na.rm = TRUE) / sum(weights, na.rm = TRUE),
          1
        ),
        `Weighted N` = ~ round(
          sum(weights * .x, na.rm = TRUE),
          0
        )
      ),
      .names = "{.col}|{.fn}"
    ),
    
    `#Total|Weighted N` = round(sum(weights, na.rm = TRUE), 0)
  ) %>%
  
  mutate(
    row_labels = "Total|Total"
  ) %>%
  
  select(row_labels, everything())

# 6. Combine final output

tab_for_medicine_types_binary <- bind_rows(
  medicine_by_urban_rural,
  medicine_by_zone,
  medicine_by_lga,
  medicine_by_wealth,
  medicine_total
)

tab_for_medicine_types_binary
write.csv(
  tab_for_medicine_types_binary,
  "Malaria_SNT_Output/children_medicine_type (grouped ACT v - positive malaria).csv",
  row.names = FALSE
)