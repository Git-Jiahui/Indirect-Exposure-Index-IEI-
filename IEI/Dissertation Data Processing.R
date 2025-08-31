#Install Packages
install.packages("countrycode")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("viridis")
install.packages("gridExtra")
install.packages("kableExtra")
install.packages("patchwork")
install.packages("gt")
install.packages("tidyverse")
install.packages("webshot2")
install.packages("writexl")

#Library
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(gridExtra)
library(patchwork)
library(kableExtra)
library(gt)
library(tidyverse)
library(webshot2)

#1.Import Data
ndgain <- read.csv("~/Desktop/Data Collection/ND-GAIN/vulnerability.csv")
worldbank <- read_excel("~/Desktop/Data Collection/World Bank Global Bilateral Migration/P_Data_Extract_From_Global_Bilateral_Migration.xlsx")

#2.Clean Data
#2.1 Clean World Bank Data
#2.1.1 Rename
wb_clean <- worldbank %>%
  select (
    ori_country = `Country Origin Name`,
    ori_code = `Country Origin Code`,
    dest_country = `Country Dest Name`,
    dest_code = `Country Dest Code`,
    `1960` = `1960 [1960]`,
    `1970` =  `1970 [1970]`,
    `1980` = `1980 [1980]`,
    `1990` = `1990 [1990]`,
    `2000` = `2000 [2000]`,
  )
#2.1.2 Check Coding System
wb_all_codes <- unique(c(wb_clean$ori_code, wb_clean$dest_code))
wb_all_codes <- wb_all_codes[!is.na(wb_all_codes)]

print(paste("Data has", length(wb_all_codes), "unique code"))

sample_codes <- head(wb_all_codes, 10)
iso3c_valid <- sum(!is.na(countrycode(sample_codes, "iso3c", "country.name", warn = FALSE)))
wb_valid <- sum(!is.na(countrycode(sample_codes, "wb", "country.name", warn = FALSE)))

print(paste("Sample ISO3c recognition rate:", iso3c_valid, "/", length(sample_codes)))
print(paste("Sample WB recognition rate:", wb_valid, "/", length(sample_codes)))

if(iso3c_valid >= wb_valid) {
  print("Confirmation: World Bank data uses the ISO 3c code system.")
  code_system <- "iso3c"
} else {
  print("Confirmation: World Bank data uses the WB code system.")
  code_system <- "wb"
}
#2.1.3 Identify problem code
invalid_codes <- wb_all_codes[is.na(countrycode(wb_all_codes, 
                                                origin = code_system, 
                                                destination = "iso3c", 
                                                warn = FALSE))]

if(length(invalid_codes) > 0) {
  print(paste("Find", length(invalid_codes), "invalid code："))
  print(invalid_codes)
  
  for(code in invalid_codes) {
    if(!is.na(code)) {
      count_origin <- sum(wb_clean$ori_code == code, na.rm = TRUE)
      count_dest <- sum(wb_clean$dest_code == code, na.rm = TRUE)
      print(paste("  ", code, ": as origin", count_origin, " times, as dest", count_dest, "times"))
    }
  }
}
#2.1.4 Process problem code
problem_codes_treatment <- tribble(
  ~code, ~action, ~new_code, ~reason,
  #Exclude
  "zzz", "exclude", NA, "Special code for refugees/stateless",
  "ANT", "exclude", NA, "Netherlands Antilles - dissolved 2010",
  "SCG", "exclude", NA, "Serbia & Montenegro",
  "XKX", "exclude", NA, "Kosovo - limited recognition",
  "CHI", "exclude", NA, "Channel Islands - British Crown Dependencies",
)
#2.1.5 Process Exclude
codes_to_exclude <- problem_codes_treatment %>%
  filter(action == "exclude") %>%
  pull(code)

print(paste("Exclude codes:", paste(codes_to_exclude, collapse = ", ")))

wb_clean1 <- wb_clean %>%
  filter(!ori_code %in% codes_to_exclude,
         !dest_code %in% codes_to_exclude)
# #2.1.6 Process Merge
# codes_to_merge <- problem_codes_treatment %>%
#   filter(action %in% c("merge"))
# 
# for(i in 1:nrow(codes_to_merge)) {
#   old_code <- codes_to_merge$code[i]
#   new_code <- codes_to_merge$new_code[i]
#   
#   print(paste("Merge:", old_code, "->", new_code, 
#               "(", codes_to_merge$reason[i], ")"))
#   
#   wb_clean1 <- wb_clean1 %>%
#     mutate(
#       ori_code = if_else(ori_code == old_code, new_code, ori_code),
#       dest_code = if_else(dest_code == old_code, new_code, dest_code)
#     )
# }
# #2.1.6b Process Split
# year_cols <- c("1960", "1970", "1980", "1990", "2000")
# wb_clean1[year_cols] <- lapply(wb_clean1[year_cols], function(x) {
#   x <- as.character(x) 
#   x[x == ".."] <- NA    
#   as.numeric(x)          
# })
# #As Origin
# scg_as_origin <- wb_clean1 %>%
#   filter(ori_code == "SCG")
# if(nrow(scg_as_origin) > 0) {
#   #Create Serbia 
#   srb_records <- scg_as_origin %>%
#     mutate(
#       ori_code = "SRB",
#       ori_country = "Serbia",
#       across(c("1960", "1970", "1980", "1990", "2000"), ~ .x * 0.5)
#     )
#   
#   #Creat Montenegro
#   mne_records <- scg_as_origin %>%
#     mutate(
#       ori_code = "MNE", 
#       ori_country = "Montenegro",
#       across(c("1960", "1970", "1980", "1990", "2000"), ~ .x * 0.5)
#     )
#   wb_clean1 <- wb_clean1 %>%
#     filter(ori_code != "SCG") %>%
#     bind_rows(srb_records, mne_records)
#   
#   print(paste("✓ Split SCG as origin:", nrow(scg_as_origin), " -> SRB & MNE each", nrow(scg_as_origin), "times"))
# }
# #As Dest
# scg_as_dest <- wb_clean1 %>%
#   filter(dest_code == "SCG")
# 
# if(nrow(scg_as_dest) > 0) {
#   srb_dest <- scg_as_dest %>%
#     mutate(
#       dest_code = "SRB",
#       dest_country = "Serbia", 
#       across(c("1960", "1970", "1980", "1990", "2000"), ~ .x * 0.5)
#     )
#   
#   mne_dest <- scg_as_dest %>%
#     mutate(
#       dest_code = "MNE",
#       dest_country = "Montenegro",
#       across(c("1960", "1970", "1980", "1990", "2000"), ~ .x * 0.5)
#     )
#   wb_clean1 <- wb_clean1 %>%
#     filter(dest_code != "SCG") %>%
#     bind_rows(srb_dest, mne_dest)
#   
#   print(paste("✓ Split SCG as destination:", nrow(scg_as_dest), "-> SRB & MNE each", nrow(scg_as_dest), "times"))
# }
# wb_clean1 <- wb_clean1 %>%
#   mutate(
# 
#     ori_country = case_when(
#       ori_code == "SRB" & ori_country %in% c("Kosovo", "Serbia and Montenegro") ~ "Serbia",
#       ori_code == "GBR" & ori_country == "Channel Islands" ~ "United Kingdom",
#       ori_code == "NLD" & ori_country == "Netherlands Antilles" ~ "Netherlands",
#       TRUE ~ ori_country
#     ),
# 
#     dest_country = case_when(
#       dest_code == "SRB" & dest_country %in% c("Kosovo", "Serbia and Montenegro") ~ "Serbia",
#       dest_code == "GBR" & dest_country == "Channel Islands" ~ "United Kingdom",
#       dest_code == "NLD" & dest_country == "Netherlands Antilles" ~ "Netherlands",
#       TRUE ~ dest_country
#     )
#   )
#2.1.7 Remove NA code
initial_rows <- nrow(wb_clean1)
wb_clean1<- wb_clean1 %>%
  filter(!is.na(ori_code), !is.na(dest_code))
removed_rows <- initial_rows - nrow(wb_clean1)

print(paste("Remove NA Code", removed_rows))
#2.1.8 Fill NA data from old year, if not, keep NA
cols_to_fix <- c("1960", "1970", "1980", "1990", "2000")

wb_clean1[cols_to_fix] <- lapply(wb_clean1[cols_to_fix], function(x) {
  x <- as.character(x)                  
  x[x == ".."] <- NA                
  as.numeric(x)                        
})
na_2000 <- sum(is.na(wb_clean1$`2000`))
print(paste("Year 2000 data missing（before filled）:", na_2000))
na_1990 <- sum(is.na(wb_clean1$`1990`))
print(paste("Year 1990 data missing（before filled）:", na_2000))
#World bank year 2000 and 1990
wb_clean2 <- wb_clean1 %>%
  mutate(
    Filled_2000 = if_else(
      is.na(`2000`),
      coalesce(`2000`, `1990`, `1980`, `1970`, `1960`),
      `2000`
    )
  )
wb_clean2 <- wb_clean2 %>%
  mutate(
    Filled_1990 = if_else(
      is.na(`1990`),
      coalesce(`2000`, `1990`, `1980`, `1970`, `1960`),
      `1990`
    )
  )

missing_2000 <- sum(is.na(wb_clean2$Filled_2000))
print(paste("Year 2000 data missing（after filled）:", missing_2000))
missing_1990 <- sum(is.na(wb_clean2$Filled_1990))
print(paste("Year 1990 data missing（after filled）:", missing_2000))
#2.1.9 Aggregate identical origin-destination pairs
wb_final <- wb_clean2 %>%
  select(ori_code, dest_code, ori_country, dest_country, 
         N_od_2000 = Filled_2000,N_od_1990 = Filled_1990) %>%
  group_by(ori_code, dest_code) %>%
  summarise(
    ori_country = first(ori_country),
    dest_country = first(dest_country),
    N_od_2000 = sum(N_od_2000, na.rm = TRUE),
    N_od_1990 = sum(N_od_1990, na.rm = TRUE),
    .groups = 'drop'
  )
#2.1.10 Check for World Bank Cleaned Data
print(paste("Final Record Number:", nrow(wb_final)))
print(paste("Number of Origin:", n_distinct(wb_final$ori_code)))
print(paste("Number of Destination:", n_distinct(wb_final$dest_code)))
print(paste("Total Country/Region:", n_distinct(c(wb_final$ori_code, wb_final$dest_code))))

wb_missing_analysis <- wb_clean2 %>%
  filter(is.na(Filled_2000) | is.na(Filled_1990))

missing_pairs_2000 <- wb_missing_analysis %>%
  filter(is.na(Filled_2000)) %>%
  group_by(ori_code, dest_code) %>%
  summarise(
    ori_country = first(ori_country),
    dest_country = first(dest_country),
    .groups = 'drop'
  )

print(paste("=== World Bank 2000 NA Data Analysis ==="))
print(paste("Total Pairing:", nrow(wb_clean2)))
print(paste("Missing Pairing:", nrow(missing_pairs_2000)))
print(paste("Missing Percentage:", round(nrow(missing_pairs_2000)/nrow(wb_clean2)*100, 2), "%"))

missing_as_origin <- missing_pairs_2000 %>%
  group_by(ori_code, ori_country) %>%
  summarise(missing_count = n(), .groups = 'drop') %>%
  arrange(desc(missing_count))

missing_as_dest <- missing_pairs_2000 %>%
  group_by(dest_code, dest_country) %>%
  summarise(missing_count = n(), .groups = 'drop') %>%
  arrange(desc(missing_count))

print("Top 10 Missing as origin:")
print(head(missing_as_origin, 10))

print("Top 10 Missing as destination）:")
print(head(missing_as_dest, 10))

write.csv(missing_pairs_2000, 
          "~/Desktop/Data Collection/Dissertation Result/WB_Missing_Pairs_2000.csv",
          row.names = FALSE)
#2.1.11 Check if there is still have unavailable code
final_codes <- unique(c(wb_final$ori_code, wb_final$dest_code))
still_invalid <- final_codes[is.na(countrycode(final_codes, "iso3c", "country.name", warn = FALSE))]

if(length(still_invalid) > 0) {
  print("Warning: with unavailable code")
  print(still_invalid)
} else {
  print("✓ All code are available ISO3c code")
}
#2.1.12Finish Clean World Bank Data
wb_migration_clean <- wb_final

#2.2 Clean NDGAIN DATA
#2.2.1 Check Country Code
ndgain_codes <- unique(ndgain$ISO3)
ndgain_codes <- ndgain_codes[!is.na(ndgain_codes)]
print(paste("ND-GAIN include", length(ndgain_codes), "Country/Region"))
#2.2.2 Check ISO3c
sample_ndgain <- head(ndgain_codes, 10)
iso3c_valid_ndgain <- sum(!is.na(countrycode(sample_ndgain, "iso3c", "country.name", warn = FALSE)))

print(paste("ISO3c identification rate:", iso3c_valid_ndgain, "/", length(sample_ndgain)))
#2.2.3 Find unavailable code
invalid_ndgain <- ndgain_codes[is.na(countrycode(ndgain_codes, "iso3c", "country.name", warn = FALSE))]

if(length(invalid_ndgain) > 0) {
  print(paste("Find", length(invalid_ndgain), "unavailable ISO3c code："))
  print(invalid_ndgain)
} else {
  print("✓ All code are available ISO3c code")
}

missing_2000_before <- sum(is.na(ndgain$X2000))
print(paste("Missing Value before Fill NA:", missing_2000_before, "/", nrow(ndgain)))
missing_1995_before <- sum(is.na(ndgain$X1995))
print(paste("Missing Value before Fill NA:", missing_1995_before, "/", nrow(ndgain)))
#2.2.4 Fill NA in NDGAIN
ndgain_clean <- ndgain %>%
  mutate(
    vul_2000 = if_else(
      is.na(`X2000`),
      coalesce(`X2005`, `X2004`, `X2003`, `X2002`, `X2001`,`X2000`, 
               `X1999`, `X1998`, `X1997`, `X1996`,`X1995`),
      `X2000`
    )
  )
ndgain_clean <- ndgain_clean %>%
  mutate(
    vul_1995 = if_else(
      is.na(`X1995`),
      coalesce(`X2005`, `X2004`, `X2003`, `X2002`, `X2001`,`X2000`, 
               `X1999`, `X1998`, `X1997`, `X1996`,`X1995`),
      `X1995`
    )
  )
missing_2000_after <- sum(is.na(ndgain_clean$vul_2000))
print(paste("Missing Value after Fill NA:", missing_2000_after, "/", nrow(ndgain_clean)))
missing_1995_after <- sum(is.na(ndgain_clean$vul_1995))
print(paste("Missing Value after Fill NA:", missing_1995_after, "/", nrow(ndgain_clean)))
#2.2.5 Keep 2000, 1995 year in NDGAIN
ndgain_clean1 <- ndgain_clean %>%
  select (
    ori_country = Name,
    ori_code =  ISO3,
    V_2000 = vul_2000,
    V_1995 = vul_1995
  )%>%
  filter(!is.na(V_2000) | !is.na(V_1995))
print(paste("Final ND-GAIN data with：", nrow(ndgain_clean1), "Country/Region"))

ndgain_2000 <- ndgain_clean1 %>%
  select(ori_country, ori_code, V = V_2000) %>%
  filter(!is.na(V))

ndgain_1995 <- ndgain_clean1 %>%
  select(ori_country, ori_code, V = V_1995) %>%
  filter(!is.na(V))

#3. Combine NDGAIN with World bank
#3.1 Check pairing with world bank
wb_countries <- unique(c(wb_migration_clean$ori_code, wb_migration_clean$dest_code))
ndgain_2000_countries <- ndgain_2000$ori_code
ndgain_1995_countries <- ndgain_1995$ori_code

only_wb_2000 <- setdiff(wb_countries, ndgain_2000_countries)
only_wb_1995 <- setdiff(wb_countries, ndgain_1995_countries)

print(paste("Missing in ND-GAIN 2000:", length(only_wb_2000)))
print(paste("Missing in ND-GAIN 1995:", length(only_wb_1995)))
#3.1.2 Report of WB countries with NA vulnerability
missing_report <- data.frame(
  iso3c = only_wb_2000,
  country = sapply(only_wb_2000, function(x) {
    name <- countrycode(x, "iso3c", "country.name", warn = FALSE)
    if(is.na(name)) return(x) else return(name)
  }),
  stringsAsFactors = FALSE
)
for(i in 1:nrow(missing_report)) {
  code <- missing_report$iso3c[i]
  as_origin_2000 <- wb_migration_clean %>%
    filter(ori_code == code) %>%
    summarise(total = sum(N_od_2000, na.rm = TRUE)) %>%
    pull(total)
  as_dest_2000 <- wb_migration_clean %>%
    filter(dest_code == code) %>%
    summarise(total = sum(N_od_2000, na.rm = TRUE)) %>%
    pull(total)
  as_origin_1990 <- wb_migration_clean %>%
    filter(ori_code == code) %>%
    summarise(total = sum(N_od_1990, na.rm = TRUE)) %>%
    pull(total)
  as_dest_1990 <- wb_migration_clean %>%
    filter(dest_code == code) %>%
    summarise(total = sum(N_od_1990, na.rm = TRUE)) %>%
    pull(total)
  
  missing_report$emigrants_2000[i] <- ifelse(length(as_origin_2000) > 0, as_origin_2000, 0)
  missing_report$immigrants_2000[i] <- ifelse(length(as_dest_2000) > 0, as_dest_2000, 0)
  missing_report$emigrants_1990[i] <- ifelse(length(as_origin_1990) > 0, as_origin_1990, 0)
  missing_report$immigrants_1990[i] <- ifelse(length(as_dest_1990) > 0, as_dest_1990, 0)
  
  missing_report$in_ndgain_1995[i] <- code %in% ndgain_1995_countries
}
missing_report <- missing_report %>%
  mutate(
    total_migration_2000 = emigrants_2000 + immigrants_2000,
    total_migration_1990 = emigrants_1990 + immigrants_1990
  ) %>%
  arrange(desc(total_migration_2000))
write.csv(missing_report, 
          "~/Desktop/Data Collection/Dissertation Result/Countries_Missing_NDGAIN.csv", 
          row.names = FALSE)

#3.1.3 Process with NA vulnerability country
print(paste("Need process", nrow(missing_report), "missing country/region"))
ndgain_mapping <- tribble(
  ~territory_code, ~sovereign_code, ~mapping_type, ~reason,
  
  # French overseas territory
  "GLP", "FRA", "sovereign", "Guadeloupe - French overseas department",
  "MTQ", "FRA", "sovereign", "Martinique - French overseas department",
  "GUF", "FRA", "sovereign", "French Guiana - French overseas department",
  "REU", "FRA", "sovereign", "Réunion - French overseas department",
  "NCL", "FRA", "sovereign", "New Caledonia - French overseas territory",
  "PYF", "FRA", "sovereign", "French Polynesia - French overseas territory",
  "SPM", "FRA", "sovereign", "Saint Pierre and Miquelon - French overseas territory",
  "MYT", "FRA", "sovereign", "Mayotte - French overseas department",
  "WLF", "FRA", "sovereign", "Wallis and Futuna - French overseas territory",
  # U.S
  "PRI", "USA", "sovereign", "Puerto Rico - US territory",
  "VIR", "USA", "sovereign", "US Virgin Islands - US territory",
  "GUM", "USA", "sovereign", "Guam - US territory",
  "ASM", "USA", "sovereign", "American Samoa - US territory",
  "MNP", "USA", "sovereign", "Northern Mariana Islands - US territory",
  # U.K
  "GIB", "GBR", "sovereign", "Gibraltar - British Overseas Territory",
  "BMU", "GBR", "sovereign", "Bermuda - British Overseas Territory",
  "CYM", "GBR", "sovereign", "Cayman Islands - British Overseas Territory",
  "TCA", "GBR", "sovereign", "Turks and Caicos - British Overseas Territory",
  "VGB", "GBR", "sovereign", "British Virgin Islands - British Overseas Territory",
  "AIA", "GBR", "sovereign", "Anguilla - British Overseas Territory",
  "MSR", "GBR", "sovereign", "Montserrat - British Overseas Territory",
  "FLK", "GBR", "sovereign", "Falkland Islands - British Overseas Territory",
  "SHN", "GBR", "sovereign", "Saint Helena - British Overseas Territory",
  "IMN", "GBR", "sovereign", "Isle of Man - British Crown Dependency",
  # Netherlands
  "ABW", "NLD", "sovereign", "Aruba - Netherlands constituent country",
  # Denmark
  "GRL", "DNK", "sovereign", "Greenland - Danish autonomous territory",
  "FRO", "DNK", "sovereign", "Faroe Islands - Danish autonomous territory",
  # China
  "HKG", "CHN", "sovereign", "Hong Kong - Chinese SAR",
  "MAC", "CHN", "sovereign", "Macao - Chinese SAR",
  "TWN", "CHN", "sovereign", "Taiwan - Chinese Province",
  # Australia
  "NFK", "AUS", "sovereign", "Norfolk Island - Australian territory",
  # Related to New Zealand
  "COK", "NZL", "sovereign", "Cook Islands - associated with NZ",
  "NIU", "NZL", "sovereign", "Niue - associated with NZ",
  "TKL", "NZL", "sovereign", "Tokelau - NZ dependency",
  # Geographical
  "LIE", "CHE", "geographic", "Liechtenstein - use Switzerland data",
  "PSE", "ISR", "geographic","Palestinian Territories - use Israel as proxy",
  "AND", "ESP", "geographic","Andorra - use Spain as neighbor",
  "KNA", "LCA", "geographic","St. Kitts & Nevis - use nearby St. Lucia",
  "MCO", "FRA", "geographic","Monaco - use France as neighbor",
  "SMR", "ITA", "geographic","San Marino - use Italy as neighbor"
)
print(paste("Define", nrow(ndgain_mapping), "mapping rules"))
setdiff(ndgain_mapping$territory_code,missing_report$iso3c)
#3.1.4 Use mapping rules to extend NDGAIN
#2000
if(!exists("ndgain_2000")) {
  ndgain_2000 <- ndgain_clean %>%
    select (
      ori_country = Name,
      ori_code =  ISO3,
      V = vul_2000,
    )%>%
    filter(!is.na(V))
}
ndgain_2000_extended <- ndgain_2000
success_count_2000 <- 0
no_sovereign_count_2000 <- 0
no_rule_count_2000 <- 0
# Process Missing country
for(i in 1:nrow(missing_report)) {
  territory_code <- missing_report$iso3c[i]
  territory_name <- missing_report$country[i]
  
  # Check if there is mapping rule
  if(territory_code %in% ndgain_mapping$territory_code) {
    # Gain sovereign code
    sovereign_code <- ndgain_mapping$sovereign_code[ndgain_mapping$territory_code == territory_code][1]
    
    # Search vulnerability index of sovereign
    sovereign_rows <- ndgain_2000[ndgain_2000$ori_code == sovereign_code, ]
    
    if(nrow(sovereign_rows) > 0) {
      sovereign_vul <- sovereign_rows$V[1]
      new_row <- data.frame(
        ori_country = territory_name,
        ori_code = territory_code,
        V = sovereign_vul,
        stringsAsFactors = FALSE
      )
      
      ndgain_2000_extended <- rbind(ndgain_2000_extended, new_row)
      success_count_2000 <- success_count_2000 + 1
      
      print(paste("✓ Year 2000 Mapping Success:", territory_code, "(", territory_name, ") ->", 
                  sovereign_code, "Vulnerability:", round(sovereign_vul, 2)))
      
    } else {
      # Missing soverign country
      no_sovereign_count_2000 <- no_sovereign_count_2000 + 1
      print(paste("✗ Year 2000 Missing soverign country:", territory_code, "->", sovereign_code, "Not find"))
    }
  } else {
    # No rule
    no_rule_count_2000 <- no_rule_count_2000 + 1
    print(paste("⚠ Year 2000 No rules:", territory_code, "(", territory_name, ")"))
  }
}

#Summary of mapping 2000
print(paste("Success:", success_count_2000))
print(paste("Missing soverign:", no_sovereign_count_2000))
print(paste("No rules:", no_rule_count_2000))
print(paste("Total:", nrow(missing_report)))

#1995
ndgain_1995_extended <- ndgain_1995
success_count_1995 <- 0

for(i in 1:nrow(missing_report)) {
  territory_code <- missing_report$iso3c[i]
  territory_name <- missing_report$country[i]
  
  if(!missing_report$in_ndgain_1995[i]) {
    if(territory_code %in% ndgain_mapping$territory_code) {
      sovereign_code <- ndgain_mapping$sovereign_code[ndgain_mapping$territory_code == territory_code][1]
      sovereign_rows <- ndgain_1995[ndgain_1995$ori_code == sovereign_code, ]
      
      if(nrow(sovereign_rows) > 0) {
        sovereign_vul <- sovereign_rows$V[1]
        new_row <- data.frame(
          ori_country = territory_name,
          ori_code = territory_code,
          V = sovereign_vul,
          stringsAsFactors = FALSE
        )
        
        ndgain_1995_extended <- rbind(ndgain_1995_extended, new_row)
        success_count_1995 <- success_count_1995 + 1
      }
    }
  }
}

print(paste("1995 Mapping Success:", success_count_1995))

#3.1.5 Summary of Extended ND-GAIN for 2000 and 1995
print(paste("Final ND-GAIN 2000 extended:", nrow(ndgain_2000_extended), "countries"))
print(paste("Final ND-GAIN 1995 extended:", nrow(ndgain_1995_extended), "countries"))

#3.1.6 Matching Check
wb_countries <- unique(c(wb_migration_clean$ori_code, wb_migration_clean$dest_code))
print(paste("World Bank:", length(wb_countries)))

#4. Calculation
#4.1 N_d
N_d_2000 <- wb_migration_clean %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    N_d = sum(N_od_2000, na.rm = TRUE),
    .groups = 'drop'
  )
# Check if N_d = 0
zero_immigration <- N_d_2000 %>%
  filter(N_d == 0)

if(nrow(zero_immigration) > 0) {
  print(paste("Noted：", nrow(zero_immigration), "countriy do not have migration data（N_d = 0）："))
  print(zero_immigration$dest_code)
  
  #Remove missing countries
  N_d_2000 <- N_d_2000 %>%
    filter(!dest_code %in% zero_immigration$dest_code)
  
  print(paste("Remove", nrow(zero_immigration), "countries with no migration data"))
}
wb_migration_clean <- wb_migration_clean %>%
  filter(!dest_code %in% zero_immigration$dest_code)

print(paste("Calculate", nrow(N_d_2000), "dest countries for total migration"))
#4.2 N_od/N_d
weighted_2000 <- wb_migration_clean %>%
  left_join(N_d_2000, by = c("dest_country", "dest_code")) %>%
  mutate(
    weight = ifelse(N_d == 0, 0, N_od_2000 / N_d)
  )
#4.3 Check if weight = 1
weight_check <- weighted_2000 %>%
  group_by(dest_code) %>%
  summarise(weight_sum = sum(weight, na.rm = TRUE)) %>%
  filter(abs(weight_sum - 1) > 0.001)

if(nrow(weight_check) > 0) {
  print("Warning：Not equal to 1：")
  print(head(weight_check))
}
#4.4 Add ND-GAIN
exposure_2000 <- weighted_2000 %>%
  left_join(
    ndgain_2000_extended %>% select(ori_code, V),
    by = c("ori_code")
  ) %>%
  mutate(
    exposure_component = weight * V
  )
#4.5 Check if Pairing Vulnerability
unmatched <- exposure_2000 %>%
  filter(is.na(V)) %>%
  group_by(ori_code) %>%
  summarise(
    total_migration = sum(N_od_2000, na.rm = TRUE)
  ) %>%
  arrange(desc(total_migration))

if(nrow(unmatched) > 0) {
  print(paste("Warning：", n_distinct(unmatched$ori_code), "no vulverabilty"))
}
#4.6 Calculate Exposure Index
exposure_index_2000 <- exposure_2000 %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    exposure_index = sum(exposure_component, na.rm = TRUE),
    n_origins = sum(!is.na(exposure_component)),
    .groups = 'drop'
  ) %>%
  arrange(desc(exposure_index)) %>%
  mutate(
    rank = row_number()
  )
#4.7 Ranking
exposure_ranking_2000 <- exposure_index_2000 %>%
  select(
    Rank = rank,
    Country= dest_country,
    ISO3 = dest_code,
    `Exposure Index` = exposure_index,
  ) %>%
  filter(!is.na(Country))
#4.8 Output Result
exposure_ranking_2000_print <- exposure_ranking_2000 %>%
  mutate(
    `Exposure Index` = sprintf("%.6f", `Exposure Index`)
  )

write.csv(exposure_ranking_2000_print, 
          "~/Desktop/Data Collection/Dissertation Result/Exposure_Ranking_2000.csv", 
          row.names = FALSE)

#5. Robust Test
#5.1. Robust Test 1: Replace ND-GAIN Year (1995)
#5.1.1 Add WB 2000
weighted_2000_ndgain1995 <- wb_migration_clean %>%
  left_join(N_d_2000, by = c("dest_country", "dest_code")) %>%
  mutate(
    weight = case_when(
      N_d == 0 ~ 0,
      TRUE ~ N_od_2000 / N_d
    )
  )
#5.1.2 Add ND1995
exposure_2000_1995 <- weighted_2000_ndgain1995 %>%
  left_join(
    ndgain_1995_extended %>% select(ori_code, V_1995 = V),
    by = c("ori_code")
  ) %>%
  mutate(
    exposure_component = weight * V_1995
  )
#5.1.3 Check Paring
unmatched_1995 <- exposure_2000_1995 %>%
  filter(is.na(V_1995) & N_od_2000 > 0) %>%
  summarise(n = n_distinct(ori_code)) %>%
  pull(n)
print(paste("Note: Using 1995 ND-GAIN，", unmatched_1995, "no vulneralability data"))
#5.1.4 Calculate Exposure (1995 ND)
exposure_index_robust1 <- exposure_2000_1995 %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    exposure_index_1995 = sum(exposure_component, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(exposure_index_1995)) %>%
  mutate(rank_1995 = row_number())
#5.1.5 Compare with Main
robustness_comparison <- exposure_index_2000 %>%
  select(dest_country, dest_code, 
         exposure_index_2000 = exposure_index, 
         rank_2000 = rank) %>%
  inner_join(
    exposure_index_robust1 %>%
      select(dest_code, exposure_index_1995, rank_1995),
    by = "dest_code"
  ) %>%
  mutate(
    rank_change = rank_2000 - rank_1995,
    index_change = exposure_index_2000 - exposure_index_1995,
    index_change_pct = (exposure_index_2000 - exposure_index_1995) / exposure_index_2000 * 100
  )
#5.1.6 Correlation
pearson_cor <- cor(robustness_comparison$exposure_index_2000, 
                   robustness_comparison$exposure_index_1995, 
                   use = "complete.obs")

spearman_cor <- cor(robustness_comparison$rank_2000, 
                    robustness_comparison$rank_1995, 
                    method = "spearman", 
                    use = "complete.obs")

print(paste("Pearson correlation coefficient（Index）:", round(pearson_cor, 4)))
print(paste("Spearman correlation coefficient（Ranking）:", round(spearman_cor, 4)))
#5.1.7 Analysis top changing ranking countries
top_climbers <- robustness_comparison %>%
  arrange(rank_change) %>%
  head(10) %>%
  select(Country = dest_country, 
         `2000 Rank` = rank_2000, 
         `1995 Rank` = rank_1995, 
         `Rank Change` = rank_change)

print("Rank go up countries (top 10)(negative means up)：")
print(top_climbers)
#5.1.7 Analysis top changing ranking countries
top_fallers <- robustness_comparison %>%
  arrange(desc(rank_change)) %>%
  head(10) %>%
  select(Country = dest_country, 
         `2000 Rank` = rank_2000, 
         `1995 Rank` = rank_1995, 
         `Rank Change` = rank_change)

print("Rank go down countries (bottom 10)(positive means down)）：")
print(top_fallers)
#5.1.8 Comparing top 20
top20_2000 <- robustness_comparison %>%
  filter(rank_2000 <= 20) %>%
  arrange(rank_2000) %>%
  pull(dest_code)

top20_1995 <- robustness_comparison %>%
  filter(rank_1995 <= 20) %>%
  arrange(rank_1995) %>%
  pull(dest_code)

overlap_top20 <- length(intersect(top20_2000, top20_1995))
print(paste("All in top 20:", overlap_top20, "/ 20"))

only_2000 <- setdiff(top20_2000, top20_1995)
only_1995 <- setdiff(top20_1995, top20_2000)
if(length(only_2000) > 0) {
  print(paste("Top 20 only in Year 2000:", paste(only_2000, collapse = ", ")))
} else {
  print("Top 20 in year 2000 all in top 20 year 1995")
}
if(length(only_1995) > 0) {
  print(paste("Top 20 only in Year 1995:", paste(only_1995, collapse = ", ")))
} else {
  print("Top 20 in year 1995 all in top 20 year 2000中")
}

#5.1.9 Plot
plot_robust1 <- ggplot(robustness_comparison, 
                       aes(x = exposure_index_2000, y = exposure_index_1995)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  labs(
    title = "Robustness Test 1: WB 2000 + ND-GAIN 1995",
    subtitle = paste("Pearson r =", round(pearson_cor, 3), 
                     "| Spearman rho =", round(spearman_cor, 3)),
    x = "Exposure Index (ND-GAIN 2000)",
    y = "Exposure Index (ND-GAIN 1995)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave("~/Desktop/Data Collection/Dissertation Result/Robustness_Test1_WB2000_NDGAIN1995.pdf",
       plot = plot_robust1, width = 8, height = 6)
#5.1.10 Summary Robustness Test 1
robustness_output <- robustness_comparison %>%
  arrange(rank_2000) %>%
  select(
    Country = dest_country,
    ISO3 = dest_code,
    `Index 2000` = exposure_index_2000,
    `Index 1995` = exposure_index_1995,
    `Rank 2000` = rank_2000,
    `Rank 1995` = rank_1995,
    `Rank Change` = rank_change
  ) %>%
  mutate(
    `Index 2000` = round(`Index 2000`, 6),
    `Index 1995` = round(`Index 1995`, 6)
  )
write.csv(robustness_output,
          "~/Desktop/Data Collection/Dissertation Result/Robustness_Test1_Comparison.csv",
          row.names = FALSE)
print("========== Summary Robustness Test 1 ==========")
print(paste("Countries:", nrow(robustness_comparison)))
print(paste("Average ranking change (absolute):", round(mean(abs(robustness_comparison$rank_change)), 2)))
print(paste("Max ranking change:", max(abs(robustness_comparison$rank_change))))
print(paste("Average index change:", round(mean(robustness_comparison$index_change), 6)))


if(pearson_cor > 0.9 & spearman_cor > 0.9) {
  print("✓ Robustness test passed: the results of the two periods are highly consistent")
} else if(pearson_cor > 0.7 & spearman_cor > 0.7) {
  print("○ Moderate robustness: Results are generally consistent, but some variations exist")
} else {
  print("△ Note: There are significant differences between the results of the two periods")
}
#5.1.11 Check if ND2000 = ND1995
if(abs(pearson_cor - 1) < 0.001 & abs(spearman_cor - 1) < 0.001) {
  print("⚠ Note: The correlation close to 1, so the ND-GAIN data for 1995 and 2000 may be the same")
  
  check_same <- exposure_2000_1995 %>%
    left_join(
      exposure_2000 %>% select(ori_code, dest_code, V_2000 = V),
      by = c("ori_code", "dest_code")
    ) %>%
    filter(!is.na(V_1995) & !is.na(V_2000)) %>%
    mutate(diff = abs(V_1995 - V_2000)) %>%
    summarise(
      max_diff = max(diff, na.rm = TRUE),
      mean_diff = mean(diff, na.rm = TRUE)
    )
  
  if(check_same$max_diff < 0.001) {
    print("Confirmation: The ND-GAIN vulnerability data for 1995 and 2000 are identical")
    print("Recommendation: Use other years for robustness testing")
  }
}
#5.2 Robustness Test 2 WB1990, ND2000
#5.2.1 WB1990
N_d_1990 <- wb_migration_clean %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    N_d = sum(N_od_1990, na.rm = TRUE),
    .groups = 'drop'
  )
print(paste("Calculated total immigration for", nrow(N_d_1990), "destination countries"))
#5.2.2 Check 0 migration country
zero_immigration_1990 <- N_d_1990 %>%
  filter(N_d == 0)

if(nrow(zero_immigration_1990) > 0) {
  print(paste("Note:", nrow(zero_immigration_1990), "countries have no immigration data in 1990:"))
  print(paste(zero_immigration_1990$dest_code, collapse = ", "))
}
#5.2.3 Weight WB1990
weighted_1990 <- wb_migration_clean %>%
  left_join(N_d_1990, by = c("dest_country", "dest_code")) %>%
  mutate(
    weight = case_when(
      N_d == 0 ~ 0,
      TRUE ~ N_od_1990 / N_d
    )
  )
#5.2.4 Add ND2000
exposure_1990_2000 <- weighted_1990 %>%
  left_join(
    ndgain_2000_extended %>% select(ori_code, V_2000 = V),
    by = c("ori_code")
  ) %>%
  mutate(
    exposure_component = weight * V_2000
  )
#5.2.5 Check Matching
unmatched_robust2 <- exposure_1990_2000 %>%
  filter(is.na(V_2000) & N_od_1990 > 0) %>%
  summarise(n = n_distinct(ori_code)) %>%
  pull(n)

if(unmatched_robust2 > 0) {
  print(paste("Note:", unmatched_robust2, "origin countries have no vulnerability data"))
}
#5.2.6 Calculate exposure index
exposure_index_robust2 <- exposure_1990_2000 %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    exposure_index_1990_2000 = sum(exposure_component, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(exposure_index_1990_2000)) %>%
  mutate(rank_1990_2000 = row_number())
#5.2.7 Compare with Main
robustness_comparison2 <- exposure_index_2000 %>%
  select(dest_country, dest_code, 
         exposure_index_baseline = exposure_index, 
         rank_baseline = rank) %>%
  inner_join(
    exposure_index_robust2 %>%
      select(dest_code, exposure_index_1990_2000, rank_1990_2000),
    by = "dest_code"
  ) %>%
  mutate(
    rank_change = rank_baseline - rank_1990_2000,
    index_change = exposure_index_baseline - exposure_index_1990_2000,
    index_change_pct = (exposure_index_baseline - exposure_index_1990_2000) / 
      exposure_index_baseline * 100
  )
#5.2.8 Correlation
pearson_cor2 <- cor(robustness_comparison2$exposure_index_baseline, 
                    robustness_comparison2$exposure_index_1990_2000, 
                    use = "complete.obs")

spearman_cor2 <- cor(robustness_comparison2$rank_baseline, 
                     robustness_comparison2$rank_1990_2000, 
                     method = "spearman", 
                     use = "complete.obs")

print(paste("Pearson correlation (index values):", round(pearson_cor2, 4)))
print(paste("Spearman correlation (rankings):", round(spearman_cor2, 4)))
#5.2.9 Identify countries with largest rank changes
#5.2.9.1 Countries with largest rank improvements
top_climbers2 <- robustness_comparison2 %>%
  arrange(rank_change) %>%
  head(10) %>%
  select(Country = dest_country, 
         `Baseline Rank` = rank_baseline, 
         `1990-2000 Rank` = rank_1990_2000, 
         `Rank Change` = rank_change)

print("Top 10 countries with improved rankings (negative = improvement):")
print(top_climbers2)
#5.2.9.2 Countries with largest rank declines
top_fallers2 <- robustness_comparison2 %>%
  arrange(desc(rank_change)) %>%
  head(10) %>%
  select(Country = dest_country, 
         `Baseline Rank` = rank_baseline, 
         `1990-2000 Rank` = rank_1990_2000, 
         `Rank Change` = rank_change)

print("Top 10 countries with declined rankings (positive = decline):")
print(top_fallers2)
#5.2.10 Top 20 comparison
top20_baseline <- robustness_comparison2 %>%
  filter(rank_baseline <= 20) %>%
  arrange(rank_baseline) %>%
  pull(dest_code)

top20_1990_2000 <- robustness_comparison2 %>%
  filter(rank_1990_2000 <= 20) %>%
  arrange(rank_1990_2000) %>%
  pull(dest_code)

overlap_top20_2 <- length(intersect(top20_baseline, top20_1990_2000))

print(paste("Top 20 overlap:", overlap_top20_2, "/ 20"))

only_baseline <- setdiff(top20_baseline, top20_1990_2000)
only_1990_2000 <- setdiff(top20_1990_2000, top20_baseline)

if(length(only_baseline) > 0) {
  print(paste("Only in baseline top 20:", paste(only_baseline, collapse = ", ")))
}

if(length(only_1990_2000) > 0) {
  print(paste("Only in 1990-2000 top 20:", paste(only_1990_2000, collapse = ", ")))
}
#5.2.11 Plot
plot_robust2 <- ggplot(robustness_comparison2, 
                       aes(x = exposure_index_baseline, y = exposure_index_1990_2000)) +
  geom_point(alpha = 0.6, color = "coral") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  labs(
    title = "Robustness Test 2: WB 1990 + ND-GAIN 2000",
    subtitle = paste("Pearson r =", round(pearson_cor2, 3), 
                     "| Spearman rho =", round(spearman_cor2, 3)),
    x = "Baseline Exposure Index (WB 2000 + ND-GAIN 2000)",
    y = "Robust Exposure Index (WB 1990 + ND-GAIN 2000)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )
ggsave("~/Desktop/Data Collection/Dissertation Result/Robustness_Test2_WB1990_NDGAIN2000.pdf",
       plot = plot_robust2, width = 8, height = 6)
#5.2.12 Save detailed comparison results
robustness_output2 <- robustness_comparison2 %>%
  arrange(rank_baseline) %>%
  select(
    Country = dest_country,
    ISO3 = dest_code,
    `Baseline Index` = exposure_index_baseline,
    `1990-2000 Index` = exposure_index_1990_2000,
    `Baseline Rank` = rank_baseline,
    `1990-2000 Rank` = rank_1990_2000,
    `Rank Change` = rank_change,
    `Index Change %` = index_change_pct
  ) %>%
  mutate(
    `Baseline Index` = round(`Baseline Index`, 6),
    `1990-2000 Index` = round(`1990-2000 Index`, 6),
    `Index Change %` = round(`Index Change %`, 2)
  )
write.csv(robustness_output2,
          "~/Desktop/Data Collection/Dissertation Result/Robustness_Test2_Comparison.csv",
          row.names = FALSE)
#5.2.13 Statistical summary
print(paste("Number of countries analyzed:", nrow(robustness_comparison2)))
print(paste("Mean absolute rank change:", round(mean(abs(robustness_comparison2$rank_change)), 2)))
print(paste("Maximum rank change:", max(abs(robustness_comparison2$rank_change))))
print(paste("Mean absolute index change:", round(mean(abs(robustness_comparison2$index_change)), 6)))
print(paste("Standard deviation of index change:", round(sd(robustness_comparison2$index_change), 6)))
print(paste("Correlation summary:"))
print(paste("  Pearson r =", round(pearson_cor2, 4), "(index values)"))
print(paste("  Spearman rho =", round(spearman_cor2, 4), "(rankings)"))
#5.2.14 Robustness Assessment
if(pearson_cor2 > 0.9 & spearman_cor2 > 0.9) {
  print("Robustness test PASSED: Very high consistency between different time periods")
} else if(pearson_cor2 > 0.7 & spearman_cor2 > 0.7) {
  print("Moderate robustness: Results are generally consistent with some variations")
} else {
  print("Warning: Significant differences between time periods detected")
}
#5.3 Robustness Test 3 UNPD
unpd <- read_excel("~/Desktop/Data Collection/UN International Migrants Stock/undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx",
                   sheet = 2,skip = 10)
#5.3.1 Clean UNPD
unpd_clean <- unpd %>%
  select(
    dest_country = `Region, development group, country or area of destination`,
    dest_code_un = `Location code of destination`,
    ori_country = `Region, development group, country or area of origin`,
    ori_code_un = `Location code of origin`,
    `1990` = `1990...8`,
    `1995` = `1995...9`,
    `2000` = `2000...10`,
    `2005` = `2005...11`,
    `2010` = `2010...12`,
    `2015` = `2015...13`,
    `2020` = `2020...14`,
    `2024` = `2024...15`
  )
unpd_clean <- unpd_clean %>%
  filter(dest_code_un < 900,
         ori_code_un < 900)
#5.3.2 Switch country code
unpd_clean$dest_code <- countrycode(
  unpd_clean$dest_code_un, 
  origin = "un", 
  destination = "iso3c",
  warn = FALSE
)
unpd_clean$ori_code <- countrycode(
  unpd_clean$ori_code_un, 
  origin = "un", 
  destination = "iso3c",
  warn = FALSE
)
#5.3.3 Check for conversion problem
dest_na <- sum(is.na(unpd_clean$dest_code))
ori_na <- sum(is.na(unpd_clean$ori_code))

print(paste("Destination codes failed to convert:", dest_na))
print(paste("Origin codes failed to convert:", ori_na))

if(dest_na > 0) {
  failed_dest <- unpd_clean %>%
    filter(is.na(dest_code)) %>%
    select(dest_country, dest_code_un) %>%
    distinct()
  print("Failed destination conversions:")
  print(head(failed_dest, 10))
}

if(ori_na > 0) {
  failed_ori <- unpd_clean %>%
    filter(is.na(ori_code)) %>%
    select(ori_country, ori_code_un) %>%
    distinct()
  print("Failed origin conversions:")
  print(head(failed_ori, 10))
}
#5.3.4 Mapping
manual_mapping_unpd <- tribble(
  ~un_code, ~iso3c_code, ~entity_name, ~action,
  158, "TWN", "Taiwan", "direct_mapping",
  830, NA, "Channel Islands", "exclude"
)

unpd_clean <- unpd_clean %>%
  filter(dest_code_un != 830,
         ori_code_un != 830)

removed_rows <- initial_rows - nrow(unpd_clean)
print(paste("Channel Islands excluded. Records removed:", removed_rows))

unpd_clean <- unpd_clean %>%
  mutate(
    dest_code = case_when(
      dest_code_un == 158 ~ "TWN",
      TRUE ~ dest_code
    ),
    ori_code = case_when(
      ori_code_un == 158 ~ "TWN",
      TRUE ~ ori_code
    )
  )

print(paste("Taiwan (UN:158) mapped to ISO3c:TWN"))

dest_na_after <- sum(is.na(unpd_clean$dest_code))
ori_na_after <- sum(is.na(unpd_clean$ori_code))

print(paste("After processing:"))
print(paste("  Destination codes with NA:", dest_na_after))
print(paste("  Origin codes with NA:", ori_na_after))

# Verify Taiwan is properly coded
taiwan_check <- unpd_clean %>%
  filter(dest_code == "TWN" | ori_code == "TWN") %>%
  nrow()

print(paste("Records involving Taiwan (TWN):", taiwan_check))

# for(i in 1:nrow(manual_mapping_unpd)) {
#   un_code <- manual_mapping_unpd$un_code[i]
#   iso_code <- manual_mapping_unpd$iso3c_code[i]
#   entity <- manual_mapping_unpd$entity_name[i]
#   notes <- manual_mapping_unpd$notes[i]
#   
#   affected_dest <- sum(unpd_clean$dest_code_un == un_code, na.rm = TRUE)
#   affected_ori <- sum(unpd_clean$ori_code_un == un_code, na.rm = TRUE)
#   unpd_clean <- unpd_clean %>%
#     mutate(
#       dest_code = case_when(
#         dest_code_un == un_code ~ iso_code,
#         TRUE ~ dest_code
#       )
#     )
#   unpd_clean <- unpd_clean %>%
#     mutate(
#       ori_code = case_when(
#         ori_code_un == un_code ~ iso_code,
#         TRUE ~ ori_code
#       )
#     )
#   
#   print(paste("Mapped:", entity, "->", iso_code, 
#               "(", notes, ")",
#               "| Affected records: dest =", affected_dest, 
#               ", origin =", affected_ori))
# }
# dest_na_after <- sum(is.na(unpd_clean$dest_code))
# ori_na_after <- sum(is.na(unpd_clean$ori_code))
# 
# print(paste("After manual mapping:"))
# print(paste("  Destination codes with NA:", dest_na_after))
# print(paste("  Origin codes with NA:", ori_na_after))

#5.3.5 Remove records with NA codes
initial_rows <- nrow(unpd_clean)
unpd_clean <- unpd_clean %>%
  filter(!is.na(ori_code), !is.na(dest_code))
removed_rows <- initial_rows - nrow(unpd_clean)

print(paste("Removed", removed_rows, "records with NA codes"))
print(paste("Remaining records:", nrow(unpd_clean)))
#5.3.6 Convert migration data to numeric and handle missing values
year_columns <- c("1990", "1995", "2000", "2005", "2010", "2015", "2020", "2024")

for(col in year_columns) {
  unpd_clean[[col]] <- as.numeric(unpd_clean[[col]])
}
#Check Missing in 2000
missing_2000_before <- sum(is.na(unpd_clean$`2000`))
print(paste("Missing values in year 2000 before filling:", missing_2000_before))
#5.3.7 Aggregate by origin-destination pairs
unpd_final <- unpd_clean %>%
  group_by(ori_code, dest_code) %>%
  summarise(
    ori_country = first(ori_country),
    dest_country = first(dest_country),
    N_od_2000_unpd = sum(`2000`, na.rm = TRUE),
    n_merged = n(), 
    .groups = 'drop'
  )
merged_records <- unpd_final %>%
  filter(n_merged > 1) %>%
  arrange(desc(n_merged))

if(nrow(merged_records) > 0) {
  print(paste("Found", nrow(merged_records), "origin-destination pairs that were merged"))
}
#Compare cleaned WB and UNPD
wb_countries <- unique(c(wb_migration_clean$ori_code, wb_migration_clean$dest_code))
unpd_countries <- unique(c(unpd_final$ori_code, unpd_final$dest_code))
common <- intersect(wb_countries, unpd_countries)
only_wb <- setdiff(wb_countries, unpd_countries)
only_unpd <- setdiff(unpd_countries, wb_countries)

print(paste("WB：", length(wb_countries)))
print(paste("UNPD：", length(unpd_countries)))
print(paste("Common：", length(common)))
print(paste("only in WB：", length(only_wb)))
print(paste("only in UNPD：", length(only_unpd)))
print("Only in WB：")
print(only_wb)
print("Only in UNPD：")
print(only_unpd)
#Overlap Rate
overlap_rate <- length(common) / max(length(wb_countries), length(unpd_countries))
print(paste("Overlap Rate：", round(overlap_rate * 100, 1), "%"))
#5.3.8 Summary of Cleaned UNPD
print(paste("Final records:", nrow(unpd_final)))
print(paste("Number of origin countries:", n_distinct(unpd_final$ori_code)))
print(paste("Number of destination countries:", n_distinct(unpd_final$dest_code)))
print(paste("Total migration stock:", sum(unpd_final$N_od_2000_unpd, na.rm = TRUE)))
setdiff(unpd_final$ori_code,unpd_final$dest_code)
#5.3.9 N_d UNPD
#Check if repeated dest_code
duplicate_dest <- unpd_final %>%
  group_by(dest_code) %>%
  filter(n() > 1) %>%
  distinct(dest_code, dest_country)

if(nrow(duplicate_dest) > 0) {
  print("Warning: Found duplicate destination codes in unpd_final:")
  print(duplicate_dest)
}

N_d_unpd <- unpd_final %>%
  group_by(dest_code) %>%
  summarise(
    dest_country = first(dest_country), 
    N_d = sum(N_od_2000_unpd, na.rm = TRUE),
    n_origins = n_distinct(ori_code),
    .groups = 'drop'
  ) %>%
  arrange(desc(N_d))
#5.3.10 Weight
weighted_unpd <- unpd_final %>%
  left_join(N_d_unpd %>% select(dest_code, N_d), 
            by = "dest_code") %>%
  mutate(
    weight = case_when(
      is.na(N_d) ~ 0,
      N_d == 0 ~ 0,
      TRUE ~ N_od_2000_unpd / N_d
    )
  )
#5.3.11 Check sum weight = 1
weight_check_unpd <- weighted_unpd %>%
  group_by(dest_code) %>%
  summarise(
    weight_sum = sum(weight, na.rm = TRUE),
    n_origins = n()
  ) %>%
  mutate(
    deviation = abs(weight_sum - 1)
  ) %>%
  filter(deviation > 0.001)

if(nrow(weight_check_unpd) > 0) {
  print("Warning: Some countries have weights not summing to 1:")
  problematic <- weight_check_unpd %>%
    arrange(desc(deviation)) %>%
    head(10)
  print(problematic)
} else {
  print("✓ All weights sum to 1 correctly")
}
countries_to_exclude <- c("SSD")

print(paste("Remove", length(countries_to_exclude), "problem country"))
#5.3.12 Add ND-GAIN vulnerability data
if(!exists("ndgain_2000_extended")) {
  stop("Error: ndgain_2000_extended not found. Please run the main analysis first.")
}

exposure_unpd <- weighted_unpd %>%
  filter(!dest_code %in% countries_to_exclude,
         !ori_code %in% countries_to_exclude) %>%
  left_join(
    ndgain_2000_extended %>% select(ori_code, V),
    by = "ori_code"
  ) %>%
  mutate(
    exposure_component = weight * V
  )

#5.3.13 Check unmatched vulnerability data
unmatched_unpd <- exposure_unpd %>%
  filter(is.na(V) & N_od_2000_unpd > 0) %>%
  group_by(ori_code, ori_country) %>%
  summarise(
    total_migration = sum(N_od_2000_unpd, na.rm = TRUE),
    n_destinations = n_distinct(dest_code),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_migration))

if(nrow(unmatched_unpd) > 0) {
  print(paste("\nOrigins without vulnerability data:", nrow(unmatched_unpd)))
  print("Top 10 by migration volume:")
  print(head(unmatched_unpd, 10))
  total_unmatched_migration <- sum(unmatched_unpd$total_migration)
  total_migration <- sum(unpd_final$N_od_2000_unpd, na.rm = TRUE)
  pct_unmatched <- (total_unmatched_migration / total_migration) * 100
  
  print(paste("Impact: ", round(pct_unmatched, 2), "% of total migration lacks vulnerability data"))
}
#5.3.14 Mapping
vulnerability_mapping <- tribble(
  ~missing_code, ~map_to_code, ~entity_name, ~sovereign, ~reason,
  "SSD", "SDN", "South Sudan", "Sudan", "Belonged to Sudan before 2011",
  "ESH", "MAR", "Western Sahara", "Morocco", "actual controller",
  "CUW", "NLD", "Curaçao", "Netherlands", "Constituent countries of the Netherlands",
  "BES", "NLD", "Bonaire, Sint Eustatius and Saba", "Netherlands", "Special Municipality of the Netherlands",
  "SXM", "NLD", "Sint Maarten", "Netherlands", "Constituent countries of the Netherlands",
  "VAT", "ITA", "Holy See", "Italy", "Near Italy"
)
ndgain_2000_extended_unpd <- ndgain_2000_extended

for(i in 1:nrow(vulnerability_mapping)) {
  missing_code <- vulnerability_mapping$missing_code[i]
  map_to_code <- vulnerability_mapping$map_to_code[i]
  entity_name <- vulnerability_mapping$entity_name[i]
  sovereign <- vulnerability_mapping$sovereign[i]
  
  target_vulnerability <- ndgain_2000_extended %>%
    filter(ori_code == map_to_code) %>%
    pull(V)
  
  if(length(target_vulnerability) > 0) {
    new_row <- data.frame(
      ori_country = entity_name,
      ori_code = missing_code,
      V = target_vulnerability[1],
      stringsAsFactors = FALSE
    )
    ndgain_2000_extended_unpd <- rbind(ndgain_2000_extended_unpd, new_row)
    
    print(paste("✓ Mapped:", entity_name, "(", missing_code, ") ->", 
                sovereign, "(", map_to_code, "), Vulnerability =", 
                round(target_vulnerability[1], 2)))
  } else {
    print(paste("✗ Failed to map:", entity_name, "- No data for", map_to_code))
  }
}
print(paste("Extended ND-GAIN now has", nrow(ndgain_2000_extended_unpd), "countries"))
#5.3.15 use extend NDGAIN
exposure_index_unpd <- weighted_unpd %>%
  left_join(
    ndgain_2000_extended_unpd %>% select(ori_code, V),
    by = "ori_code"
  ) %>%
  mutate(
    exposure_component = weight * V
  )
exposure_index_unpd <- exposure_unpd %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    exposure_index_unpd = sum(exposure_component, na.rm = TRUE),
    n_origins_with_data = sum(!is.na(exposure_component)),
    total_origins = n(),
    data_coverage = n_origins_with_data / total_origins,
    .groups = 'drop'
  ) %>%
  arrange(desc(exposure_index_unpd)) %>%
  mutate(rank_unpd = row_number())

exposure_unpd_extended <- weighted_unpd %>%
  left_join(
    ndgain_2000_extended_unpd %>% select(ori_code, V),
    by = "ori_code"
  ) %>%
  mutate(
    exposure_component = weight * V
  )
exposure_index_unpd <- exposure_unpd_extended %>%
  group_by(dest_country, dest_code) %>%
  summarise(
    exposure_index_unpd = sum(exposure_component, na.rm = TRUE),
    n_origins_with_data = sum(!is.na(exposure_component)),
    total_origins = n(),
    data_coverage = n_origins_with_data / total_origins,
    .groups = 'drop'
  ) %>%
  arrange(desc(exposure_index_unpd)) %>%
  mutate(rank_unpd = row_number())
#Check if still no match
unmatched_unpd_after <-exposure_unpd_extended %>%
  filter(is.na(V) & N_od_2000_unpd > 0) %>%
  group_by(ori_code, ori_country) %>%
  summarise(
    total_migration = sum(N_od_2000_unpd, na.rm = TRUE),
    n_destinations = n_distinct(dest_code),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_migration))

if(nrow(unmatched_unpd_after) > 0) {
  print(paste("Remaining origins without vulnerability data:", nrow(unmatched_unpd_after)))
  print("Top unmatched by migration volume:")
  print(head(unmatched_unpd_after, 10))
  total_unmatched_migration <- sum(unmatched_unpd_after$total_migration)
  total_migration <- sum(unpd_final$N_od_2000_unpd, na.rm = TRUE)
  pct_unmatched <- (total_unmatched_migration / total_migration) * 100
  
  print(paste("Reduced impact: ", round(pct_unmatched, 2), "% of total migration lacks vulnerability data"))
} else {
  print("✓ All origins now have vulnerability data!")
}
#5.3.16 Compare with World Bank Results
if(!exists("exposure_index_2000")) {
  stop("Error: exposure_index_2000 not found. Please run the World Bank analysis first.")
}

comparison_unpd_wb <- exposure_index_2000 %>%
  select(dest_code, dest_country,
         exposure_index_wb = exposure_index,
         rank_wb = rank) %>%
  inner_join(
    exposure_index_unpd %>%
      select(dest_code, exposure_index_unpd, rank_unpd, data_coverage),
    by = "dest_code"
  ) %>%
  mutate(
    rank_change = rank_wb - rank_unpd,
    index_change = exposure_index_wb - exposure_index_unpd,
    index_change_pct = (exposure_index_wb - exposure_index_unpd) / exposure_index_wb * 100
  )
print(paste("Countries in comparison:", nrow(comparison_unpd_wb)))

#5.3.17 Correlation
pearson_cor_unpd <- cor(comparison_unpd_wb$exposure_index_wb,
                        comparison_unpd_wb$exposure_index_unpd,
                        use = "complete.obs")

spearman_cor_unpd <- cor(comparison_unpd_wb$rank_wb,
                         comparison_unpd_wb$rank_unpd,
                         method = "spearman",
                         use = "complete.obs")
print(paste("Pearson correlation (index values):", round(pearson_cor_unpd, 4)))
print(paste("Spearman correlation (rankings):", round(spearman_cor_unpd, 4)))

#5.3.18 Identify major rank changes
# Top climbers
top_climbers_unpd <- comparison_unpd_wb %>%
  arrange(rank_change) %>%
  head(10) %>%
  select(Country = dest_country,
         `WB Rank` = rank_wb,
         `UNPD Rank` = rank_unpd,
         `Rank Change` = rank_change)

print("Top 10 countries with improved rankings in UNPD (negative = improvement):")
print(top_climbers_unpd)
# Top fallers
top_fallers_unpd <- comparison_unpd_wb %>%
  arrange(desc(rank_change)) %>%
  head(10) %>%
  select(Country = dest_country,
         `WB Rank` = rank_wb,
         `UNPD Rank` = rank_unpd,
         `Rank Change` = rank_change)
print("Top 10 countries with declined rankings in UNPD (positive = decline):")
print(top_fallers_unpd)

#5.3.19 Top 20 comparison
top20_wb <- comparison_unpd_wb %>%
  filter(rank_wb <= 20) %>%
  pull(dest_code)

top20_unpd <- comparison_unpd_wb %>%
  filter(rank_unpd <= 20) %>%
  pull(dest_code)

overlap_top20_unpd <- length(intersect(top20_wb, top20_unpd))
print(paste("Top 20 overlap between WB and UNPD:", overlap_top20_unpd, "/ 20"))

only_wb <- setdiff(top20_wb, top20_unpd)
only_unpd <- setdiff(top20_unpd, top20_wb)

if(length(only_wb) > 0) {
  print(paste("Only in WB top 20:", paste(only_wb, collapse = ", ")))
}

if(length(only_unpd) > 0) {
  print(paste("Only in UNPD top 20:", paste(only_unpd, collapse = ", ")))
}

#5.3.20 plot
plot_robust3 <- ggplot(comparison_unpd_wb,
                       aes(x = exposure_index_wb, y = exposure_index_unpd)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  labs(
    title = "Robustness Test 3: World Bank vs UNPD (Both 2000)",
    subtitle = paste("Pearson r =", round(pearson_cor_unpd, 3),
                     "| Spearman rho =", round(spearman_cor_unpd, 3)),
    x = "Exposure Index (World Bank 2000)",
    y = "Exposure Index (UNPD 2000)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave("~/Desktop/Data Collection/Dissertation Result/Robustness_Test3_WB_vs_UNPD_2000.pdf",
       plot = plot_robust3, width = 8, height = 6)

#5.3.21 Save detailed comparison
comparison_output_unpd <- comparison_unpd_wb %>%
  arrange(rank_wb) %>%
  select(
    Country = dest_country,
    ISO3 = dest_code,
    `WB Index` = exposure_index_wb,
    `UNPD Index` = exposure_index_unpd,
    `WB Rank` = rank_wb,
    `UNPD Rank` = rank_unpd,
    `Rank Change` = rank_change,
    `Index Change %` = index_change_pct,
    `Data Coverage` = data_coverage
  ) %>%
  mutate(
    `WB Index` = round(`WB Index`, 6),
    `UNPD Index` = round(`UNPD Index`, 6),
    `Index Change %` = round(`Index Change %`, 2),
    `Data Coverage` = round(`Data Coverage`, 3)
  )

write.csv(comparison_output_unpd,
          "~/Desktop/Data Collection/Dissertation Result/Robustness_Test3_WB_vs_UNPD.csv",
          row.names = FALSE)

#5.3.22 Summary of robustness 3
print(paste("Countries compared:", nrow(comparison_unpd_wb)))
print(paste("Mean absolute rank change:", round(mean(abs(comparison_unpd_wb$rank_change)), 2)))
print(paste("Maximum rank change:", max(abs(comparison_unpd_wb$rank_change))))
print(paste("Mean index difference:", round(mean(comparison_unpd_wb$index_change), 6)))
print(paste("Standard deviation of index difference:", round(sd(comparison_unpd_wb$index_change), 6)))

quantiles <- quantile(abs(comparison_unpd_wb$rank_change), probs = c(0.25, 0.5, 0.75, 0.9))
print("Rank change distribution:")
print(paste("  25th percentile:", round(quantiles[1], 1)))
print(paste("  Median:", round(quantiles[2], 1)))
print(paste("  75th percentile:", round(quantiles[3], 1)))
print(paste("  90th percentile:", round(quantiles[4], 1)))

if(pearson_cor_unpd > 0.9 & spearman_cor_unpd > 0.9) {
  assessment <- "✓ EXCELLENT: Very high consistency between WB and UNPD data sources"
} else if(pearson_cor_unpd > 0.7 & spearman_cor_unpd > 0.7) {
  assessment <- "✓ GOOD: Strong consistency between data sources with some variations"
} else if(pearson_cor_unpd > 0.5 & spearman_cor_unpd > 0.5) {
  assessment <- "○ MODERATE: Notable differences between data sources (expected given different definitions)"
} else {
  assessment <- "△ LOW: Significant differences between data sources"
}
print(assessment)

#5.4 Summary of Robustness 1-3
robustness_summary <- data.frame(
  Test = c("Test 1: WB2000 + ND1995", 
           "Test 2: WB1990 + ND2000", 
           "Test 3: UNPD2000 + ND2000"),
  Pearson = c(pearson_cor, pearson_cor2, pearson_cor_unpd),
  Spearman = c(spearman_cor, spearman_cor2, spearman_cor_unpd),
  Top20_Overlap = c(overlap_top20, overlap_top20_2, overlap_top20_unpd),
  Assessment = c(
    ifelse(pearson_cor > 0.9, "Excellent", "Good"),
    ifelse(pearson_cor2 > 0.9, "Excellent", "Good"),
    ifelse(pearson_cor_unpd > 0.6, "As Expected", "Moderate")
  )
)

print(robustness_summary)

write.csv(robustness_summary,
          "~/Desktop/Data Collection/Dissertation Result/Robustness_Summary_All_Tests.csv",
          row.names = FALSE)

#6 Figures and Tables
library(gridExtra)
library(scales)
library(kableExtra)
library(patchwork)
library(RColorBrewer)

#Set theme
theme_dissertation <- theme_minimal() +
  theme(
    text = element_text(family = "Times", size = 12),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
#Color
dissertation_colors <- list(
  primary = "#2C3E50",      
  secondary = "#E74C3C",    
  tertiary = "#3498DB",     
  fill = "#ECF0F1",       
  gradient_low = "#FEE08B", 
  gradient_high = "#D53E4F" 
)

# FIGURE 1: World Map Heat Map
#IEI
world <- ne_countries(scale = "medium", returnclass = "sf")

world_exposure <- world %>%
  left_join(exposure_ranking_2000, by = c("iso_a3_eh" = "ISO3"))

world_exposure_enhanced <- world_exposure %>%
  mutate(
    exposure_category = cut(`Exposure Index`,
                            breaks = c(-Inf, 
                                       quantile(`Exposure Index`, 0.25, na.rm = TRUE),
                                       quantile(`Exposure Index`, 0.50, na.rm = TRUE),
                                       quantile(`Exposure Index`, 0.75, na.rm = TRUE),
                                       Inf),
                            labels = c("Low (Q1)", "Medium-Low (Q2)", 
                                       "Medium-High (Q3)", "High (Q4)"),
                            include.lowest = TRUE)
  )
fig1_world_map <- ggplot(data = world_exposure_enhanced) +
  geom_sf(aes(fill = `Exposure Index`), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Indirect Exposure Index",
    option = "plasma",
    na.value = "grey85",
    limits = c(0, max(exposure_ranking_2000$`Exposure Index`, na.rm = TRUE)),
    breaks = pretty_breaks(n = 5),
    labels = number_format(accuracy = 0.01)
  ) +
  labs(
    title = "(a) Global Distribution of IEI Index to Climate Migration",
    subtitle = "(Year 2000)",
    caption = "Note: Grey areas indicate missing data. Index ranges from 0 (lowest) to 1 (highest exposure)."
  ) +  coord_sf(crs = "+proj=robin") +  
  theme_dissertation +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.caption = element_text(size = 9, hjust = 0, face = "italic")
  )
# FIGURE: Original ND-GAIN Vulnerability Index World Map
ndgain_original_map <- ndgain_clean %>%
  select(ISO3 = ISO3, vulnerability_score = X2000) %>%
  filter(!is.na(vulnerability_score)) %>%
  distinct() 

world_ndgain_vulnerability <- world %>%
  left_join(ndgain_original_map, by = c("iso_a3_eh" = "ISO3"))

unmatched_ndgain_countries <- ndgain_original_map %>%
  filter(!ISO3 %in% world$iso_a3_eh)

if(nrow(unmatched_ndgain_countries) > 0) {
  print(paste("Warning:", nrow(unmatched_ndgain_countries), "countries cannot be matched to map"))
  print(head(unmatched_ndgain_countries$ISO3, 10))
}

ndgain_vulnerability_map <- ggplot(data = world_ndgain_vulnerability) +
  geom_sf(aes(fill = vulnerability_score), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Vulnerability Score",
    option = "plasma",  
    na.value = "grey85",
    limits = c(0, max(ndgain_original_map$vulnerability_score, na.rm = TRUE)),
    breaks = pretty_breaks(n = 5),
    labels = number_format(accuracy = 0.01),
  ) +
  labs(
    title = "(b) Global Distribution of ND-GAIN Index of the Vulnerability of Countries to Cliamte Change",
    subtitle = "(Year 2000)",,
    caption = "Note: Grey areas indicate missing data. Index ranges from 0 (lowest) to 1 (highest vulnerability).Data Source: ND-GAIN Country Index, vulnerability sub-component"
  ) + coord_sf(crs = "+proj=robin") +
  theme_dissertation + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    panel.background = element_rect(fill = "white"),  
    plot.caption = element_text(size = 9, hjust = 0, face = "italic")
  )

# Save high-resolution map
combined_map <- fig1_world_map / ndgain_vulnerability_map

ggsave("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Figure1_WorldMap.pdf", 
       combined_map, 
       width = 10, height = 12, dpi = 300)
ggsave("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Figure1_WorldMap.png", 
       combined_map, 
       width = 10, height = 12, dpi = 300)

#Table 2 Ranking Compare between IEI(left), vulnerability index (right)
library(gridExtra)
library(grid)
library(countrycode)

iei_top20 <- exposure_ranking_2000 %>%
  head(20) %>%
  mutate(
    Region = countrycode(ISO3, "iso3c", "region", warn = FALSE),
    Region_Short = case_when(
      Region == "Sub-Saharan Africa" ~ "SSA",
      Region == "Middle East & North Africa" ~ "MENA",
      Region == "Europe & Central Asia" ~ "Europe",
      Region == "East Asia & Pacific" ~ "EAP",
      Region == "Latin America & Caribbean" ~ "LAC",
      Region == "North America" ~ "NAm",
      Region == "South Asia" ~ "SAs",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    Rank,
    Country,
    `IEI Score` = `Exposure Index`,
    Region = Region_Short
  ) %>%
  mutate(`IEI Score` = sprintf("%.3f", `IEI Score`))

ndgain_top20 <- ndgain_clean %>%
  filter(!is.na(vul_2000)) %>%
  arrange(desc(vul_2000)) %>%
  head(20) %>%
  mutate(
    Rank = row_number(),
    Region = countrycode(ISO3, "iso3c", "region", warn = FALSE),
    Region_Short = case_when(
      Region == "Sub-Saharan Africa" ~ "SSA",
      Region == "Middle East & North Africa" ~ "MENA",
      Region == "Europe & Central Asia" ~ "Europe",
      Region == "East Asia & Pacific" ~ "EAP",
      Region == "Latin America & Caribbean" ~ "LAC",
      Region == "North America" ~ "NAm",
      Region == "South Asia" ~ "SAs",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    Rank,
    Country = Name,
    `ND-GAIN` = vul_2000,
    Region = Region_Short
  ) %>%
  mutate(`ND-GAIN` = sprintf("%.3f", `ND-GAIN`))

region_colors <- c(
  "SSA" = "#FFD700",           
  "MENA" = "#87CEEB",          
  "Europe" = "#DDA0DD",        
  "EAP" = "#98FB98",          
  "LAC" = "#FFB6C1",           
  "NAm" = "#F0E68C",          
  "SAs" = "#FFA07A",           
  "Other" = "#F5F5F5"         
)

iei_row_colors <- sapply(1:nrow(iei_top20), function(i) {
  region <- iei_top20$Region[i]
  color <- region_colors[region]
  if(is.na(color)) color <- "#F5F5F5"
  return(color)
})

iei_table <- tableGrob(
  iei_top20,
  rows = NULL,
  theme = ttheme_default(
    base_size = 8,
    core = list(
      fg_params = list(col = "black", fontsize = 8),
      bg_params = list(
        fill = matrix(iei_row_colors, nrow = 20, ncol = 4, byrow = FALSE),
        col = "grey40",
        lwd = 0.5
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontsize = 9, fontface = "bold"),
      bg_params = list(fill = "#2C3E50", col = "grey40", lwd = 0.5)
    )
  )
)

ndgain_row_colors <- sapply(1:nrow(ndgain_top20), function(i) {
  region <- ndgain_top20$Region[i]
  color <- region_colors[region]
  if(is.na(color)) color <- "#F5F5F5"
  return(color)
})

ndgain_table <- tableGrob(
  ndgain_top20,
  rows = NULL,
  theme = ttheme_default(
    base_size = 8,
    core = list(
      fg_params = list(col = "black", fontsize = 8),
      bg_params = list(
        fill = matrix(ndgain_row_colors, nrow = 20, ncol = 4, byrow = FALSE),
        col = "grey40",
        lwd = 0.5
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontsize = 9, fontface = "bold"),
      bg_params = list(fill = "#2C3E50", col = "grey40", lwd = 0.5)
    )
  )
)

iei_title <- textGrob("IEI Index", 
                      gp = gpar(fontsize = 12, fontface = "bold"))
ndgain_title <- textGrob("ND-GAIN Index", 
                         gp = gpar(fontsize = 12, fontface = "bold"))

iei_with_title <- arrangeGrob(
  iei_title,
  iei_table,
  ncol = 1,
  heights = unit(c(0.1, 1), c("null", "null"))
)

ndgain_with_title <- arrangeGrob(
  ndgain_title,
  ndgain_table,
  ncol = 1,
  heights = unit(c(0.1, 1), c("null", "null"))
)

final_plot <- arrangeGrob(
  iei_with_title,
  ndgain_with_title,
  ncol = 2,
  widths = unit(c(1, 1), "null"),
  padding = unit(0.1, "cm")  
)

pdf("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Table 1_Ranking Compare between IEI(left), vulnerability (right).pdf",
    width = 8, height = 6.5)  
grid.draw(final_plot)
dev.off()

png("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Table 1_Ranking Compare between IEI(left), vulnerability (right).png",
    width = 800, height = 650, res = 110)
grid.draw(final_plot)
dev.off()

#Table 2 Top20 Bottom20
iei_top20 <- exposure_ranking_2000 %>%
  head(20) %>%
  mutate(
    Region = countrycode(ISO3, "iso3c", "region", warn = FALSE),
    Region_Short = case_when(
      Region == "Sub-Saharan Africa" ~ "SSA",
      Region == "Middle East & North Africa" ~ "MENA",
      Region == "Europe & Central Asia" ~ "Europe",
      Region == "East Asia & Pacific" ~ "EAP",
      Region == "Latin America & Caribbean" ~ "LAC",
      Region == "North America" ~ "NAm",
      Region == "South Asia" ~ "SAs",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    Rank,
    Country,
    `IEI Score` = `Exposure Index`,
    Region = Region_Short
  ) %>%
  mutate(`IEI Score` = sprintf("%.3f", `IEI Score`))

iei_bottom20 <- exposure_ranking_2000 %>%
  tail(20) %>%
  mutate(
    Region = countrycode(ISO3, "iso3c", "region", warn = FALSE),
    Region_Short = case_when(
      Region == "Sub-Saharan Africa" ~ "SSA",
      Region == "Middle East & North Africa" ~ "MENA",
      Region == "Europe & Central Asia" ~ "Europe",
      Region == "East Asia & Pacific" ~ "EAP",
      Region == "Latin America & Caribbean" ~ "LAC",
      Region == "North America" ~ "NAm",
      Region == "South Asia" ~ "SAs",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    Rank,
    Country,
    `IEI Score` = `Exposure Index`,
    Region = Region_Short
  ) %>%
  mutate(`IEI Score` = sprintf("%.3f", `IEI Score`))

region_colors <- c(
  "SSA" = "#FFD700",           
  "MENA" = "#87CEEB",          
  "Europe" = "#DDA0DD",      
  "EAP" = "#98FB98",           
  "LAC" = "#FFB6C1",           
  "NAm" = "#F0E68C",         
  "SAs" = "#FFA07A",          
  "Other" = "#F5F5F5"         
)

iei_row_colors_top <- sapply(1:nrow(iei_top20), function(i) {
  region <- iei_top20$Region[i]
  color <- region_colors[region]
  if(is.na(color)) color <- "#F5F5F5"
  return(color)
})

iei_row_colors_bottom <- sapply(1:nrow(iei_bottom20), function(i) {
  region <- iei_bottom20$Region[i]
  color <- region_colors[region]
  if(is.na(color)) color <- "#F5F5F5"
  return(color)
})

iei_table_top <- tableGrob(
  iei_top20,
  rows = NULL,
  theme = ttheme_default(
    base_size = 8,
    core = list(
      fg_params = list(col = "black", fontsize = 8),
      bg_params = list(
        fill = matrix(iei_row_colors_top, nrow = 20, ncol = 4, byrow = FALSE), 
        col = "grey40",
        lwd = 0.5
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontsize = 9, fontface = "bold"),
      bg_params = list(fill = "#2C3E50", col = "grey40", lwd = 0.5)
    )
  )
)

iei_table_bottom <- tableGrob(
  iei_bottom20,
  rows = NULL,
  theme = ttheme_default(
    base_size = 8,
    core = list(
      fg_params = list(col = "black", fontsize = 8),
      bg_params = list(
        fill = matrix(iei_row_colors_bottom, nrow = 20, ncol = 4, byrow = FALSE), 
        col = "grey40",
        lwd = 0.5
      )
    ),
    colhead = list(
      fg_params = list(col = "white", fontsize = 9, fontface = "bold"),
      bg_params = list(fill = "#2C3E50", col = "grey40", lwd = 0.5)
    )
  )
)

ieitop_title <- textGrob("IEI Index Top 20", 
                      gp = gpar(fontsize = 12, fontface = "bold"))
ieibottom_title <- textGrob("IEI Index Bottom 20", 
                         gp = gpar(fontsize = 12, fontface = "bold"))

iei20_with_title <- arrangeGrob(
  ieitop_title,
  iei_table_top,
  ncol = 1,
  heights = unit(c(0.1, 1), c("null", "null"))
)

iei20b_with_title <- arrangeGrob(
  ieibottom_title,
  iei_table_bottom,
  ncol = 1,
  heights = unit(c(0.1, 1), c("null", "null"))
)


final_plot <- arrangeGrob(
  iei20_with_title,
  iei20b_with_title,
  ncol = 2,
  widths = unit(c(1, 1), "null"),
  padding = unit(0.1, "cm")  
)

pdf("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Table 2_Ranking Compare between IEI top&bottom20.pdf",
    width = 8, height = 10)  
grid.draw(final_plot)
dev.off()

png("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Table 2_Ranking Compare between IEI top&bottom20.png",
    width = 800, height = 650, res = 110)
grid.draw(final_plot)
dev.off()

#FIGURE 2: Decomposition Analysis
# Get Top 10 destination countries by exposure
top10_destinations <- exposure_index_2000 %>%
  arrange(desc(exposure_index)) %>%
  head(10) %>%
  pull(dest_code)
bottom10_destinations <- exposure_index_2000 %>%
  arrange(desc(exposure_index)) %>%
  tail(10) %>%
  pull(dest_code)

# Calculate contribution of each origin to destination exposure
decomposition_data <- exposure_2000 %>%
  filter(dest_code %in% top10_destinations, !is.na(V)) %>%
  group_by(dest_code, dest_country) %>%
  mutate(
    # Calculate percentage contribution
    total_exposure = sum(exposure_component, na.rm = TRUE),
    contribution_pct = (exposure_component / total_exposure) * 100
  ) %>%
  # Select top 5 contributors, group rest as "Others"
  arrange(desc(contribution_pct)) %>%
  mutate(
    rank = row_number(),
    ori_country_grouped = ifelse(rank <= 5, ori_country, "Others")
  ) %>%
  group_by(dest_code, dest_country, ori_country_grouped) %>%
  summarise(
    contribution_pct = sum(contribution_pct, na.rm = TRUE),
    avg_vulnerability = weighted.mean(V, w = N_od_2000, na.rm = TRUE),
    total_migrants = sum(N_od_2000, na.rm = TRUE),
    .groups = 'drop'
  )

decomposition_data_b <- exposure_2000 %>%
  filter(dest_code %in% bottom10_destinations, !is.na(V)) %>%
  group_by(dest_code, dest_country) %>%
  mutate(
    # Calculate percentage contribution
    total_exposure = sum(exposure_component, na.rm = TRUE),
    contribution_pct = (exposure_component / total_exposure) * 100
  ) %>%
  # Select top 5 contributors, group rest as "Others"
  arrange(desc(contribution_pct)) %>%
  mutate(
    rank = row_number(),
    ori_country_grouped = ifelse(rank <= 5, ori_country, "Others")
  ) %>%
  group_by(dest_code, dest_country, ori_country_grouped) %>%
  summarise(
    contribution_pct = sum(contribution_pct, na.rm = TRUE),
    avg_vulnerability = weighted.mean(V, w = N_od_2000, na.rm = TRUE),
    total_migrants = sum(N_od_2000, na.rm = TRUE),
    .groups = 'drop'
  )
# Get destination order by total exposure
dest_order <- exposure_index_2000 %>%
  filter(dest_code %in% top10_destinations) %>%
  arrange(desc(exposure_index)) %>%
  pull(dest_country)

dest_orderb <- exposure_index_2000 %>%
  filter(dest_code %in% bottom10_destinations) %>%
  arrange(desc(exposure_index)) %>%
  pull(dest_country)
## Set factor levels for proper ordering
#decomposition_data$dest_country <- factor(decomposition_data$dest_country, 
                                          #levels = rev(dest_order))

#decomposition_data_b$dest_country <- factor(decomposition_data_b$dest_country, 
                                         # levels = rev(dest_orderb))

# Get top 10 destinations ordered by exposure index
top10_order <- exposure_index_2000 %>%
  arrange(desc(exposure_index)) %>%
  head(10) %>%
  pull(dest_country)
bottom10_order <- exposure_index_2000 %>%
  arrange(desc(exposure_index)) %>%
  tail(10) %>%
  pull(dest_country)

# Prepare data for faceted plot
facet_data <- decomposition_data %>%
  filter(ori_country_grouped != "Others") %>%
  group_by(dest_country) %>%
  arrange(desc(contribution_pct)) %>%
  slice_head(n = 5) %>%
  # Add rank within each destination for ordering
  mutate(rank_within = row_number()) %>%
  ungroup() %>%
  # Set destination order
  mutate(dest_country = factor(dest_country, levels = top10_order))

facet_datab <- decomposition_data_b %>%
  filter(ori_country_grouped != "Others") %>%
  group_by(dest_country) %>%
  arrange(desc(contribution_pct)) %>%
  slice_head(n = 5) %>%
  # Add rank within each destination for ordering
  mutate(rank_within = row_number()) %>%
  ungroup() %>%
  # Set destination order
  mutate(dest_country = factor(dest_country, levels = bottom10_order))


# Create a unique identifier for each origin-destination pair
# This allows independent ordering within each facet
facet_data <- facet_data %>%
  mutate(
    # Create unique labels with invisible unicode characters to make them unique
    ori_label = paste0(ori_country_grouped, 
                       strrep("\u200B", rank_within)),  # Zero-width space character
    # Order by contribution within each destination
    ori_label = reorder(ori_label, contribution_pct)
  )

facet_datab <- facet_datab %>%
  mutate(
    # Create unique labels with invisible unicode characters to make them unique
    ori_label = paste0(ori_country_grouped, 
                       strrep("\u200B", rank_within)),  # Zero-width space character
    # Order by contribution within each destination
    ori_label = reorder(ori_label, contribution_pct)
  )

figure2_faceted <- ggplot(facet_data, 
                          aes(x = ori_label, 
                              y = contribution_pct)) +
  geom_bar(stat = "identity", fill = dissertation_colors$primary, alpha = 0.8) +
  geom_text(aes(label = paste0(round(contribution_pct, 1), "%")),
            hjust = -0.1, size = 2.5) +
  facet_wrap(~ dest_country, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("\u200B", "", x)) +  # Remove invisible characters from labels
  scale_y_continuous(limits = c(0, max(facet_data$contribution_pct) * 1.1)) +
  labs(
    title = "Top Origin Countries by Destination (IEI Top 10)",
    x = "",
    y = "Contribution to IEI (%)"
  ) +
  theme_dissertation +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = dissertation_colors$fill)
  )

figure3_faceted <- ggplot(facet_datab, 
                          aes(x = ori_label, 
                              y = contribution_pct)) +
  geom_bar(stat = "identity", fill = dissertation_colors$primary, alpha = 0.8) +
  geom_text(aes(label = paste0(round(contribution_pct, 1), "%")),
            hjust = -0.1, size = 2.5) +
  facet_wrap(~ dest_country, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("\u200B", "", x)) +  # Remove invisible characters from labels
  scale_y_continuous(limits = c(0, max(facet_data$contribution_pct) * 1.1)) +
  labs(
    title = "Top Origin Countries by Destination (IEI Bottom 10)",
    x = "",
    y = "Contribution to IEI (%)"
  ) +
  theme_dissertation +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = dissertation_colors$fill)
  )
#Save Outputs
output_dir <- "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/"

ggsave(
  filename = paste0(output_dir, "Figure2_Decomposition Analysis(top10).pdf"),
  plot = figure2_faceted,
  width = 12,
  height = 10,
  dpi = 300,
  device = "pdf"
)
ggsave("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Figure2_Decomposition Analysis(top10).png", 
       width = 12,
       height = 10,
       dpi = 400)

ggsave(
  filename = paste0(output_dir, "Figure3_Decomposition Analysis(bottom10).pdf"),
  plot = figure3_faceted,
  width = 12,
  height = 10,
  dpi = 300,
  device = "pdf"
)
ggsave("~/Desktop/Data Collection/Dissertation Result/Figures and Tables/Figure3_Decomposition Analysis(bottom10).png", 
       width = 12,
       height = 10,
       dpi = 400)

#FIGURE 4: Relationship Analysis (2x2 Matrix)
#theme for F2
theme_figure2 <- theme_dissertation +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0),
    legend.position = "right",  
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.3, "cm")
  )
#Network
network_stats <- wb_final %>%
  filter(N_od_2000 > 0) %>%
  group_by(dest_code) %>%
  summarise(
    total_immigrants = sum(N_od_2000, na.rm = TRUE),
    n_source_countries = n(),
    n_unique_origins = n_distinct(ori_code),
    herfindahl = sum((N_od_2000/sum(N_od_2000, na.rm = TRUE))^2, na.rm = TRUE),
    max_share = max(N_od_2000/sum(N_od_2000, na.rm = TRUE), na.rm = TRUE),
    .groups = 'drop'
  )
#Combine Analysis
analysis_data <- exposure_index_2000 %>%
  left_join(
    ndgain_2000_extended %>% 
      select(dest_code = ori_code, direct_vulnerability = V),
    by = "dest_code"
  ) %>%
  mutate(
    region = countrycode(dest_code, "iso3c", "region", warn = FALSE),
    region_short = case_when(
      region == "Europe & Central Asia" ~ "Europe & C. Asia",
      region == "Latin America & Caribbean" ~ "LAC",
      region == "Middle East & North Africa" ~ "MENA",
      region == "East Asia & Pacific" ~ "E. Asia & Pacific",
      TRUE ~ region
    )
  ) %>%
  left_join(network_stats, by = "dest_code")
#Key countries
key_countries <- c("USA", "DEU", "FRA", "GBR", "CHN", "IND", "JPN", 
                   "CAN", "AUS", "BRA", "RUS", "ZAF", "NGA", "MEX")
#Cor direct and indi
cor_direct_indirect <- cor(
  analysis_data$direct_vulnerability,
  analysis_data$exposure_index,
  use = "complete.obs"
)
#Panel A: Direct vs Indirect Vulnerability
panel_a <- ggplot(analysis_data %>% filter(!is.na(direct_vulnerability)), 
                  aes(x = direct_vulnerability, y = exposure_index)) +
  geom_point(alpha = 0.6, color = dissertation_colors$primary, size = 2) +
  geom_smooth(method = "lm", se = TRUE, 
              color = dissertation_colors$secondary, 
              fill = dissertation_colors$secondary, 
              alpha = 0.15, size = 0.8) +
  geom_text_repel(
    data = . %>% filter(dest_code %in% key_countries |
                          exposure_index > quantile(exposure_index, 0.95, na.rm = TRUE) |
                          exposure_index < quantile(exposure_index, 0.05, na.rm = TRUE)),
    aes(label = dest_code),
    size = 2.5,
    max.overlaps = 20,
    segment.size = 0.3,
    segment.alpha = 0.5
  ) +
  annotate("text", 
           x = min(analysis_data$direct_vulnerability, na.rm = TRUE) + 0.02,
           y = max(analysis_data$exposure_index, na.rm = TRUE) - 0.02,
           label = paste("r =", round(cor_direct_indirect, 3)),
           size = 3, hjust = 0, fontface = "italic") +
  labs(
    title = "(a) Direct vs Indirect Climate Vulnerability",
    x = "Direct Vulnerability (ND-GAIN)",
    y = "Indirect Exposure Index"
  ) +
  theme_figure2

#Panel B: Network Diversity and Exposure
panel_b <- ggplot(analysis_data %>% filter(!is.na(n_source_countries)), 
                  aes(x = n_source_countries, y = exposure_index)) +
  geom_point(aes(color = herfindahl), size = 2.5, alpha = 0.7) +
  scale_color_gradient(
    low = dissertation_colors$gradient_low,
    high = dissertation_colors$gradient_high,
    name = "Network\nConcentration"
  ) +
  geom_smooth(method = "loess", se = TRUE, 
              color = dissertation_colors$secondary,
              fill = dissertation_colors$secondary, 
              alpha = 0.15, size = 0.8) +
  scale_color_viridis_c(
    name = "Network\nConcentration",
    option = "plasma",
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0\n(Diverse)", "0.25", "0.5", "0.75", "1\n(Concentrated)")
  ) +
  labs(
    title = "(b) Network Diversity and Indirect Exposure",
    x = "Number of Origin Countries",
    y = "Indirect Exposure Index"
  ) +
  theme_figure2

#Panel C: Regional Distribution
regional_summary <- analysis_data %>%
  filter(!is.na(region_short)) %>%
  group_by(region_short) %>%
  summarise(
    median_exposure = median(exposure_index, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(median_exposure)

analysis_data$region_ordered <- factor(
  analysis_data$region_short,
  levels = regional_summary$region_short
)

panel_c <- ggplot(analysis_data %>% filter(!is.na(region_ordered)), 
                  aes(x = region_ordered, y = exposure_index)) +
  geom_boxplot(fill = dissertation_colors$fill, 
               color = dissertation_colors$primary,
               alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, 
              color = dissertation_colors$primary, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, 
               size = 3, color = dissertation_colors$secondary) +
  geom_text(data = regional_summary,
            aes(x = region_short, y = 0.28, 
                label = paste("n =", n)),
            size = 2.5, color = "grey50") +
  coord_flip() +
  labs(
    title = "(c) Regional Distribution of Indirect Exposure",
    x = "",
    y = "Indirect Exposure Index"
  ) +
  theme_figure2 +
  theme(legend.position = "none") 

#Panel D: Immigration Scale and Exposure
panel_d <- ggplot(analysis_data %>% filter(total_immigrants > 0), 
                  aes(x = total_immigrants, y = exposure_index)) +
  geom_point(aes(size = n_source_countries), 
             color = dissertation_colors$primary, 
             alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, 
              color = dissertation_colors$secondary,
              fill = dissertation_colors$secondary, 
              alpha = 0.15, size = 0.8) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_color_viridis_c(
    name = "Origin\nCountries",
    option = "viridis",
    breaks = pretty_breaks(n = 4)
  ) +
  scale_size_continuous(
    range = c(1, 5),
    guide = "none"
  ) +
  labs(
    title = "(d) Immigration Scale and Indirect Exposure",
    x = expression("Total Immigration Stock (log"[10]*" scale)"),
    y = "Indirect Exposure Index"
  ) +
  theme_figure2

#Combine Figure 2
figure2_combined <- (panel_a + panel_b) / (panel_c + panel_d) +
  plot_annotation(
    title = "Multi-dimensional Analysis of Indirect Exposure to Climate Migration",
    caption = paste("Note: Panel (a) shows the relationship between direct climate vulnerability and indirect exposure (r =",
                    round(cor_direct_indirect, 3), 
                    "). Panel (b) illustrates how network diversity relates to exposure,",
                    "with color indicating concentration (Herfindahl index). Panel (c) presents regional variations.",
                    "Panel (d) examines the relationship between total immigration stock and exposure.",
                    "Data sources: World Bank Global Bilateral Migration Database (2000) and ND-GAIN Vulnerability Index (2000)."),
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", family = "Times", hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0, family = "Times", 
                                  color = "grey40", lineheight = 1.2)
    )
  )
output_dir <- "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
#pdf
ggsave(
  filename = paste0(output_dir, "Figure4_Multidimensional_Analysis.pdf"),
  plot = figure2_combined,
  width = 12,
  height = 10,
  dpi = 300,
  device = "pdf"
)
#png
ggsave(
  filename = paste0(output_dir, "Figure4_Multidimensional_Analysis.png"),
  plot = figure2_combined,
  width = 12,
  height = 10,
  dpi = 300,
  device = "png",
)

#TABLE 3: Robustness Check Summary
# Comprehensive summary of all robustness tests

library(gt)
library(kableExtra)
library(tidyverse)

# Compile Robustness Test Results
# Create comprehensive robustness summary
robustness_summary <- data.frame(
  Test_Number = c("Baseline", "Test 1", "Test 2", "Test 3"),
  Test_Description = c(
    "WB 2000 + ND-GAIN 2000",
    "WB 2000 + ND-GAIN 1995", 
    "WB 1990 + ND-GAIN 2000",
    "UNPD 2000 + ND-GAIN 2000"
  ),
  Data_Change = c(
    "Reference",
    "Vulnerability year",
    "Migration year",
    "Migration source"
  ),
  Sample_Size = c(
    nrow(exposure_index_2000),
    nrow(robustness_comparison),
    nrow(robustness_comparison2),
    nrow(comparison_unpd_wb)
  ),
  Pearson_Correlation = c(
    1.000,
    pearson_cor,
    pearson_cor2,
    pearson_cor_unpd
  ),
  Spearman_Correlation = c(
    1.000,
    spearman_cor,
    spearman_cor2,
    spearman_cor_unpd
  ),
  Top20_Overlap = c(
    20,
    overlap_top20,
    overlap_top20_2,
    overlap_top20_unpd
  ),
  Mean_Rank_Change = c(
    0,
    mean(abs(robustness_comparison$rank_change), na.rm = TRUE),
    mean(abs(robustness_comparison2$rank_change), na.rm = TRUE),
    mean(abs(comparison_unpd_wb$rank_change), na.rm = TRUE)
  ),
  Max_Rank_Change = c(
    0,
    max(abs(robustness_comparison$rank_change), na.rm = TRUE),
    max(abs(robustness_comparison2$rank_change), na.rm = TRUE),
    max(abs(comparison_unpd_wb$rank_change), na.rm = TRUE)
  )
)


# Create Professional Table using GT

table2_gt <- robustness_summary %>%
  select(
    Test = Test_Number,
    Description = Test_Description,
    `Changed Variable` = Data_Change,
    N = Sample_Size,
    `Pearson r` = Pearson_Correlation,
    `Spearman ρ` = Spearman_Correlation,
    `Top 20 Overlap` = Top20_Overlap,
    `Mean Rank Δ` = Mean_Rank_Change,
    `Max Rank Δ` = Max_Rank_Change,
  ) %>%
  gt() %>%
  # Title and subtitle
  tab_header(
    title = "Robustness Tests Summary",
    subtitle = "Stability of indirect exposure index across different specifications"
  ) %>%
  # Format numbers
  fmt_number(
    columns = c(`Pearson r`, `Spearman ρ`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(`Mean Rank Δ`),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(`Max Rank Δ`),
    decimals = 0
  ) %>%
  # Column spanners
  tab_spanner(
    label = "Correlation Coefficients",
    columns = c(`Pearson r`, `Spearman ρ`)
  ) %>%
  tab_spanner(
    label = "Rank Changes",
    columns = c(`Top 20 Overlap`, `Mean Rank Δ`, `Max Rank Δ`)
  ) %>%
  # Footnotes
  tab_footnote(
    footnote = "Pearson correlation measures linear association between index values",
    locations = cells_column_labels(columns = `Pearson r`)
  ) %>%
  tab_footnote(
    footnote = "Spearman correlation measures rank-order consistency",
    locations = cells_column_labels(columns = `Spearman ρ`)
  ) %>%
  tab_footnote(
    footnote = "Number of countries appearing in both baseline and test top 20",
    locations = cells_column_labels(columns = `Top 20 Overlap`)
  )

# Add source note using tab_footnote instead (GT compatible method)
table2_gt <- table2_gt %>%
  tab_footnote(
    footnote = "WB = World Bank Global Bilateral Migration Database; UNPD = UN Population Division International Migrant Stock",
    locations = cells_title()
  )

# Format data for simple table
table2_simple <- robustness_summary %>%
  select(
    Test = Test_Number,
    Description = Test_Description,
    N = Sample_Size,
    `Pearson r` = Pearson_Correlation,
    `Spearman ρ` = Spearman_Correlation,
    `Top 20\nOverlap` = Top20_Overlap,
    `Mean Rank\nChange` = Mean_Rank_Change,
  ) %>%
  mutate(
    `Pearson r` = sprintf("%.3f", `Pearson r`),
    `Spearman ρ` = sprintf("%.3f", `Spearman ρ`),
    `Top 20\nOverlap` = paste0(`Top 20\nOverlap`, "/20"),
    `Mean Rank\nChange` = sprintf("%.1f", `Mean Rank\nChange`)
  )


# Create Publication-Ready Version using gridExtra

library(gridExtra)
library(grid)

# Create table grob
table2_grob <- tableGrob(
  table2_simple,
  rows = NULL,
  theme = ttheme_default(
    base_size = 10,
    base_family = "serif",
    core = list(
      fg_params = list(col = "black"),
      bg_params = list(fill = c("white", "#F5F5F5"), col = NA)
    ),
    colhead = list(
      fg_params = list(col = "white", fontface = "bold"),
      bg_params = list(fill = dissertation_colors$primary, col = NA)
    )
  )
)

# Add title and notes
title_grob <- textGrob(
  "Table 2. Robustness Tests Summary",
  gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "serif"),
  hjust = 0, x = 0.05
)

subtitle_grob <- textGrob(
  "Stability of indirect exposure index across different specifications",
  gp = gpar(fontsize = 11, fontfamily = "serif", col = "grey40"),
  hjust = 0, x = 0.05
)

note_grob <- textGrob(
  "Note: WB = World Bank Global Bilateral Migration Database; UNPD = UN Population Division International Migrant Stock.\nAll correlations are statistically significant at p < 0.001 level.",
  gp = gpar(fontsize = 9, fontfamily = "serif", col = "grey40"),
  hjust = 0, x = 0.05
)

# Combine all elements
table2_final <- arrangeGrob(
  title_grob,
  subtitle_grob,
  table2_grob,
  note_grob,
  ncol = 1,
  heights = unit(c(0.5, 0.3, 4, 0.5), "cm")
)

# Save Outputs
output_dir <- "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/"

# Save as PDF using gridExtra version
pdf(paste0(output_dir, "Table3_Robustness_Summary.pdf"), 
    width = 10, height = 6)
grid.draw(table2_final)
dev.off()

# Save GT table as HTML (if gt is properly installed)
if(exists("table2_gt")) {
  gtsave(table2_gt, 
         filename = paste0(output_dir, "Table3_Robustness_Summary.html"))
}

# Save as CSV
write.csv(robustness_summary, 
          paste0(output_dir, "Table3_Robustness_Summary.csv"),
          row.names = FALSE)

#Appendix 1 IEI Ranking
library(writexl)
write_xlsx(exposure_ranking_2000, 
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixI_IEI_Ranking_2000.xlsx")

#Appendix II Problem Codes Treatment (ISO3 World Bank)
table_a1 <- data.frame(
  Code = c("ANT", "CHI", "XKX", "SCG", "ZZZ"),
  Country_Territory = c("Netherlands Antilles", "Channel Islands", "Kosovo", 
                        "Serbia and Montenegro", "Refugees"),
  Treatment = rep("Excluded", 5),
  Reason = c("Dissolved 2010", "Crown Dependency", "Disputed territory", 
             "Dissolved 2006", "Special code"),
  As_Origin = 0,
  As_Destination = 0,
  Total = 0,
  stringsAsFactors = FALSE
)

wb_clean_numeric <- wb_clean %>%
  mutate(
    `2000` = as.numeric(gsub("\\.\\.", NA, `2000`))
  )

if(exists("wb_clean_numeric")) {
  for(i in 1:nrow(table_a1)) {
    code <- table_a1$Code[i]
    
    if(code != "ZZZ") {
      # As origin
      as_origin <- wb_clean_numeric %>%
        filter(ori_code == code) %>%
        summarise(total = sum(`2000`, na.rm = TRUE)) %>%
        pull(total)
      
      # As destination  
      as_dest <- wb_clean_numeric %>%
        filter(dest_code == code) %>%
        summarise(total = sum(`2000`, na.rm = TRUE)) %>%
        pull(total)
      
      table_a1$As_Origin[i] <- ifelse(length(as_origin) > 0 && !is.na(as_origin), as_origin, 0)
      table_a1$As_Destination[i] <- ifelse(length(as_dest) > 0 && !is.na(as_dest), as_dest, 0)
      table_a1$Total[i] <- table_a1$As_Origin[i] + table_a1$As_Destination[i]
    }
  }
}

table_a1_formatted <- table_a1 %>%
  mutate(
    As_Origin = format(As_Origin, big.mark = ","),
    As_Destination = format(As_Destination, big.mark = ","),
    Total = format(Total, big.mark = ",")
  )

write_xlsx(table_a1_formatted,"~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixII_Excluded_Codes_ISOWB.xlsx")

#Appendix III Countries with Zero Immigration (World Bank)
table_a2 <- zero_immigration %>%
  left_join(
    wb_migration_clean %>%
      group_by(ori_code) %>%
      summarise(
        emigrants = sum(N_od_2000, na.rm = TRUE),
        n_destinations = n_distinct(dest_code)
      ),
    by = c("dest_code" = "ori_code")
  ) %>%
  mutate(
    Country = countrycode(dest_code, "iso3c", "country.name", warn = FALSE),
    emigrants = replace_na(emigrants, 0),
    n_destinations = replace_na(n_destinations, 0),
    Treatment = "Excluded from IEI calculation"
  ) %>%
  arrange(desc(emigrants)) %>% 
  select(
    Code = dest_code,
    Country = dest_country,
    `Immigration (N_d)` = N_d,
    Emigration = emigrants,
    `Destinations Reached` = n_destinations,
    Treatment
  )

table_a2_note <- "Note: These countries have no recorded immigration data (N_d = 0) in the World Bank Global Bilateral Migration Database (2000), preventing weight calculation for the Indirect Exposure Index."

write_xlsx(table_a2, 
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixIII_Zero_Immigration.xlsx")

#Appendix IV Problem Codes Treatment (ISO3 UNPD)
table_a3 <- data.frame(
  UN_Code = c(830, 158),
  ISO3_Code = c(NA, "TWN"),
  Country_Territory = c("Channel Islands", "Taiwan"),
  Treatment = c("Excluded", "Mapped to TWN"),
  Reason = c("British Crown Dependency", 
             "Standard ISO3c code for Taiwan"),
  As_Origin = 0,
  As_Destination = 0,
  Total = 0,
  stringsAsFactors = FALSE
)

if(exists("unpd")) {  
  for(i in 1:nrow(table_a3)) {
    un_code <- table_a3$UN_Code[i]
    
    # As origin
    as_origin <- unpd %>%
      filter(`Location code of destination` == un_code) %>%
      summarise(total = sum(`2000...18`, na.rm = TRUE)) %>%
      pull(total)
    
    # As destination  
    as_dest <- unpd %>%
      filter(`Location code of origin` == un_code) %>%
      summarise(total = sum(`2000...18`, na.rm = TRUE)) %>%
      pull(total)
    
    table_a3$As_Origin[i] <- ifelse(length(as_origin) > 0 && !is.na(as_origin), as_origin, 0)
    table_a3$As_Destination[i] <- ifelse(length(as_dest) > 0 && !is.na(as_dest), as_dest, 0)
    table_a3$Total[i] <- table_a3$As_Origin[i] + table_a3$As_Destination[i]
  }
}
# Format the table for export
table_a3_formatted <- table_a3 %>%
  mutate(
    As_Origin = format(As_Origin, big.mark = ","),
    As_Destination = format(As_Destination, big.mark = ","),
    Total = format(Total, big.mark = ",")
  )

# Export to Excel
write_xlsx(table_a3_formatted,
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixIV_Special_Cases_UNPD.xlsx")

#Appendix V Countries with Zero Immigration (UNPD)
# Identify countries to exclude
countries_to_exclude <- c("SSD") 

table_a4 <- data.frame(
  Code = countries_to_exclude,
  stringsAsFactors = FALSE
)

# Add country names and immigration data
table_a4 <- table_a4 %>%
  left_join(
    N_d_unpd %>%
      select(dest_code, dest_country, N_d),
    by = c("Code" = "dest_code")
  ) %>%
  left_join(
    # Get emigration information
    unpd_final %>%
      group_by(ori_code) %>%
      summarise(
        emigrants = sum(N_od_2000_unpd, na.rm = TRUE),
        n_destinations = n_distinct(dest_code)
      ),
    by = c("Code" = "ori_code")
  ) %>%
  mutate(
    Country = ifelse(is.na(dest_country), 
                     countrycode(Code, "iso3c", "country.name", warn = FALSE),
                     dest_country),
    `Immigration (N_d)` = replace_na(N_d, 0),
    Emigration = replace_na(emigrants, 0),
    `Destinations Reached` = replace_na(n_destinations, 0),
    Treatment = "Excluded from IEI calculation (weight sum ≠ 1)"
  ) %>%
  arrange(desc(Emigration)) %>%
  select(
    Code,
    Country,
    `Immigration (N_d)`,
    Emigration,
    `Destinations Reached`,
    Treatment
  )

table_a4_formatted <- table_a4 %>%
  mutate(
    `Immigration (N_d)` = format(`Immigration (N_d)`, big.mark = ","),
    Emigration = format(Emigration, big.mark = ",")
  )

table_a4_note <- "Note: These countries were excluded from the Indirect Exposure Index calculation due to weight summation issues (weights do not sum to 1) in the UNPD International Migrant Stock database (2000). South Sudan (SSD) appears due to its independence in 2011, occurring after the 2000 data collection period."

# Export to Excel
write_xlsx(table_a4_formatted,
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixV_Zero Immigration_UNPD.xlsx")

#Appendix VI
table_ndgain_wb <- ndgain_mapping %>%
  arrange(mapping_type, territory_code) %>%
  mutate(
    `Territory Code` = territory_code,
    `Territory Name` = case_when(
      territory_code == "GLP" ~ "Guadeloupe",
      territory_code == "MTQ" ~ "Martinique",
      territory_code == "GUF" ~ "French Guiana",
      territory_code == "REU" ~ "Réunion",
      territory_code == "NCL" ~ "New Caledonia",
      territory_code == "PYF" ~ "French Polynesia",
      territory_code == "SPM" ~ "Saint Pierre and Miquelon",
      territory_code == "MYT" ~ "Mayotte",
      territory_code == "WLF" ~ "Wallis and Futuna",
      territory_code == "PRI" ~ "Puerto Rico",
      territory_code == "VIR" ~ "US Virgin Islands",
      territory_code == "GUM" ~ "Guam",
      territory_code == "ASM" ~ "American Samoa",
      territory_code == "MNP" ~ "Northern Mariana Islands",
      territory_code == "GIB" ~ "Gibraltar",
      territory_code == "BMU" ~ "Bermuda",
      territory_code == "CYM" ~ "Cayman Islands",
      territory_code == "TCA" ~ "Turks and Caicos",
      territory_code == "VGB" ~ "British Virgin Islands",
      territory_code == "AIA" ~ "Anguilla",
      territory_code == "MSR" ~ "Montserrat",
      territory_code == "FLK" ~ "Falkland Islands",
      territory_code == "SHN" ~ "Saint Helena",
      territory_code == "IMN" ~ "Isle of Man",
      territory_code == "ABW" ~ "Aruba",
      territory_code == "GRL" ~ "Greenland",
      territory_code == "FRO" ~ "Faroe Islands",
      territory_code == "HKG" ~ "Hong Kong",
      territory_code == "MAC" ~ "Macao",
      territory_code == "TWN" ~ "Taiwan",
      territory_code == "NFK" ~ "Norfolk Island",
      territory_code == "COK" ~ "Cook Islands",
      territory_code == "NIU" ~ "Niue",
      territory_code == "TKL" ~ "Tokelau",
      territory_code == "LIE" ~ "Liechtenstein",
      territory_code == "PSE" ~ "Palestinian Territories",
      territory_code == "AND" ~ "Andorra",
      territory_code == "KNA" ~ "St. Kitts & Nevis",
      territory_code == "MCO" ~ "Monaco",
      territory_code == "SMR" ~ "San Marino",
      TRUE ~ territory_code
    ),
    `Mapped to Code` = sovereign_code,
    `Mapped to Country` = countrycode(sovereign_code, "iso3c", "country.name", warn = FALSE),
    `Mapping Type` = case_when(
      mapping_type == "sovereign" ~ "Sovereign State",
      mapping_type == "geographic" ~ "Geographic Proximity",
      TRUE ~ mapping_type
    ),
    Reason = reason
  ) %>%
  select(`Territory Code`, `Territory Name`, `Mapped to Code`, 
         `Mapped to Country`, `Mapping Type`, Reason)

# Export World Bank ND-GAIN mapping table
write_xlsx(table_ndgain_wb,
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixVI_NDGAIN_Mapping_WorldBank.xlsx")

#Appendix VII
table_ndgain_unpd <- vulnerability_mapping %>%
  mutate(
    `Territory Code` = missing_code,
    `Territory Name` = entity_name,
    `Mapped to Code` = map_to_code,
    `Mapped to Country` = sovereign,
    `Mapping Type` = "Sovereign State",
    Reason = reason
  ) %>%
  select(`Territory Code`, `Territory Name`, `Mapped to Code`, 
         `Mapped to Country`, `Mapping Type`, Reason)

# Export UNPD ND-GAIN mapping table
write_xlsx(table_ndgain_unpd,
           "~/Desktop/Data Collection/Dissertation Result/Figures and Tables/AppendixVII_NDGAIN_Mapping_UNPD.xlsx")
