## Rieke ETL
## February 2026
## Dave Moyer

library(tidyverse)
library(readxl)
library(stringr)
library(sf)
library(googlesheets4)
library(ggmap)
library(janitor)
library(here)

# notes ####
# most files: https://www.oregon.gov/ode/schools-and-districts/reportcards/reportcards/Pages/Accountability-Measures.aspx

# directory ####
#https://www.ode.state.or.us/instID/

directory_raw <- read_csv('raw/directory/Institution_Search_Results.csv')

directory <- directory_raw %>%
  filter(`Inst Type` == 'Oregon Public School' & Class == 'School') %>%
  select(district_id = `Parent ID`,
         district_name = `Parent Inst`,
         school_id = Institution_ID,
         school_name = Directory_Name,
         address = Street_StrAddr1,
         city = Street_City,
         state = Street_State,
         zip = Street_Zip,
         county = County) %>%
  mutate(full_address = paste0(address," ",city,", ",state," ",zip))

gmap_key <- readLines('C:/Users/moyer/work/creds/gmap-key.txt')

register_google(gmap_key)

# directory_mapped <- directory %>%
#    mutate_geocode(full_address) 
# write_csv(directory_mapped, 'prc/dir-mapped-temp.csv')

directory_mapped <- read_csv('prc/dir-mapped-temp.csv')

directory_final <- directory_mapped %>%
  mutate(
    lon = case_when(
      full_address == '6th & Esther Ave Imbler, OR 97841' ~ -117.96489362513624, 
      full_address == '1700 California Klamath Falls, OR 97601' ~ -121.79583425902996,
      full_address == '10th & Ingersoll Coos Bay, OR 97420' ~ -124.221804,
      full_address == 'Fifth & Park Sts Riddle, OR 97469' ~ -123.3620615905213,
      full_address == '737 Succor Creek Rd Jordan Valley, OR 97910' ~ -117.10827545365018,
      T ~ lon
    ),
    lat  = case_when(
      full_address == '6th & Esther Ave Imbler, OR 97841' ~ 45.46188021205186,
      full_address == '1700 California Klamath Falls, OR 97601' ~ 42.23482039511676, 
      full_address == '10th & Ingersoll Coos Bay, OR 97420' ~ 43.360233, 
      full_address == 'Fifth & Park Sts Riddle, OR 97469' ~ 42.954195199399734, 
      full_address == '737 Succor Creek Rd Jordan Valley, OR 97910' ~ 43.31663036407091, 
      T ~ lat
    )
  )

write_csv(directory_final, 'prc/directory.csv', na = "")

# enroll ####

enroll_files <- list.files("raw/enroll", 
                           pattern = "*.xls*",
                           full.names = TRUE)
  
enroll_sheets <- lapply(enroll_files,excel_sheets)

names(enroll_sheets) <- basename(enroll_files)

enroll_sheets_filtered <- lapply(
  enroll_sheets,
  function(x) x[str_detect(x, "School|District")]
)

enroll_raw <- map2(
  enroll_files,
  enroll_sheets_filtered,
  function(file, sheets) {
    sheets %>%
      map(function(sheet_name) {
        read_excel(file, sheet = sheet_name, col_types = "text") %>%
          clean_names() %>%
          mutate(
            source_file  = basename(file),
            source_sheet = sheet_name
          )
      }) %>%
      set_names(paste0(tools::file_path_sans_ext(basename(file)), "_", sheets))
  }
) %>%
  list_flatten()

enroll_combined <- enroll_raw %>%
  map(~ .x %>%
        rename(
          district_id = any_of(c("attending_district_institutional_id","attending_district_institution_id","district_institution_id")),
          school_id = any_of(c("attending_school_institutional_id","attending_school_institution_id","school_institution_id","institution_id")),
          district_name = any_of("district"),
          school_name = any_of(c("school","institution_name"))
        ) %>%
        select(-any_of(c('county','report_year'))) %>%
        pivot_longer(
          cols = -any_of(c("district_id", "district_name", "school_id", "school_name","source_file", "source_sheet")),
          names_to = "variable",
          values_to = "value"
        )
  ) %>%
  bind_rows() %>%
  relocate(c(school_id, school_name), .after = district_name)


enroll <- enroll_combined %>%
  filter(!value %in% c("-","*")) %>%
  mutate(
    value_reported = value,
    value = as.numeric(value),
    value_reported = ifelse(is.na(value), value_reported, NA_character_),
    season = case_when(
      str_detect(source_file,'fall') ~ 'fall',
      str_detect(source_file,'spring') ~ 'spring'
  ),
  school_year = case_when(
    str_detect(source_file,'20182019') ~ 2019,
    str_detect(source_file,'20212022') ~ 2022,
    str_detect(source_file,'20222023') ~ 2023,
    str_detect(source_file,'20232024') ~ 2024,
    str_detect(source_file,'20242025') ~ 2025,
    str_detect(source_file,'20252026') ~ 2026
  ),
  report_year = case_when(
    str_detect(variable,'2017_18') ~ 2018,
    str_detect(variable,'2018_19') ~ 2019,
    str_detect(variable,'2020_21') ~ 2021,
    str_detect(variable,'2021_22|2021_2022') ~ 2022,
    str_detect(variable,'2022_23|2022_2023') ~ 2023,
    str_detect(variable,'2023_24|2023_2024') ~ 2024,
    str_detect(variable,'2024_25|2024_2025') ~ 2025,
    str_detect(variable,'2025_26|2025_2026') ~ 2026
  ),
  level = case_when(
    str_detect(source_sheet,'(?i)school') ~ 'school',
    str_detect(source_sheet, '(?i)district') ~ 'district'
  ),
  grade = case_when(
    str_detect(variable,'kinder') ~ 'k',
    str_detect(variable,'grade_one') ~ '1',
    str_detect(variable,'grade_two') ~ '2',
    str_detect(variable,'grade_three') ~ '3',
    str_detect(variable,'grade_four') ~ '4',
    str_detect(variable,'grade_five') ~ '5',
    str_detect(variable,'grade_six') ~ '6',
    str_detect(variable,'grade_seven') ~ '7',
    str_detect(variable,'grade_eight') ~ '8',
    str_detect(variable,'grade_nine') ~ '9',
    str_detect(variable,'grade_ten') ~ '10',
    str_detect(variable,'grade_eleven') ~ '11',
    str_detect(variable,'grade_twelve') ~ '12',
    T ~ 'all'
  ),
  variable = str_remove(variable, "^x\\d+_\\d+_"),
  variable = case_when(
    variable == 'x2022_23multi_racial' ~ 'multi_racial',
    str_detect(variable, 'grade_|kinder') ~ 'all',
    T ~ variable),
  variable = case_match(
    variable,
    
    'total_number_of_students' ~ 'all',
    'total_enrollment' ~ 'all',
    'total_students' ~ 'all',
    
    'number_of_economically_disadvantaged_students' ~ 'ct_eco_dis',
    'students_experiencing_poverty' ~ 'ct_eco_dis',
    'percentage_of_economically_disadvantaged_students' ~ 'pct_eco_dis',
    'percentage_students_experiencing_poverty' ~ 'pct_eco_dis',
    'percentage_of_students_experiencing_poverty' ~ 'pct_eco_dis',
    
    'special_education_students' ~ 'ct_swd',
    'students_with_disabilities' ~ 'ct_swd',
    'percentage_of_students_with_disabilities' ~ 'pct_swd',
    'percentage_special_education_students' ~ 'pct_swd',
    
    'ever_an_english_learner' ~ 'ct_ever_el',
    'number_of_ever_an_english_learners' ~ 'ct_ever_el',
    'percentage_ever_an_english_learner' ~ 'pct_ever_el',
    'percentage_of_ever_english_learners'~ 'pct_ever_el',
    
    'asian' ~ 'ct_asian',
    'asian_students' ~ 'ct_asian',
    'number_of_asian_sutdents' ~ 'ct_asian',
    'number_of_asian_students' ~ 'ct_asian',
    'percent_asian' ~ 'pct_asian',
    'percentage_asian_students' ~ 'pct_asian',
    'percentage_of_asian_students' ~ 'pct_asian',
    
    'american_indian_alaska_native' ~ 'ct_aian',
    'american_indian_alaska_native_students' ~ 'ct_aian',
    'number_of_american_indian_alaskan_native_students' ~ 'ct_aian',
    'percent_american_indian_alaska_native' ~ 'pct_aian',
    'percentage_american_indian_alaska_native_students' ~ 'pct_aian',
    'percentage_of_american_indian_alaskan_native_students'~ 'pct_aian',
    
    'black_african_american' ~ 'ct_black',
    'black_african_american_students' ~ 'ct_black',
    'number_of_black_african_american_students'~ 'ct_black',
    'percent_black_african_american' ~ 'pct_black',
    'percentage_black_african_american_students' ~ 'pct_black',
    'percentage_of_black_african_american_students' ~ 'pct_black',
    
    'hispanic_latino' ~ 'ct_hispanic',
    'hispanic_latino_students' ~ 'ct_hispanic',
    'number_of_hispanci_latino_students' ~ 'ct_hispanic',
    'number_of_hispanic_latino_students' ~ 'ct_hispanic',
    'percent_hispanic_latino' ~ 'pct_hispanic',
    'percentage_hispanic_latino_students' ~ 'pct_hispanic',
    'percentage_of_hispanic_latino_students' ~ 'pct_hispanic',
    
    'multiracial' ~ 'ct_multi',
    'multi_racial_students' ~ 'ct_multi',
    'number_of_multi_racial_students' ~ 'ct_multi',
    'percent_multi_racial' ~ 'pct_multi',
    'percent_multiracial' ~ 'pct_multi',
    'percentage_multi_racial_students' ~ 'pct_multi',
    'percent_multi_racial_students' ~ 'pct_multi',
    'percentage_of_multi_racial_students'~ 'pct_multi',
    
    'native_hawaiian_pacific_islander' ~ 'ct_nhpi',
    'native_hawaiian_pacific_islander_students' ~ 'ct_nhpi',
    'number_of_native_hawaiian_pacific_islander_students' ~ 'ct_nhpi',
    'percent_native_hawaiian_pacific_islander' ~ 'pct_nhpi',
    'percentage_native_hawaiian_pacific_islanders_students' ~ 'pct_nhpi',
    'percentage_of_native_hawaiian_pacific_islander_students' ~ 'pct_nhpi',
    
    'white' ~ 'ct_white',
    'white_students' ~ 'ct_white',
    'number_of_white_students' ~ 'ct_white',
    'percent_white' ~ 'pct_white',
    'percentage_white_students' ~ 'pct_white',
    'percentage_of_white_students' ~  'pct_white',
    
    .default = variable)
  ) %>%
  filter((report_year == school_year | is.na(report_year)),
         (str_detect(variable,'ct_') | variable %in% c('all'))) %>%
  select(-source_file,-source_sheet,-report_year) %>%
  mutate(student_group = case_when(
    str_detect(variable,'all') ~ 'all',
    T ~ str_extract(variable, '[^_]+$')
  ),
  var = case_when(
    str_detect(variable,'all') ~ 'ct',
    T~ str_extract(variable, '^[^_]+'))
  ) %>%
  select(-variable) %>%
  distinct() %>%
  pivot_wider(names_from = c(season,var), values_from = c(value, value_reported)) %>%
  rename_with(.cols = everything(), ~gsub('value_',"", .x)) %>%
  mutate(fall_pct = round(100*fall_pct,1)) %>%
  group_by(school_id,district_name) %>%
  fill(district_id, .direction = 'down') %>%
  select(-reported_spring_pct,-reported_fall_pct)

write_csv(enroll, 'prc/enroll.csv', na = '')

# filter groups ####

exclude <- c('non_binary','recent_arrivers','tag','combined_dis',
             'ever_el','military','migrant','homeless','underserved_race',
             'extended_assess','incarcerated','foster_care','swd_accom',
             'indian_ed')
  
# attend ####

attend_files <- list.files("raw/attend",
                           pattern = "*.xls*",
                           full.names = TRUE)

attend_raw <- map(attend_files, function(file) {
  sheets <- excel_sheets(file)
  data_sheet <- sheets[!str_detect(sheets, "(?i)note|definition")]
  read_excel(file, sheet = data_sheet[1], col_types = "text") %>%
    clean_names() %>%
    mutate(source_file = basename(file))
}) %>%
  bind_rows()

attend_temp <- attend_raw %>%
  rename(
    district_name = any_of("district"),
    school_id     = any_of("institution_id"),
    school_name   = any_of("institution"),
    inst_type     = any_of("institution_type"),
    n_regular     = any_of("number_regular_attenders"),
    pct_regular   = any_of("percent_regular_attenders"),
    n_absent      = any_of("number_chronically_absent"),
    pct_absent    = any_of("percent_chronically_absent")
  ) %>%
  mutate(
    school_year = case_when(
      str_detect(source_file, "1819") ~ 2019L,
      str_detect(source_file, "2122") ~ 2022L,
      str_detect(source_file, "2223") ~ 2023L,
      str_detect(source_file, "2324") ~ 2024L,
      str_detect(source_file, "2425") ~ 2025L
    ),
    level = case_when(
      inst_type == "State"    ~ "state",
      inst_type == "District" ~ "district",
      TRUE                    ~ "school"
    ),
    student_group = case_match(
      student_group,
      "All Students"                            ~ "all",
      "Asian"                                   ~ "asian",
      "Black/African American"                  ~ "black",
      "Hispanic/Latino"                         ~ "hispanic",
      "American Indian/Alaska Native"           ~ "aian",
      "Multi-Racial"                            ~ "multi",
      "Native Hawaiian/Pacific Islander"        ~ "nhpi",
      "White"                                   ~ "white",
      c("Economically Disadvantaged",
        "Students Experiencing Poverty")        ~ "eco_dis",
      c("English Learner", "English Learners")  ~ "el",
      "Ever English Learners"                   ~ "ever_el",
      "Students with Disabilities"              ~ "swd",
      "Talented and Gifted"                     ~ "tag",
      "Homeless"                                ~ "homeless",
      "Migrant"                                 ~ "migrant",
      "Female"                                  ~ "female",
      "Male"                                    ~ "male",
      "Non-Binary"                              ~ "non_binary",
      "Foster Care"                             ~ "foster_care",
      "Underserved Races/Ethnicities"           ~ "underserved_race",
      "Combined Disadvantaged"                  ~ "combined_dis",
      "Military Connected"                      ~ "military",
      "Recent Arrivers"                         ~ "recent_arrivers",
      "Currently or Formerly Incarcerated"      ~ "incarcerated",
      .default = str_to_lower(str_replace_all(student_group, "[ /]", "_"))
    ),
    grade = case_when(
      str_detect(student_group,'grade_') ~ gsub('grade_','',student_group),
      student_group == 'kindergarten' ~ 'k',
      T ~ 'all'
    ),
    student_group = ifelse(grade != 'all','all',student_group),
    n_regular   = as.numeric(n_regular),
    pct_regular = as.numeric(pct_regular),
    n_absent    = as.numeric(n_absent),
    pct_absent  = as.numeric(pct_absent)
  ) %>%
  select(-source_file, -any_of(c("report_year", "students_included", "inst_type"))) %>%
  filter(!(student_group %in% exclude)) %>%
  relocate(school_year,.before = district_id) %>%
  relocate(c(grade,student_group), .after = school_name)

district_attend <- attend_temp %>% filter(level == "district")
school_attend   <- attend_temp %>% filter(level == "school")


# tests ####

test_files <- list.files("raw/tests",
                         pattern = "*.xls*",
                         full.names = TRUE)

tests_raw <- map(test_files, function(file) {
  sheets <- excel_sheets(file)
  data_sheet <- sheets[!str_detect(sheets, "(?i)definition|note")]
  read_excel(file, sheet = data_sheet[1], col_types = "text") %>%
    clean_names() %>%
    mutate(source_file = basename(file))
}) %>%
  bind_rows()

tests_temp <- tests_raw %>%
  mutate(pct_proficient = coalesce(percent_proficient_level_3_or_4,percent_proficient)) %>%
  select(-percent_proficient,-percent_proficient_level_3_or_4) %>%
  rename(
    district_name      = any_of("district"),
    school_name        = any_of("school"),
    n_proficient       = any_of("number_proficient"),
    n_level_4          = any_of("number_level_4"),
    pct_level_4        = any_of("percent_level_4"),
    n_level_3          = any_of("number_level_3"),
    pct_level_3        = any_of("percent_level_3"),
    n_level_2          = any_of("number_level_2"),
    pct_level_2        = any_of("percent_level_2"),
    n_level_1          = any_of("number_level_1"),
    pct_level_1        = any_of("percent_level_1"),
    n_tested           = any_of("number_of_participants")
  ) %>%
  mutate(
    school_year = case_when(
      str_detect(source_file, "1819") ~ 2019L,
      str_detect(source_file, "2122") ~ 2022L,
      str_detect(source_file, "2223") ~ 2023L,
      str_detect(source_file, "2324") ~ 2024L,
      str_detect(source_file, "2425") ~ 2025L
    ),
    subject = case_match(
      str_to_lower(subject),
      "english language arts" ~ "ela",
      "mathematics"           ~ "math",
      "science"               ~ "science",
      .default = str_to_lower(subject)
    ),
    student_group = case_match(
      student_group,
      "Total Population (All Students)"     ~ "all",
      "Asian"                               ~ "asian",
      "Black/African American"              ~ "black",
      "Hispanic/Latino"                     ~ "hispanic",
      "American Indian/Alaskan Native"      ~ "aian",
      "Multi-Racial"                        ~ "multi",
      "Pacific Islander"                    ~ "nhpi",
      "White"                               ~ "white",
      c("Econo. Disadvantaged",
        "Students Experiencing Poverty")    ~ "eco_dis",
      "Students Not Experiencing Poverty"   ~ "no_eco_dis",
      c("LEP", "English Learners",
        "Current English Learners")         ~ "el",
      "Students with Disabilities (SWD)"    ~ "swd",
      "SWD with Accommodations"             ~ "swd_accom",
      "Students without Disabilities"       ~ "no_swd",
      "Talented and Gifted (TAG)"           ~ "tag",
      "Homeless"                            ~ "homeless",
      "Migrant Education"                   ~ "migrant",
      "Female"                              ~ "female",
      "Male"                                ~ "male",
      "Non-Binary"                          ~ "non_binary",
      "Students in Foster Care"             ~ "foster_care",
      c("Military-connected",
        "Military-Connected")               ~ "military",
      "Recent Arrivers"                     ~ "recent_arrivers",
      "Currently/Formerly Incarcerated"     ~ "incarcerated",
      "Indian Education"                    ~ "indian_ed",
      "Extended Assessment"                 ~ "extended_assess",
      .default = student_group
    ),
    grade = case_match(
      grade_level,
      "All Grades" ~ "all",
      "Grade HS (11)" ~ '11',
      .default = str_remove(grade_level, "^Grade ")
    ),
    across(c(n_proficient, pct_proficient,
             n_level_4, pct_level_4,
             n_level_3, pct_level_3,
             n_level_2, pct_level_2,
             n_level_1, pct_level_1,
             n_tested, participation_rate),
           as.numeric)
  ) %>%
  select(-source_file, -any_of("academic_year"),-grade_level)

tests <- tests_temp %>%
  filter(!(student_group %in% exclude)) %>%
  filter((grade == 'all' | (grade != 'all' & student_group == 'all'))) %>%
  relocate(school_year,.before = district_id) %>%
  relocate(c(subject,grade,student_group), .after = school_name) %>%
  distinct()

write_csv(tests,'prc/full_tests.csv',na = '')

tests_wide <- tests %>%
  select(school_year,
         district_id,
         district_name,
         school_id,
         school_name,
         grade,
         student_group,
         subject,
         pct_proficient,
         pct_level_4,
         pct_level_3,
         pct_level_2,
         pct_level_1,
         n_tested,
         participation_rate
         ) %>%
  pivot_wider(names_from = subject, values_from = c(starts_with('pct'),n_tested), values_fn = {max}) ## a couple places where the participation rate varies

dups <- tests  %>% 
  mutate(n = dplyr::n(), .by = c(school_year, district_id, district_name, school_id, school_name, grade, student_group,subject)) |>
  filter(n > 1L) %>%
  arrange(school_year, district_id, district_name, school_id, school_name, grade, student_group,subject)


# funding ####
funding_files <- list.files("raw/funding",
                         pattern = "*.xls*",
                         full.names = TRUE)

funding_csvs <- list.files('raw/funding',
                           pattern = "*.csv",
                           full.names = TRUE)

funding_raw <- bind_rows(
  map(funding_files, function(file) {
    sheets <- excel_sheets(file)
    data_sheet <- sheets[!str_detect(sheets, "(?i)definition|note")]
    read_excel(file, sheet = data_sheet[1], col_types = "text") |>
      clean_names() |>
      mutate(source_file = basename(file))
  }),
  map(funding_csvs, function(file) {
    read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      clean_names() |>
      mutate(source_file = basename(file))
  })
)

funding <- funding_raw %>%
  mutate(
    school_year = case_when(
      str_detect(school_yr, "19-20") ~ 2020L,
      str_detect(school_yr, "20-21") ~ 2021L,
      str_detect(school_yr, "21-22") ~ 2022L,
      str_detect(school_yr, "22-23") ~ 2023L,
      str_detect(school_yr, "23-24") ~ 2024L,
      str_detect(school_yr, "24-25") ~ 2025L
    )) %>%
  select(school_year,
         district_id,
         district_name,
         school_id,
         school_name,
         total_exp = total_expenditures,
         per_pupil_exp,
         fed_per_pupil_exp,
         state_local_per_pupil_exp) %>%
  mutate(across(contains("exp"), ~round(as.numeric(.x))))

# combine ####
analysis <- school_attend %>%
  select(-level) %>%
  left_join(tests_wide, by = c('district_id', 'school_id','school_year','grade','student_group'), suffix = c("","_1")) %>%
  left_join(funding, by = c('district_id','school_id','school_year'), suffix = c("","_2")) %>%
  select(-school_name_1,-district_name_1,-school_name_2,-district_name_2)

write_csv(analysis, 'prc/attend_tests_funding.csv', na = "")  

    