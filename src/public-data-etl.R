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

directory_updated <- directory_mapped %>%
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

## manual pps data ####
gs4_auth(email = "moyer.david@gmail.com")

katie_pps_sheet <- as_sheets_id('https://docs.google.com/spreadsheets/d/179Kbm1LIO52magk27T2dGTJVfiBLQmF-89m_xO9yUDk/edit?gid=0#gid=0')

manual_pps <- read_sheet(ss = katie_pps_sheet, col_types = 'c')

manual <- manual_pps %>%
  janitor::clean_names() %>%
  select(
    school_name        = school,
    area,
    capacity           = building_capacity,
    predicted_enroll_27 = x26_27_predicted_enrollment,
    alternative_focus,
    building_seismic_rating,
    blended_classrooms
  ) %>%
  mutate(
    across(c(capacity, predicted_enroll_27)),
    school_name = str_squish(school_name)
  ) %>%
  left_join(
    directory %>%
      filter(district_name == 'Portland SD 1J') %>%
      select(school_id, school_name) %>%
      mutate(school_name_short = gsub(' Elem| High | Middle | Elementary School| School', '', school_name)),
    by = c('school_name' = 'school_name_short')
  ) %>%
  mutate(school_id = case_when(
    school_name == 'Odyssey'                  ~ NA_integer_,
    school_name == 'Metropolitan Learning Center' ~ 916,
    school_name == 'Alliance'                 ~ 4507,
    school_name == 'MLK Jr.'                  ~ 866,
    school_name == 'George'                   ~ 849,
    school_name == 'Jefferson'                ~ 913,
    school_name == 'Beaumont'                 ~ 831,
    school_name == 'Mt. Tabor'                ~ 877,
    school_name == 'Cesar Chavez'             ~ 841,
    school_name == 'Lane'                     ~ 1243,
    school_name == 'da Vinci'                 ~ 1363,
    school_name == 'Gray'                     ~ 852,
    school_name == 'Ockley Green'             ~ 878,
    school_name == 'Harriet Tubman'           ~ 894,
    school_name == 'Kellogg'                  ~ 863,
    school_name == 'Hosford'                  ~ 858,
    school_name == 'Sellwood'                 ~ 888,
    school_name == 'West Sylvan'              ~ 898,
    school_name == 'Benson'                   ~ 906,
    school_name == 'Roosevelt'                ~ 918,
    school_name == 'Cleveland'                ~ 909,
    school_name == 'Ida B. Wells'             ~ 922,
    school_name == 'McDaniel'                 ~ 915,
    school_name == 'Lincoln'                  ~ 914,
    school_name == 'Franklin'                 ~ 911,
    school_name == 'Grant'                    ~ 912,
    .default = school_id
  ))

directory_final <- directory_updated %>%
  left_join(manual %>%
              select(-school_name,-school_name.y),
            by = 'school_id')

## write directory data ####

write_csv(directory_final, 'prc/directory.csv', na = "")

# enroll ####

## state ####
enroll_files <- list.files("raw/enroll", 
                           pattern = "(fall|spring).*\\.xlsx?$",
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

## pps ####
enroll_pps_raw <- read_excel('raw/enroll/SchoolProfiles-October_2025_Enrollment_Summary10yrdetail.xlsx',
                             range = "A2:M144", col_types = 'text')

enroll_pps <- enroll_pps_raw %>%
  select(-2) %>%
  clean_names() %>%
  filter(!(x2025_26 %in% c('Closed','Merged'))) %>%
  rename(school_short = school,
         grades_served = grade) %>%
  pivot_longer(cols = starts_with('x'), names_to = 'school_year', values_to = 'fall_ct_pps') %>%
  mutate(grades_served = gsub('G','',grades_served),
         school_year = as.numeric(paste0("20",str_sub(school_year,-2,-1)))) %>%
  filter(!is.na(fall_ct_pps)) %>%
  left_join(
    directory %>%
      filter(district_name == 'Portland SD 1J') %>%
      select(school_id, school_name) %>%
      mutate(school_name_short = gsub(' Elem| High | Middle | Elementary School| School| High School', '', school_name)),
    by = c('school_short' = 'school_name_short')
  ) %>%
  mutate(school_id = case_when(
    school_short == 'Odyssey'                  ~ NA_integer_,
    school_short == 'Metropolitan Learning Center' ~ 916,
    str_detect(school_short, 'Boise-Eliot') ~ 833,
    school_short == 'Alliance'                 ~ 4507,
    school_short == 'MLK Jr'                  ~ 866,
    school_short == 'George'                   ~ 849,
    school_short == 'Jefferson'                ~ 913,
    school_short == 'Beaumont'                 ~ 831,
    school_short == 'Mt. Tabor'                ~ 877,
    school_short == 'César Chávez'       ~ 841,
    school_short == 'Lane'                     ~ 1243,
    school_short == 'da Vinci'                 ~ 1363,
    school_short == 'Gray'                     ~ 852,
    school_short == 'Ockley Green'             ~ 878,
    school_short == 'Harriet Tubman'           ~ 894,
    school_short == 'Kellogg'                  ~ 863,
    school_short == 'Hosford'                  ~ 858,
    school_short == 'Sellwood'                 ~ 888,
    school_short == 'West Sylvan'              ~ 898,
    school_short == 'Benson'                   ~ 906,
    school_short == 'Roosevelt'                ~ 918,
    school_short == 'Cleveland'                ~ 909,
    school_short == 'Ida B. Wells'             ~ 922,
    school_short == 'McDaniel'                 ~ 915,
    school_short == 'Lincoln'                  ~ 914,
    school_short == 'Franklin'                 ~ 911,
    school_short == 'Grant'                    ~ 912,
    .default = school_id
  ),
  district_id = '2180',
  district_name = 'Portland SD 1J',
  school_id = as.character(school_id),
  grade = 'all',
  student_group = 'all',
  level = 'school') %>%
  select(-school_short) %>% 
  filter(is.na(school_id))

enroll_pps_group_files <- list.files("raw/enroll-pps", 
                                     pattern = "(nderserv).*\\.xlsx?$",
                                     full.names = TRUE)

enroll_pps_group_raw <- lapply(enroll_pps_group_files, function(f) {
  read_excel(f) %>%
    clean_names() %>%
    rename(school_short = x1) %>%
    filter(!is.na(school_short) & school_short != 'Name' & !is.na(enroll) & 
             str_detect(school_short, 'Total', negate = TRUE))
}) %>%
  setNames(enroll_pps_group_files) %>%
  bind_rows(.id = 'src')

enroll_pps_group <- enroll_pps_group_raw %>%
  select(
    src,
    school_short,
    all = enroll,
    combined_underserved,
    direct_cert          = direct_certification,
    lep,
    lep2 = multilingual_learner_lep,
    sped                 = sp_ed,
    historically_underserved,
    latino,
    black,
    multi                = multi_race
  ) %>%
  mutate(
    school_year = case_when(
      str_detect(src, "\\d{2}-\\d{2}\\.xlsx") ~
        2000L + as.integer(str_extract(src, "(?<=-)\\d{2}(?=\\.xlsx)")),
      str_detect(src, "oct25") ~ 2025L,
      TRUE ~ 2000L + as.integer(str_extract(src, "\\d{2}(?=\\.xlsx)"))
    )
  ) %>%
  select(-src) %>%
  left_join(
    directory %>%
      filter(district_name == "Portland SD 1J") %>%
      select(school_id, school_name) %>%
      mutate(school_name_short = gsub(
        " Elem| High | Middle | Elementary School| School| High School",
        "", school_name
      )),
    by = c("school_short" = "school_name_short")
  ) %>%
  mutate(
    school_id = case_when(
      school_short == "Odyssey"                      ~ NA_integer_,
      school_short %in% c("Metropolitan Learning Center",
                          "Metro. Learning Center")  ~ 916L,
      school_short == "Metropolitan Learning Center" ~ 916L,
      str_detect(school_short, "Boise-Eliot")        ~ 833L,
      school_short == "Alliance"                     ~ 4507L,
      str_detect(school_short, "MLK")                ~ 866L,
      school_short == "George"                       ~ 849L,
      school_short == "Jefferson"                    ~ 913L,
      str_detect(school_short, "Jackson")            ~ 1277L,
      school_short == "Beaumont"                     ~ 831L,
      str_detect(school_short, "Tabor")              ~ 877L,
      school_short == "César Chávez"                 ~ 841L,
      school_short == "Lane"                         ~ 1243L,
      school_short == "da Vinci"                     ~ 1363L,
      school_short == "Gray"                         ~ 852L,
      school_short == "Ockley Green"                 ~ 878L,
      school_short == "Harriet Tubman"               ~ 894L,
      school_short == "Kellogg"                      ~ 863L,
      school_short == "Hosford"                      ~ 858L,
      school_short == "Sellwood"                     ~ 888L,
      school_short == "West Sylvan"                  ~ 898L,
      school_short == "Benson"                       ~ 906L,
      school_short == "Roosevelt"                    ~ 918L,
      school_short == "Cleveland"                    ~ 909L,
      school_short %in% c("Ida B. Wells", "Wilson",
                          'Ida B. Wells-Barnett')  ~ 922L,
      school_short %in%  c("McDaniel","Leodis V. McDaniel")  ~ 915L,
      school_short == "Lincoln" ~ 914L,
      school_short == "Franklin" ~ 911L,
      school_short == "Grant" ~ 912L,
      str_detect(school_short, "Bridger")   ~ 834L,
      school_short %in% c('Creative Science') ~ 4640L,
      str_detect(school_short, 'NAYA') ~ 4587,
      str_detect(school_short, 'Le Monde') ~ 5060,
      str_detect(school_short, 'Portland Village') ~ 4534,
      str_detect(school_short, 'Trillium') ~ 3616,
      str_detect(school_short, 'Opal') ~ 3451,
      str_detect(school_short, 'Madison') ~ 915,
      str_detect(school_short, 'Emerson') ~ 3991,
      str_detect(school_short, 'Rosemary Anderson') ~ 5877,
      str_detect(school_short, 'Mt Scott') ~ 1803,
      .default = school_id
    ),
    district_id   = "2180",
    district_name = "Portland SD 1J",
    school_id     = as.character(school_id),
    lep = coalesce(lep,lep2),
  ) %>%
  relocate(c(school_year,district_id, district_name,school_id, school_name), .before = school_short) %>%
  select(-school_name,-lep2) %>%
  mutate(across(c(all, combined_underserved:multi), as.numeric)) %>%
  mutate(across(c(combined_underserved:multi),
                list(pct = ~round(100 * (.x / all), 1)),
                .names = "pct_{.col}")) %>%
  mutate(pct_all = 100) %>%  # temp rename so all columns share the pct_/raw_ pattern
  pivot_longer(
    cols      = c(all, combined_underserved:multi, starts_with("pct_")),
    names_to  = c("student_group")
  ) %>%
  mutate(var = ifelse(str_detect(student_group,'pct_'), 'pct','ct'),
         student_group = gsub("pct_","",student_group)) %>%
  pivot_wider(names_from = var)

write_csv(enroll_pps_group, 'prc/enroll-pps.csv')

## combine ####
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
    str_detect(source_file,'20252026') ~ 202
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
    str_detect(source_sheet, '(?i)district') ~ 'district',
    is.na(school_id) ~ 'district',
    school_id == district_id ~ 'district',
    school_name == district_name ~ 'district',
    !is.na(school_id) & school_id != district_id ~ 'school',
    T ~ 'school'
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
  fill(c(district_id,district_name), .direction = 'downup') %>%
  mutate(district_id = ifelse(is.na(district_id) & level == 'district', school_id,district_id),
         district_name = ifelse(is.na(district_name) & level == 'district', school_name,district_name)) %>%
  select(-reported_spring_pct,-reported_fall_pct) %>%
  distinct() %>%
  left_join(enroll_pps, by = c('school_year','district_id','district_name','school_id', 'student_group', 'level', 'grade'))

write_csv(enroll, 'prc/enroll.csv', na = '')

# grades served ####
grades_served <- enroll %>%
  filter(level == 'school' & grade != 'all' & student_group == 'all' & fall_ct > 0) %>%
  select(district_id,
         district_name,
         school_id,
         school_name,
         school_year,
         grade,
         fall_ct) %>%
  mutate(grade_num = case_when(
    grade == 'k' ~ 0,
    T ~ as.numeric(grade)
  )) %>%
  group_by(district_id,district_name,school_id,school_name,school_year) %>%
  arrange(district_id,school_id,school_year,grade_num) %>%
  summarise(min_grade = min(grade_num),
            max_grade = max(grade_num),
            all_grades = paste(grade, collapse = ", ")) %>%
  mutate(across(c(min_grade,max_grade), ~ifelse(.x == 0, 'K',as.character(.x))),
         grades_served = paste0(min_grade,"-",max_grade),
         school_level = case_when(
           max_grade %in% c('K','1','2','3','4','5','6') ~ 'ES',
           grades_served %in% c('5-8','6-8','7-8') ~ 'MS',
           grades_served %in% c('K-7','K-8') ~ 'ES/MS',
           grades_served %in% c('1-12','K-11','K-12') ~ 'K-12',
           grades_served %in% c('9-12') ~ 'HS',
           T ~ 'Other'
         ))

write_csv(grades_served, 'prc/grades-served.csv')

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

funding_full <- funding_raw %>%
  filter(!is.na(school_yr)) %>%
  remove_empty("cols") %>%
  remove_constant() %>%
  mutate(
    school_year = case_when(
      str_detect(school_yr, "19-20") ~ 2020L,
      str_detect(school_yr, "20-21") ~ 2021L,
      str_detect(school_yr, "21-22") ~ 2022L,
      str_detect(school_yr, "22-23") ~ 2023L,
      str_detect(school_yr, "23-24") ~ 2024L,
      str_detect(school_yr, "24-25") ~ 2025L
    )) 

write_csv(funding_full, 'prc/funding.csv')

funding <- funding_full %>%
  select(school_year,
         district_id,
         district_name,
         school_id,
         school_name,
         adm,
         total_exp = total_expenditures,
         per_pupil_exp,
         fed_per_pupil_exp,
         state_local_per_pupil_exp) %>%
  mutate(across(contains("exp"), ~round(as.numeric(.x))))


# combine ####
analysis <- school_attend %>%
  select(-level) %>%
  left_join(grades_served[, c('district_id','school_id','school_year','min_grade','max_grade','all_grades','grades_served','school_level')]) %>%
  left_join(tests_wide, by = c('district_id', 'school_id','school_year','grade','student_group'), suffix = c("","_1")) %>%
  left_join(funding, by = c('district_id','school_id','school_year'), suffix = c("","_2")) %>%
  select(-school_name_1,-district_name_1,-school_name_2,-district_name_2)

write_csv(analysis, 'prc/attend_tests_funding.csv', na = "")  

    