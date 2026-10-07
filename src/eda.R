## Rieke Plots
## February 2026
## Dave Moyer

library(tidyverse)
library(readxl)
library(stringr)
library(sf)
library(googlesheets4)
library(hrbrthemes)
library(ggrepel)
library(tidycensus)
library(janitor)
library(here)

# data ####

directory <- read_csv('prc/directory.csv')

dir <- directory %>%
  select(-district_name) %>%
  mutate(district_id = as.numeric(district_id))

grades_served <- read_csv('prc/grades-served.csv') %>%
  mutate(grades_served = ifelse(str_detect(school_name, 'Hayhurst'), 'K-5',grades_served),
         school_level =  ifelse(str_detect(school_name, 'Hayhurst'), 'ES',school_level)) # fix Hayhurst

enroll <- read_csv('prc/enroll.csv')
analysis <- read_csv('prc/attend_tests_funding.csv')
tests_long <- read_csv('prc/full_tests.csv') %>%
  rename(n_participants = n_tested)

full_enroll <- enroll %>%
  filter(level == 'school') %>%
  left_join(dir, by = c('district_id','school_id')) %>%
  distinct()

enroll_pps <- read_csv('prc/enroll-pps.csv')
enroll_pps_district <- read_csv('prc/enroll-pps-district.csv')

enroll_pps_adjusted <- enroll_pps %>%
  mutate(ct = case_when(
    school_short == 'Hayhurst' & student_group == 'all' & school_year == 2019 ~ 390,
    school_short == 'Hayhurst' & student_group == 'all' & school_year == 2020 ~ 396,
    T ~ ct
  ))

sw_pps_elem <- c(823,  #Ainsworth
                 835,  #Bridlemile
                 855,  #Hayhurst
                 1299, #Rieke
                 838,  #Capitol Hill
                 873,  #Maplewood
                 1278, #Markham
                 892  #Stevenson
                 )

all_pps_elem <- grades_served %>%
  filter(district_name == 'Portland SD 1J' & grades_served == 'K-5') %>%
  pull(school_id) %>%
  unique()

all_or_elem <- grades_served %>%
  filter(grades_served %in% c('K-5',"K-6","K-4")) %>%
  pull(school_id) %>%
  unique()

bps_elem <- c(1278, #Montclair
              1172, #Raleigh Hills Elem
              1173 #Raleigh Park Elem
)

dist_enroll <- enroll %>%
  filter(level == 'district' & is.na(school_id) & grade == 'all' & student_group == 'all')

# palette ####
rieke_colors <- c(
  navy   = "#1B2A4A",
  red    = "#C0392B",
  gold   = "#E8A020",
  white  = "#F5F5F5",
  gray   = "#B4B2A9"
)

# enrollment change ####

sw_elem_enroll_all <- enroll_pps_adjusted %>%
  filter((school_id %in% sw_pps_elem) & 
           student_group == 'all') %>%
  mutate(shade = ifelse(school_short == 'Rieke', "1","2"))

enroll_change <- sw_elem_enroll_all %>%
  filter(school_year %in% c(2019,2026)) %>%
  arrange(school_id,school_year) %>%
  group_by(school_id) %>%
  mutate(enroll_change = ct-lag(ct),
         enroll_change_pct = 100*(ct-lag(ct))/lag(ct)) %>%
  select(school_id,school_year,enroll_change,enroll_change_pct) %>%
  filter(school_year == 2026)

sw_elem_enroll_all_with_change <- sw_elem_enroll_all %>%
  left_join(enroll_change, by = c('school_id','school_year'))

enroll_change_plt <- ggplot(sw_elem_enroll_all_with_change, aes(school_year, ct, group = school_id)) +
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data        = \(x) slice_max(x, school_year, n = 1, by = school_id),
    aes(label   = paste0(school_short, ": ", ct, ' (',round(enroll_change_pct),'%)'), color = shade),
    hjust       = 0,
    nudge_x     = 0.2,
    direction   = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data        = \(x) slice_min(x, school_year, n = 1, by = school_id),
    aes(label   = ct, color = shade),
    hjust       = 1,
    nudge_x     = -0.2,
    direction   = "y",
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    "1"       = "#1B2A4A",
    "2" = "#B4B2A9",
    "3" = "#B4B2A9"
  )) +
  scale_x_continuous(limits = c(2018.75,2028),
                     breaks = 2019:2026,
                     labels = 2019:2026) +
  ylim(240,650) +
  labs(x = "",
       y = "",
       title = 'SW Portland Elementary School Enrollment',
       subtitle = 'Change since 2019') +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

enroll_change_plt

ggsave(
  plot = enroll_change_plt,
  file = 'prc/enroll-change-plt.png',
  width = 9,
  height = 6,
  units = 'in',
  dpi = 800
)

## dist enrollment change ####

peer_districts <- c(2180, # Portland
                    2243, # Beaverton
                    2142, # Salem-Keizer
                    2239, # Hillsboro
                    1924, # North Clackamas
                    2082, # Eugene
                    2048  # Medford
)


dist_enroll_all <- dist_enroll %>%
  filter((district_id %in% peer_districts) &
           student_group == 'all') %>%
  mutate(shade = ifelse(district_name == 'Portland SD 1J', "1", "2"),
         district_short = gsub(" SD.*", "", district_name))

dist_enroll_change <- dist_enroll_all %>%
  filter(school_year %in% c(2019,2026)) %>%
  arrange(district_id,school_year) %>%
  group_by(district_id) %>%
  mutate(enroll_change = fall_ct-lag(fall_ct),
         enroll_change_pct = 100*(fall_ct-lag(fall_ct))/lag(fall_ct)) %>%
  select(district_id,school_year,enroll_change,enroll_change_pct) %>%
  filter(school_year == 2026)

dist_enroll_all_with_change <- dist_enroll_all %>%
  left_join(dist_enroll_change, by = c('district_id','school_year'))

dist_enroll_change_plt <- ggplot(dist_enroll_all_with_change, aes(school_year, fall_ct, group = district_id)) +
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data        = \(x) slice_max(x, school_year, n = 1, by = district_id),
    aes(label   = paste0(district_short, ": ", scales::comma(fall_ct), ' (',round(enroll_change_pct),'%)'), color = shade),
    hjust       = 0,
    nudge_x     = 0.2,
    direction   = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data        = \(x) slice_min(x, school_year, n = 1, by = district_id),
    aes(label   = scales::comma(fall_ct), color = shade),
    hjust       = 1,
    nudge_x     = -0.2,
    direction   = "y",
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    "1"       = "#1B2A4A",
    "2" = "#B4B2A9",
    "3" = "#B4B2A9"
  )) +
  scale_x_continuous(limits = c(2018.75,2029),
                     breaks = 2019:2026,
                     labels = 2019:2026) +
  ylim(10000,49000) +
  labs(x = "",
       y = "",
       title = 'PPS enrollment is down sharply, in line with its peers',
       subtitle = 'Fall Enrollment Change since 2019',
       caption = 'Source: Oregon Dept. of Education') +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

dist_enroll_change_plt

ggsave(
  plot = dist_enroll_change_plt,
  file = 'prc/dist-enroll-change-plt.png',
  width = 9,
  height = 6,
  units = 'in',
  dpi = 800
)
  

# enrollment breakdown ####
district_enroll_group <- enroll_pps_district %>%
  filter(student_group %in% c('hispanic','black','historically_underserved','swd') &
           school_year == 2026) %>%
  select(school_year,school_short,student_group,pct) %>%
  mutate(school_name = case_when(
    school_short == 'District Total' ~ 'PPS',
    school_short == 'Elementary Schools Total' ~ 'All PPS Elem.'
  ),
  student_group = case_when(
    student_group == 'swd' ~ 'Students with Disabilities',
    student_group == 'historically_underserved' ~ 'Historically Underserved',
    T ~ str_to_title(student_group)))

sw_pps_enroll_group <- enroll_pps_adjusted %>%
    filter(school_id %in% sw_pps_elem & school_year == 2026  & 
             student_group %in% c('all','hispanic','black','historically_underserved','swd')) %>%
    filter(school_id != 1299) %>% # pull out Rieke
    select(school_year,school_id,student_group,ct) %>%
    pivot_wider(names_from = student_group, values_from = ct) %>%
    summarise(across(c(all:black), ~sum(.x))) %>%
  mutate(across(c(swd:black), ~round(100*(.x/all)))) %>%
  pivot_longer(c(swd:black), names_to = 'student_group',values_to = 'pct') %>%
  mutate(school_name = 'Other SW Elem',
         student_group = case_when(
           student_group == 'swd' ~ 'Students with Disabilities',
           student_group == 'historically_underserved' ~ 'Historically Underserved',
           T ~ str_to_title(student_group)
         )) %>%
  select(-all)

rieke_enroll_group <- enroll_pps_adjusted %>%
  filter(school_id == 1299 & school_year == 2026 &
           student_group %in% c('hispanic','black','historically_underserved','swd')) %>%
  select(student_group,pct) %>%
  mutate(school_name = 'Rieke',
         student_group = case_when(
           student_group == 'swd' ~ 'Students with Disabilities',
           student_group == 'historically_underserved' ~ 'Historically Underserved',
           T ~ str_to_title(student_group)
         )
         )
  
ref_enroll_group <- bind_rows(sw_pps_enroll_group,district_enroll_group) %>%
  select(school_name,student_group,pct) %>%
  mutate(
  student_group = factor(student_group, levels = c('Black',
                                                   'Hispanic',
                                                   'Historically Underserved',
                                                   'Students with Disabilities')))

enroll_group_plt <- ggplot(rieke_enroll_group, aes(pct, student_group)) +
  geom_col(fill = "#1D9E75", width = 0.6) +
  geom_errorbar(
    data     = ref_enroll_group,
    aes(x    = pct,
        xmin = pct,
        xmax = pct,
        color = school_name),
    width     = 0.6,
    linewidth = 0.9
  ) +
  geom_text(aes(x = ifelse(pct >=10, pct-3,2), label = paste0(pct,'%')),
            color = 'white') +
  scale_color_manual(values = c(
    "Other SW Elem" = "black",
    "PPS"   = "grey"
  )) +
  labs(
    title = 'Fall 2026 Enrollment by Student Group'
  ) +
  theme_ipsum_pub(grid = F) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
enroll_group_plt

ggsave(
  plot = enroll_group_plt,
  file = 'prc/enroll-group-plt.png',
  width = 8,
  height = 6,
  units = 'in',
  dpi = 800
)


combined_enroll_group <- bind_rows(rieke_enroll_group,ref_enroll_group) %>%
  mutate(school_name = factor(school_name, levels = c('PPS',"All PPS Elem.",'Other SW Elem','Rieke'))) %>%
  filter(school_name != 'All PPS Elem.')

enroll_group_grouped_plt <- ggplot(
  combined_enroll_group,
  aes(pct, student_group, fill = school_name)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(x = pct + 2, label = paste0(round(pct), "%"), color = school_name),
    position = position_dodge(width = 0.8),
    hjust    = 0,
    size     = 4
  ) +
  scale_fill_manual(values = c(
    "Rieke"         = "#1B2A4A",
    "Other SW Elem" = "#A8C4E0",
    "PPS"     = "#B4B2A9"
  )) +
  scale_color_manual(values = c(
    "Rieke"         = "#1B2A4A",
    "Other SW Elem" = "#A8C4E0",
    "PPS"     = "#B4B2A9"
  )) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(title = "Fall 2026 Enrollment by Student Group") +
  theme_ipsum_pub(grid = F) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    axis.text.x      = element_blank()
  )

enroll_group_grouped_plt

ggsave(
  plot = enroll_group_grouped_plt,
  file = 'prc/enroll-group-grouped-plt.png',
  width = 8,
  height = 6,
  units = 'in',
  dpi = 800
)

# performance ####

rieke_prof <- tests_long %>%
  filter(str_detect(school_name,'Rieke') & grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

sw_pps_prof <- tests_long %>%
  filter(school_id %in% sw_pps_elem & grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

pps_elem_prof <- tests_long %>%
  filter(school_id %in% all_pps_elem &
           grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

state_elem_prof <- tests_long %>%
  filter(school_id %in% all_or_elem &
           grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)


## proficiency trend ####

pps_elem_avg_trend <- pps_elem_prof %>%
  filter(!is.na(pct_proficient) & subject %in% c('ela', 'math','science')) %>%
  group_by(school_year, subject) %>%
  summarise(across(c(n_proficient, n_participants), ~sum(.x, na.rm = T))) %>% 
  mutate(pct_proficient = 100*(n_proficient/n_participants), 
         label = 'PPS Elem Avg', shade = '3')

sw_elem_avg_trend <- sw_pps_prof %>%
  filter(school_id != 1299 & !is.na(pct_proficient) & subject %in% c('ela', 'math','science')) %>%
  group_by(school_year, subject) %>%
  summarise(across(c(n_proficient, n_participants), ~sum(.x, na.rm = T))) %>% 
  mutate(pct_proficient = 100*(n_proficient/n_participants), 
         label = 'Other SW Elem', shade = '2')

rieke_trend <- rieke_prof %>%
  filter(!is.na(pct_proficient) & subject %in% c('ela', 'math','science')) %>%
  mutate(label = 'Rieke', shade = '1') %>%
  select(school_year, subject, pct_proficient, label, shade)

prof_trend_data <- bind_rows(rieke_trend, sw_elem_avg_trend, pps_elem_avg_trend) %>%
  mutate(subject_label = case_when(
    subject == 'ela'  ~ 'ELA',
    subject == 'math' ~ 'Math',
    subject == 'science' ~ 'Science'
  ))

prof_trend_plt <- ggplot(prof_trend_data, aes(school_year, pct_proficient, group = interaction(label, subject_label))) +
  geom_line(aes(color = shade), size = 1) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data          = \(x) slice_max(x, school_year, n = 1, by = c(label, subject_label)),
    aes(label     = paste0(label, ': ', round(pct_proficient), '%'), color = shade),
    hjust         = 0,
    nudge_x       = 0.2,
    direction     = 'y',
    segment.color = NA,
    fontface = "bold"
  ) +
  scale_color_manual(values = c(
    '1' = '#1B2A4A',
    '2' = '#A8C4E0',
    '3' = '#B4B2A9'
  )) +
  scale_x_continuous(
    limits = c(2018.5, 2028.5),
    breaks = c(2019, 2022, 2023, 2024, 2025),
    labels = c("'19","'22","'23","'24","'25")
  ) +
  facet_wrap(~subject_label) +
  labs(
    x        = '',
    y        = '',
    title    = 'Rieke has experienced strong test score gains since the pandemic',
    subtitle = 'Percent proficient on Oregon state assessments'
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position = 'none',
    axis.text.y     = element_blank()
  )

prof_trend_plt

ggsave(
  plot   = prof_trend_plt,
  file   = 'prc/prof-trend-plt.png',
  width  = 12,
  height = 6,
  units  = 'in',
  dpi    = 800
)


## PPS elem ranking ####

pps_rank_2025 <- pps_elem_prof %>%
  filter(school_year == 2025 & subject %in% c('ela', 'math') & !is.na(pct_proficient)) %>%
  mutate(
    subject_label = case_when(subject == 'ela' ~ 'ELA', subject == 'math' ~ 'Math'),
    shade         = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short  = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(subject, desc(pct_proficient)) %>%
  group_by(subject) %>%
  mutate(rank = row_number()) %>%
  ungroup()

prof_rank_pps_plt <- ggplot(pps_rank_2025, aes(pct_proficient, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade %in% c('1', '2')),
    aes(label          = paste0(school_short, ': ', pct_proficient, '%'), color = shade),
    hjust              = 1,
    nudge_x            = -5,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 1)) +
  scale_y_reverse(breaks = NULL) +
  scale_x_continuous(limits = c(0, 95)) +
  facet_wrap(~subject_label) +
  labs(
    title    = 'Rieke was the top PPS elementary school in 2024-2025',
    subtitle = '2024-25 proficiency on Oregon state assessments',
    x        = '% Proficient (Levels 3 & 4)',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position = 'none',
    axis.text.y     = element_blank()
  )

prof_rank_pps_plt

ggsave(
  plot   = prof_rank_pps_plt,
  file   = 'prc/prof-rank-pps-plt.png',
  width  = 10,
  height = 6,
  units  = 'in',
  dpi    = 800
)

prof_rank_pps_ela <- ggplot(
  pps_rank_2025 |> filter(subject == "ela") |> mutate(school_short = reorder(school_short, pct_proficient)),
  aes(school_short, pct_proficient)
) +
  geom_col(aes(fill = shade)) +
  geom_text(
    data     = \(x) filter(x, shade %in% c("1", "2")),
    aes(label = school_short, color = shade, y = 0),
    hjust    = 1,
    nudge_y  = -1,
    fontface = 'bold',
    size = 3.5
  ) +
  geom_text(
    data     = \(x) filter(x, shade %in% c("1", "2")),
    aes(label = paste0(round(pct_proficient,1), "%"), color = shade),
    hjust    = 0,
    nudge_y  = 1,
    fontface = 'bold',
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values  = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_color_manual(values = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_y_continuous(limits = c(-25, 100)) +
  facet_wrap(~subject_label) +
  labs(
    #title    = "Rieke was the top PPS elementary school in 2024-2025",
    #subtitle = "2024-25 proficiency on Oregon state assessments",
    y        = "% Proficient (Levels 3 & 4)",
    x        = ""
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),
    axis.title.y     = element_text(hjust = 0.5),
    strip.text       = element_text(hjust = 0.5)
  )
prof_rank_pps_ela

ggsave(
  plot   = prof_rank_pps_ela,
  file   = 'prc/prof-bar-pps-ela.png',
  width  = 6,
  height = 7,
  units  = 'in',
  dpi    = 800
)

prof_rank_pps_math <- ggplot(
  pps_rank_2025 |> filter(subject == "math") |> mutate(school_short = reorder(school_short, pct_proficient)),
  aes(school_short, pct_proficient)
) +
  geom_col(aes(fill = shade)) +
  geom_text(
    data     = \(x) filter(x, shade %in% c("1", "2")),
    aes(label = school_short, color = shade, y = 0),
    hjust    = 1,
    nudge_y  = -1,
    fontface = 'bold',
    size = 3.5
  ) +
  geom_text(
    data     = \(x) filter(x, shade %in% c("1", "2")),
    aes(label = paste0(round(pct_proficient,1), "%"), color = shade),
    hjust    = 0,
    nudge_y  = 1,
    fontface = 'bold',
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values  = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_color_manual(values = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_y_continuous(limits = c(-25, 100)) +
  facet_wrap(~subject_label) +
  labs(
    #title    = "Rieke was the top PPS elementary school in 2024-2025",
    #subtitle = "2024-25 proficiency on Oregon state assessments",
    y        = "% Proficient (Levels 3 & 4)",
    x        = ""
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),
    axis.title.y     = element_text(hjust = 0.5),
    strip.text       = element_text(hjust = 0.5)
  )
prof_rank_pps_math

ggsave(
  plot   = prof_rank_pps_math,
  file   = 'prc/prof-bar-pps-math.png',
  width  = 6,
  height = 7,
  units  = 'in',
  dpi    = 800
)

## state elem rank ####
state_rank_2025 <- state_elem_prof %>%
  filter(school_year == 2025 & subject %in% c('ela', 'math') & !is.na(pct_proficient)) %>%
  mutate(
    subject_label = case_when(subject == 'ela' ~ 'ELA', subject == 'math' ~ 'Math'),
    shade         = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short  = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(subject, desc(pct_proficient)) %>%
  group_by(subject) %>%
  mutate(rank = row_number()) %>%
  ungroup()

prof_rank_state_plt <- ggplot(state_rank_2025, aes(pct_proficient, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade %in% c('1')),
    aes(label          = paste0(school_short, ': ', pct_proficient, '%'), color = shade),
    hjust              = 1,
    nudge_x            = -5,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 1)) +
  scale_y_reverse(breaks = NULL) +
  scale_x_continuous(limits = c(0, 95)) +
  facet_wrap(~subject_label) +
  labs(
    title    = 'Rieke was the third best elementary school in Oregon in 2024-2025',
    subtitle = '2024-25 proficiency on Oregon state assessments',
    x        = '% Proficient (Levels 3 & 4)',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position = 'none',
    axis.text.y     = element_blank()
  )

prof_rank_state_plt

ggsave(
  plot   = prof_rank_state_plt,
  file   = 'prc/prof-rank-state-plt.png',
  width  = 10,
  height = 6,
  units  = 'in',
  dpi    = 800
)


## SW Portland comparison ####

sw_ela_order <- sw_pps_prof %>%
  filter(school_year == 2025 & subject == 'ela' & !is.na(pct_proficient)) %>%
  arrange(pct_proficient) %>%
  mutate(school_short = gsub(' Elementary School', '', school_name)) %>%
  pull(school_short)

sw_prof_2025 <- sw_pps_prof %>%
  filter(school_year == 2025 & subject %in% c('ela', 'math') & !is.na(pct_proficient)) %>%
  mutate(
    subject_label = case_when(subject == 'ela' ~ 'ELA', subject == 'math' ~ 'Math'),
    shade         = case_when(school_id == 1299 ~ '1', TRUE ~ '2'),
    school_short  = factor(gsub(' Elementary School', '', school_name), levels = sw_ela_order)
  )

prof_sw_plt <- ggplot(sw_prof_2025, aes(pct_proficient, school_short)) +
  geom_col(aes(fill = shade), width = 0.65) +
  geom_text(
    aes(x = pct_proficient - 2, label = paste0(round(pct_proficient), '%')),
    color = 'white',
    hjust = 1,
    size  = 3.5
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0')) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~subject_label) +
  labs(
    title    = 'SW Portland Elementary School Proficiency',
    subtitle = '2024-25 percent proficient on Oregon state assessments',
    x        = '',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position = 'none',
    axis.text.x     = element_blank()
  )

prof_sw_plt

ggsave(
  plot   = prof_sw_plt,
  file   = 'prc/prof-sw-plt.png',
  width  = 8,
  height = 5.5,
  units  = 'in',
  dpi    = 800
)


# finance ####

fin_base <- analysis %>%
  filter(grade == 'all' & student_group == 'all' &
           !is.na(per_pupil_exp) & per_pupil_exp > 0 &
           !is.na(total_exp)) %>%
  distinct(school_id, school_year, .keep_all = TRUE) %>%
  select(district_id, district_name, school_id, school_name, school_year,
         adm,total_exp, per_pupil_exp)

rieke_fin      <- fin_base %>% filter(school_id == 1299)
sw_pps_fin     <- fin_base %>% filter(school_id %in% sw_pps_elem)
pps_elem_fin   <- fin_base %>% filter(school_id %in% all_pps_elem)
state_elem_fin <- fin_base %>% filter(school_id %in% all_or_elem)


## finance trend ####

pps_fin_avg <- pps_elem_fin %>%
  group_by(school_year) %>%
  summarise(per_pupil_exp = sum(total_exp) / sum(adm), .groups = 'drop') %>%
  mutate(label = 'PPS Elem Avg', shade = '3')

sw_fin_avg <- sw_pps_fin %>%
  filter(school_id != 1299) %>%
  group_by(school_year) %>%
  summarise(per_pupil_exp = sum(total_exp) / sum(adm), .groups = 'drop') %>%
  mutate(label = 'Other SW Elem', shade = '2')

state_fin_avg <- state_elem_fin %>%
  group_by(school_year) %>%
  summarise(per_pupil_exp = sum(total_exp) / sum(adm), .groups = 'drop') %>%
  mutate(label = 'State Elem Avg', shade = '4')

rieke_fin_trend <- rieke_fin %>%
  mutate(label = 'Rieke', shade = '1') %>%
  select(school_year, per_pupil_exp, label, shade)

fin_trend_data <- bind_rows(rieke_fin_trend, sw_fin_avg, pps_fin_avg, state_fin_avg)

fin_trend_plt <- ggplot(fin_trend_data, aes(school_year, per_pupil_exp, group = label)) +
  geom_line(aes(color = shade), size = 1) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data          = \(x) slice_max(x, school_year, n = 1, by = label),
    aes(label     = paste0(label, ': $', format(round(per_pupil_exp, -2), big.mark = ',')),
        color     = shade),
    hjust         = 0,
    nudge_x       = 0.05,
    direction     = 'y',
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    '1' = '#1B2A4A',
    '2' = '#A8C4E0',
    '3' = '#B4B2A9',
    '4' = '#D3D1C7'
  )) +
  scale_x_continuous(
    limits = c(2021.5, 2026.5),
    breaks = c(2022, 2023, 2024),
    labels = c("'21-22", "'22-23", "'23-24")
  ) +
  scale_y_continuous(labels = \(x) paste0('$', x / 1000, 'k')) +
  labs(
    x        = '',
    y        = '',
    title    = "Rieke spending growth mirrors district peers",
    subtitle = 'Total expenditures per pupil'
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none')

fin_trend_plt

ggsave(plot = fin_trend_plt, file = 'prc/fin-trend-plt.png',
       width = 11, height = 5.5, units = 'in', dpi = 800)


## PPS ranking dot plot ####

pps_fin_rank <- pps_elem_fin %>%
  filter(school_year == max(school_year)) %>%
  mutate(
    shade        = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(per_pupil_exp) %>%
  mutate(rank = row_number())

fin_rank_pps_plt <- ggplot(pps_fin_rank, aes(per_pupil_exp, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade %in% c('1', '2')),
    aes(label          = paste0(school_short, ': $', format(round(per_pupil_exp, -2), big.mark = ',')),
        color          = shade),
    hjust              = 0,
    nudge_x            = 300,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 2)) +
  scale_x_continuous(
    #expand = expansion(mult = c(0.02, 0.35)),
    labels = \(x) paste0('$', x / 1000, 'k'),
    limits = c(0,55000)
  ) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = 'Rieke Among All PPS Elementary Schools',
    subtitle = '2023-24 per-pupil expenditures',
    caption = 'Whitman Elementary excluded ',
    x = 'Per-Pupil Expenditure', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.y = element_blank())

fin_rank_pps_plt

ggsave(plot = fin_rank_pps_plt, file = 'prc/fin-rank-pps-plt.png',
       width = 8, height = 6, units = 'in', dpi = 800)


## state ranking dot plot ####

state_fin_rank <- state_elem_fin %>%
  filter(school_year == max(school_year) & per_pupil_exp <= 60000) %>%
  mutate(
    shade        = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(per_pupil_exp) %>%
  mutate(rank = row_number())

fin_rank_state_plt <- ggplot(state_fin_rank, aes(per_pupil_exp, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade == '1'),
    aes(label          = paste0(school_short, ': $', format(round(per_pupil_exp, -2), big.mark = ',')),
        color          = shade),
    hjust              = 1,
    nudge_x            = -500,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 1)) +
  scale_x_continuous(labels = \(x) paste0('$', x / 1000, 'k')) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = 'Rieke Among All Oregon Elementary Schools',
    subtitle = '2023-24 per-pupil expenditures',
    caption = 'Schools above $60k not shown',
    x = 'Per-Pupil Expenditure', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.y = element_blank())

fin_rank_state_plt

ggsave(plot = fin_rank_state_plt, file = 'prc/fin-rank-state-plt.png',
       width = 10, height = 6, units = 'in', dpi = 800)


## SW Portland bar chart ####

sw_fin_order <- sw_pps_fin %>%
  filter(school_year == max(school_year)) %>%
  arrange(per_pupil_exp) %>%
  mutate(school_short = gsub(' Elementary School', '', school_name)) %>%
  pull(school_short)

sw_fin_data <- sw_pps_fin %>%
  filter(school_year == max(school_year)) %>%
  mutate(
    shade        = case_when(school_id == 1299 ~ '1', TRUE ~ '2'),
    school_short = factor(gsub(' Elementary School', '', school_name), levels = sw_fin_order)
  )

fin_sw_plt <- ggplot(sw_fin_data, aes(per_pupil_exp, school_short)) +
  geom_col(aes(fill = shade), width = 0.65) +
  geom_text(
    aes(x     = per_pupil_exp - 200,
        label = paste0('$', format(round(per_pupil_exp, -2), big.mark = ','))),
    color = 'white', hjust = 1, size = 3.5
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0')) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = 'SW Portland Elementary Per-Pupil Spending',
    subtitle = '2023-24 total expenditures per pupil',
    x = '', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.x = element_blank())

fin_sw_plt

ggsave(plot = fin_sw_plt, file = 'prc/fin-sw-plt.png',
       width = 8, height = 5.5, units = 'in', dpi = 800)

## district context ####


dist_funding_files <- list.files("raw/funding/dist",
                            pattern = "*.xls*",
                            full.names = TRUE)

dist_funding_csvs <- list.files('raw/funding/dist',
                          pattern = "Actual Expenditure Data.csv$",
                           full.names = TRUE)

dist_funding_raw <- bind_rows(
  map(dist_funding_files, function(file) {
    sheets <- excel_sheets(file)
    data_sheet <- sheets[!str_detect(sheets, "(?i)definition|note")]
    read_excel(file, sheet = data_sheet[1], col_types = "text") |>
      clean_names() |>
      mutate(source_file = basename(file))
  }),
  map(dist_funding_csvs, function(file) {
    read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      clean_names() |>
      mutate(source_file = basename(file))
  })
)


## dist per-pupil funding change ####

dist_spending <- dist_funding_raw %>%
  mutate(actual_exp_amt = as.numeric(actual_exp_amt),
         school_year = case_match(school_year,
           '2018-19' ~ 2019,
           '2019-20' ~ 2020,
           '2020-21' ~ 2021,
           '2021-22' ~ 2022,
           '2022-23' ~ 2023,
           '2023-24' ~ 2024,
           '2024-25' ~ 2025,
           '2025-26' ~ 2026
         ),
         institution_id = as.numeric(institution_id)) %>%
  rename(district_id = institution_id,
         district_name = institution_name) %>%
  group_by(school_year,district_id) %>%
  summarise(district_name = first(district_name),
            exp = sum(actual_exp_amt))

dist_pp_all <- dist_spending %>%
  filter(district_id %in% peer_districts) %>%
  left_join(
    dist_enroll %>% select(district_id, school_year, ct = fall_ct),
    by = c('district_id','school_year')
  ) %>%
  mutate(per_pupil = exp / ct,
         shade = ifelse(district_name == 'Portland SD 1J', "1", "2"),
         district_short = gsub(" SD.*", "", district_name)) %>%
  filter(school_year %in% c(2019,2022,2023,2024,2025))

pp_first_yr <- min(dist_pp_all$school_year)
pp_last_yr  <- max(dist_pp_all$school_year)

dist_pp_change <- dist_pp_all %>%
  filter(school_year %in% c(pp_first_yr,pp_last_yr)) %>%
  arrange(district_id,school_year) %>%
  group_by(district_id) %>%
  mutate(pp_change_pct = 100*(per_pupil-lag(per_pupil))/lag(per_pupil),
         exp_change_pct = 100*(exp -lag(exp))/lag(exp)) %>%
  select(district_id,school_year,pp_change_pct,exp_change_pct) %>%
  filter(school_year == pp_last_yr)

dist_pp_all_with_change <- dist_pp_all %>%
  left_join(dist_pp_change, by = c('district_id','school_year')) %>%
  ungroup()

dist_pp_change_plt <- ggplot(dist_pp_all_with_change, aes(school_year, exp, group = district_id)) +
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data        = \(x) slice_max(x, school_year, n = 1, by = district_id),
    aes(label   = paste0(district_short, ": ", scales::dollar(exp, scale = 1e-6, suffix = "M", accuracy = 1), ' (+',round(exp_change_pct),'%)'), color = shade),
    hjust       = 0,
    nudge_x     = 0.2,
    direction   = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data        = \(x) slice_min(x, school_year, n = 1, by = district_id),
    aes(label   = scales::dollar(exp, scale = 1e-6, suffix = "M", accuracy = 1), color = shade),
    hjust       = 1,
    nudge_x     = -0.2,
    direction   = "y",
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    "1"       = "#1B2A4A",
    "2" = "#B4B2A9",
    "3" = "#B4B2A9"
  )) +
  scale_x_continuous(limits = c(pp_first_yr - 0.25, pp_last_yr + 2),
                     breaks = pp_first_yr:pp_last_yr,
                     labels = pp_first_yr:pp_last_yr) +
  labs(x = "",
       y = "",
       title = "PPS' Spending Has Grown since the Pandemic, along with its peers",
       subtitle = paste0('Overall Expenditures Change Since ', pp_first_yr),
       caption = 'Source: Actual Reported Ependitures from Oregon Dept. of Education') +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_blank())

dist_pp_change_plt

ggsave(
  plot = dist_pp_change_plt,
  file = 'prc/dist-pp-change-plt.png',
  width = 9,
  height = 6,
  units = 'in',
  dpi = 1000
)

## PPS revenue by source ####

revenue_files <- list.files("raw/funding/dist", pattern = "Actual Revenue Data.csv$", full.names = TRUE)

pps_revenue <- revenue_files %>%
  map_df(read_csv) %>%
  filter(Institution_Name == "Portland SD 1J")

# General Fund contains two non-recurring accounting lines that distort a
# revenue trend: bond proceeds ("Long Term Debt Financing Sources") and last
# year's carryover cash ("Resources - Beginning Fund Balance"). Neither is
# new operating money, so they're excluded from "core" General Fund revenue.
core_general_fund <- pps_revenue %>%
  filter(FundDesc == "General Fund",
         !SourceDesc %in% c("Long Term Debt Financing Sources",
                            "Resources - Beginning Fund Balance")) %>%
  group_by(SchoolYear) %>%
  summarise(core_general_fund = sum(ActualRevAmt))


federal_fund <- pps_revenue %>%
  filter(FundDesc == "Federal Sources") %>%
  group_by(SchoolYear) %>%
  summarise(federal_fund = sum(ActualRevAmt))

state_school_fund <- pps_revenue %>%
  filter(SourceDesc == "State School Fund --General Support") %>%
  group_by(SchoolYear) %>%
  summarise(state_school_fund = sum(ActualRevAmt))

property_tax <- pps_revenue %>%
  filter(SourceDesc == "Ad valorem taxes levied by district") %>%
  group_by(SchoolYear) %>%
  summarise(property_tax = sum(ActualRevAmt))

# bond proceeds - the one-time financing entry that drove the 2021-22 spike,
# broken back out as its own series to show it explicitly rather than
# folding it into (or excluding it from) general fund revenue
bond_proceeds <- pps_revenue %>%
  filter(SourceDesc == "Long Term Debt Financing Sources") %>%
  group_by(SchoolYear) %>%
  summarise(bond_proceeds = sum(ActualRevAmt))

pps_revenue_by_source <- federal_fund %>%
  left_join(state_school_fund, by = "SchoolYear") %>%
  left_join(property_tax, by = "SchoolYear") %>%
  left_join(bond_proceeds, by = "SchoolYear") %>%
  mutate(federal_fund = replace_na(federal_fund, 0),
         bond_proceeds = replace_na(bond_proceeds, 0))

print(pps_revenue_by_source)

series_labels <- c(federal_fund      = "Federal Sources fund",
                   state_school_fund = "State School Fund",
                   property_tax      = "Property tax (ad valorem)",
                   bond_proceeds     = "Bond proceeds")

pps_revenue_long <- pps_revenue_by_source %>%
  select(SchoolYear,
         federal_fund,
         state_school_fund,
         property_tax,
         bond_proceeds) %>%
  pivot_longer(-SchoolYear, names_to = "series", values_to = "amount") %>%
  mutate(series_label = series_labels[series],
         year_num = as.numeric(substr(SchoolYear, 1, 4)))

year_breaks <- pps_revenue_long %>% distinct(year_num, SchoolYear) %>% arrange(year_num)

p_revenue <- ggplot(pps_revenue_long, aes(year_num, amount, group = series, color = series)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_text_repel(
    data          = \(x) slice_max(x, year_num, n = 1, by = series),
    aes(label     = paste0(series_label, ": ", scales::dollar(amount, scale = 1e-6, suffix = "M", accuracy = 1))),
    hjust         = 0,
    nudge_x       = 0.2,
    direction     = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data          = \(x) slice_min(x, year_num, n = 1, by = series),
    aes(label     = scales::dollar(amount, scale = 1e-6, suffix = "M", accuracy = 1)),
    hjust         = 1,
    nudge_x       = -0.2,
    direction     = "y",
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    bond_proceeds     = "darkgrey",
    federal_fund      = "#B4863C",
    state_school_fund = "#1B2A4A",
    property_tax      = "#6E8B7C"
  )) +
  scale_x_continuous(limits = c(min(year_breaks$year_num) - 0.25, max(year_breaks$year_num) + 2),
                     breaks = year_breaks$year_num,
                     labels = year_breaks$SchoolYear) +
  labs(x = "", y = "",
       title = "PPS Revenues Over Time",
       caption = "Source: Selected major categories of reported revenues from Oregon Dept. of Education") +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.y = element_blank())
p_revenue

ggsave("prc/pps-revenue-by-source-plt.png", p_revenue, width = 9, height = 6, dpi = 1000)



# attendance ####

attend_base <- analysis %>%
  filter(grade == 'all' & student_group == 'all' & !is.na(pct_regular)) %>%
  distinct(school_id, school_year, .keep_all = TRUE) %>%
  select(district_id, district_name, school_id, school_name, school_year,
         n_regular, pct_regular, n_absent, pct_absent)

rieke_attend      <- attend_base %>% filter(school_id == 1299)
sw_pps_attend     <- attend_base %>% filter(school_id %in% sw_pps_elem)
pps_elem_attend   <- attend_base %>% filter(school_id %in% all_pps_elem)
state_elem_attend <- attend_base %>% filter(school_id %in% all_or_elem)


## attendance trend ####

pps_attend_avg <- pps_elem_attend %>%
  group_by(school_year) %>%
  summarise(across(c(n_regular, n_absent), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(pct_regular = 100 * n_regular / (n_regular + n_absent),
         label = 'PPS Elem Avg', shade = '3')

sw_attend_avg <- sw_pps_attend %>%
  filter(school_id != 1299) %>%
  group_by(school_year) %>%
  summarise(across(c(n_regular, n_absent), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(pct_regular = 100 * n_regular / (n_regular + n_absent),
         label = 'Other SW Elem', shade = '2')

rieke_attend_trend <- rieke_attend %>%
  mutate(label = 'Rieke', shade = '1') %>%
  select(school_year, pct_regular, label, shade)

attend_trend_data <- bind_rows(rieke_attend_trend, sw_attend_avg, pps_attend_avg)

attend_trend_plt <- ggplot(attend_trend_data, aes(school_year, pct_regular, group = label)) +
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data          = \(x) slice_max(x, school_year, n = 1, by = label),
    aes(label     = paste0(label, ': ', round(pct_regular, 1), '%'), color = shade),
    hjust         = 0,
    nudge_x       = 0.2,
    direction     = 'y',
    segment.color = NA
  ) +
  scale_color_manual(values = c(
    '1' = '#1B2A4A',
    '2' = '#A8C4E0',
    '3' = '#B4B2A9'
  )) +
  scale_x_continuous(
    limits = c(2018.5, 2028.5),
    breaks = c(2019, 2022, 2023, 2024, 2025),
    labels = c("'19", "'22", "'23", "'24", "'25")
  ) +
  labs(
    x        = '',
    y        = '',
    title    = 'Rieke attendance has recovered from its post-pandemic low',
    subtitle = 'Percent of students attending school regularly (inverse of chronic absenteeism)'
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(
    legend.position = 'none',
    axis.text.y     = element_blank()
  )

attend_trend_plt

ggsave(
  plot   = attend_trend_plt,
  file   = 'prc/attend-trend-plt.png',
  width  = 9,
  height = 5.5,
  units  = 'in',
  dpi    = 800
)


## PPS ranking dot plot ####

pps_attend_rank <- pps_elem_attend %>%
  filter(school_year == 2025) %>%
  mutate(
    shade        = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(pct_regular) %>%
  mutate(rank = row_number())

attend_rank_pps_plt <- ggplot(pps_attend_rank, aes(pct_regular, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade %in% c('1', '2')),
    aes(label          = paste0(school_short, ': ', round(pct_regular, 1), '%'),
        color          = shade),
    hjust              = 0,
    nudge_x            = 1,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 2)) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.35)),
    labels = \(x) paste0(x, '%')
  ) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = 'Rieke Attendance Among All PPS Elementary Schools',
    subtitle = '2024-25 percent of students attending regularly',
    x        = '% Regular Attenders',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.y = element_blank())

attend_rank_pps_plt

ggsave(plot = attend_rank_pps_plt, file = 'prc/attend-rank-pps-plt.png',
       width = 8, height = 6, units = 'in', dpi = 800)


## state ranking dot plot ####

state_attend_rank <- state_elem_attend %>%
  filter(school_year == 2025) %>%
  mutate(
    shade        = case_when(
      school_id == 1299          ~ '1',
      school_id %in% sw_pps_elem ~ '2',
      TRUE                       ~ '3'
    ),
    school_short = gsub(' Elementary School', '', school_name)
  ) %>%
  arrange(pct_regular) %>%
  mutate(rank = row_number())

attend_rank_state_plt <- ggplot(state_attend_rank, aes(pct_regular, rank)) +
  geom_point(aes(color = shade, size = shade)) +
  geom_text_repel(
    data               = \(x) filter(x, shade == '1'),
    aes(label          = paste0(school_short, ': ', round(pct_regular, 1), '%'),
        color          = shade),
    hjust              = 1,
    nudge_x            = -1,
    direction          = 'y',
    segment.color      = 'gray70',
    segment.alpha      = 0.5,
    size               = 2.8,
    min.segment.length = 0
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_size_manual(values  = c('1' = 4,          '2' = 3,          '3' = 1)) +
  scale_x_continuous(labels = \(x) paste0(x, '%')) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = 'Rieke Attendance Among All Oregon Elementary Schools',
    subtitle = '2024-25 percent of students attending regularly',
    x        = '% Regular Attenders',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.y = element_blank())

attend_rank_state_plt

ggsave(plot = attend_rank_state_plt, file = 'prc/attend-rank-state-plt.png',
       width = 10, height = 6, units = 'in', dpi = 800)


## SW Portland bar chart ####

sw_attend_order <- sw_pps_attend %>%
  filter(school_year == 2025) %>%
  arrange(pct_regular) %>%
  mutate(school_short = gsub(' Elementary School', '', school_name)) %>%
  pull(school_short)

sw_attend_data <- sw_pps_attend %>%
  filter(school_year == 2025) %>%
  mutate(
    shade        = case_when(school_id == 1299 ~ '1', TRUE ~ '2'),
    school_short = factor(gsub(' Elementary School', '', school_name), levels = sw_attend_order)
  )

attend_sw_plt <- ggplot(sw_attend_data, aes(pct_regular, school_short)) +
  geom_col(aes(fill = shade), width = 0.65) +
  geom_text(
    aes(x = pct_regular - 1, label = paste0(round(pct_regular, 1), '%')),
    color = 'white', hjust = 1, size = 3.5
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0')) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = 'SW Portland Elementary Regular Attendance',
    subtitle = '2024-25 percent of students attending regularly',
    x        = '',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.x = element_blank())

attend_sw_plt

ggsave(plot = attend_sw_plt, file = 'prc/attend-sw-plt.png',
       width = 8, height = 5.5, units = 'in', dpi = 800)

# pps-info metrics ####

pps_info <- read_csv('raw/pps-info/pps_schools_2026-04-29.csv') %>%
  mutate(school_short = gsub(' Elementary School| Program \\(K-8\\)', '', school_name))

sw_pps_info <- pps_info %>%
  filter(ode_school_id %in% sw_pps_elem)

elem_pps_info <- pps_info %>%
  filter(level %in% c('elementary','k8'))

## seismic ####

sw_seismic <- elem_pps_info %>%
  mutate(
    shade = case_when(ode_school_id == 1299 ~ '1', 
                             ode_school_id %in% sw_pps_elem ~ '2',
                             T ~ '3')) %>%
  filter(!is.na(retrofit_cost_remaining_usd))

seismic_sw_plt <- ggplot(sw_seismic, aes(retrofit_cost_remaining_usd, reorder(school_short,retrofit_cost_remaining_usd))) +
  geom_col(
    aes(
      fill   = shade,
      color  = seismic_retrofit_status %in% c('planned_targeted', 'full', 'planned_full'),
      linewidth = seismic_retrofit_status %in% c('planned_targeted', 'full', 'planned_full')
    ),
    width = 0.85
  ) +
  geom_text(
    aes(
      x     = retrofit_cost_remaining_usd + 600000,
      label = paste0(
        '$', format(round(retrofit_cost_remaining_usd / 1e6, 1), nsmall = 1), 'M',
        ifelse(seismic_retrofit_status %in% c('planned_targeted', 'full', 'planned_full'), '*', '')
      ),
      fontface = ifelse(shade != '1', 'plain', 'bold')
    ),
    size = 2.5
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_color_manual(values = c('TRUE' = 'orange', 'FALSE' = NA)) +
  scale_linewidth_manual(values = c('TRUE' = 1.0, 'FALSE' = 0)) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = \(x) paste0('$', x / 1e6, 'M')
  ) +
  labs(
    title    = 'Portland Elementary Seismic Retrofit Cost',
    subtitle = 'Remaining retrofit cost (USD). Retrofit planned or completed in orange.',
    x = '', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(0.9), vjust = 0.5, lineheight = 0.85))

seismic_sw_plt

ggsave(plot = seismic_sw_plt, 
       file = 'prc/seismic-sw-plt.png',
       width = 11, height = 8, 
       units = 'in', dpi = 800)


## neighborhood ####

sw_neighbor <- elem_pps_info %>%
  mutate(
    pct_neighborhood = case_when(
      has_dli == TRUE  ~ 100 * neighborhood_students_2526 / enrollment_2025_26,
      TRUE             ~ 100
    ),
    shade        = case_when(ode_school_id == 1299 ~ '1', TRUE ~ '2')
  ) %>%
  arrange(pct_neighborhood) %>%
  mutate(school_short = factor(school_short, levels = school_short))

neighbor_sw_plt <- ggplot(sw_neighbor, aes(pct_neighborhood, school_short)) +
  geom_col(aes(fill = shade), width = 0.65) +
  geom_text(
    aes(x = pct_neighborhood - 2, label = paste0(round(pct_neighborhood), '%')),
    color = 'white', hjust = 1, size = 3.5
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0')) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = 'SW Portland Elementary Neighborhood Students',
    subtitle = 'Fall 2025-26 percent of students assigned to school boundary',
    x = '', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.x = element_blank())

neighbor_sw_plt

ggsave(plot = neighbor_sw_plt, file = 'prc/neighbor-sw-plt.png',
       width = 8, height = 5.5, units = 'in', dpi = 800)


## distance traveled ####

closure_distance <- elem_pps_info %>%
  mutate(
    shade = case_when(ode_school_id == 1299 ~ '1', TRUE ~ '2')
  ) %>%
  arrange(nearest_alt_school_mi) %>%
  mutate(rank = row_number())

distance_rank_plt <- ggplot(closure_distance, aes(nearest_alt_school_mi, rank)) +
  geom_col(aes(color = shade, size = shade)) +
  # geom_text_repel(
  #   data               = \(x) filter(x, shade == '1'),
  #   aes(label          = paste0(school_short, ': ', nearest_alt_school_mi, ' mi'),
  #       color          = shade),
  #   hjust              = 0,
  #   nudge_x            = 0.15,
  #   direction          = 'y',
  #   segment.color      = 'gray70',
  #   segment.alpha      = 0.5,
  #   size               = 2.8,
  #   min.segment.length = 0
  # ) +
  # scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#B4B2A9')) +
  # scale_size_manual(values  = c('1' = 4,          '2' = 2)) +
  # scale_x_continuous(
  #   expand = expansion(mult = c(0.02, 0.35)),
  #   labels = \(x) paste0(x, ' mi')
  # ) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = 'Distance to Nearest Alternative School',
    subtitle = 'Miles to nearest alternative school for 2025-26 PPS closure candidates',
    x        = 'Miles to Nearest Alternative School',
    y        = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', axis.text.y = element_blank())

distance_rank_plt

ggsave(plot = distance_rank_plt, file = 'prc/distance-rank-plt.png',
       width = 8, height = 6, units = 'in', dpi = 800)

## new housing ####

new_housing <- elem_pps_info %>%
  filter(level %in% c('elementary','k8')) %>%
  mutate(
    shade = case_when(ode_school_id == 1299 ~ '1', ode_school_id %in% sw_pps_elem ~ '2', T ~ '3')) %>%
  arrange(desc(bli_forecast_units_within_catchment)) %>%
  mutate(rank = row_number())


new_housing_plt <- ggplot(new_housing, aes(bli_forecast_units_within_catchment, reorder(school_short,bli_forecast_units_within_catchment))) +
  geom_col(
    aes(
      fill   = shade
    ),
    width = 0.85
  ) +
  geom_text(
    aes(
      x = bli_forecast_units_within_catchment + 1200,
      label = scales::comma(round(bli_forecast_units_within_catchment)),
      fontface = ifelse(shade != '1', 'plain', 'bold')
    ),
    size = 2
  ) +
  scale_fill_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +

  labs(
    title    = 'New Housing in the Area',
    subtitle = 'BLI Estimates on New Residential Units by 2035',
    x = '', y = ''
  ) +
  theme_ipsum_pub(grid = FALSE) +
  theme(legend.position = 'none', 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(0.9), vjust = 0.5, lineheight = 0.85))

new_housing_plt

ggsave(plot = new_housing_plt, file = 'prc/new-housing-plt.png',
       width = 9, height = 7, units = 'in', dpi = 1000)

## new housing ####

enroll_forecast <- elem_pps_info %>%
  filter(ode_school_id %in% sw_pps_elem) %>%
  select(ode_school_id, school_name,enrollment_forecast_2034_35_low,enrollment_forecast_2034_35_high,enrollment_forecast_2034_35) %>%
  mutate(shade = case_when(ode_school_id == 1299 ~ '1', ode_school_id %in% sw_pps_elem ~ '2', T ~ '3'),
         school_name = gsub(" Elementary School","",school_name)) %>%
  pivot_longer(cols = contains('forecast')) %>%
  mutate(name = case_when(
    str_detect(name,'low') ~ 'low',
    str_detect(name,'high') ~ 'high',
    T ~ 'mid'
  )) %>%
  pivot_wider()
                 

enroll_forecast_plt <- ggplot(
  enroll_forecast,
  aes(y = reorder(school_name, mid), color = shade, fill = shade)
) +
  geom_boxplot(
    aes(xmin = low, xlower = low, xmiddle = mid, xupper = high, xmax = high),
    stat  = "identity",
    width = 0.6,
    alpha = 0.3
  ) +
  scale_color_manual(values = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_fill_manual(values  = c('1' = '#1B2A4A', '2' = '#A8C4E0', '3' = '#B4B2A9')) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = 'PSU 2034-35 Enrollment Forecasts',
    subtitle = 'Low, mid, and high scenarios',
    x = 'Projected enrollment', y = ''
  ) +
  theme_ipsum_pub(grid = "X") +
  theme(
    legend.position = 'none')
enroll_forecast_plt

ggsave(plot = enroll_forecast_plt, file = 'prc/enroll-forecast-plt.png',
       width = 9, height = 7, units = 'in', dpi = 1000)


# map ####
library(ggspatial)

boundaries <- st_read("raw/boundaries/PPS_AttendanceBoundaries_20260423.shp") |>
  st_transform(4326)

boundaries_sw <- boundaries %>%
  filter(K5 %in% c('Ainsworth',
                   'Bridlemile',
                   'Hayhurst',
                   'Rieke',
                   'Capitol Hill',
                   'Maplewood',
                   'Markham',
                   'Stephenson'))

sw_schools_geo <- dir %>%
  filter(school_id %in% sw_pps_elem) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 
  

map <- ggplot() +
  annotation_map_tile( type="osm", zoom = 12, quiet = TRUE) +
  geom_sf(
    data  = boundaries_sw,
    aes(fill = K5),  
    color = "white",
    linewidth = 0.5,
    alpha = 0.3
  ) +
  geom_sf(
    data  = sw_schools_geo,
    size = 2,
    shape = 21,
    fill  = "steelblue",
    color = 'white'
  ) +
  geom_sf_text(
    data  = sw_schools_geo,
    aes(label = gsub(' Elem','',school_name)),
    size  = 2.8,
    nudge_y = 400
  ) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  theme_ipsum_pub(grid = F) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
map

ggsave(plot = map, 
       "prc/pps_sw_map.png", 
       width = 5, 
       height = 8, 
       units = 'in', 
       dpi = 800)

# population density ####

age_vars <- c(
  pop_total     = "B01001_001",
  male_under5   = "B01001_003",
  male_5_9      = "B01001_004",
  male_10_14    = "B01001_005",
  female_under5 = "B01001_027",
  female_5_9    = "B01001_028",
  female_10_14  = "B01001_029"
)

bg_pop <- get_acs(
  geography = "block group",
  variables = age_vars,
  state     = "OR",
  county    = "Multnomah",
  year      = 2023,
  survey    = "acs5",
  geometry  = TRUE,
  output    = "wide"
) |>
  st_transform(4326) |>
  mutate(
    pop_school_age = male_5_9E + male_10_14E + female_5_9E + female_10_14E,
    pop_under5     = male_under5E + female_under5E,
    pop_0_14       = pop_under5 + pop_school_age,
    pct_0_14       = pop_0_14 / pop_totalE
  )


bg_sw <- bg_pop |>
  st_filter(boundaries_sw, .predicate = st_intersects)


map_with_pop <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12, quiet = TRUE) +
  geom_sf(
    data  = bg_sw,
    aes(fill = pop_0_14),
    color = NA,
    alpha = 0.5
  ) +
  geom_sf(
    data      = boundaries_sw,
    fill      = NA,
    color     = "white",
    linewidth = 0.8
  ) +
  geom_sf(
    data   = sw_schools_geo,
    size   = 3, shape = 21,
    fill   = "steelblue", color = "white", stroke = 1.2
  ) +
  geom_sf_text(
    data  = sw_schools_geo,
    aes(label = gsub(' Elem','',school_name)),
    size  = 2.8,
    nudge_y = 400
  ) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    name   = "Children\naged 0-14",
    labels = scales::comma
  ) +
  coord_sf(crs = 4326) +
  labs(
    title    = "PPS SW Portland Elementary Schools",
    subtitle = "Block group population aged 0-14 (ACS 2023 5-year)"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.margin   = margin(10, 10, 10, 10)
  )
map_with_pop

# school size to performance ####

e_all <- enroll_pps %>%
  filter(student_group == "all") %>%
  select(school_year, school_id, school_short, enroll = ct)

e_frl <- enroll_pps %>%
  filter(student_group == "direct_cert") %>%
  select(school_year, school_id, frl_pct = pct)

# school-level proficiency, elementary only, all-grade rows

pps_size_perf <- e_all %>%
  left_join(e_frl, by = c("school_year", "school_id")) %>%
  inner_join(pps_elem_prof %>% select(school_year,school_id, school_name,subject,pct_proficient), by = c("school_year", "school_id"))

pps_size_perf_wide <- pps_size_perf %>%
  pivot_wider(names_from = 'subject', values_from = 'pct_proficient')


# 2025 cross-section, run separately per subject: FRL explains most of the
# variance in each subject; enrollment adds little on its own
df25 <- pps_size_perf %>% filter(school_year == 2025)

subject_models_2025 <- df25 %>%
  group_by(subject) %>%
  group_map(~ lm(pct_proficient ~ frl_pct + enroll, data = .x), .keep = TRUE) %>%
  set_names(unique(df25$subject))

walk2(subject_models_2025, names(subject_models_2025), ~ {
  cat("---", .y, "(2025 cross-section) ---\n")
  print(summary(.x)$coefficients)
})


# residual = actual proficiency minus what FRL alone predicts
df25$resid <- resid(m_frl)

# pooled panel with year fixed effects, clustered SEs by school (needs `estimatr`)
# library(estimatr)
# m_panel <- lm_robust(prof_avg ~ frl_pct + enroll + factor(school_year),
#                       data = pps_size_perf, clusters = school_short)
# summary(m_panel)

# chart: enrollment vs FRL-adjusted residual, Rieke highlighted
p <- ggplot(pps_size_perf, aes(x = enroll, y = resid)) +
  geom_point(aes(color = school_short == "Rieke",
                 size = school_short == "Rieke")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "grey40"), guide = "none") +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
  labs(title = "School size vs. proficiency, net of FRL (PPS elementary, 2025)",
       subtitle = "Flat pattern: enrollment adds little once poverty rate is accounted for",
       x = "Enrollment", y = "Residual proficiency (points vs. FRL-predicted)") +
  theme_ipsum_pub()

ggsave("prc/size-perf-frl-resid-plt.png", p, width = 8, height = 5.5, dpi = 300)

# school size versus spending ####
pps_size_spend_perf <- pps_size_perf %>%
  filter(school_year == 2025) %>%
  left_join(pps_fin_rank %>% select(school_id,school_short,school_year,total_exp, per_pupil_exp), by = c('school_id','school_year'))

pps_size_spend_perf_clean <- pps_size_spend_perf %>%
  filter(!school_short.x %in% c("Whitman", "Clark")) %>%
  distinct(school_id, school_short.x, enroll, per_pupil_exp)

m_size_cost <- lm(per_pupil_exp ~ enroll, data = pps_size_spend_perf_clean)
summary(m_size_cost)

# naive size -> cost relationship (what we ran last time, FRL left out)
m_cost_naive <- lm(per_pupil_exp ~ enroll, data = pps_size_spend_perf)
cat("--- cost ~ enroll only ---\n")
print(summary(m_cost_naive)$coefficients)
cat("R2:", summary(m_cost_naive)$r.squared, "\n\n")

# does size still predict cost once FRL is controlled for?
m_cost_frl <- lm(per_pupil_exp ~ enroll + frl_pct, data = pps_size_spend_perf)
cat("--- cost ~ enroll + frl_pct ---\n")
print(summary(m_cost_frl)$coefficients)
cat("R2:", summary(m_cost_frl)$r.squared, "\n\n")

m_perf <- lm(pct_proficient ~ frl_pct + enroll + per_pupil_exp, data = pps_size_spend_perf %>% filter(subject == 'math'))
cat("--- prof_avg ~ frl_pct + enroll + per_pupil_exp ---\n")
print(summary(m_perf)$coefficients)
cat("R2:", summary(m_perf)$r.squared, "\n\n")

# edunomics scatter ####

