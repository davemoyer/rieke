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
library(janitor)
library(here)

# data 

directory <- read_csv('prc/directory.csv')

dir <- directory %>%
  select(-school_name,-district_name) %>%
  mutate(district_id = as.numeric(district_id))

grades_served <- read_csv('prc/grades-served.csv')

enroll <- read_csv('prc/enroll.csv')
analysis <- read_csv('prc/attend_tests_funding.csv')
tests_long <- read_csv('prc/full_tests.csv') %>%
  rename(n_participants = n_tested)

full_enroll <- enroll %>%
  filter(level == 'school') %>%
  left_join(dir, by = c('district_id','school_id'))

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

# palette ####
rieke_colors <- c(
  navy   = "#1B2A4A",
  red    = "#C0392B",
  gold   = "#E8A020",
  white  = "#F5F5F5",
  gray   = "#B4B2A9"
)


# enrollment change ####
district_enroll_all <- enroll %>%
  filter(str_detect(district_name, 'Portland|Beaverton') & level == 'district') %>%
  filter(grade == 'all' & student_group == 'all') %>%
  select(district_id:school_year,fall_ct, spring_ct)

sw_elem_enroll_all <- full_enroll %>%
  filter((school_id %in% sw_pps_elem) & 
           grade == 'all' & student_group == 'all') %>%
  select(district_id:school_year, fall_ct, spring_ct) %>%
  mutate(
    shade = case_when(
      district_name == 'Beaverton SD 48J'          ~ '3',
      school_name   == 'Rieke Elementary School'   ~ '1',
      district_name == 'Portland SD 1J'            ~ '2'
    ),
    school_name = gsub(' Elementary School', '', school_name)
  ) 

enroll_change <- sw_elem_enroll_all %>%
  filter(school_year %in% c(2019,2026)) %>%
  arrange(school_id,school_year) %>%
  group_by(school_id) %>%
  mutate(enroll_change = fall_ct-lag(fall_ct),
         enroll_change_pct = 100*(fall_ct-lag(fall_ct))/lag(fall_ct)) %>%
  select(school_id,school_year,enroll_change,enroll_change_pct) %>%
  filter(school_year == 2026)

sw_elem_enroll_all_with_change <- sw_elem_enroll_all %>%
  left_join(enroll_change, by = c('school_id','school_year'))

enroll_change_plt <- ggplot(sw_elem_enroll_all_with_change, aes(school_year, fall_ct, group = school_id)) +
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data        = \(x) slice_max(x, school_year, n = 1, by = school_id),
    aes(label   = paste0(school_name, ": ", fall_ct, ' (',round(enroll_change_pct),'%)'), color = shade),
    hjust       = 0,
    nudge_x     = 0.2,
    direction   = "y",
    segment.color = NA
  ) +
  geom_text_repel(
    data        = \(x) slice_min(x, school_year, n = 1, by = school_id),
    aes(label   = fall_ct, color = shade),
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
  ylim(240,630) +
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
  

# enrollment breakdown ####
district_enroll_group <- enroll %>%
  filter(student_group %in% c('asian','hispanic','white','black','dis','swd') &
           district_name == 'Portland SD 1J' & grade == 'all' & school_year == 2025 & level == 'district') %>%
  select(district_id:school_year,student_group,spring_pct) %>%
  mutate(school_name = 'PPS')

sw_pps_enroll_group <- full_enroll %>%
    filter(school_id %in% sw_pps_elem & school_year == 2025 & grade == 'all' & 
             student_group %in% c('all','asian','hispanic','white','black','dis','swd')) %>%
    filter(school_id != 1299) %>% # pull out Rieke
    select(district_id:school_year,student_group,spring_ct) %>%
    pivot_wider(names_from = student_group, values_from = spring_ct) %>%
    summarise(across(c(all:dis), ~sum(.x))) %>%
  mutate(across(c(asian:dis), ~round(100*(.x/all)))) %>%
  pivot_longer(c(asian:dis), names_to = 'student_group',values_to = 'spring_pct') %>%
  mutate(school_name = 'Other SW Elem') %>%
  select(-all)

rieke_enroll_group <- full_enroll %>%
  filter(school_id == 1299 & school_year == 2025 & grade == 'all' & 
           student_group %in% c('asian','hispanic','white','black','dis','swd')) %>%
  select(school_name,student_group,spring_pct) %>%
  mutate(school_name = 'Rieke',
         student_group = case_when(
           student_group == 'swd' ~ 'Students with Disabilities',
           student_group == 'dis' ~ 'Economically Disadvantaged',
           T ~ str_to_title(student_group)
         ),
         student_group = factor(student_group, levels = c('Asian',
                                                          'Black',
                                                          'Hispanic',
                                                          'White',
                                                          'Economically Disadvantaged',
                                                          'Students with Disabilities'))
         )
  
ref_enroll_group <- bind_rows(sw_pps_enroll_group,district_enroll_group) %>%
  select(school_name,student_group,spring_pct) %>%
  mutate(student_group = case_when(
    student_group == 'swd' ~ 'Students with Disabilities',
    student_group == 'dis' ~ 'Economically Disadvantaged',
    T ~ str_to_title(student_group)
  ),
  student_group = factor(student_group, levels = c('Asian',
                                                   'Black',
                                                   'Hispanic',
                                                   'White',
                                                   'Economically Disadvantaged',
                                                   'Students with Disabilities')))

enroll_group_plt <- ggplot(rieke_enroll_group, aes(spring_pct, student_group)) +
  geom_col(fill = "#1D9E75", width = 0.6) +
  geom_errorbar(
    data     = ref_enroll_group,
    aes(x    = spring_pct,
        xmin = spring_pct,
        xmax = spring_pct,
        color = school_name),
    width     = 0.6,
    linewidth = 0.9
  ) +
  geom_text(aes(x = ifelse(spring_pct >=10, spring_pct-3,2), label = paste0(spring_pct,'%')),
            color = 'white') +
  scale_color_manual(values = c(
    "Other SW Elem" = "black",
    "PPS"   = "grey"
  )) +
  labs(
    title = 'Spring 2025 Enrollment by Student Group'
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
  mutate(school_name = factor(school_name, levels = c('PPS','Other SW Elem','Rieke')))

enroll_group_grouped_plt <- ggplot(
  combined_enroll_group,
  aes(spring_pct, student_group, fill = school_name)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(x = spring_pct + 2, label = paste0(spring_pct, "%"), color = school_name),
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
  labs(title = "Spring 2025 Enrollment by Student Group") +
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
    fontface = "bold"
  ) +
  geom_text(
    data     = \(x) filter(x, shade %in% c("1", "2")),
    aes(label = paste0(round(pct_proficient,1), "%"), color = shade),
    hjust    = 0,
    nudge_y  = 1,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values  = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_color_manual(values = c("1" = "#1B2A4A", "2" = "#A8C4E0", "3" = "#B4B2A9")) +
  scale_y_continuous(limits = c(-25, 100)) +
  facet_wrap(~subject_label) +
  labs(
    title    = "Rieke was the top PPS elementary school in 2024-2025",
    subtitle = "2024-25 proficiency on Oregon state assessments",
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
