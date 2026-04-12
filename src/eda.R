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

enroll <- read_csv('prc/enroll.csv')
analysis <- read_csv('prc/attend_tests_funding.csv')

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
  filter(school_year %in% c(2019,2025)) %>%
  arrange(school_id,school_year) %>%
  group_by(school_id) %>%
  mutate(enroll_change = fall_ct-lag(fall_ct),
         enroll_change_pct = 100*(fall_ct-lag(fall_ct))/lag(fall_ct)) %>%
  select(school_id,school_year,enroll_change,enroll_change_pct) %>%
  filter(school_year == 2025)

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
  scale_x_continuous(limits = c(2018.75,2027),
                     breaks = 2019:2025,
                     labels = 2019:2025) +
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
  width = 8,
  height = 5.5,
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

rieke_prof <- analysis %>%
  filter(str_detect(school_name,'Rieke') & grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

sw_pps_prof <- analysis %>%
  filter(school_id %in% sw_pps_elem & grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

pps_elem_prof <- analysis %>%
  filter(str_detect(school_name, 'Elementary') &
           district_name == 'Portland SD 1J' &
           grade == 'all' & student_group == 'all') %>%
  select(district_id:school_name,
         school_year,
         subject,
         n_proficient,
         n_participants,
         pct_proficient,
         pct_level_3,
         pct_level_4)

state_elem_prof <- analysis %>%
  filter(str_detect(school_name, 'Elementary') &
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
  geom_line(aes(color = shade)) +
  geom_point(aes(color = shade)) +
  geom_text_repel(
    data          = \(x) slice_max(x, school_year, n = 1, by = c(label, subject_label)),
    aes(label     = paste0(label, ': ', round(pct_proficient), '%'), color = shade),
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
    title    = 'Rieke was the top PPS Elementary School in 2024-2025',
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
    title    = 'Rieke was the third best Elementary School in Oregon in 2024-2025',
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



