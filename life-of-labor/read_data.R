library(tidyverse)
library(readxl)
library(ggridges)
library(quantmod)

# Design parameters ------------------------------------
color1 <- "#DDCC77"
color2 <- "#88CCEE"
color3 <- "#44AA99"
color4 <- "#117733"
color5 <- "#332288"
color6 <- "#CC6677"
color7 <- "#AA4499"
color8 <- "#882255"
color9 <- "#e6e6e6" # grey10
color_pal <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9)
filler <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Eget felis eget nunc lobortis. Viverra nam libero justo laoreet sit amet cursus sit amet. Gravida quis blandit turpis cursus in hac habitasse platea. Id interdum velit laoreet id donec ultrices tincidunt. Diam ut venenatis tellus in metus. Donec adipiscing tristique risus nec feugiat in fermentum posuere urna. Penatibus et magnis dis parturient montes nascetur. Orci a scelerisque purus semper. Nisi vitae suscipit tellus mauris a diam maecenas sed enim. Leo vel fringilla est ullamcorper eget nulla facilisi etiam. Sed arcu non odio euismod. Turpis egestas maecenas pharetra convallis posuere morbi. At volutpat diam ut venenatis tellus in metus vulputate. Morbi tincidunt ornare massa eget. Enim sit amet venenatis urna cursus eget nunc."
names_m <- c("Juan", "Jose", "Luis", "Carlos", "Mario", "Jorge", "Victor", "Miguel", "Pedro", "Antonio", "Fernando", "Roberto", "Felix", "Julio")
names_w <- c("Maria", "Juana", "Ana", "Martha", "Carmen", "Rosa", "Julia", "Elizabeth", "Cristina", "Lidia", "Patricia", "Sonia", "Isabel", "Victoria")

# Summary: representative ages
a1 <- 16.5  # working
a2 <- 23  # in school
a3 <- 35  # paid work
a4 <- 45  # earning X bolivianos per month
a5 <- 65  # retired


# Read data ----------------------------------------------------
personas <- read.csv("data/personas.csv")

educ_yrs <- c(0, 0, #11, 12
              0, #13
              6,11, #21 22
              11, 6, 11, #23, 31, 32,
              6, 11,  #41, 42
              6, 11,  #51, 52
              11,6,  #61, 62
              11,6,  #63, 64
              11, #65
              16, 16, 16, #71, 72, 73
              16, 16, 16, #74, 75, 76
              16, 16, 16, # 77, 79, 80
              16) #81

educ_list <- sort(unique(personas$edu))

personas <- personas %>%
  mutate(yrs_educ = plyr::mapvalues(edu, educ_list, educ_yrs)) %>% 
  mutate(why_not_work = s06a_10)

# Segment by age groups ----------------------------------------------

# Children
children <- personas %>%
  filter(age < 18) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence, education, is_student,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         emp_status,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct, pc_inc, size, rest_of_hh,
         factor) %>%
  mutate(edu_status = case_when(  # children 0-3 are all NA
    startsWith(in_school, "1") & startsWith(in_attendance, "1") ~ "enrolled,\nattending",
    startsWith(in_school, "1") & startsWith(in_attendance, "2") ~ "enrolled,\nnot attending",
    startsWith(in_school, "2") ~ "not\nenrolled"
  ))


# Youth
youth <- personas %>%
  filter(age %in% 18:24) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence, education, is_student,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         emp_status,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct, pc_inc, size, rest_of_hh,
         factor) %>%
  mutate(edu_status = case_when(  # children 0-3 are all NA
    startsWith(in_school, "1") & startsWith(in_attendance, "1") ~ "enrolled,\nattending",
    startsWith(in_school, "1") & startsWith(in_attendance, "2") ~ "enrolled,\nnot attending",
    startsWith(in_school, "2") ~ "not\nenrolled"
  ))


# Adults
adults <- personas %>%
  filter(age %in% 25:60) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence, education, is_student,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         emp_status,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct, pc_inc, size, rest_of_hh,
         factor, looked_for_work,s06a_10) %>%
  mutate(paid = ifelse(str_detect(work_type, "^[78]"), "unpaid", "paid"))


# Older adults
older <- personas %>%
  filter(age > 60) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence, education, is_student,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         emp_status,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct, pc_inc, size, rest_of_hh,
         factor) %>%
  mutate(paid = ifelse(str_detect(work_type, "^[78]"), "unpaid", "paid"))


# Segment by employment status -------------------------------
employed <- adults %>% filter(emp_status == "Employed") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

unemployed <-  adults %>% filter(emp_status == "Unemployed") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

inactive <- adults %>% filter(emp_status == "Inactive") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

neet <- personas %>% filter(emp_status == "Inactive" & is_student == "No") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, why_not_in_school, why_not_work, factor)  %>% filter(age %in% 14:30)

ages_neet <- personas %>% filter(age %in% 14:30) %>%
  mutate(neet_cat = case_when(emp_status == "Inactive" & is_student == "No" ~ "NEET",
                              emp_status == "Inactive" & is_student == "Yes" ~ "Only studies",
                              emp_status != "Inactive" & is_student == "Yes" ~ "Works and studies",
                              emp_status != "Inactive" & is_student == "No" ~ "Only works",
  ))

employed_gender <- employed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

inactive_gender <- inactive %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

unemp_gender <- unemployed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor)) 

emp_per <- adults %>% filter(!is.na(emp_status)) %>% group_by(emp_status, sex) %>% summarize(sum_peep = sum(factor, na.rm = T)) %>%
  mutate(sum_peep = round(sum_peep/ 20000,0))

# Other data frames -------------------------------------
why_not_in_school_df <- children %>%
  filter(!is.na(why_not_in_school)) %>%
  mutate(why_not_in_school = case_when(startsWith(why_not_in_school, "14") ~ "reasons not\nlisted in survey",
                                       startsWith(why_not_in_school, "11") ~ "work",
                                       startsWith(why_not_in_school, "2") ~ "illness,\naccident,\ndisability",
                                       startsWith(why_not_in_school, "3") ~ "pregnancy",
                                       startsWith(why_not_in_school, "4") ~ "lack of money",
                                       startsWith(why_not_in_school, "5") ~ "school is\ntoo far",
                                       startsWith(why_not_in_school, "8") ~ "lack of\ninterest",
                                       startsWith(why_not_in_school, "9") ~ "household chores/\nchildcare",
                                       !is.na(why_not_in_school) ~ "everything\nelse"),
         sex = case_when(startsWith(sex, "1") ~ "boys",
                         startsWith(sex, "2") ~ "girls")) %>%
  group_by(why_not_in_school, sex) %>%
  summarize(sum = sum(factor))

children_depto <- children %>%
  filter(age > 6) %>%
  group_by(depto, area, sex) %>%
  summarize(cw = sum((!is.na(lab_monthly_inc)) * factor) / sum(factor) * 100, tot = sum(factor),
            school = sum((in_school == "2. No") * factor) / sum(factor) * 100)

why_neet_no_study <- neet %>%
  filter(!is.na(why_not_in_school)) %>%
  mutate(why_not_in_school = case_when(startsWith(why_not_in_school, "14") ~ "reasons not\nlisted in survey",
                                       startsWith(why_not_in_school, "11") ~ "work",
                                       startsWith(why_not_in_school, "2") ~ "illness,\naccident,\ndisability",
                                       startsWith(why_not_in_school, "3") ~ "pregnancy",
                                       startsWith(why_not_in_school, "4") ~ "lack of money",
                                       startsWith(why_not_in_school, "5") ~ "school is\ntoo far",
                                       startsWith(why_not_in_school, "8") ~ "lack of\ninterest",
                                       startsWith(why_not_in_school, "9") ~ "household chores/\nchildcare",
                                       !is.na(why_not_in_school) ~ "everything\nelse"),
         sex = case_when(startsWith(sex, "1") ~ "Men",
                         startsWith(sex, "2") ~ "Women")) %>%
  group_by(why_not_in_school, sex) %>%
  summarize(sum = sum(factor))

why_neet_no_work <- neet %>%
  filter(!is.na(why_not_work)) %>%
  mutate(why_not_work = case_when(startsWith(why_not_work, "10") ~ "doesn't neet to work",
                                  startsWith(why_not_work, "11") ~ "household chores/\nchildcare",
                                  startsWith(why_not_work, "9") ~ "illness,\naccident,\ndisability",
                                  startsWith(why_not_work, "13") ~ "reasons not\nlisted in survey",
                                  !is.na(why_not_work) ~ "everything\nelse"),
         sex = case_when(startsWith(sex, "1") ~ "Men",
                         startsWith(sex, "2") ~ "Women")) %>%
  group_by(why_not_work, sex) %>%
  summarize(sum = sum(factor))

neets_waff <- neet  %>% group_by(sex) %>% summarize(sum_peep = sum(factor, na.rm = T)) %>%
  mutate(sum_peep = round(sum_peep/ 1000,0))

adults_unpaid_worth <- adults %>%
  filter(paid == "unpaid") %>%
  mutate(unpaid_worth = hh_lab_inc * hh_hr_pct / 100 * factor) %>%
  group_by(sex) %>%
  summarize(sum = sum(unpaid_worth))

adults_unpaid_worth_ru <- adults %>%
  mutate(paid = paid == "unpaid", unpaid_hr = tot_work_week_hr * paid) %>%
  group_by(folio) %>%
  summarize(area = first(area), factor = first(factor), hr = first(hh_hr), inc = first(hh_lab_inc), unpaid_hr = sum(unpaid_hr, na.rm = T),
            unpaid_inc = ifelse(hr == 0, 0, inc * unpaid_hr / hr)) %>%
  group_by(area) %>%
  summarize(tot_inc = sum(inc * factor), tot_unpaid_inc = sum(unpaid_inc * factor), unpaid_pct = round(tot_unpaid_inc / tot_inc * 100, 1))

adults_pay_depto <- adults %>%
  filter(!is.na(paid)) %>%
  group_by(depto, area) %>%
  summarize(mean_unpaid = sum((paid == "unpaid") * factor) / sum(factor) * 100) %>%
  pivot_wider(names_from = area, values_from = mean_unpaid) %>%
  arrange(Rural)
depto_ru <- adults_pay_depto$depto

adults_marital <- adults %>%
  filter(!is.na(paid)) %>%
  mutate(marital = as.numeric(substr(marital, 1, 1))) %>%
  mutate(marital = case_when(marital == 1 ~ "single", marital == 2 ~ "married", marital == 3 ~ "cohabiting",
                             marital == 4 ~ "separated", marital == 5 ~ "divorced", marital == 6 ~ "widowed")) %>%
  group_by(marital, sex) %>%
  summarize(mean_unpaid = sum((paid == "unpaid") * factor) / sum(factor) * 100) %>%
  separate(sex, into = c(NA, "sex"), sep = 2, remove = T) %>%
  pivot_wider(names_from = sex, values_from = mean_unpaid, names_sep = "_") %>%
  arrange(Mujer)
marital_sex <- adults_marital$marital

child_worker <- personas %>%
  filter(!is.na(primary_job)) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct) %>%
  filter(age < 15)
