# Plots and functions ------------------------------------------

# Children ------------------------------
children_1_p <- function(df) {
  df %>%
    filter(age > 6) %>%
    group_by(age, sex) %>%
    summarize(tot = sum(factor), in_school = sum((in_school == "1. Si") * factor) / tot * 100, job = sum((!is.na(primary_job)) * factor) / tot * 100) %>%
    ggplot() +
    geom_line(aes(age, in_school), color = color1, size = 1) +
    geom_text(aes(12, 85, label = "% of children who are in school"), color = color1) +
    geom_line(aes(age, job), color = color2, size = 1) +
    geom_text(aes(12, 25, label = "% of children\nwho are working"), color = color2) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(7, 17, 2)) +
    ylab("")
}

children_edu4_p <- function(df) {
  ggplot(df) +
    geom_col(aes(x = sum / 1000, y = why_not_in_school, fill = sex), position = "dodge", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    ylab("") + xlab("population (thousand)")
}

children_ses_depto <- function(df, x_var, x_lab) {
  ggplot(df) +
    geom_col(aes(.data[[x_var]], depto, fill = sex), position = "dodge", width = 0.5) +
    facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    labs(x = x_lab, y = "", fill = "")
}

children_ses1_inc <- function(df) {
  df %>%
    filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0) %>%
    ggplot() +
    geom_point(aes(hh_tot_inc + 1, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
    geom_smooth(aes(hh_tot_inc + 1, hh_lab_inc_pct, color = sex), alpha = 0.1) +
    geom_text(aes(120, 75, label = "circle size =\nweekly work hours"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(trans = "log10") +
    ylim(-15, 100) +
    scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    scale_size(range = c(0.1, 10), guide = F) +
    labs(x = "household monthly income (BOB)", y = "contribution to household labor income (%)", color = "average")
}

children_ses2_inc <- function(df) {
  df %>%
    filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0) %>%
    ggplot() +
    geom_jitter(aes(hh_tot_inc + 1, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.15) +
    geom_smooth(aes(hh_tot_inc + 1, hh_hr_pct, color = sex), alpha = 0.1) +
    geom_text(aes(120, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(trans = "log10") +
    scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    scale_size(range = c(0.1, 30), guide = F) +
    labs(x = "household monthly income (BOB)", y = "share of household work hours (%)", color = "average")
}

ses1_p <- function(df, var, labels) {
  df %>%
    filter(!is.na(lab_monthly_inc)) %>%
    group_by(age, .data[[var]]) %>%
    summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor),
              mean_inc = sum(lab_monthly_inc * factor) / sum(factor),
              mean_pct = sum(hh_lab_inc_pct * factor) / sum(factor)) %>%
    ggplot() +
    geom_jitter(data = df %>% filter(!is.na(lab_monthly_inc)),
                aes(x = age, y = hh_lab_inc_pct, size = tot_work_week_hr, color = .data[[var]]), alpha = 0.15) +
    geom_line(aes(y = mean_pct, x = age, color = .data[[var]]), size = 1) +
    geom_point(aes(y = mean_pct, x = age, size = mean_hr, color = .data[[var]])) +
    geom_text(aes(7, 85, label = "circle size =\nweekly work hours"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(7, 17.8)) +
    scale_color_manual(values = c(color1, color2), labels = labels) +
    scale_size(range = c(0.1, 10), guide = F) +
    labs(y = "contribution to household labor income (%)", color = "average")
}

ses2_p <- function(df, var, labels) {
  df %>%
    filter(!is.na(lab_monthly_inc)) %>%
    group_by(age, .data[[var]]) %>%
    summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor),
              mean_inc = sum(lab_monthly_inc * factor) / sum(factor),
              mean_pct = sum(hh_hr_pct * factor) / sum(factor)) %>%
    ggplot() +
    geom_jitter(data = df %>% filter(!is.na(lab_monthly_inc)),
                aes(x = age, y = hh_hr_pct, size = lab_monthly_inc, color = .data[[var]]), alpha = 0.15) +
    geom_line(aes(y = mean_pct, x = age, color = .data[[var]]), size = 1) +
    geom_point(aes(y = mean_pct, x = age, size = mean_inc, color = .data[[var]])) +
    geom_text(aes(7, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(7, 17.8)) +
    scale_color_manual(values = c(color1, color2), labels = labels) +
    scale_size(range = c(0.1, 30), guide = F) +
    labs(y = "share of household work hours (%)", color = "average")
}

children_lfp1_p <- function(df) {
  ggplot(df %>% filter(!is.na(lab_monthly_inc))) +
    geom_jitter(aes(x = age, y = tot_work_week_hr, size = lab_monthly_inc, color = sex), alpha = 0.15) +
    geom_line(data = . %>% group_by(age, sex) %>%
                summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor), mean_inc = sum(lab_monthly_inc * factor) / sum(factor)),
              aes(y = mean_hr, x = age, color = sex), size = 1) +
    geom_point(data = . %>% group_by(age, sex) %>%
                 summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor), mean_inc = sum(lab_monthly_inc * factor) / sum(factor)),
               aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
    geom_text(aes(7, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(7, 17.8)) +
    scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    scale_size(range = c(0.1, 30), guide = F) +
    labs(y = "weekly work hours", color = "")
}

children_lfp2_p <- function(df) {
  ggplot(df %>% filter(!is.na(lab_monthly_inc))) +
    geom_jitter(aes(x = age, y = tot_work_week_hr, size = lab_monthly_inc / tot_work_week_hr / 4.33, color = sex), alpha = 0.15) +
    geom_line(data = . %>% group_by(age, sex) %>%
                summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor),
                          mean_inc = sum(lab_monthly_inc / tot_work_week_hr / 4.33 * factor) / sum(factor)),
              aes(y = mean_hr, x = age, color = sex), size = 1) +
    geom_point(data = . %>% group_by(age, sex) %>%
                 summarize(mean_hr = sum(tot_work_week_hr * factor) / sum(factor),
                           mean_inc = sum(lab_monthly_inc / tot_work_week_hr / 4.33 * factor) / sum(factor)),
               aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
    geom_text(aes(7, 85, label = "circle size =\nhourly income (BOB)"), color = "grey", hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(7, 17.8)) +
    scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
    scale_size(range = c(0.1, 30), guide = F) +
    labs(y = "weekly work hours", color = "")
}

# Youth ---------------------------------------------
youth_overview_p <- function(df) {
  df %>%
    group_by(age, sex) %>%
    summarize(tot = sum(factor), in_school = sum((in_school == "1. Si") * factor) / tot * 100, job = sum((!is.na(primary_job)) * factor) / tot * 100) %>%
    ggplot() +
    geom_line(aes(age, in_school), color = color1, size = 1) +
    geom_text(aes(20.5, 70, label = "% of youth\nwho are in school"), color = color1) +
    geom_line(aes(age, job), color = color2, size = 1) +
    geom_text(aes(19, 30, label = "% of youth\nwho are working"), color = color2) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    ylim(0, 100) +
    ylab("")
}

youth_edu_imp_p <- read_csv("data/youth_ses_rf1_imp.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

youth_edu1_p <- function(df, var, labels) {
  df2 <- df %>%
    group_by(age, sex, .data[[var]]) %>%
    summarize(mean = sum((in_school == "1. Si") * factor) / sum(factor) * 100)
  
  n <- length(unique(df2[[var]]))
  
  ggplot(df2) +
    geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_pal[1:n], labels = labels) +
    labs(y = "% student", color = "") + ylim(0, 100)
}

youth_edu1_internet <- function(df) {
  df %>%
    mutate(decile = cut(pc_inc, 
                        breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                        include.lowest = T)) %>%
    group_by(sex, decile, internet_use) %>%
    summarize(mean = sum((in_school == "1. Si") * factor) / sum(factor) * 100) %>%
    mutate(decile = as.numeric(decile)) %>%
    ggplot() +
    geom_line(aes(decile, mean, color = internet_use), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_pal[1:2], labels = c("with Internet", "without Internet")) +
    scale_x_continuous(breaks = 1:10) +
    labs(x = "per capita household income decile", y = "% student", color = "") + ylim(0, 100)
}

youth_edu1_depto <- function(df) {
  df %>%
    group_by(depto, area, sex) %>%
    summarize(mean = sum((in_school == "1. Si") * factor) / sum(factor) * 100) %>%
    ggplot() +
    geom_col(aes(mean, depto, fill = sex), position = "dodge", width = 0.5) +
    facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    labs(x = "% student", y = "", fill = "")
}

youth_edu1_lang <- function(df) {
  df %>%
    mutate(language_1 = ifelse(language_1 %in% c("QUECHUA", "CASTELLANO"), language_1, "OTHER"),
           indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)) %>%
    group_by(language_1, sex, indigenous) %>%
    summarize(mean = sum((in_school == "1. Si") * factor) / sum(factor) * 100) %>%
    ggplot() +
    geom_col(aes(mean, language_1, fill = sex), position = "dodge", width = 0.5) +
    facet_wrap(vars(indigenous), labeller = labeller(indigenous = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not indigenous"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    labs(x = "% student", y = "", fill = "")
}

youth_edu2_p <- function(df) {
  ggplot(df) +
    geom_bar(aes(age, fill = education), position = "fill") +
    scale_fill_manual(values = c(color1, color2, color3, color4)) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylab("proportion")
}

youth_edu_emp_sex_p <- function(df) {
  ggplot(df) +
    geom_bar(aes(age, fill = sex), width = 0.5, position = "fill") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    ylab("proportion")
}

youth_emp_imp_p <- read_csv("data/youth_ses_rf2_imp.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

youth_emp1_p <- function(df, var, labels) {
  df2 <- df %>%
    group_by(age, sex, .data[[var]]) %>%
    summarize(mean = sum((!is.na(primary_job)) * factor) / sum(factor) * 100)
  
  n <- length(unique(df2[[var]]))
  
  ggplot(df2) +
    geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_pal[1:n], labels = labels) +
    labs(y = "% working", color = "") + ylim(0, 100)
}

youth_inc_paid_p <- function(df) {
  df %>%
    filter(!is.na(primary_job)) %>%
    mutate(paid = ifelse(str_detect(work_type, "^[78]"), "unpaid", "paid")) %>%
    group_by(age, sex) %>%
    summarize(mean = sum((paid == "unpaid") * factor) / sum(factor) * 100) %>%
    ggplot() +
    geom_line(aes(age, mean, color = sex), size = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
    ylab("% unpaid worker")
}

youth_inc_inc_p <- function(df) {
  df %>%
    filter(!is.na(lab_monthly_inc) & lab_monthly_inc != 0) %>%
    group_by(age, sex) %>%
    summarize(mean = sum(lab_monthly_inc * factor) / sum(factor), mean_hr = sum(tot_work_week_hr * factor) / sum(factor)) %>%
    ggplot() +
    geom_jitter(data = youth %>% filter(!is.na(lab_monthly_inc) & lab_monthly_inc != 0),
                aes(age, lab_monthly_inc, color = sex, size = tot_work_week_hr), alpha = 0.08, width = 0.5) +
    geom_line(aes(age, mean, color = sex), size = 1) +
    geom_point(aes(age, mean, color = sex, size = mean_hr)) +
    geom_text(aes(18, 8500, label = "circle size =\nweekly work hours"), color = "grey") +
    theme_minimal() +
    theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
    scale_size_continuous(range = c(0.1, 8), guide = F) +
    ylim(0, 10000) +
    labs(y = "Monthly labor income (BOB)", color = "average")
}

# Adults ---------------------------------------------
wfl_plot <- function(df,people) {
  waffle(df$total_pop/people, rows=4, size=0.5, 
         colors=c("#DDCC77", "#88CCEE"),
         xlab=paste0("1 sq. = ", as.integer(people/1000), " k people"))
}

ridge_educ <- function(df) {
  df %>%
    mutate(educFct = fct_rev(as.factor(education))) %>%
    ggplot(aes(y = educFct)) +
    geom_density_ridges(
      aes(x = age, fill = paste(educFct, sex)), 
      alpha = .8, color = "white", from = 25, to = 60
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_cyclical(
      breaks = c("Less than Primary 1.Hombre", "Less than Primary 2.Mujer"),
      labels = c(`Less than Primary 1.Hombre` = "male", `Less than Primary 2.Mujer` = "female"),
      values = c("#DDCC77", "#88CCEE")
    ) +
    coord_cartesian(clip = "off") +
    theme_ridges(grid = TRUE) +
    theme(axis.text.y = element_text(angle = 70, hjust = 1, vjust = 0.5))
}

ridge_indigen <- function(df) {
  df %>% filter(indigenous!= "3. No soy boliviana o boliviano") %>%
    mutate(educFct = fct_rev(as.factor(indigenous))) %>%
    ggplot(aes(y = educFct)) +
    geom_density_ridges(
      aes(x = age, fill = paste(educFct, sex)), 
      alpha = .8, color = "white", from = 25, to = 60
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_cyclical(
      breaks = c("Pertenece 1.Hombre", "Pertenece 2.Mujer"),
      labels = c(`Pertenece 1.Hombre` = "male", `Pertenece 2.Mujer` = "female"),
      values = c("#DDCC77", "#88CCEE"),
      name = "Gender", guide = "legend"
    ) +
    coord_cartesian(clip = "off") +
    theme_ridges(grid = TRUE)
}

ridge_urban <- function(df) {
  df %>%
    mutate(educFct = fct_rev(as.factor(area))) %>%
    ggplot(aes(y = educFct)) +
    geom_density_ridges(
      aes(x = age, fill = paste(educFct, sex)), 
      alpha = .8, color = "white", from = 25, to = 60
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_cyclical(
      breaks = c("Urbana 1.Hombre", "Urbana 2.Mujer"),
      labels = c(`Urbana 1.Hombre` = "male", `Urbana 2.Mujer` = "female"),
      values = c("#DDCC77", "#88CCEE")
    ) +
    coord_cartesian(clip = "off") +
    theme_ridges(grid = TRUE)
}

age_lfp <- function(df) {
  ggplot(df) +
    geom_density(aes(x = age, fill = sex), position = "dodge", width = 0.5, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values=c("#DDCC77", "#88CCEE"))
}

area_chart_sex <- function(df) {
  ggplot(df) +
    geom_bar(aes(age, fill = education), position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = c(color1, color2, color3, color4)) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylab("proportion")
}

waffl_work <- function(df) {
  ggplot(df, aes(fill = emp_status, values = sum_peep)) +
    geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
    facet_wrap(~sex, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_manual(
      name = NULL,
      values = c(color3, color7, color4)
    ) +
    coord_equal() +
    labs(
      x = "Gender",
      y = "Adults aged 25-60 (1 tile = 20k)"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = FALSE)) 
}

hours_worked_graph <- function(df) {
  ggplot(df %>% filter(emp_status == "Employed")) +
    geom_jitter(aes(x = age, y = tot_work_week_hr, color = sex), alpha = 0.05) +
    geom_line(data = df %>% filter(emp_status == "Employed") %>% group_by(age, sex) %>%
                summarize(mean = sum(tot_work_week_hr * factor, na.rm = T) / sum(factor)),
              aes(x = age, y = mean, color = sex), size = 1) +
    geom_point(data = df %>% filter(emp_status == "Employed") %>% group_by(age, sex) %>%
                 summarize(mean = sum(tot_work_week_hr * factor, na.rm = T) / sum(factor)),
               aes(x = age, y = mean, color = sex), size = 2.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
    ylab("average hours worked") +
    ylim(5, 60)
}

plot_bars_neet <- function(df) {
  ggplot(df) +
    geom_col(aes(x = sum/1000, y = why_not_work, fill = sex), position = "dodge", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
    ylab("") + xlab("population (thousands)") + scale_x_continuous(limits = c(0,300))
}

plot_bars_neet_study <- function(df) {
  ggplot(df) +
    geom_col(aes(x = sum/1000, y = why_not_in_school, fill = sex), position = "dodge", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
    ylab("") + xlab("population(thousands)")  + scale_x_continuous(limits = c(0,300))
}

area_neet_cat_sex <- function(df) {
  ggplot(df) +
    geom_bar(aes(age, fill = neet_cat), position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = c(color1, color2, color3, color4)) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylab("proportion") 
}

neets_rfplot <- read_csv("data/neets_rf.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

emp_rfplot <- read_csv("data/adults_rf.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

waffl_neet <- function(df) {
  ggplot(df, aes(fill = sex, values = sum_peep)) +
    geom_waffle(color = "white", size = .25, n_rows = 20) +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * 20, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
    labs(y = "Aged 14-30 (1 tile = 1k)") +
    theme_minimal() 
}

neet_pop_p <- function(df, var, labels) {
  df2 <- df %>%
    group_by(age, sex, .data[[var]]) %>%
    summarize(mean = sum((neet_cat == "NEET") * factor) / sum(factor) * 100, count = n()) %>%
    filter(count > 9)
  
  n <- length(unique(df2[[var]]))
  
  ggplot(df2) +
    geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_pal[1:n], labels = labels) +
    labs(y = "% NEET", color = "") + ylim(0, 100)
}

pay_age_p <- function(df) {
  ggplot(df %>% filter(!is.na(paid))) +
    geom_bar(aes(age, fill = sex), position = "fill", width = 0.9) +
    geom_hline(yintercept = 0.5, alpha = 0.2, linetype = "dashed") +
    facet_wrap(vars(paid)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    ylab("proportion")
}

pay_lab_inc1_p <- function(df) {
  ggplot(df %>% filter(!is.na(paid))) +
    geom_jitter(aes(x = age, y = lab_monthly_inc, color = sex), alpha = 0.05) +
    geom_line(data = df %>% filter(!is.na(paid) & !is.na(lab_monthly_inc)) %>% group_by(age, sex) %>%
                summarize(mean = sum(lab_monthly_inc * factor) / sum(factor)),
              aes(x = age, y = mean, color = sex), size = 1) +
    geom_point(data = df %>% filter(!is.na(paid) & !is.na(lab_monthly_inc)) %>% group_by(age, sex) %>%
                 summarize(mean = sum(lab_monthly_inc * factor) / sum(factor)),
               aes(x = age, y = mean, color = sex), size = 2.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
    ylab("monthly labor income (BOB)") +
    ylim(0, 20000)
}

pay_lab_inc2_p <- function(df) {
  ggplot(df %>% filter(paid == "paid")) +
    geom_jitter(aes(x = age, y = lab_monthly_inc, color = sex), alpha = 0.05) +
    geom_line(data = df %>% filter(paid == "paid" & !is.na(lab_monthly_inc)) %>% group_by(age, sex) %>%
                summarize(mean = sum(lab_monthly_inc * factor) / sum(factor)),
              aes(x = age, y = mean, color = sex), size = 1) +
    geom_point(data = df %>% filter(paid == "paid" & !is.na(lab_monthly_inc)) %>% group_by(age, sex) %>%
                 summarize(mean = sum(lab_monthly_inc * factor) / sum(factor)),
               aes(x = age, y = mean, color = sex), size = 2.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
    ylab("monthly labor income (BOB)") +
    ylim(0, 20000)
}

pay_worth_p <- function(df) {
  df %>%
    filter(paid == "unpaid") %>%
    mutate(unpaid_worth = hh_lab_inc * hh_hr_pct / 100) %>%
    ggplot() +
    geom_density(aes(unpaid_worth, fill = sex), alpha = 0.35, color = "grey") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    xlim(0, 17000) + xlab("Contribution to monthly household labor income (BOB)")
}

pay_ru1_p <- function(df) {
  ggplot(df %>% filter(paid == "unpaid")) +
    geom_density(aes(hh_lab_inc, fill = sex), alpha = 0.35, color = "grey") +
    facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    xlim(0, 40000) + xlab("Household monthly labor income (BOB)")
}

pay_depto1_marital1_p <- function(df, x_var, xend_var, y_var, lev, point1_x, point1_y, text1_x, text1_lab, point2_y, text2_lab) {
  ggplot(df) +
    geom_segment(aes(x = .data[[x_var]], xend = .data[[xend_var]], y = factor(.data[[y_var]], levels = lev), yend = .data[[y_var]]), color = "grey") +
    geom_point(aes(y = .data[[y_var]], x = .data[[xend_var]]), color = color1, size = 6) +
    geom_point(aes(y = .data[[y_var]], x = .data[[x_var]]), color = color2, size = 6) +
    geom_point(aes(point1_x, point1_y), color = color1, size = 6) +
    geom_text(aes(text1_x, point1_y, label = text1_lab), hjust = "left") +
    geom_point(aes(point1_x, point2_y), color = color2, size = 6) +
    geom_text(aes(text1_x, point2_y, label = text2_lab), hjust = "left") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    xlab("% unpaid workers") + ylab("")
}

pay_cell1_p <- function(df) {
  ggplot(df %>% filter(paid == "unpaid")) +
    geom_density(aes(hh_lab_inc, fill = sex), alpha = 0.35, color = "grey") +
    facet_wrap(vars(cellphone), labeller = labeller(cellphone = c("1. Si" = "with cellphone", "2. No" = "without cellphone"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    xlim(0, 40000) + xlab("household monthly labor income (BOB)")
}

neets_dec_graph_internet <- function(df) {
  df %>%
    mutate(decile = cut(pc_inc, 
                        breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                        include.lowest = T)) %>%
    group_by(sex, decile, internet_use) %>%
    summarize(mean = sum((neet_cat == "NEET") * factor) / sum(factor) * 100) %>%
    mutate(decile = as.numeric(decile)) %>%
    ggplot() +
    geom_line(aes(decile, mean, color = internet_use), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_pal[1:2], labels = c("with Internet", "without Internet")) +
    scale_x_continuous(breaks = 1:10) +
    labs(x = "per capita household income decile", y = "% NEET", color = "") + ylim(0, 100)
}

neets_dec_graph_depto <- function(df) {
  df %>%
    group_by(depto, area, sex) %>%
    summarize(mean = sum((neet_cat == "NEET") * factor) / sum(factor) * 100) %>%
    ggplot() +
    geom_col(aes(mean, depto, fill = sex), position = "dodge", width = 0.5) +
    facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    labs(x = "% NEET", y = "", fill = "")
}

neets_dec_graph_lang <- function(df) {
  df %>%
    mutate(language_1 = ifelse(language_1 %in% c("QUECHUA", "CASTELLANO"), language_1, "OTHER"),
           indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)) %>%
    group_by(language_1, sex, indigenous) %>%
    summarize(mean = sum((neet_cat == "NEET") * factor) / sum(factor) * 100) %>%
    ggplot() +
    geom_col(aes(mean, language_1, fill = sex), position = "dodge", width = 0.5) +
    facet_wrap(vars(indigenous), labeller = labeller(indigenous = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not indigenous"))) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
    labs(x = "% NEET", y = "", fill = "")
}

# Older adults --------------------------------
older_p1_p <- function(df) {
  df %>%
    ggplot() +
    geom_jitter(aes(age, sp_monthly_inc, color = sex), width = 1, alpha = 0.25) +
    geom_smooth(aes(age, sp_monthly_inc, color = sex), method = "loess", se = F, span = 0.1, method.args = list(degree = 1)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
    ylab("monthly social protection income (BOB)") + ylim(-1, 7000)
}

older_p2_p <- read_csv("data/older_ses_rf1_imp.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

older_f1 <- function(var, ylab) {
  fit1 <- loess(older %>% filter(is.na(primary_job) & startsWith(sex, "1")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                data = older %>% filter(is.na(primary_job) & startsWith(sex, "1")))
  fit2 <- loess(older %>% filter(is.na(primary_job) & startsWith(sex, "2")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                data = older %>% filter(is.na(primary_job) & startsWith(sex, "2")))
  fit3 <- loess(older %>% filter(!is.na(primary_job) & startsWith(sex, "1")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                data = older %>% filter(!is.na(primary_job) & startsWith(sex, "1")))
  fit4 <- loess(older %>% filter(!is.na(primary_job) & startsWith(sex, "2")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                data = older %>% filter(!is.na(primary_job) & startsWith(sex, "2")))
  
  older %>%
    mutate(primary_job = ifelse(!is.na(primary_job), "1 with job", "2 without job")) %>%
    arrange(primary_job, sex) %>%
    mutate(smooth = c(fit3$fitted, fit4$fitted, fit1$fitted, fit2$fitted)) %>%
    ggplot() +
    geom_jitter(aes(age, .data[[var]], color = primary_job), width = 1, alpha = 0.1) +
    geom_line(aes(age, smooth, color = primary_job), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = c("working", "not working"), name = "") +
    ylab(ylab) + ylim(-1, 5000)
}

older_f2 <- function(var, labels) {
  df <- older %>%
    mutate(primary_job = !is.na(primary_job)) %>%
    group_by(age, .data[[var]]) %>%
    summarize(count = n(), n = sum(factor), mean = sum(primary_job * factor) / sum(factor) * 100) %>%
    filter(count > 9)
  
  ggplot(df) +
    geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = labels, name = "average") +
    ylab("% working")
}

older_p3_p <- read_csv("data/older_ses_rf2_imp.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

older_f3 <- function(var, labels) {
  older %>%
    filter(!is.na(paid)) %>%
    mutate(marital = case_when(str_detect(marital, "^[23]") ~ "married/cohabiting",
                               str_detect(marital, "^[1,456]") ~ "single/separated/divorced/widowed")) %>%
    group_by(age, sex, .data[[var]]) %>%
    summarize(count = n(), n = sum(factor), mean = sum((paid == "paid") * factor) / sum(factor) * 100) %>%
    filter(count > 5) %>%
    ggplot() +
    geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = labels, name = "average") +
    ylab("% workers who are paid")
}

older_pay_p_sex <- function(df) {
  df %>%
    filter(!is.na(paid)) %>%
    mutate(marital = case_when(str_detect(marital, "^[23]") ~ "married/cohabiting",
                               str_detect(marital, "^[1,456]") ~ "single/separated/divorced/widowed")) %>%
    group_by(age, sex) %>%
    summarize(count = n(), n = sum(factor), mean = sum((paid == "paid") * factor) / sum(factor) * 100) %>%
    filter(count > 9) %>%
    ggplot() +
    geom_line(aes(age, mean, color = sex), size = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = c("men", "women"), name = "average") +
    ylab("% workers who are paid")
}

older_pay_p_rest_of_hh <- function(df) {
  df %>%
    filter(!is.na(paid)) %>%
    ggplot() +
    geom_density(aes(rest_of_hh, fill = paid), color = "grey", alpha = 0.3) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.y = element_blank()) +
    scale_fill_manual(values = color_pal[1:2], labels = c("paid", "unpaid"), name = "") +
    xlab("monthly per capita income for rest of household (BOB)") + xlim(0, 4000) + ylab("density")
}

older_pay_p_size <- function(df) {
  df %>%
    filter(!is.na(paid)) %>%
    group_by(size, sex) %>%
    summarize(count = n(), n = sum(factor), mean = sum((paid == "paid") * factor) / sum(factor) * 100) %>%
    filter(count > 5) %>%
    ggplot() +
    geom_line(aes(size, mean, color = sex), size = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = color_pal[1:2], labels = c("men", "women"), name = "") +
    scale_x_continuous(breaks = seq(1, 9, 2)) +
    ylab("% works who are paid") + ylim(0, 100)
}

older_p4_p <- function(df) {
  df %>%
    filter(paid == "paid") %>%
    mutate(age_group = cut(age, breaks = c(7, seq(20, 100, 10)), include.lowest = T)) %>%
    mutate(older = age > 60) %>%
    ggplot() +
    geom_jitter(aes(lab_monthly_inc+1, age_group, color = older), alpha = 0.1) +
    geom_boxplot(aes(lab_monthly_inc+1, age_group, color = older), fill = "white", alpha = 0.5, outlier.shape = NA) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "none") +
    scale_x_continuous(trans = 'log10') +
    xlab("monthly labor income (BOB)") + ylab("age group") +
    scale_color_manual(values = c("#E5E5E5", color1))
}

# Summary -------------------------------------
m1 <- function(rep_age, range, opp) {
  df <- personas %>%
    filter(between(age, rep_age - range, rep_age + range))
  
  df <- if (opp == 1) {
    df %>%
      group_by(sex)
  } else if (opp == 2) {
    df %>%
      filter((area == "Urbana" & depto == "La Paz") | (area == "Rural" & depto == "Potosi")) %>%
      group_by(depto)
  } else if (opp == 3) {
    df %>%
      mutate(decile = cut(pc_inc, 
                          breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                          include.lowest = T)) %>%
      mutate(decile = as.numeric(decile)) %>%
      filter(decile %in% c(1, 10)) %>%
      mutate(decile = ifelse(decile == 1, "low", "high")) %>%
      group_by(decile)
  } else if (opp == 4) {
    df %>%
      filter(!is.na(marital)) %>%
      mutate(mar = ifelse(str_detect(marital, "^[1456]"), "1single", "2married")) %>%
      group_by(mar)
  }
  
  df <- if (rep_age == a1) {
    df %>% summarize(mean = sum((!is.na(primary_job)) * factor) / sum(factor) * 100) %>% pull(mean)
  } else if (rep_age == a2) {
    df %>% summarize(mean = sum((in_school == "1. Si") * factor) / sum(factor) * 100) %>% pull(mean)
  } else if (rep_age == a3) {
    df %>% mutate(paid = ifelse(is.na(paid), "unpaid", paid)) %>% summarize(mean = sum((paid == "paid") * factor) / sum(factor) * 100) %>% pull(mean)
  } else if (rep_age == a4) {
    df %>% summarize(mean = sum((tot_monthly_inc >= 3000) * factor) / sum(factor) * 100) %>% pull(mean)
  } else if (rep_age == a5) {
    df %>% summarize(mean = sum((is.na(primary_job)) * factor) / sum(factor) * 100) %>% pull(mean)
  }
  
  data.frame(x = df[2] - df[1], y = 1)
}

m_plot <- function(df) {
  neg <- ifelse(df$x < 0, T, F)
  
  p <- df %>%
    ggplot() +
    geom_col(aes(x, y, fill = x > 0), width = 0.3, orientation = "y") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text = element_blank(), legend.position = "none", axis.title = element_blank()) +
    scale_fill_manual(values = setNames(c(color2, color1), c(T, F))) +
    ylim(0.5, 1.5) +
    scale_x_continuous(breaks = c(0), limits = c(-110, 110)) + coord_fixed(ratio = 140)
  
  if (neg) {
    p + geom_text(aes(x = x, y = 1, label = round(x)), hjust = "right", nudge_x = -5)
  } else {
    p + geom_text(aes(x = x, y = 1, label = round(x)), hjust = "left", nudge_x = 5)
  }
}
