library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(tidybayes)
library(here)

source("R/utils.R")

# theme_set(theme_ambla)

clrs <- c("#AA0E28", "#ED1D3F", "#F47B8F", "#70B3FF", "#004CA3", "#002147")

set.seed(1234)

levels <- c(0.95, 0.89, 0.75, 0.67, 0.50)

# wordbank ---------------------------------------------------------------------

wb <- arrow::read_csv_arrow(
  "data/01-introduction/wordbank_administration_data.csv"
) |>
  add_count(language) |>
  filter(
    typically_developing,
    monolingual,
    language != "English (British)",
    between(age, 10, 30)
  )

comp_label <- "Comprehension\n(Words and Gestures + Words and Sentences)"
prod_label <- "Production\n(Words and Sentences)"

wb_fig <- wb |>
  pivot_longer(c(comprehension, production),
    names_to = "type",
    values_to = "size",
    names_transform = tools::toTitleCase
  ) |>
  mutate(
    form = if_else(type == "Comprehension" & age <= 15, "WG", "WS"),
    type = if_else(type == "Comprehension", comp_label, prod_label)
  )

wb_summary <- wb_fig |>
  summarise(
    size = median(size),
    .by = c(type, age, form)
  )

plot_comp <- wb_fig |>
  filter(type == comp_label) |>
  ggplot(aes(age, size, group = form)) +
  facet_wrap(~type) +
  ggdist::stat_interval(
    .width = c(0.95, 0.89, 0.75, 0.67, 0.50),
    size = 4.75,
    position = "dodge"
  ) +
  geom_line(
    data = filter(wb_summary, type == comp_label),
    colour = "white",
    linewidth = 1
  ) +
  geom_point(
    data = filter(wb_summary, type == comp_label),
    colour = "white",
    size = 2.25
  ) +
  scale_colour_manual(
    values = colorRampPalette(c("grey95", clrs[5]))(5),
    labels = scales::percent(levels),
    guide = guide_legend(
      override.aes = list(colour = c(
        "#F2F2F2",
        "#C5C5C5",
        "#989898",
        "#6B6B6B",
        "#3F3F3F"
      ))
    )
  ) +
  theme(
    legend.position = "left",
    legend.justification = c(0, 1)
  )


plot_prod <- wb_fig |>
  filter(type == prod_label) |>
  ggplot(aes(age, size,
    group = form
  )) +
  facet_wrap(~type) +
  stat_interval(
    .width = levels,
    size = 4.75,
    position = "dodge"
  ) +
  geom_line(
    data = filter(wb_summary, type == prod_label),
    colour = "white",
    linewidth = 1
  ) +
  geom_point(
    data = filter(wb_summary, type == prod_label),
    colour = "white",
    size = 2.25
  ) +
  scale_colour_manual(values = colorRampPalette(c("grey95", clrs[1]))(5)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )


plot_comp + plot_prod +
  plot_layout(nrow = 1) &
  labs(
    x = "\nAge (months)",
    y = "Vocabulary size",
    colour = "Sample coverage",
    colour_ramp = "% sample"
  ) &
  scale_x_continuous(breaks = seq(0, 30, 2)) &
  scale_y_continuous(breaks = seq(0, 1000, 100)) &
  theme_ambla() &
  theme(
    panel.grid.major.y = element_line(
      colour = "grey",
      linetype = "dotted"
    ),
    axis.line = element_blank(),
    legend.background = element_blank()
  )


ggsave("assets/img/wordbank.png", width = 9, height = 4)


# lexical similarity -----------------------------------------------------------

bvq_data <- readRDS("data/01-introduction/bvq.rds")

flatten_xsampa <- function(x) {
  str_rm <- c("\\.", "\\\\", ",", "/", "?", "'", '"')
  str <- gsub(paste0(str_rm, collapse = "|"), "", x)
  str <- gsub("\\{", "\\\\{", str)
  return(str)
}

dist <- bvq_data$pool |>
  mutate(phon = flatten_xsampa(xsampa)) |>
  filter(!(semantic_category %in% c("Interjections"))) |>
  distinct(te, language, .keep_all = TRUE) |>
  pivot_wider(
    id_cols = c(te),
    names_from = language,
    values_from = phon,
    names_repair = janitor::make_clean_names
  ) |>
  mutate(distance = stringdist::stringsim(catalan, spanish)) |>
  pull(distance) |>
  mean(na.rm = TRUE)

tibble::tribble(
  ~language, ~dist,
  "Dutch", 0.2214,
  "Welsh", 0.2163,
  "German", 0.1975,
  "Italian", 0.1076,
  "French", 0.1034,
  "Bengali", 0.0941,
  "Hindi", 0.0899,
  "Spanish", 0.0874,
  "Polish", 0.0828,
  "Greek", 0.0807,
  "Portuguese", 0.0801,
  "Cantonese", 0.0422,
  "Mandarin", 0.0197
) |>
  mutate(language = paste0("English-", language)) |>
  add_row(language = "Catalan-Spanish", dist = dist) |>
  mutate(study = if_else(grepl("English", language),
    "Floccia et al. (2018)", "Own"
  )) |>
  ggplot(aes(dist, reorder(language, dist),
    fill = study
  )) +
  geom_col(
    colour = "white",
    width = 1,
    size = 1
  ) +
  geom_text(aes(label = scales::percent(dist, accuracy = 0.1)),
    size = 3.5,
    hjust = 1,
    colour = "white",
    position = position_nudge(x = -0.005)
  ) +
  labs(
    x = "\nPairwise lexical similarity (Levenshtein similarity)",
    y = "Language pair",
    fill = "Source",
  ) +
  scale_fill_manual(values = rev(clrs[c(2, 5)])) +
  scale_x_continuous(
    limits = c(0, 0.50),
    labels = scales::percent,
    breaks = seq(0, 1, 0.1)
  ) +
  theme_ambla() +
  theme(
    legend.position = c(0.8, 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.margin = margin(0.3, 0.3, 0.3, 0.3, unit = "cm"),
    legend.background = element_rect(fill = "grey95", colour = NA),
    legend.justification = c(1, 0),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "dotted",
      colour = "grey"
    )
  )

ggsave("assets/img/similarity.png", width = 9, height = 4)

# Study 1: predictions ---------------------------------------------------------

model_epreds <- arrow::read_csv_arrow(here(
  "data", "02-chapter-2",
  "results", "predictions",
  "predictions.csv"
))

responses <- arrow::read_csv_arrow(here(
  "data", "02-chapter-2",
  "data", "responses.csv"
))

fig_data <- model_epreds |>
  ungroup() |>
  reframe(
    .median = mean(.value),
    .lower = tidybayes::qi(.value, .width = 0.95)[, 1],
    .upper = tidybayes::qi(.value, .width = 0.95)[, 2],
    .by = c(ends_with("_std"), .category)
  ) |>
  mutate(
    age = (age_std * sd(responses$age)) + mean(responses$age),
    exposure = factor(exposure_std,
      levels = c(-1, 1),
      labels = c(
        "Lower-exposure (-1 SD)",
        "Higher-exposure (+1 SD)"
      ),
      ordered = TRUE
    ),
    lv = factor(lv_std,
      levels = unique(lv_std),
      labels = paste0(c(0, 50, 100), "%"),
      ordered = TRUE
    ),
    .category = ifelse(.category == "Understands",
      "Comprehension",
      "Production"
    )
  ) |>
  drop_na()

clrs_1 <- colorRampPalette(c("#F2F2F2", clrs[2]))(4)
clrs_2 <- colorRampPalette(c("#F2F2F2", clrs[5]))(4)

fig_data |>
  filter(.category == "Comprehension") |>
  ggplot(aes(age, .median,
    ymin = .lower,
    ymax = .upper,
    colour = lv,
    fill = lv,
    linetype = lv
  )) +
  facet_wrap(~exposure) +
  geom_vline(
    xintercept = mean(responses$age),
    linewidth = 2 / 4,
    alpha = 0.5,
    colour = "grey"
  ) +
  geom_hline(
    yintercept = 1 / 2,
    linewidth = 2 / 4,
    alpha = 0.5,
    colour = "grey"
  ) +
  geom_ribbon(
    linewidth = 0,
    colour = NA,
    alpha = 1
  ) +
  geom_line(
    linewidth = 1 / 2,
    colour = "black"
  ) +
  scale_fill_manual(
    values = clrs_1[2:4],
  ) +
  labs(
    x = "Age (months)",
    y = "*p*(Comprehension)",
    colour = "Cognateness (phonological similarity)",
    fill = "Cognateness (phonological similarity)",
    linetype = "Cognateness (phonological similarity)"
  ) +
  scale_linetype_manual(values = rev(c("solid", "dashed", "dotdash"))) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  theme_ambla() +
  theme(
    axis.title.y = ggtext::element_markdown(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.justification = "right",
    legend.direction = "horizontal",
    legend.key.size = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    strip.text.y = ggtext::element_markdown(
      size = 10,
      angle = 270,
      margin = margin(0, 0.2, 0, 0.2, "cm")
    ),
    strip.text.x = ggtext::element_markdown(
      size = 10,
      margin = margin(0.2, 0, 0.2, 0, "cm")
    ),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("assets/img/s1-predictions-comp.png", width = 8, height = 3)

fig_data |>
  filter(.category == "Production") |>
  ggplot(aes(age, .median,
    ymin = .lower,
    ymax = .upper,
    colour = lv,
    fill = lv,
    linetype = lv
  )) +
  facet_wrap(~exposure) +
  geom_vline(
    xintercept = mean(responses$age),
    linewidth = 2 / 4,
    alpha = 0.5,
    colour = "grey"
  ) +
  geom_hline(
    yintercept = 1 / 2,
    linewidth = 2 / 4,
    alpha = 0.5,
    colour = "grey"
  ) +
  geom_ribbon(
    linewidth = 0,
    colour = NA,
    alpha = 1
  ) +
  geom_line(
    linewidth = 1 / 2,
    colour = "black"
  ) +
  scale_fill_manual(values = clrs_2[2:4]) +
  labs(
    x = "Age (months)",
    y = "*p*(Production)",
    colour = "Cognateness (phonological similarity)",
    fill = "Cognateness (phonological similarity)",
    linetype = "Cognateness (phonological similarity)"
  ) +
  scale_linetype_manual(values = rev(c("solid", "dashed", "dotdash"))) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  theme_ambla() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = ggtext::element_markdown(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.justification = "right",
    legend.direction = "horizontal",
    legend.key.size = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    strip.text.y = ggtext::element_markdown(
      size = 10,
      angle = 270,
      margin = margin(0, 0.2, 0, 0.2, "cm")
    ),
    strip.text.x = ggtext::element_markdown(
      size = 10,
      margin = margin(0.2, 0, 0.2, 0, "cm")
    ),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("assets/img/s1-predictions-prod.png", width = 8, height = 3)

# Study 2: Exp. 1 predictions --------------------------------------------------

data_oxf <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "data_oxf.csv"))
model_fits_oxf <- readRDS(here("data", "03-chapter-3", "results", "fit_list_oxf.rds"))
model_loos_oxf <- readRDS(here("data", "03-chapter-3", "results", "loos_oxf.rds"))
attrition_trials <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "attrition_trials.csv"))
attrition_participants <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "attrition_participants.csv"))

clrs <- c(clrs[5], clrs[2], "#FF9E1F")

rope_interval <- c(lower = -0.1, upper = 0.1)

make_std <- function(x, mean, sd) (x - mean) / sd

timebin_values <- make_std(
  seq(0, 16, length.out = 100),
  mean(data_oxf$timebin),
  sd(data_oxf$timebin)
)

nd <- expand_grid(distinct(data_oxf, condition),
  timebin_std = timebin_values,
  age_std = 0
)

epreds <- tidybayes::add_epred_draws(nd, model_fits_oxf$fit_oxf_1,
  re_formula = NA,
  ndraws = NULL,
  value = ".epred"
)

obs_time <- data_oxf |>
  summarise(
    .sd = sd(.elog),
    .elog = mean(.elog),
    .n = n(),
    .by = c(condition, timebin)
  ) |>
  mutate(
    .lower = .elog - (.sd / sqrt(.n)),
    .upper = .elog + (.sd / sqrt(.n)),
    timebin = make_std(timebin,
      mean = mean(data_oxf$timebin),
      sd = sd(data_oxf$timebin)
    )
  )

obs_summary <- data_oxf |>
  summarise(
    .sd = sd(.elog),
    .elog = mean(.elog),
    .n = n(),
    .by = c(condition, child_id)
  ) |>
  mutate(
    .lower = .elog - (.sd / sqrt(.n)),
    .upper = .elog + (.sd / sqrt(.n))
  )

epreds_summary <- epreds |>
  ungroup() |>
  summarise(
    .value = tidybayes::mean_qi(.epred)[[1]],
    .lower = tidybayes::mean_qi(.epred)[[2]],
    .upper = tidybayes::mean_qi(.epred)[[3]],
    .by = c(condition, timebin_std)
  )

plot_summary <- obs_summary |>
  pivot_wider(
    names_from = condition,
    values_from = .elog,
    id_cols = child_id
  ) |>
  mutate(diff = Related - Unrelated) |>
  pivot_longer(c(Related, Unrelated),
    names_to = "condition",
    values_to = ".elog"
  ) |>
  summarise(
    .elog = mean(.elog),
    .by = c(child_id, condition)
  ) |>
  ggplot(aes(condition, .elog,
    colour = condition,
    fill = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  geom_line(aes(group = child_id),
    alpha = 1 / 2,
    colour = "grey",
    linewidth = 3 / 4
  ) +
  ggdist::geom_dots(
    side = "both",
    layout = "swarm",
    dotsize = 2,
    stroke = 0,
    alpha = 1 / 2
  ) +
  geom_errorbar(
    data = summarise(obs_summary,
      across(c(.elog, .upper, .lower), mean),
      .by = c(condition)
    ),
    aes(
      ymin = .lower,
      ymax = .upper
    ),
    width = 0.1,
    colour = "black",
    linewidth = 3 / 4,
    show.legend = FALSE
  ) +
  geom_point(
    data = summarise(obs_summary,
      across(
        c(.elog, .lower, .upper),
        mean
      ),
      .by = condition
    ),
    size = 2, colour = "black",
    show.legend = FALSE
  ) +
  labs(
    x = "Condition",
    y = "Logit PTLT",
    colour = "Condition",
    fill = "Condition"
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank()
  )

plot_time <- epreds |>
  ggplot(aes(timebin_std, .epred,
    colour = condition,
    fill = condition,
    linetype = condition,
    shape = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  stat_summary(
    fun.data = tidybayes::mean_qi,
    geom = "ribbon",
    alpha = 1 / 4,
    linewidth = NA
  ) +
  stat_summary(
    fun = "mean",
    geom = "line",
    linewidth = 3 / 4
  ) +
  # geom_errorbar(
  #     data = obs,
  #     aes(y = .elog, ymin = .lower, ymax = .upper),
  #     linewidth = 3/4,
  #     linetype = "solid",
  #     width = 0.1) +
  geom_point(
    data = obs_time,
    aes(y = .elog, x = timebin),
    stroke = 1,
    size = 2
  ) +
  labs(
    x = "Time (ms)",
    y = "Logit *PTLT*",
    colour = "Condition",
    fill = "Condition",
    linetype = "Condition",
    shape = "Condition"
  ) +
  scale_x_continuous(
    breaks = make_std(
      seq(0, 17, 3),
      mean(data_oxf$timebin),
      sd(data_oxf$timebin)
    ),
    labels = \(x) format(seq(3e2, 2e3, 3e2),
      big.mark = ","
    )
  ) +
  theme(
    legend.position = c(0.5, 1),
    legend.direction = "horizontal",
    legend.justification = c(0.5, 1)
  )

plot_time + plot_summary +
  plot_layout(
    nrow = 1, widths = c(0.5, 0.5),
    guides = "collect"
  ) &
  scale_y_continuous(limits = c(-1, 1.4)) &
  # scale_shape_manual(values = c(1, 2, 3)) &
  # scale_linetype_manual(values = c("solid", "dashed", "dotdash")) &
  scale_colour_manual(values = clrs) &
  scale_fill_manual(values = clrs) &
  theme_ambla() &
  theme(
    legend.position = "top",
    axis.title.y = ggtext::element_markdown(),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank()
  )

ggsave("assets/img/s2-1-predictions.png", width = 6, height = 4)

# Study 2: Exp. 2 - predictions ------------------------------------------------

data_bcn <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "data_bcn.csv"))
model_fits_bcn <- readRDS(here("data", "03-chapter-3", "results", "fit_list_bcn.rds"))
model_loos_bcn <- readRDS(here("data", "03-chapter-3", "results", "loos_bcn.rds"))

make_std <- function(x, mean, sd) (x - mean) / sd

timebin_values <- make_std(
  seq(0, 16, length.out = 100),
  mean(data_bcn$timebin),
  sd(data_bcn$timebin)
)

nd <- expand_grid(distinct(data_bcn, condition, lp),
  age_std = 0,
  timebin_std = timebin_values
)

epreds <- tidybayes::add_epred_draws(nd,
  model_fits_bcn$fit_0,
  ndraws = NULL,
  re_formula = NA
) |>
  mutate(lp = factor(lp, levels = c("Monolingual", "Bilingual")))

obs_time <- data_bcn |>
  summarise(
    .sd = sd(.elog),
    .elog = mean(.elog),
    .n = n(),
    .by = c(condition, lp, timebin)
  ) |>
  mutate(
    .lower = .elog - (.sd / sqrt(.n)),
    .upper = .elog + (.sd / sqrt(.n)),
    timebin = make_std(timebin,
      mean = mean(data_bcn$timebin),
      sd = sd(data_bcn$timebin)
    ),
    lp = factor(lp, levels = c("Monolingual", "Bilingual"))
  )

obs_summary <- data_bcn |>
  summarise(
    .sd = sd(.elog),
    .elog = mean(.elog),
    .n = n(),
    .by = c(condition, lp, child_id)
  ) |>
  mutate(
    .lower = .elog - (.sd / sqrt(.n)),
    .upper = .elog + (.sd / sqrt(.n)),
    lp = factor(lp, levels = c("Monolingual", "Bilingual"))
  )

epreds_summary <- epreds |>
  ungroup() |>
  summarise(
    .value = tidybayes::mean_qi(.epred)[[1]],
    .lower = tidybayes::mean_qi(.epred)[[2]],
    .upper = tidybayes::mean_qi(.epred)[[3]],
    .by = c(condition, lp, timebin_std)
  )

plot_time <- epreds |>
  filter(lp == "Monolingual") |>
  ggplot(aes(timebin_std, .epred,
    colour = condition,
    fill = condition,
    linetype = condition,
    shape = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  stat_summary(
    fun.data = tidybayes::mean_qi,
    geom = "ribbon",
    alpha = 1 / 4,
    linewidth = NA
  ) +
  stat_summary(
    fun = "mean",
    geom = "line",
    linewidth = 3 / 4
  ) +
  # geom_errorbar(
  #     data = obs,
  #     aes(y = .elog, ymin = .lower, ymax = .upper),
  #     linewidth = 3/4,
  #     linetype = "solid",
  #     width = 0.1) +
  geom_point(
    data = obs_time |>
      filter(lp == "Monolingual"),
    aes(y = .elog, x = timebin),
    stroke = 1,
    size = 2
  ) +
  labs(
    x = "Time (ms)",
    y = "Logit *PTLT*",
    colour = "Condition",
    fill = "Condition",
    linetype = "Condition",
    shape = "Condition"
  ) +
  scale_x_continuous(
    breaks = make_std(
      seq(0, 17, 3),
      mean(data_bcn$timebin),
      sd(data_bcn$timebin)
    ),
    labels = \(x) format(seq(3e2, 2e3, 3e2),
      big.mark = ","
    )
  ) +
  theme(legend.position = "top")


plot_summary <- obs_summary |>
  filter(lp == "Monolingual") |>
  summarise(
    .elog = mean(.elog),
    .by = c(child_id, lp, condition)
  ) |>
  ggplot(aes(condition, .elog,
    colour = condition,
    fill = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  geom_line(aes(group = child_id),
    alpha = 1 / 3,
    colour = "grey",
    linewidth = 1 / 2
  ) +
  ggdist::geom_dots(
    side = "both",
    layout = "swarm",
    alpha = 1 / 2,
    dotsize = 2,
    stroke = 0,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = epreds_summary |>
      filter(lp == "Monolingual") |>
      summarise(across(c(.value, .upper, .lower), mean),
        .by = c(lp, condition)
      ),
    aes(
      y = .value,
      ymin = .lower,
      ymax = .upper
    ),
    width = 0.1,
    colour = "black",
    linewidth = 3 / 4,
    show.legend = FALSE
  ) +
  geom_point(
    data = epreds_summary |>
      filter(lp == "Monolingual") |>
      summarise(across(c(.value, .upper, .lower), mean),
        .by = c(lp, condition)
      ),
    aes(y = .value),
    size = 2,
    colour = "black",
    show.legend = FALSE
  ) +
  labs(
    x = "Condition",
    y = "Logit *PTLT*",
    colour = "Condition",
    fill = "Condition"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  )

plot_time + plot_summary +
  plot_layout(
    nrow = 1, widths = c(0.5, 0.5),
    guides = "collect"
  ) &
  scale_y_continuous(limits = c(-1, 1.4)) &
  # scale_shape_manual(values = c(1, 2, 3)) &
  # scale_linetype_manual(values = c("solid", "dashed", "dotdash")) &
  scale_colour_manual(values = clrs) &
  scale_fill_manual(values = clrs) &
  theme_ambla() &
  theme(
    axis.title.y = ggtext::element_markdown(),
    legend.position = "top",
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank()
  )

ggsave("assets/img/s2-2-predictions-mon.png", width = 6, height = 4)

# bilinguals

plot_time <- epreds |>
  filter(lp == "Bilingual") |>
  ggplot(aes(timebin_std, .epred,
    colour = condition,
    fill = condition,
    linetype = condition,
    shape = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  stat_summary(
    fun.data = tidybayes::mean_qi,
    geom = "ribbon",
    alpha = 1 / 4,
    linewidth = NA
  ) +
  stat_summary(
    fun = "mean",
    geom = "line",
    linewidth = 3 / 4
  ) +
  # geom_errorbar(
  #     data = obs,
  #     aes(y = .elog, ymin = .lower, ymax = .upper),
  #     linewidth = 3/4,
  #     linetype = "solid",
  #     width = 0.1) +
  geom_point(
    data = obs_time |>
      filter(lp == "Bilingual"),
    aes(y = .elog, x = timebin),
    stroke = 1,
    size = 2
  ) +
  labs(
    x = "Time (ms)",
    y = "Logit *PTLT*",
    colour = "Condition",
    fill = "Condition",
    linetype = "Condition",
    shape = "Condition"
  ) +
  scale_x_continuous(
    breaks = make_std(
      seq(0, 17, 3),
      mean(data_bcn$timebin),
      sd(data_bcn$timebin)
    ),
    labels = \(x) format(seq(3e2, 2e3, 3e2),
      big.mark = ","
    )
  ) +
  theme(legend.position = "top")


plot_summary <- obs_summary |>
  summarise(
    .elog = mean(.elog),
    .by = c(child_id, lp, condition)
  ) |>
  filter(lp == "Bilingual") |>
  ggplot(aes(condition, .elog,
    colour = condition,
    fill = condition
  )) +
  geom_hline(
    yintercept = 0,
    colour = "grey"
  ) +
  geom_line(aes(group = child_id),
    alpha = 1 / 3,
    colour = "grey",
    linewidth = 1 / 2
  ) +
  ggdist::geom_dots(
    side = "both",
    layout = "swarm",
    alpha = 1 / 2,
    dotsize = 2,
    stroke = 0,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = epreds_summary |>
      filter(lp == "Bilingual") |>
      summarise(across(c(.value, .upper, .lower), mean),
        .by = c(lp, condition)
      ),
    aes(
      y = .value,
      ymin = .lower,
      ymax = .upper
    ),
    width = 0.1,
    colour = "black",
    linewidth = 3 / 4,
    show.legend = FALSE
  ) +
  geom_point(
    data = epreds_summary |>
      filter(lp == "Bilingual") |>
      summarise(across(c(.value, .upper, .lower), mean),
        .by = c(lp, condition)
      ),
    aes(y = .value),
    size = 2,
    colour = "black",
    show.legend = FALSE
  ) +
  labs(
    x = "Condition",
    y = "Logit *PTLT*",
    colour = "Condition",
    fill = "Condition"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  )

plot_time + plot_summary +
  plot_layout(
    nrow = 1, widths = c(0.5, 0.5),
    guides = "collect"
  ) &
  scale_y_continuous(limits = c(-1, 1.4)) &
  # scale_shape_manual(values = c(1, 2, 3)) &
  # scale_linetype_manual(values = c("solid", "dashed", "dotdash")) &
  scale_colour_manual(values = clrs) &
  scale_fill_manual(values = clrs) &
  theme_ambla() &
  theme(
    axis.title.y = ggtext::element_markdown(),
    legend.position = "top",
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank()
  )

ggsave("assets/img/s2-2-predictions-bil.png", width = 6, height = 4)

# Appendix ---------------------------------------------------------------------

# Introduction: cognate proportion

clrs_full <- c("#AA0E28", "#ED1D3F", "#F47B8F", "#70B3FF", "#004CA3", "#002147")

tibble::tribble(
  ~type, ~l_cog, ~l_noncog, ~v_cog, ~v_noncog,
  "No cognate facilitation", 50, 50, 50, 50,
  "Cognate facilitation", 50, 50, 80, 20,
) |>
  tidyr::pivot_longer(-c(type),
    names_to = c("measure", "cognate"),
    names_sep = "_"
  ) |>
  mutate(
    cognate = factor(cognate,
      levels = c("noncog", "cog"),
      labels = c("Non-cognates", "Cognates")
    ),
    measure = factor(measure,
      levels = c("l", "v"),
      labels = c("Language", "Vocabulary")
    ),
    type = factor(type, levels = c(
      "No cognate facilitation",
      "Cognate facilitation"
    ))
  ) |>
  ggplot(aes(value, reorder(measure, desc(measure)),
    fill = type,
    colour = type,
    alpha = cognate
  )) +
  facet_wrap(~type, ncol = 1) +
  geom_col(
    position = position_fill(),
    colour = "white"
  ) +
  geom_vline(xintercept = 0.5, colour = "grey", linewidth = 1) +
  guides() +
  labs(x = "Proportion of cognates") +
  scale_fill_manual(values = clrs_full[c(2, 5)]) +
  scale_colour_manual(values = clrs_full[c(2, 5)]) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_x_continuous(labels = scales::percent) +
  theme_ambla() +
  theme(
    legend.position = "top",
    strip.text = ggtext::element_markdown(
      size = 10,
      face = "bold",
      margin = margin(0.35, 0, 0.35, 0, "cm")
    ),
    axis.title.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("assets/img/cognate-proportion.png", width = 7, height = 4)

# Study 1: posterior coefficients ----

model_summary <- arrow::read_csv_arrow(here(
  "data", "02-chapter-2",
  "results", "posterior", "model_summary.csv"
))

model_draws <- arrow::read_csv_arrow(here(
  "data", "02-chapter-2",
  "results", "posterior", "posterior_draws.csv"
))

intercept_label <- glue::glue("Intercepts (at average{round(mean(responses$age), 2)} months)")

rope_interval <- c(lower = -0.1, upper = 0.1) / 4

model_vars_dict <- get_vars_dict(responses)

sum_data <- model_summary |>
  mutate(
    across(
      .value:.upper,
      \(x) ifelse(.type == intercept_label, plogis(x), x / 4)
    ),
    .variable_name = factor(.variable_name,
      levels = unique(model_vars_dict),
      ordered = TRUE
    ),
    .variable_name = gsub(
      "Comprehension and Production", "Production",
      .variable_name
    )
  )

draws_data <- model_draws |>
  mutate(
    across(
      .value,
      \(x) ifelse(.type == intercept_label, plogis(x), x / 4)
    ),
    .variable_name = factor(.variable_name,
      levels = unique(model_vars_dict),
      ordered = TRUE
    )
  )


draws_data |>
  filter(.type == "Slopes") |>
  ggplot(aes(.value,
    factor(.variable_name, levels = unique(model_vars_dict)),
    fill = after_stat(abs(x) < rope_interval["upper"])
  )) +
  facet_wrap(vars(.type), ncol = 1, scales = "free") +
  annotate(
    geom = "rect",
    colour = NA,
    fill = "grey",
    alpha = 1 / 4,
    ymin = -Inf,
    ymax = Inf,
    xmin = rope_interval["lower"],
    xmax = rope_interval["upper"]
  ) +
  annotate(
    geom = "text",
    label = "ROPE",
    colour = "grey",
    angle = 90,
    x = rope_interval["upper"],
    y = 9,
    size = 3,
    hjust = 1.1,
    vjust = 1.5
  ) +
  geom_vline(
    xintercept = 0,
    colour = "grey"
  ) +
  stat_slab() +
  geom_errorbar(
    data = filter(sum_data, .type == "Slopes"),
    aes(xmin = .lower, xmax = .upper),
    width = 0.2, position = position_nudge(y = -0.2)
  ) +
  geom_point(
    data = filter(sum_data, .type == "Slopes"),
    position = position_nudge(y = -0.2),
    size = 1.5, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(clrs[2], "grey80"),
    labels = c(
      "Inside ROPE",
      "Outside ROPE"
    )
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    x = "\nChange in acquisition probability",
    y = "Predictor",
    fill = "Inside ROPE"
  ) +
  theme_ambla() +
  scale_x_continuous(limits = c(-0.2, 0.6)) +
  theme(
    legend.position = c(0.9, 0.4),
    legend.background = element_rect(colour = NA, fill = "grey95"),
    legend.justification = c(1, 1),
    legend.text.align = 0,
    legend.title = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.text.y = element_text(angle = 30),
    axis.title.y = element_blank()
  )

ggsave("assets/img/s1-coefficients.png", width = 8, height = 4)

# Study 1: convergence ----

clrs_3 <- colorRampPalette(c("grey80", clrs_full[5]))(4)

model_draws |>
  mutate(
    .chain = as.factor(.chain),
    .variable_name = factor(.variable_name,
      levels = unique(model_vars_dict)
    )
  ) |>
  ggplot(aes(.iteration, .value,
    colour = .chain,
    fill = .chain
  )) +
  facet_wrap(~.variable_name, scales = "free_y") +
  geom_line(alpha = 3 / 4) +
  labs(
    x = "Iteration",
    y = "Value",
    colour = "MCMC chain",
    fill = "MCMC chain"
  ) +
  scale_colour_manual(values = clrs_3) +
  theme_ambla() +
  theme(
    legend.position = c(0.9, 0.05),
    legend.background = element_rect(
      fill = "grey95",
      colour = NA
    ),
    legend.direction = "horizontal",
    legend.justification = c(1, 0),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("assets/img/s1-convergence.png", width = 10, height = 4)


# Study 2: vocabulary ----

attrition_trials <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "attrition_trials.csv"))
attrition_participants <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "attrition_participants.csv"))
participants <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "participants.csv"))

vocabulary <- arrow::read_csv_arrow(here("data", "03-chapter-3", "data", "vocabulary.csv"))

vocab_plot_data <- vocabulary |>
  inner_join(dplyr::filter(attrition_participants, is_valid_participant),
    by = join_by(session_id)
  ) |>
  inner_join(
    select(
      participants, location, lp, age,
      child_id, session_id, age_group
    ),
    by = join_by(child_id, session_id)
  ) |>
  dplyr::filter(is_valid_participant) |>
  select(
    child_id, session_id, vocab_id, is_imputed, l1_count,
    lp, location, age_group, age
  ) |>
  mutate(
    location = factor(location,
      levels = c("Oxford", "Barcelona")
    ),
    lp = gsub(" \\(English\\)", "", lp),
    lp = factor(lp, levels = c("Monolingual", "Bilingual")),
    side = case_when(
      location == "Oxford" ~ "both",
      lp == "Monolingual" ~ "left",
      lp == "Bilingual" ~ "right"
    ),
    study = if_else(location == "Oxford",
      "Study 1 (English)", "Study 2 (Catalan/Spanish)"
    )
  )

vocab_plot_means <- vocab_plot_data |>
  summarise(
    l1_count_sd = sd(l1_count, na.rm = TRUE),
    l1_count = mean(l1_count, na.rm = TRUE),
    n = n(),
    .by = c(age, side, lp, study)
  ) |>
  mutate(
    .lower = l1_count - (l1_count_sd / sqrt(n)),
    .upper = l1_count + (l1_count_sd / sqrt(n))
  )

vocab_plot_data |>
  ggplot(aes(age, l1_count,
    colour = interaction(study, lp),
    fill = interaction(study, lp),
  )) +
  facet_wrap(~ study + lp) +
  geom_point(
    size = 2,
    shape = 1,
    alpha = 1 / 4,
    stroke = 1
  ) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x",
  ) +
  labs(
    x = "\nAge (months)",
    y = "Vocabulary size\n",
    colour = "Group",
    fill = "Group"
  ) +
  scale_y_continuous(
    limits = c(0, 575),
    breaks = seq(0, 1e3, 100)
  ) +
  scale_x_continuous(breaks = seq(0, 50, 2)) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  theme_ambla() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.line = element_blank()
  )

ggsave("assets/img/vocabulary.png", width = 8, height = 4)

## Study 2: Exp. 1 - convergence ----

clrs_4 <- colorRampPalette(c("grey80", clrs_full[5]))(2)

model_fits_oxf$fit_oxf_0 |>
  tidybayes::gather_draws(`b_.*`, regex = TRUE) |>
  mutate(
    .variable = factor(.variable),
    .chain = as.factor(.chain)
  ) |>
  ggplot(aes(.iteration, .value,
    colour = .chain,
    fill = .chain
  )) +
  facet_wrap(~.variable, scales = "free_y") +
  geom_line(alpha = 3 / 4) +
  labs(
    x = "Iteration",
    y = "Value",
    colour = "MCMC chain",
    fill = "MCMC chain"
  ) +
  scale_colour_manual(values = clrs_4) +
  theme_ambla() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("assets/img/s2-1-convergence.png", width = 8, height = 3)

## Study 2: Exp. 2 - convergence ----

clrs_4 <- colorRampPalette(c("grey80", clrs_full[5]))(2)

model_fits_bcn$fit_0 |>
  tidybayes::gather_draws(`b_.*`, regex = TRUE) |>
  mutate(
    .variable = factor(.variable),
    .chain = as.factor(.chain)
  ) |>
  ggplot(aes(.iteration, .value,
    colour = .chain,
    fill = .chain
  )) +
  facet_wrap(~.variable, scales = "free_y") +
  geom_line(alpha = 3 / 4) +
  labs(
    x = "Iteration",
    y = "Value",
    colour = "MCMC chain",
    fill = "MCMC chain"
  ) +
  scale_colour_manual(values = clrs_4) +
  theme_ambla() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("assets/img/s2-2-convergence.png", width = 8, height = 5)
