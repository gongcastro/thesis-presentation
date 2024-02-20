library(dplyr)
library(tidyr)
library(ggplot2)
library(stringdist)
library(purrr)
library(ggdist)
library(patchwork)
library(magick)
library(ggtext)
library(gganimate)

set.seed(1234)

# make functions ---------------------------------------------------------------
generate_eli <- function(
    te = list(
      noncognate = c("gos", "pero"),
      cognate = c("gat", "gato")
    ),
    age = 0:40,
    freq_month = 5,
    freq_beta = 0.1,
    parallel_beta = 1,
    threshold = 500,
    l1_doe = 0.65) {
  freq_adj <- freq_month / (length(age) - 1)
  nms <- if (!is.null(names(te))) names(te) else 1:length(te)
  items_df <- as_tibble(t(as.data.frame(te)))
  items_df$.te <- nms
  items_df <- items_df[, c(3, 1, 2)]
  names(items_df) <- c(".te", "l1.item", "l2.item")

  item_df <- items_df |>
    mutate(lv = map_dbl(te, \(x) stringsim(x[1], x[2]))) |>
    mutate(l1.doe = l1_doe, l2.doe = 1 - l1_doe)

  eli <- expand_grid(item_df, age = age) |>
    mutate(
      n_month = rpois(n(), freq_adj) * age,
      l1.h0_eli = n_month * l1.doe,
      l2.h0_eli = n_month * l2.doe,
      l1.h1_eli = l1.h0_eli + parallel_beta * (lv * l2.h0_eli),
      l2.h1_eli = l2.h0_eli + parallel_beta * (lv * l1.h0_eli),
      across(
        matches("h0|h1"),
        \(x) floor(ifelse(x < 0, 0, x))
      )
    ) |>
    mutate(across(matches("h0|h1"), cumsum),
      .by = .te
    ) |>
    select(te = .te, lv, age, n_month, matches("l1|l2")) |>
    pivot_longer(-c(te, lv, age, n_month),
      names_to = c("language", ".value"),
      names_pattern = "(.+)\\.(.+)"
    ) |>
    mutate(language = toupper(language)) |>
    pivot_longer(matches("h0|h1"),
      names_to = c("hypothesis", ".value"),
      names_pattern = "(.+)_(.+)",
      names_transform = toupper
    ) |>
    rename_with(tolower) |>
    arrange(te, language, hypothesis, age)

  return(eli)
}

generate_aoa <- function(eli, threshold = 500, .by = NULL) {
  aoa <- eli |>
    select(te, language, hypothesis, language, iteration, age, eli, any_of(.by)) |>
    mutate(
      aoa = age[which.min(abs(.env$threshold - eli))],
      aoa = case_when(aoa <= min(age) ~ NA,
        aoa >= max(age) ~ NA,
        .default = aoa
      ),
      .by = c(te, hypothesis, language, iteration, any_of(.by))
    ) |>
    rename_with(\(x) gsub("_eli_aoa", "_aoa", x)) |>
    distinct(pick(-c(age, eli))) |>
    arrange(te, language) |>
    mutate(threshold = .env$threshold)

  return(aoa)
}


# cumulative learning instances plot -------------------------------------------

generate_figure <- function(...) {
  # simulate data ---------------
  eli_df <- generate_eli(threshold = 500, ...) |>
    dplyr::filter(!(hypothesis == "H1" & grepl("Non-cognate", te)))

  aoa_df <- generate_aoa(eli_df, threshold = 500) |>
    mutate(across(aoa, lst(min, max)), .by = c(te, language)) |>
    dplyr::filter(!(hypothesis == "H1" & grepl("Non-cognate", te)))

  img <- c(
    cat = here::here("_assets", "img", "diagram-cat.png"),
    dog = here::here("_assets", "img", "diagram-dog.png"),
  ) |>
    map(magick::image_read) |>
    map(\(x) magick::image_ggplot(x, interpolate = FALSE))

  plot <- eli_df |>
    ggplot(aes(age, eli,
      colour = language,
      linetype = hypothesis,
      shape = hypothesis
    )) +
    geom_segment(
      data = aoa_df,
      aes(
        x = aoa,
        xend = aoa,
        y = 0,
        yend = threshold
      ),
      linewidth = 3 / 4
    ) +
    geom_hline(yintercept = aoa_df$threshold) +
    geom_line(linewidth = 1) +
    geom_point(
      data = aoa_df,
      stroke = 0.75,
      aes(x = aoa, y = threshold),
      size = 2.25
    ) +
    labs(
      x = "Age (months)",
      y = "Learning instances",
      colour = "Language (exposure)",
      shape = "Hypothesis",
      linetype = "Hypothesis"
    ) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
    theme_ggdist() +
    theme(
      panel.background = element_rect(fill = NA),
      legend.key.width = unit(1.5, "cm"),
      legend.position = "bottom",
      legend.margin = margin(c(0, 0, 0, 0)),
      legend.justification = c(0, 1),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      strip.background = element_rect(fill = "grey90", colour = "grey90"),
      strip.text = element_markdown(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(
        colour = "grey",
        linetype = "dotted"
      ),
      panel.grid.minor.y = element_line(
        colour = "grey",
        linetype = "dotted"
      )
    ) +
    inset_element(img$cat,
      on_top = FALSE, ignore_tag = TRUE,
      left = -0.25, bottom = 0.50, right = 0.5, top = 1
    ) +
    inset_element(img$dog,
      on_top = FALSE, ignore_tag = TRUE,
      left = 0.25, bottom = 0.50, right = 1, top = 1
    ) +
    scale_x_continuous(breaks = seq(
      min(eli_df$age),
      max(eli_df$age), 4
    )) +
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
  return(plot)
}

# ggplot theme
theme_ambla <- function() {
  theme(
    strip.background = element_rect(fill = "grey90", colour = "grey90"),
    strip.text = ggtext::element_markdown(size = 10),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey",
      linetype = "dotted"
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    axis.line = element_line(colour = "grey", linewidth = 1),
    strip.text.x = element_text(face = "bold"),
    panel.grid = element_blank()
  )
}

logistic <- function(x, slope, mid, upper = 1) {
  upper / (1 + exp(-slope * (x - mid)))
}

# make figures -----------------------------------------------------------------

clrs <- c("#004AAD", "#C8102E", "#FF9E1F")

words <- list(
  "Non-cognate" = c("gos", "pero"),
  "Cognate" = c("gat", "gato")
)

te_labels <- c(
  "Non-cognate: /'gos/ (Catalan) /'pe.ro/ (Spanish)",
  "Cognate: /'gat/ (Catalan), /'ga.to/ (Spanish)"
)

img <- c(
  cat = here::here("assets", "img", "diagram-cat.png"),
  dog = here::here("assets", "img", "diagram-dog.png")
) |>
  map(magick::image_read) |>
  map(\(x) magick::image_ggplot(x, interpolate = FALSE))

# parameters
threshold <- 300
age_max <- 50
freq_month <- 50
l1_doe <- 0.65
age_step <- 100
logistic_slope <- 0.75
n_iter <- 50

eli_mon <- map_df(
  1:n_iter,
  function(x) {
    generate_eli(
      te = words,
      age = seq(1, age_max, length.out = age_step),
      freq_month = freq_month,
      freq_beta = 1,
      parallel_beta = 1,
      threshold = threshold,
      l1_doe = 1
    )
  },
  .id = "iteration"
) |>
  mutate(lp = "Monolingual")

eli_bil <- map_df(
  1:n_iter,
  function(x) {
    generate_eli(
      te = words,
      age = seq(1, age_max, length.out = age_step),
      freq_month = freq_month,
      freq_beta = 1,
      parallel_beta = 1,
      threshold = threshold,
      l1_doe = 0.65
    )
  },
  .id = "iteration"
) |>
  mutate(lp = "Bilingual")

eli_df <- bind_rows(eli_mon, eli_bil) |>
  mutate(
    lp = factor(lp, levels = c(
      "Monolingual",
      "Bilingual"
    )),
    language = ifelse(language == "L1", "Catalan form", "Spanish form"),
    language = factor(language, levels = c("Catalan form", "Spanish form")),
    id = paste0(lp, ", ", language),
    id = factor(id, levels = c(
      "Monolingual, Catalan form",
      "Monolingual, Spanish form",
      "Bilingual, Catalan form",
      "Bilingual, Spanish form"
    )),
    te = factor(te, levels = names(words), labels = as.character(te_labels))
  ) |> # nolint
  filter(id != "Monolingual, Spanish form")

# effective learning instances
eli_df_summary <- eli_df |>
  summarise(
    eli_sd = sd(eli),
    eli = mean(eli),
    .by = c(age, id, hypothesis, te)
  ) |>
  mutate(
    .lower = eli - eli_sd,
    .upper = eli + eli_sd
  )

# age of acquisition
aoa_df <- generate_aoa(eli_df,
  threshold = threshold,
  .by = c("id", "iteration")
)

aoa_df_summary <- aoa_df |>
  summarise(
    aoa_sd = sd(aoa, na.rm = TRUE),
    aoa = mean(aoa, na.rm = TRUE),
    .by = c(id, hypothesis, te)
  ) |>
  mutate(
    .lower = aoa - aoa_sd,
    .upper = aoa + aoa_sd
  )

# logistic acquisition curves
logistic_df <- aoa_df |>
  expand_grid(age = seq(1, age_max, length.out = age_step)) |>
  mutate(prob = logistic(age, logistic_slope, aoa))

logistic_df_summary <- aoa_df_summary |>
  expand_grid(age = seq(1, age_max, length.out = age_step)) |>
  mutate(
    prob = logistic(age, logistic_slope, aoa),
    .lower = logistic(age, logistic_slope, .lower),
    .upper = logistic(age, logistic_slope, .upper)
  )

# no facilitation, single iteration, monolingual -------------------------------
iter_id <- 1

eli_plot_iter <- eli_df |>
  filter(
    hypothesis == "H0",
    iteration == iter_id,
    id == "Monolingual, Catalan form",
    te == te_labels[1]
  ) |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  geom_label(
    data = filter(
      aoa_df,
      hypothesis == "H0",
      iteration == iter_id,
      id == "Monolingual, Catalan form",
      te == te_labels[1]
    ),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla()

aoa_plot_iter <- aoa_df |>
  filter(
    hypothesis == "H0",
    iteration == iter_id,
    id == "Monolingual, Catalan form",
    te == te_labels[1]
  ) |>
  ggplot(aes(aoa, 0.5,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_point(size = 3, na.rm = TRUE, show.legend = FALSE) +
  geom_line(
    data = filter(
      logistic_df,
      iteration == iter_id,
      id == "Monolingual, Catalan form",
      hypothesis == "H0",
      te == te_labels[1]
    ),
    aes(x = age, y = prob),
    linewidth = 3 / 4,
    na.rm = TRUE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    na.rm = TRUE
  ) +
  labs(
    x = "Age (months)",
    y = "*p*(Acquisition)",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown()
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() &
  theme(panel.grid = element_blank())

eli_plot_iter / aoa_plot_iter +
  plot_layout(
    ncol = 1,
    guides = "keep",
    heights = c(0.7, 0.3)
  )


ggsave("assets/img/ambla-single-nc-mon.png",
  height = 5,
  width = 6
)

# no facilitation, all iterations, monolingual ---------------------------------

eli_plot_iter <- eli_df |>
  filter(
    hypothesis == "H0",
    id == "Monolingual, Catalan form",
    te == te_labels[1]
  ) |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration)),
    linewidth = 3 / 4,
    alpha = 1 / 10
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  geom_line(
    data = filter(
      eli_df_summary,
      hypothesis == "H0",
      id == "Monolingual, Catalan form",
      te == te_labels[1]
    ),
    linewidth = 3 / 4
  ) +
  geom_label(
    data = filter(
      aoa_df_summary,
      hypothesis == "H0",
      id == "Monolingual, Catalan form",
      te == te_labels[1]
    ),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() +
  theme(panel.grid = element_blank())



aoa_plot_iter <- aoa_df |>
  filter(
    hypothesis == "H0",
    id == "Monolingual, Catalan form",
    te == te_labels[1]
  ) |>
  ggplot(aes(aoa, 0.5,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_ribbon(
    data = filter(
      logistic_df_summary,
      id == "Monolingual, Catalan form",
      hypothesis == "H1",
      te == te_labels[1]
    ),
    aes(
      x = age, y = prob,
      ymin = .lower,
      ymax = .upper,
      fill = id
    ),
    linewidth = NA,
    alpha = 0.5
  ) +
  geom_point(
    data = filter(
      aoa_df_summary,
      id == "Monolingual, Catalan form",
      hypothesis == "H0",
      te == te_labels[1]
    ),
    size = 3, na.rm = TRUE, show.legend = FALSE
  ) +
  geom_line(
    data = filter(
      logistic_df_summary,
      id == "Monolingual, Catalan form",
      hypothesis == "H0",
      te == te_labels[1]
    ),
    aes(x = age, y = prob),
    linewidth = 3 / 4,
    na.rm = TRUE
  ) +
  labs(
    x = "Age (months)",
    y = "*p*(Acquisition)",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown()
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() +
  theme(panel.grid = element_blank())

eli_plot_iter / aoa_plot_iter +
  plot_layout(
    ncol = 1,
    guides = "keep",
    heights = c(0.7, 0.3)
  )


ggsave("assets/img/ambla-all-nc-mon.png",
  height = 5,
  width = 6
)


# no facilitation, all iterations, all groups ----------------------------------
eli_plot <- eli_df |>
  filter(
    hypothesis == "H1",
    te == te_labels[1]
  ) |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4,
    alpha = 1 / 10
  ) +
  geom_line(
    data = filter(
      eli_df_summary,
      hypothesis == "H1",
      te == te_labels[1]
    ),
    aes(group = interaction(id)),
    linewidth = 3 / 4
  ) +
  geom_label(
    data = filter(
      aoa_df_summary,
      hypothesis == "H1",
      te == te_labels[1]
    ),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  # inset_element(img$cat, on_top = FALSE, ignore_tag = TRUE,
  #               left = 0, bottom = 0.75, right = 0.15, top = 0.9) +
  # inset_element(img$dog, on_top = FALSE, ignore_tag = TRUE,
  #               left = 1, bottom = 1, right = 1, top = 1) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

aoa_plot <- aoa_df_summary |>
  filter(
    hypothesis == "H1",
    te == te_labels[1]
  ) |>
  ggplot(aes(aoa, 0.5,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_ribbon(
    data = filter(
      logistic_df_summary,
      hypothesis == "H1",
      te == te_labels[1]
    ),
    aes(
      x = age, y = prob,
      ymin = .lower,
      ymax = .upper,
      fill = id
    ),
    linewidth = NA,
    alpha = 0.5
  ) +
  geom_point(size = 3, na.rm = TRUE, show.legend = FALSE) +
  geom_line(
    data = filter(
      logistic_df_summary,
      hypothesis == "H1",
      te == te_labels[1]
    ),
    aes(
      x = age, y = prob,
      group = interaction(id)
    ),
    linewidth = 3 / 4,
    na.rm = TRUE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    na.rm = TRUE
  ) +
  labs(
    x = "Age (months)",
    y = "*p*(Acquisition)",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown(),
    panel.grid.major.y = element_blank()
  )

eli_plot / aoa_plot +
  plot_layout(
    ncol = 1,
    guides = "keep",
    heights = c(0.7, 0.3)
  ) &
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) &
  scale_colour_manual(values = clrs) &
  scale_fill_manual(values = clrs) &
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) &
  theme_ambla() &
  theme(panel.grid = element_blank())

ggsave("assets/img/ambla-all-nc.png",
  height = 5,
  width = 6
)

# facilitation, all iterations -------------------------------------------------
eli_plot <- eli_df |>
  filter(
    hypothesis == "H1",
    te == te_labels[2]
  ) |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4,
    alpha = 1 / 10
  ) +
  geom_line(
    data = filter(
      eli_df_summary,
      hypothesis == "H1",
      te == te_labels[2]
    ),
    aes(group = interaction(id)),
    linewidth = 3 / 4
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  geom_label(
    data = filter(
      aoa_df_summary,
      hypothesis == "H1",
      te == te_labels[2]
    ),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )


aoa_plot <- aoa_df_summary |>
  filter(
    hypothesis == "H1",
    te == te_labels[2]
  ) |>
  ggplot(aes(aoa, 0.5,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_ribbon(
    data = filter(
      logistic_df_summary,
      hypothesis == "H1",
      te == te_labels[2]
    ),
    aes(
      x = age, y = prob,
      ymin = .lower,
      ymax = .upper,
      fill = id
    ),
    linewidth = NA,
    alpha = 0.5
  ) +
  geom_point(size = 3, na.rm = TRUE, show.legend = FALSE) +
  geom_line(
    data = filter(
      logistic_df_summary,
      hypothesis == "H1",
      te == te_labels[2]
    ),
    aes(
      x = age, y = prob,
      group = interaction(id)
    ),
    linewidth = 3 / 4,
    na.rm = TRUE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    na.rm = TRUE
  ) +
  labs(
    x = "Age (months)",
    y = "*p*(Acquisition)",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown(),
    panel.grid = element_blank()
  )

eli_plot / aoa_plot +
  plot_layout(
    ncol = 1,
    guides = "keep",
    heights = c(0.7, 0.3)
  ) &
  guides(colour = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) &
  scale_colour_manual(values = clrs) &
  scale_fill_manual(values = clrs) &
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) &
  theme_ambla() &
  theme(panel.grid = element_blank())

ggsave("assets/img/ambla-all-c.png",
  height = 5,
  width = 6
)

# animations -------------------------------------------------------------------

# single, mon
iter_id <- 1

eli_data_plot <- eli_df |>
  filter(
    hypothesis == "H0",
    id == "Monolingual, Catalan form",
    iteration == iter_id,
    te == te_labels[1]
  ) |>
  mutate(
    n_cum = cumsum(n_month),
    y_pos = max(eli),
    .by = c(te, id, id, hypothesis)
  )

eli_plot <- ggplot(
  eli_data_plot,
  aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )
) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  geom_label(
    data = filter(
      logistic_df,
      hypothesis == "H0",
      id == "Monolingual, Catalan form",
      iteration == iter_id,
      te == te_labels[1]
    ) |>
      mutate(
        aoa = ifelse(age < aoa, NA, aoa)
      ),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.grid = element_blank(),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  ) +
  transition_reveal(age)

eli_gif <- animate(eli_plot,
  width = 5,
  height = 4,
  units = "in",
  duration = 10,
  renderer = gifski_renderer(),
  res = 200
)

anim_save("assets/gif/ambla-eli-single-mon.gif", eli_gif)

# simulations all (no facilitation) ----
iter_id <- 1

eli_data_plot <- eli_df |>
  filter(
    hypothesis == "H0",
    iteration == iter_id,
    te == te_labels[1]
  ) |>
  mutate(
    n_cum = cumsum(n_month),
    y_pos = max(eli),
    .by = c(te, id, id, hypothesis)
  )

eli_plot <- eli_data_plot |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4,
    alpha = 1 / 10
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 2,
    hjust = 0
  ) +
  geom_line(
    data = eli_data_plot,
    aes(group = interaction(id)),
    linewidth = 3 / 4
  ) +
  geom_label(
    data = filter(
      logistic_df,
      hypothesis == "H0",
      iteration == iter_id,
      te == te_labels[1]
    ) |>
      mutate(aoa = ifelse(age < aoa, NA, aoa)),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.grid = element_blank()
  ) +
  transition_reveal(age)

eli_gif <- animate(eli_plot,
  width = 5,
  height = 4,
  units = "in",
  renderer = gifski_renderer(),
  duration = 10,
  res = 200
)

anim_save("assets/gif/ambla-eli-single.gif", eli_gif)


# animations (all, facilitation) -----------

iter_id <- 1

eli_data_plot <- eli_df |>
  filter(
    hypothesis == "H1",
    iteration == iter_id,
    te == te_labels[2]
  ) |>
  mutate(
    n_cum = cumsum(n_month),
    y_pos = max(eli),
    .by = c(te, id, id, hypothesis)
  )

eli_plot <- eli_data_plot |>
  ggplot(aes(age, eli,
    colour = id,
    fill = id,
    shape = id
  )) +
  geom_hline(
    yintercept = threshold,
    linewidth = 3 / 4,
    colour = "grey80"
  ) +
  geom_line(aes(group = interaction(id, iteration, language)),
    linewidth = 3 / 4,
    alpha = 1 / 10
  ) +
  annotate(
    geom = "label",
    label = glue::glue("Acquisition threshold: {threshold}"),
    label.r = unit(0, "lines"),
    label.size = 0,
    fill = "grey80",
    x = 0,
    y = threshold,
    size = 3,
    hjust = 0
  ) +
  geom_line(
    data = eli_data_plot,
    aes(group = interaction(id)),
    linewidth = 3 / 4
  ) +
  geom_label(
    data = filter(
      logistic_df,
      hypothesis == "H1",
      iteration == iter_id,
      te == te_labels[2]
    ) |>
      mutate(aoa = ifelse(age < aoa, NA, aoa)),
    aes(
      label = round(aoa, 2),
      x = aoa,
      y = threshold
    ),
    colour = "white",
    show.legend = FALSE,
    size = 4,
    label.r = unit(0, "lines")
  ) +
  labs(
    x = "Age (months)",
    y = "Learning instances",
    colour = "Language",
    shape = "Language profile",
    fill = "Language",
    linetype = "Language profile"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  scale_colour_manual(values = clrs) +
  scale_fill_manual(values = clrs) +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ","),
    limits = c(0, age_max)
  ) +
  theme_ambla() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 8),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.grid = element_blank()
  ) +
  transition_reveal(age)



eli_gif <- animate(eli_plot,
  width = 5,
  height = 4,
  units = "in",
  renderer = gifski_renderer(),
  duration = 10,
  res = 200
)

anim_save("assets/gif/ambla-single-c.gif", eli_gif)
