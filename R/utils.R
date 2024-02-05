prev <- function(...) {
        quarto::quarto_preview(browse = TRUE)
}

render <- function() {
        quarto::quarto_render()
}

get_vars_dict <- function(responses) {
    var.names <- c("b_Intercept[1]",
                   "b_Intercept[2]",
                   "b_n_phon_std",
                   "b_age_std",
                   "b_exposure_std",
                   "b_lv_std",
                   "b_exposure_std:lv_std",
                   "b_age_std:exposure_std",
                   "b_age_std:lv_std",
                   "b_age_std:exposure_std:lv_std")
    
    var.labels <- c(
        "Comprehension and Production",
        "Comprehension",
        glue::glue("Length (+1 SD, {round(sd(responses$n_phon), 2)} phonemes)"),
        glue::glue("Age (+1 SD, {round(sd(responses$age), 2)} months)"),
        glue::glue("Exposure (+1 SD, {round(sd(responses$exposure), 2)})"),
        glue::glue("Cognateness (+1 SD, {round(sd(responses$lv), 2)})"),
        "Exposure \u00d7 Cognateness",
        "Age \u00d7 Exposure",
        "Age \u00d7 Cognateness",
        "Age \u00d7 Exposure \u00d7 Cognateness"
    )
    
    vars_dict <- var.labels
    names(vars_dict) <- var.names
    
    return(vars_dict)
}

theme_ambla <- function() {
    theme(strip.background = element_rect(fill = "grey90", colour = "grey90"),
          strip.text = ggtext::element_markdown(size = 10),
          legend.title = element_blank(),
          panel.grid.major.y = element_line(colour = "grey",
                                            linetype = "dotted"),
          plot.background = element_rect(fill = "white",
                                         colour = NA),
          panel.background = element_rect(fill = "white",
                                          colour = NA),
          axis.line = element_line(colour = "grey", linewidth = 1),
          strip.text.x = element_text(face = "bold"))
}