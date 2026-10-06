####data preparation####
install.packages("tidyverse")
library(tidyverse)  

data <- read_csv("./data.csv")  
data <- data|>
  mutate(profile = case_when(
    profile == 1 ~ "advanced",
    profile == 2 ~ "intermediate",
    profile == 3 ~ "elementary",
    profile == 4 ~ "beginner"
  ))

library(dplyr)
library(tidyr)
library(ggplot2)

data <- data %>%
  mutate(education = factor(education, levels = c("below high school", "high school", "university")),
         profile = factor(profile))

long_data <- data %>%
  pivot_longer(cols = starts_with("score_"), names_to = "level", values_to = "score") %>%
  mutate(level = factor(level,
                        levels = c("score_1k", "score_2k", "score_3k", "score_4k", "score_5k"),
                        labels = c("1000", "2000", "3000", "4000", "5000")))
####


#Version1: Scientific Article
####Figure 1A: Scores by educational group####
fig1_data <- long_data %>%
  group_by(education, level) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            n = sum(!is.na(score)),
            .groups = "drop") %>%
  mutate(se = sd / sqrt(n),
         ci = qt(0.975, df = n - 1) * se)

pd <- position_dodge(width = 0.3)

fig1 <- ggplot(fig1_data, aes(x = level, y = mean, group = education,
                              linetype = education, shape = education)) +
  geom_line(position = pd, colour = "black") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.15, linetype = "solid", position = pd) +
  geom_point(position = pd, size = 2.5, colour = "black", fill = "white") +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        labels = c("Below high school", "High school", "University")) +
  scale_shape_manual(values = c(21, 24, 15),
                     labels = c("Below high school", "High school", "University")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(title = "Figure 1. Mean uVLT Scores Across Frequency Levels by Educational Group",
       x = "Frequency level (word families)",
       y = "Mean score (maximum = 30)",
       linetype = "Educational group", shape = "Educational group") +
  theme_classic(base_size = 11) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.03, 0.03),
        legend.justification = c(0, 0),
        legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(size = 12, hjust = 0))

fig1
ggsave("Figure1.png", fig1, width = 7, height = 5)

####Figure 2A: Four latent profiles####
profile_labels <- c("advanced" = "Advanced vocabulary knowledge profile",
                    "intermediate" = "Intermediate vocabulary knowledge profile",
                    "elementary" = "Elementary vocabulary knowledge profile",
                    "beginner" = "Beginner vocabulary knowledge profile")

profile_order <- c("advanced", "intermediate", "elementary", "beginner")
profile_info
profile_info <- data %>%
  mutate(profile = as.character(profile)) %>%
  count(profile) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label = paste0(profile_labels[profile], " (n = ", n, ", ", percent, "%)"))

fig2_data <- long_data %>%
  mutate(profile = as.character(profile)) %>%
  group_by(profile, level) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            n = sum(!is.na(score)),
            .groups = "drop") %>%
  mutate(se = sd / sqrt(n),
         ci = qt(0.975, df = n - 1) * se,
         profile = factor(profile,
                          levels = profile_order,
                          labels = profile_info$label[match(profile_order, profile_info$profile)]))

fig2 <- ggplot(fig2_data, aes(x = level, y = mean, group = profile,
                              linetype = profile, shape = profile)) +
  geom_line(colour = "black") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.1, linetype = "solid") +
  geom_point(size = 2.5, colour = "black", fill = "white") +
  scale_linetype_manual(values = c("solid", "dotdash", "dotted", "dashed")) +
  scale_shape_manual(values = c(16, 22, 17, 21)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(title = "Figure 2. Mean uVLT Scores by Frequency Level and Latent Profile",
       x = "Frequency level (word families)",
       y = "Mean score (maximum = 30)",
       linetype = "Latent profile", shape = "Latent profile") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(size = 12, hjust = 0))

fig2
ggsave("Figure2.png", fig2, width = 8, height = 6)

####Figure 3A: Profile distribution across educational groups####
fig3_data <- data %>%
  filter(!is.na(profile)) %>%
  mutate(profile = as.character(profile)) %>%
  count(education, profile) %>%
  group_by(education) %>%
  mutate(total = sum(n),
         percent = n / total * 100) %>%
  ungroup() %>%
  mutate(profile = factor(profile,
                          levels = profile_order,
                          labels = c("Advanced", "Intermediate", "Elementary", "Beginner")),
         education_label = factor(paste0(c("Below high school", "High school", "University")[as.integer(education)],
                                         "\n(n = ", total, ")"),
                                  levels = unique(paste0(c("Below high school", "High school", "University")[as.integer(education)],
                                                         "\n(n = ", total, ")"))),
         text_colour = ifelse(profile %in% c("Advanced", "Intermediate"), "white", "black"))

fig3_data

fig3 <- ggplot(fig3_data, aes(x = education_label, y = percent,
                              fill = profile, group = profile)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(percent, 1), "%"), colour = text_colour),
            position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
  scale_fill_manual(values = c("grey20", "grey45", "grey75", "grey95")) +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(title = "Figure 3. Distribution of Latent Profiles Within Educational Groups",
       x = "Educational group",
       y = "Learners within educational group (%)",
       fill = "Latent profile") +
  theme_classic(base_size = 11) +
  theme(legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0))

fig3
ggsave("Figure3.png", fig3, width = 8, height = 5)
####

##Version2: Accessible Outlet
####data preparation####
install.packages("plotly")
library(plotly)

edu_names   <- c("Below high school", "High school", "University")
edu_cols    <- c("#D55E00", "#009E73", "#0072B2")
edu_symbols <- c("circle", "triangle-up", "square")

prof_names   <- c("Advanced", "Intermediate", "Elementary", "Beginner")
prof_cols    <- c("#0D0887", "#7E03A8", "#CC4778", "#F89540")
prof_text    <- c("white", "white", "white", "black")
prof_symbols <- c("circle", "square", "triangle-up", "diamond")

base_font <- list(family = "Arial", size = 15, color = "#222222")
x_title   <- "Word frequency level (most common → less common)"
####
####Figure 1B More schooling, more words####
b1_data <- fig1_data %>%
  mutate(education = factor(education,
                            levels = c("below high school", "high school", "university"),
                            labels = edu_names))

gap_1k <- round(b1_data$mean[b1_data$education == "University" & b1_data$level == "1000"] -
                  b1_data$mean[b1_data$education == "Below high school" & b1_data$level == "1000"], 1)
gap_5k <- round(b1_data$mean[b1_data$education == "University" & b1_data$level == "5000"] -
                  b1_data$mean[b1_data$education == "Below high school" & b1_data$level == "5000"], 1)

b1 <- plot_ly()
for (i in seq_along(edu_names)) {
  d <- b1_data %>% filter(education == edu_names[i])
  b1 <- b1 %>%
    add_trace(data = d, x = ~level, y = ~mean,
              type = "scatter", mode = "lines+markers",
              name = edu_names[i],
              line = list(color = edu_cols[i], width = 3),
              marker = list(color = edu_cols[i], size = 11, symbol = edu_symbols[i]),
              hovertemplate = ~paste0("<b>", education, "</b><br>",
                                      level, " level<br>Average: ", round(mean, 1),
                                      " of 30 words (", round(mean / 30 * 100), "%)<extra></extra>"))
}

end_b1 <- b1_data %>% filter(level == "5000")
labels_b1 <- lapply(seq_len(nrow(end_b1)), function(i) {
  list(x = 4, y = end_b1$mean[i], text = paste0("<b>", end_b1$education[i], "</b>"),
       xanchor = "left", xshift = 14, showarrow = FALSE,
       font = list(size = 15, color = edu_cols[as.integer(end_b1$education[i])]))
})

gap_notes <- list(
  list(x = 0, y = end_b1$mean[1] + (b1_data$mean[1] - end_b1$mean[1]),
       text = paste0("Common words:<br>small gap (", gap_1k, " points)"),
       showarrow = TRUE, arrowhead = 2, ax = 40, ay = 70, font = list(size = 14)),
  list(x = 4, y = end_b1$mean[1],
       text = paste0("Rarer words:<br>gap grows to ", gap_5k, " points"),
       showarrow = TRUE, arrowhead = 2, ax = -90, ay = 60, font = list(size = 14))
)

b1 <- b1 %>%
  layout(title = list(text = "<b>More schooling, more words, especially the rarer ones</b>",
                      x = 0, xanchor = "left"),
         font = base_font, showlegend = FALSE,
         xaxis = list(title = x_title, range = c(-0.4, 5.6)),
         yaxis = list(title = "Average words known (out of 30)",
                      range = c(0, 31), dtick = 5, gridcolor = "#e5e5e5"),
         annotations = c(labels_b1, gap_notes),
         margin = list(l = 60, r = 20, t = 60, b = 60)) %>%
  config(displayModeBar = FALSE)

b1

####Figure 2B Four vocabulary profiles####
b2_data <- long_data %>%
  filter(!is.na(profile)) %>%
  mutate(profile = factor(as.character(profile), levels = profile_order, labels = prof_names)) %>%
  group_by(profile, level) %>%
  summarise(mean = mean(score, na.rm = TRUE), .groups = "drop")

b2 <- plot_ly()
for (i in seq_along(prof_names)) {
  d <- b2_data %>% filter(profile == prof_names[i])
  b2 <- b2 %>%
    add_trace(data = d, x = ~level, y = ~mean,
              type = "scatter", mode = "lines+markers",
              name = prof_names[i],
              line = list(color = prof_cols[i], width = 3),
              marker = list(color = prof_cols[i], size = 11, symbol = prof_symbols[i]),
              hovertemplate = ~paste0("<b>", profile, " profile</b><br>",
                                      level, " level<br>Average: ", round(mean, 1),
                                      " of 30 words (", round(mean / 30 * 100), "%)<extra></extra>"))
}

end_b2 <- b2_data %>% filter(level == "5000")
labels_b2 <- lapply(seq_len(nrow(end_b2)), function(i) {
  list(x = 4, y = end_b2$mean[i], text = paste0("<b>", end_b2$profile[i], "</b>"),
       xanchor = "left", xshift = 14, showarrow = FALSE,
       font = list(size = 15, color = prof_cols[as.integer(end_b2$profile[i])]))
})

buttons <- c(
  list(list(label = "Show all", method = "restyle",
            args = list("opacity", as.list(rep(1, 4))))),
  lapply(1:4, function(i) {
    list(label = prof_names[i], method = "restyle",
         args = list("opacity", as.list(ifelse(1:4 == i, 1, 0.12))))
  })
)

b2 <- b2 %>%
  layout(title = list(text = "<b>Learners fall into four distinct vocabulary patterns</b>",
                      x = 0, xanchor = "left", y = 0.98),
         font = base_font, showlegend = FALSE,
         xaxis = list(title = x_title, range = c(-0.4, 5.3)),
         yaxis = list(title = "Average words known (out of 30)",
                      range = c(0, 31), dtick = 5, gridcolor = "#e5e5e5"),
         annotations = labels_b2,
         updatemenus = list(list(type = "buttons", direction = "right",
                                 x = 0, y = 1.02, xanchor = "left", yanchor = "bottom",
                                 showactive = TRUE, font = list(size = 14),
                                 buttons = buttons)),
         margin = list(l = 60, r = 20, t = 100, b = 60)) %>%
  config(displayModeBar = FALSE)

b2
####

####Figure 3B: Every profile appears at every education level####
b3_data <- data %>%
  filter(!is.na(profile)) %>%
  mutate(profile = factor(as.character(profile), levels = profile_order, labels = prof_names),
         edu_name = factor(education,
                           levels = c("below high school", "high school", "university"),
                           labels = edu_names)) %>%
  count(edu_name, profile) %>%
  group_by(edu_name) %>%
  mutate(total = sum(n), percent = n / total * 100) %>%
  ungroup() %>%
  mutate(edu_label = paste0(edu_name, " (n = ", total, ")"))

edu_labels <- unique(b3_data$edu_label[order(b3_data$edu_name)])

b3 <- plot_ly()
for (i in seq_along(prof_names)) {
  d <- b3_data %>% filter(profile == prof_names[i])
  b3 <- b3 %>%
    add_trace(data = d, x = ~percent, y = ~edu_label,
              type = "bar", orientation = "h",
              name = prof_names[i],
              marker = list(color = prof_cols[i], line = list(color = "white", width = 1.5)),
              text = ~paste0(round(percent), "%"),
              textposition = "inside", insidetextanchor = "middle",
              textfont = list(color = prof_text[i], size = 15),
              hovertemplate = ~paste0("<b>", edu_name, "</b><br>", profile, " profile: ",
                                      n, " of ", total, " learners (",
                                      round(percent, 1), "%)<extra></extra>"))
}

b3 <- b3 %>%
  layout(title = list(text = "<b>Every vocabulary profile appears at every education level</b>",
                      x = 0.02, xanchor = "left", xref = "container",
                      y = 0.97, yanchor = "top"),
         font = base_font, barmode = "stack",
         xaxis = list(title = "Share of learners in each education group",
                      range = c(0, 100), ticksuffix = "%", dtick = 20),
         yaxis = list(title = "", categoryorder = "array",
                      categoryarray = rev(edu_labels), automargin = TRUE),
         legend = list(orientation = "h", traceorder = "normal",
                       x = 0, xanchor = "left", y = 1.02, yanchor = "bottom",
                       title = list(text = "<b>Profile:</b> ")),
         margin = list(l = 10, r = 20, t = 120, b = 60)) %>%
  config(displayModeBar = FALSE)

grey_fill <- "#D9D9D9"
grey_text <- "#666666"

b3_buttons <- c(
  list(list(label = "Show all", method = "restyle",
            args = list(list(marker.color = as.list(prof_cols),
                             textfont.color = as.list(prof_text))))),
  lapply(seq_along(prof_names), function(i) {
    list(label = prof_names[i], method = "restyle",
         args = list(list(marker.color = as.list(ifelse(seq_along(prof_names) == i, prof_cols, grey_fill)),
                          textfont.color = as.list(ifelse(seq_along(prof_names) == i, prof_text, grey_text)))))
  })
)

b3 <- b3 %>%
  layout(title = list(text = "<b>Every vocabulary profile appears at every education level</b>",
                      x = 0.5, xanchor = "center", xref = "container",
                      y = 0.97, yanchor = "top"),
         font = base_font, barmode = "stack",
         xaxis = list(title = "Share of learners in each education group",
                      range = c(0, 100), ticksuffix = "%", dtick = 20),
         yaxis = list(title = "", categoryorder = "array",
                      categoryarray = rev(edu_labels), automargin = TRUE),
         updatemenus = list(list(type = "buttons", direction = "right",
                                 x = 0.5, xanchor = "center", y = 1.03, yanchor = "bottom",
                                 showactive = TRUE, font = list(size = 14),
                                 buttons = b3_buttons)),
         legend = list(orientation = "h", traceorder = "normal",
                       x = 0.5, xanchor = "center", y = -0.18, yanchor = "top",
                       itemclick = FALSE, itemdoubleclick = FALSE),
         margin = list(l = 10, r = 20, t = 110, b = 110)) %>%
  config(displayModeBar = FALSE)

b3
####