library(ggplot2)
library(ggrepel)
library(showtext)
library(readxl)

df <- read_excel("C:/Users/ps222/Downloads/top_75_wr_epa.xlsx")

df$epa_per_play <- as.numeric(df$epa_per_target)
df$plays <- as.numeric(df$targets)

df$team_color <- as.character(df$team_color)
df$team_color2 <- as.character(df$team_color2)

team_colors <- setNames(df$team_color, df$posteam)
team_colors_secondary <- setNames(df$team_color2, df$posteam)


showtext_auto()
font_add_google("Roboto", "roboto")

ggplot(df, aes(x = targets, y = epa_per_target)) +
  geom_point(aes(fill = posteam), size = 7.2, shape = 21, stroke = 1.11, color = df$team_color2) + 
  scale_fill_manual(values = team_colors, guide = "none") +  
  
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "black", linetype = "dashed", fill = "gray40", alpha = 0.6) +
  
  scale_x_continuous(breaks = seq(25, max(df$plays, na.rm = TRUE), by = 10)) +
  scale_y_continuous(
    breaks = seq(0, round(max(df$epa_per_target, na.rm = TRUE), 1), by = 0.1),
    labels = function(x) sprintf("%.1f", x),
    limits = c(0, max(df$epa_per_target, na.rm = TRUE) * 1.05),
    expand = c(0, 0)
  ) +
  
  geom_vline(xintercept = median(df$targets, na.rm = TRUE), linetype = "dashed", color = "gray40", linewidth = 1) +  
  geom_hline(yintercept = median(df$epa_per_target, na.rm = TRUE), linetype = "dashed", color = "gray40", linewidth = 1) +  
  
  geom_text_repel(aes(label = receiver_player_name), 
                  size = 4, family = "roboto", fontface = "bold",
                  color = "black", segment.color = "black", 
                  segment.size = 1.05, force = 4, max.overlaps = Inf, 
                  box.padding = 1.13, point.padding = 1, alpha = 0.7) +
  
  labs(title = "EPA per Target vs. Targets",
       subtitle = "Top 75 Pass Catchers (By total EPA) | 2024 | Pablo Silva",
       x = "Targets",
       y = "EPA per Target",
       caption = "Data Source: nfl_data_py") +
  
  theme_minimal(base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = "#f4f4f4", color = NA),   
    panel.background = element_rect(fill = "#f4f4f4", color = NA),  
    panel.grid.major = element_line(color = "grey75"),  
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.9),  
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "gray30"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
