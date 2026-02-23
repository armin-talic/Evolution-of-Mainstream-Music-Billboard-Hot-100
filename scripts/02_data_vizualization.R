# ==============================================================================
# FILE 2: DATA VISUALIZATION
# ==============================================================================
library(tidyverse)
library(patchwork)
library(ggridges)
library(ggstream)
library(ggrepel)
library(lubridate)
library(tidytext)
library(viridis)
library(ggtext) 

# Suppress warnings for clean output
options(warn = -1)

# Set working directory
setwd("")

# ------------------------------------------------------------------------------
# STEP 1: LOAD & PRE-FILTER DATA
# ------------------------------------------------------------------------------
master <- read_csv("prep_master_songs.csv", show_col_types = FALSE) %>%
  filter(year <= 2025) 

artist_genre_map <- read_csv("prep_artist_genres.csv", show_col_types = FALSE)

top_10_list <- artist_genre_map %>% 
  count(parent_genre, sort = TRUE) %>% 
  head(10) %>% 
  pull(parent_genre)

song_debuts <- master %>%
  group_by(title, performer) %>%
  summarize(debut_year = min(year, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# STEP 2: STYLES & THEMES
# ------------------------------------------------------------------------------
infographic_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA),
    text = element_text(color = "#E0E0E0", family = "sans", size = 22),
    plot.title = element_text(face = "bold", size = 30, color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 22, color = "#A0A0A0", hjust = 0.5),
    panel.grid = element_blank(), 
    axis.text = element_text(color = "#A0A0A0", size = 20),
    axis.title = element_text(size = 22, face = "bold"),
    legend.position = "none"
  )

decade_styles <- list(
  "1960s" = list(bg = "#151313", txt = "#F4D06F", fill1 = "#FF8811", line1 = "#9DD9D2", grid = "#2B2626", font = "serif"),
  "1970s" = list(bg = "#1A0B2E", txt = "#ffffff", fill1 = "#FF00AA", line1 = "#00FFFF", grid = NA, font = "sans"),
  "1980s" = list(bg = "#000022", txt = "#00ffff", fill1 = "#ff00ff", line1 = "#00ffff", grid = NA, font = "sans"),
  "1990s" = list(bg = "#151515", txt = "#f0e6d2", fill1 = "#9e2a2b", line1 = "#e09f3e", grid = "#2a2a2a", font = "mono"),
  "2000s" = list(bg = "#09090b", txt = "#ffffff", fill1 = "#ff007f", line1 = "#00d2ff", grid = "#1c1c28", font = "sans"),
  "2010s" = list(bg = "#191414", txt = "white",   fill1 = "#1DB954", line1 = "white",   grid = NA, font = "sans"),
  "2020s" = list(bg = "black",   txt = "white",   fill1 = "#ff0050", line1 = "#00f2ea", grid = NA, font = "sans")
)

# ------------------------------------------------------------------------------
# STEP 3: DASHBOARD ENGINE
# ------------------------------------------------------------------------------
create_decade_dashboard <- function(decade_key, master_df, map_df) {
  s <- decade_styles[[decade_key]]
  start_yr <- as.numeric(gsub("s", "", decade_key))
  end_yr <- start_yr + 9
  
  era_data <- master_df %>% filter(year >= start_yr & year <= end_yr)
  
  era_genre_data <- era_data %>% 
    select(-any_of(c("genres", "strGenre", "parent_genre"))) %>% 
    left_join(map_df, by = "performer", relationship = "many-to-many") %>% 
    filter(!is.na(parent_genre), tolower(parent_genre) != "other") %>%
    select(-any_of("genres")) %>%  
    rename(genres = parent_genre)  
  
  grid_x <- if(decade_key %in% c("1970s", "1980s", "2010s", "2020s")) element_blank() else element_line(color = s$grid, linetype = "dotted")
  grid_y <- if(is.na(s$grid)) element_blank() else element_line(color = s$grid, linetype = "dotted")
  
  era_theme <- theme_minimal() + theme(
    plot.background = element_rect(fill = s$bg, color = NA), 
    panel.background = element_rect(fill = s$bg, color = NA),
    text = element_text(color = s$txt, family = s$font, size = 22), 
    panel.grid.major.y = grid_y,
    panel.grid.major.x = grid_x, 
    axis.text = element_text(color = s$txt, size = 20, face = "bold"), 
    axis.title = element_text(color = s$txt, size = 21, face = "bold"), 
    plot.title = element_text(face = "bold", size = 28, hjust = 0.5), 
    plot.subtitle = element_markdown(size = 21, color = s$txt, hjust = 0.5, lineheight = 1.3), 
    legend.position = "none",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20) 
  )
  
  smart_y_limits <- function(limits) {
    max_val <- limits[2]
    if(is.na(max_val) || max_val <= 0) return(c(0, 100))
    clean_max <- ceiling(max_val / 100) * 100
    if (clean_max == 0) clean_max <- 100
    return(c(0, clean_max))
  }
  
  smart_y_breaks <- function(limits) {
    max_val <- limits[2]
    if(is.na(max_val) || max_val <= 0) return(c(0, 50, 100))
    clean_max <- ceiling(max_val / 100) * 100
    if (clean_max == 0) clean_max <- 100
    return(c(0, clean_max / 2, clean_max))
  }
  
  # T. TITLE BLOCK 
  p_title <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = decade_key, size = 39, fontface = "bold", color = s$txt, family = s$font) + 
    theme_void() + 
    theme(plot.background = element_rect(fill = s$bg, color = NA), plot.margin = margin(t = 30, b = 10))
  
  # D. TOP ARTIST
  leaders_data <- era_data %>% group_by(year, performer) %>% summarize(wks = sum(weeks_on_chart, na.rm = TRUE), .groups = "drop") %>% group_by(year) %>% slice_max(wks, n = 1, with_ties = FALSE)
  artist_subtitle <- "Artist maintaining the highest cumulative weeks on the chart per year"
  
  p_year_leaders <- ggplot(leaders_data, aes(x = year, y = wks)) + 
    geom_col(alpha = 0.9, fill = s$fill1) + 
    geom_text(aes(label = str_wrap(performer, width = 8), y = wks + max(leaders_data$wks)*0.05), 
              vjust = 0, fontface = "bold", size = 7, color = s$txt, lineheight = 0.8) + 
    scale_y_continuous(limits = smart_y_limits, breaks = smart_y_breaks) + 
    scale_x_continuous(breaks = seq(start_yr, end_yr, by = 2)) +
    era_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "TOP ARTIST BY YEAR", subtitle = artist_subtitle, x = "Year", y = "Chart Weeks")
  
  # A. CHURN 
  era_churn_data <- era_data %>% 
    group_by(year) %>% 
    summarize(Hits = n_distinct(paste(title, performer)),
              Artists = n_distinct(performer), .groups = "drop")
  
  churn_subtitle_html <- paste0("Volume of total unique songs vs. individual artists on the billboard <br>",
                                "<span style='color:", s$fill1, "; font-size:34px;'>&#9632;</span> Artists &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                "<span style='color:", s$line1, "; font-size:34px;'>&#9632;</span> Hits")
  
  p_churn <- ggplot(era_churn_data, aes(x = year)) +
    geom_col(aes(y = Artists), fill = s$fill1, alpha = 0.3) +
    geom_line(aes(y = Hits), color = s$line1, linewidth = 2) + 
    geom_point(aes(y = Hits), color = s$line1, size = 4) +
    scale_y_continuous(limits = smart_y_limits, breaks = smart_y_breaks) + 
    scale_x_continuous(breaks = seq(start_yr, end_yr, by = 2)) +
    era_theme + labs(title = "CHART CHURN", subtitle = churn_subtitle_html, x = "Year", y = "Count")
  
  # B. VELOCITY 
  low_col <- if(decade_key == "1990s") s$grid else s$line1
  high_col <- if(decade_key == "1990s") s$line1 else s$fill1
  
  vel_subtitle_html <- paste0("Density of songs by peak rank and total lifespan <br>",
                              "<span style='color:", low_col, "; font-size:34px;'>&#9632;</span> **Min** &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ",
                              "<span style='color:", high_col, "; font-size:34px;'>&#9632;</span> **Max**")
  
  p_velocity <- ggplot(era_data, aes(x = peak_pos, y = weeks_on_chart)) + 
    geom_bin2d(bins = 20) + scale_fill_gradient(low = low_col, high = high_col) + 
    scale_x_reverse(limits = c(100, 1)) + 
    scale_y_continuous(limits = smart_y_limits, breaks = smart_y_breaks) +
    era_theme + labs(title = "CHART VELOCITY", subtitle = vel_subtitle_html, x = "Peak Rank", y = "Total Weeks")
  
  # E. WAVE 
  top_3_local <- era_genre_data %>% count(genres, sort=T) %>% head(3) %>% pull(genres)
  
  third_col <- case_when(
    decade_key == "1960s" ~ "#F4D06F",
    decade_key == "1970s" ~ "#FFFF00",
    decade_key == "1980s" ~ "#00FF00",
    decade_key == "1990s" ~ "#9e2a2b",
    decade_key == "2000s" ~ "#B026FF",
    decade_key == "2010s" ~ "#888888",
    decade_key == "2020s" ~ "#B200FF",
    TRUE ~ "white"
  )
  wave_colors <- setNames(c(low_col, high_col, third_col), top_3_local)
  
  wave_subtitle_html <- paste0("Market share evolution of the era's dominant genres <br>", 
                               paste0("<span style='color:", wave_colors[top_3_local], "; font-size: 39px;'>&#9679;</span> **", top_3_local, "**", collapse = " &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "))
  
  p_stream <- era_genre_data %>% filter(genres %in% top_3_local) %>% count(year, genres) %>%
    ggplot(aes(x = year, y = n, fill = genres)) + 
    geom_stream(type = "mirror", bw = 0.5, color = "#121212", linewidth = 0.2) + 
    scale_fill_manual(values = wave_colors) + 
    scale_x_continuous(breaks = seq(start_yr, end_yr, by = 2)) +
    scale_y_continuous(breaks = NULL) +
    era_theme + labs(title = "GENRES WAVE", subtitle = wave_subtitle_html, x = "Year", y = "")
  
  # C. FREQUENCY / LONGEVITY 
  top_5_local <- era_genre_data %>% count(genres, sort=T) %>% head(5) %>% pull(genres)
  

  era_colors <- setNames(viridis_pal(option = "turbo", begin = 0.2, end = 1)(10), top_10_list)
  
  era_median_all <- median(era_data$weeks_on_chart, na.rm = TRUE)
  
  ridge_subtitle <- "Distribution of total weeks on chart for the most dominant genres"
  
  p_ridge <- ggplot(era_genre_data %>% filter(genres %in% top_5_local), aes(x = weeks_on_chart, y = fct_reorder(genres, weeks_on_chart, .fun = median), fill = genres)) + 
    geom_density_ridges(alpha = 0.85, color = "#121212", scale = 1.2) + 
    geom_vline(xintercept = era_median_all, color = "white", linetype = "dashed", linewidth = 1.5) +
    # UPDATED: Placed higher (y + 1.2)
    annotate("text", x = era_median_all + 0.8, y = length(top_5_local) + 1.2, 
             label = paste("Typical song stays for", round(era_median_all, 1), "weeks"), 
             color = "white", size = 8, fontface = "bold", hjust = 0) + 
    scale_fill_manual(values = era_colors) + 
    scale_x_continuous(limits = c(0, 30)) + 
    # UPDATED: Added 35% empty headroom to strictly prevent text cutoffs
    scale_y_discrete(expand = expansion(mult = c(0.05, 0.35))) + 
    coord_cartesian(clip = "off") +
    era_theme + labs(title = "GENRE LONGEVITY", subtitle = ridge_subtitle, x = "Weeks on Hot 100", y = "")
  
  # ASSEMBLY LAYOUT 
  layout <- "
    11
    23
    45
    66
  "
  
  dashboard <- (p_title + p_year_leaders + p_churn + p_velocity + p_stream + p_ridge) + 
    plot_layout(design = layout, heights = c(0.15, 1, 1, 0.9)) + 
    plot_annotation(theme = theme(plot.background = element_rect(fill = s$bg, color = NA)))
  
  return(dashboard)
}

# ------------------------------------------------------------------------------
# STEP 4: RUN DASHBOARD GENERATORS
# ------------------------------------------------------------------------------
decades <- c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")
for(d in decades) {
  assign(paste0("dashboard_", d), create_decade_dashboard(d, master, artist_genre_map), envir = .GlobalEnv)
}

# ------------------------------------------------------------------------------
# STEP 5: INFOGRAPHIC ASSEMBLY & EXPORT
# ------------------------------------------------------------------------------
options(ragg.max_dim = 100000)

black_gap <- plot_spacer() + theme(plot.background = element_rect(fill = "#000000", color = NA))

grand_infographic <- (
  dashboard_1960s / black_gap /
    dashboard_1970s / black_gap /
    dashboard_1980s / black_gap /
    dashboard_1990s / black_gap /
    dashboard_2000s / black_gap /
    dashboard_2010s / black_gap /
    dashboard_2020s
) + 
  plot_layout(heights = c(1, 0.04, 1, 0.04, 1, 0.04, 1, 0.04, 1, 0.04, 1, 0.04, 1)) +
  plot_annotation(
    title = "Evolution of Mainstream Music<br>Billboard Hot 100", 
    subtitle = "Visualization of culture shifts, genre trends, and artist dominance (1960-2025)",
    theme = theme(
      plot.background = element_rect(fill = "#121212", color = NA),
      plot.title = element_markdown(size = 94, face = "bold", color = "white", hjust = 0.5, lineheight = 1.1, margin = margin(t=60, b=30)),
      plot.subtitle = element_text(size = 44, color = "#A0A0A0", hjust = 0.5, margin = margin(b=100))
    )
  )

ggsave("Billboard_Infographic.png", grand_infographic, width = 32, height = 230, dpi = 300, limitsize = FALSE)