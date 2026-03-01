# ==============================================================================
# DECADES VIZ DASHBOARDS
# ==============================================================================
library(tidyverse)
library(patchwork)
library(ggridges)
library(ggstream)
library(ggtext)
library(showtext)
library(ragg)
library(glue)
library(viridis)
library(ggrepel)

# 1. SETUP
# ------------------------------------------------------------------------------
options(ragg.max_dim = 200000)
output_dir <- "output/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive=TRUE)

font_add_google("Shrikhand", "beatles_font")
font_add_google("Montserrat", "clean_font") 
showtext_auto()

master_df <- read_csv("master_cleaned.csv", show_col_types = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year <= 2025)

# 2. DECADE-SPECIFIC THEMES
# ------------------------------------------------------------------------------
decade_styles <- list(
  "1960s" = list(bg = "#FFFFFF", accent = "#f5c518"),
  "1970s" = list(bg = "#FFF59D", accent = "#2d9cdb"),
  "1980s" = list(bg = "#FFCCBC", accent = "#d560ba"),
  "1990s" = list(bg = "#B3E5FC", accent = "#eb5757"),
  "2000s" = list(bg = "#DCEDC8", accent = "#27ae60"),
  "2010s" = list(bg = "#E1BEE7", accent = "#f5c518"),
  "2020s" = list(bg = "#B2DFDB", accent = "#2d9cdb"),
  "1960-2025" = list(bg = "#F5F5F5", accent = "#f5c518")
)

genre_colors <- c(
  "Rock"="#f5c518", "Pop"="#2d9cdb", "R&B/Soul"="#9b59b6", "Hip-Hop"="#e74c3c", 
  "Country/Folk"="#27ae60", "Electronic"="#d560ba", "Jazz/Blues"="#e67e22", 
  "Reggae/Ska"="#1abc9c", "Latin"="#c0392b", "Other"="#34495e", "Unknown"="#95a5a6"
)

# 3. METRONOME
# ------------------------------------------------------------------------------
create_metronome_grob <- function(data_subset, accent_color, current_bg) {
  
  stats <- data_subset %>%
    summarize(
      avg_bpm = mean(audio_tempo, na.rm=TRUE),
      min_bpm = quantile(audio_tempo, 0.05, na.rm=TRUE),
      max_bpm = quantile(audio_tempo, 0.95, na.rm=TRUE),
      count = n()
    )
  
  min_scale <- 40; max_scale <- 208
  
  bpm_to_coord <- function(bpm, radius = 1) {
    val <- pmax(min_scale, pmin(max_scale, bpm))
    norm <- (val - min_scale) / (max_scale - min_scale)
    angle <- (3/4 * pi) - (norm * (pi / 2)) 
    list(x = radius * cos(angle), y = radius * sin(angle))
  }
  
  needle_tip <- bpm_to_coord(stats$avg_bpm, radius = 0.85)
  weight_pos <- bpm_to_coord(stats$avg_bpm, radius = 0.65)
  
  body_shape <- tibble(
    x = c(-0.6, 0.6, 0.15, -0.15), 
    y = c(0, 0, 1.2, 1.2)
  )
  
  ticks <- tibble(bpm = seq(40, 208, by = 20)) %>%
    rowwise() %>%
    mutate(
      start = list(bpm_to_coord(bpm, 0.90)),
      end   = list(bpm_to_coord(bpm, 0.95))
    ) %>%
    unnest_wider(start, names_sep = "_") %>%
    unnest_wider(end, names_sep = "_")
  
  wood_color <- accent_color 
  face_color <- "#FAFAFA"    
  metal_color <- "#555555"
  
  ggplot() +
    geom_polygon(data = body_shape, aes(x=x, y=y), fill = wood_color, color = "#333333", linewidth = 1) +
    annotate("polygon", x = c(-0.4, 0.4, 0.1, -0.1), y = c(0.1, 0.1, 1.1, 1.1), fill = face_color, alpha = 0.95) +
    geom_segment(data = ticks, aes(x = start_x, y = start_y, xend = end_x, yend = end_y), color = "#333333", linewidth = 0.5) +
    annotate("path", x = 0.9 * cos(seq(pi/4, 3*pi/4, length=100)), 
             y = 0.9 * sin(seq(pi/4, 3*pi/4, length=100)), color = "#333333", alpha=0.3) +
    annotate("segment", x = 0, y = 0.1, xend = needle_tip$x, yend = needle_tip$y, 
             color = metal_color, linewidth = 2, lineend = "round") +
    annotate("point", x = weight_pos$x, y = weight_pos$y, size = 5, color = "#AA8800") +
    annotate("point", x = weight_pos$x, y = weight_pos$y, size = 2, color = "#FFFF00", alpha = 0.5) +
    annotate("point", x = 0, y = 0.1, size = 3, color = "#333333") +
    
    annotate("text", x = 0, y = 0.35, label = paste0(round(stats$avg_bpm), " BPM"), 
             family = "beatles_font", size = 7, color = "#333333") +
    annotate("text", x = 0, y = 0.25, label = paste0("Avg of ", stats$count, " Songs"), 
             family = "clean_font", size = 3, color = "#666666", fontface = "italic") +
    annotate("text", x = -0.5, y = 0.8, label = paste0("Min\n", round(stats$min_bpm)), 
             family = "clean_font", size = 3, color = "#555555", hjust = 1) +
    annotate("text", x = 0.5, y = 0.8, label = paste0("Max\n", round(stats$max_bpm)), 
             family = "clean_font", size = 3, color = "#555555", hjust = 0) +
    labs(title="TEMPO", subtitle="Avg BPM (Tempo)") +
    coord_fixed(xlim = c(-0.8, 0.8), ylim = c(0, 1.3), clip = "off") +
    theme_void() +
    theme(
      plot.title = element_text(family = "beatles_font", size = 24, hjust = 0.5, color = "#333333"),
      plot.subtitle = element_text(family = "clean_font", size = 14, hjust = 0.5, color = "#555555"),
      plot.background = element_rect(fill = current_bg, color = NA),
      panel.background = element_rect(fill = current_bg, color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# 4. DASHBOARD GENERATOR
# ------------------------------------------------------------------------------
create_dashboard <- function(title_text, data_subset, style_list, is_master = FALSE) {
  
  current_bg <- style_list$bg
  accent_color <- style_list$accent
  
  min_yr <- min(data_subset$year); max_yr <- max(data_subset$year)
  # Update X-axis breaks to show every year for non-master charts
  x_breaks <- if(is_master) seq(1960, 2020, by=10) else seq(min_yr, max_yr, by=1)
  
  theme_custom <- theme_minimal() + theme(
    plot.background = element_rect(fill = current_bg, color = NA),
    text = element_text(color = "#111111", size = 20, family="clean_font"), 
    panel.grid = element_blank(),
    axis.text = element_text(color = "#222222", size = 14),
    axis.title = element_text(size = 20, face = "bold"),
    plot.title = element_text(family="beatles_font", size = 32, hjust=0.5, margin=margin(b=10)),
    plot.subtitle = element_markdown(family="clean_font", size = 16, color="#222222", hjust=0.5, lineheight=1.2),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )
  
  # A. TITLE
  p_title <- ggplot() + 
    annotate("text", x=0.5, y=0.5, label=title_text, size=30, family="beatles_font", color=accent_color) + 
    theme_void() + theme(plot.background = element_rect(fill = current_bg, color = NA))
  
  # B. TOP ARTIST (Text always on top with dynamic Y limits)
  leaders <- data_subset %>% group_by(year, performer) %>% summarize(wks = sum(weeks_on_chart, na.rm=T), .groups="drop") %>% group_by(year) %>% slice_max(wks, n=1, with_ties=F)
  artist_genres <- data_subset %>% group_by(year, performer) %>% slice_max(weeks_on_chart, n=1, with_ties=FALSE) %>% select(year, performer, parent_genre)
  
  leaders <- leaders %>% left_join(artist_genres, by=c("year", "performer")) %>% arrange(year)
  
  # Dynamic Y-Axis Calculation for Top Artist
  y_max_art <- max(leaders$wks, na.rm=TRUE)
  y_lim_art <- ifelse(y_max_art > 50, ceiling(y_max_art / 100) * 100 + 100, ceiling(y_max_art / 10) * 10 + 10)
  
  top_1_perf <- leaders %>% group_by(performer) %>% summarize(total_wks = sum(wks)) %>% slice_max(total_wks, n=1, with_ties=FALSE) %>% pull(performer)
  
  if(is_master) {
    leaders <- leaders %>% mutate(fill_col = if_else(performer == top_1_perf, parent_genre, "Default"))
    legend_artist_html <- glue("<span style='color:{genre_colors[artist_genres$parent_genre[artist_genres$performer == top_1_perf][1]]};'>● {top_1_perf}</span>")
    
    p_artist <- ggplot(leaders, aes(x=year, y=wks, fill=fill_col)) + 
      geom_col(color="black", linewidth=0.2) + 
      scale_fill_manual(values = c(genre_colors, "Default" = "#CCCCCC")) + 
      scale_x_continuous(breaks=x_breaks) + scale_y_continuous(limits = c(0, y_lim_art), expand = c(0, 0)) + 
      theme_custom + labs(title="TOP ARTISTS", subtitle=paste0("Leader: ", legend_artist_html), x="", y="Weeks")
  } else {
    p_artist <- ggplot(leaders, aes(x=year, y=wks)) + 
      geom_col(fill=accent_color, color="black", linewidth=0.5) +
      geom_richtext(aes(
        y = wks + y_max_art * 0.02, # Placed just above the bar
        label = str_replace_all(str_wrap(performer, width = 15), "\n", "<br>")
      ), 
      hjust = 0, color = "black", angle=90, vjust=0.5, halign=0.5, size=4, lineheight=0.8, fontface="bold", family="clean_font",
      fill = NA, label.color = NA) +
      scale_x_continuous(breaks=x_breaks) + 
      scale_y_continuous(limits = c(0, y_lim_art), expand = c(0, 0)) + 
      theme_custom + 
      labs(title="TOP ARTISTS", subtitle="The artist with the most total weeks on the Hot 100.", x="", y="Weeks")
  }
  
  # C. MOST POPULAR SONGS (Text always on top with dynamic Y limits & 1-row legend)
  top_songs_all <- data_subset %>% 
    filter(peak_pos == 1) %>% 
    mutate(title_clean = str_remove_all(title, "\\s*\\(.*?\\)")) %>% 
    group_by(year) %>% 
    slice_max(weeks_on_chart, n = 1, with_ties = FALSE) %>% 
    ungroup() %>%
    arrange(year) %>%
    select(year, title_clean, performer, parent_genre, wks = weeks_on_chart)
  
  # Dynamic Y-Axis Calculation for Top Songs
  y_max_song <- max(top_songs_all$wks, na.rm=TRUE)
  y_lim_song <- ifelse(y_max_song > 50, ceiling(y_max_song / 100) * 100 + 200, ceiling(y_max_song / 10) * 10 + 30)
  
  if(is_master) {
    top_single_labeled <- top_songs_all %>% slice_max(wks, n=1, with_ties=FALSE) 
    p_popular <- ggplot(top_songs_all, aes(x = year, y = wks, fill = parent_genre)) +
      geom_col(color = "black", alpha = 0.9, width = 0.8) + 
      scale_fill_manual(values = genre_colors, drop = TRUE) +
      geom_text(data = top_single_labeled, 
                aes(label = paste0(performer, "\n—\n", str_wrap(title_clean, width = 20))),
                vjust = -0.5, angle = 15, size = 4.5, fontface = "bold", 
                family = "clean_font", lineheight = 0.9) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(limits = c(0, y_lim_song), expand = c(0, 0)) + 
      theme_custom +
      theme(
        legend.position = "bottom",          
        legend.title = element_blank(),      
        legend.text = element_text(size=12, family="clean_font"),
        legend.margin = margin(t=-10)
      ) +
      guides(fill = guide_legend(nrow = 1)) + # Forced 1 row
      labs(title = "BEST RANKING SONGS", subtitle = "The single defining song of each year that held the #1 spot the longest.", x = "", y = "Weeks")
  } else {
    p_popular <- ggplot(top_songs_all, aes(x = year, y = wks, fill = parent_genre)) +
      geom_col(color = "black", alpha = 0.9, width = 0.8) + 
      scale_fill_manual(values = genre_colors, drop = TRUE) +
      geom_richtext(aes(
        y = wks + y_max_song * 0.02, # Placed just above the bar
        label = str_replace_all(str_wrap(paste(title_clean, "-", performer), width = 25), "\n", "<br>")
      ), 
      hjust = 0, color = "black", angle=90, vjust=0.5, halign=0.5, size=3.5, lineheight=0.8, fontface="bold", family="clean_font",
      fill = NA, label.color = NA) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(limits = c(0, y_lim_song), expand = c(0, 0)) + 
      theme_custom +
      theme(
        legend.position = "bottom",          
        legend.title = element_blank(),      
        legend.text = element_text(size=12, family="clean_font"),
        legend.margin = margin(t=-10)
      ) +
      guides(fill = guide_legend(nrow = 1)) + # Forced 1 row
      labs(title = "BEST RANKING SONGS", subtitle = "The single defining song of each year that held the #1 spot the longest.", x = "", y = "Weeks")
  }
  
  # D. VELOCITY 
  top_outlier <- data_subset %>% slice_max(weeks_on_chart, n=1, with_ties=FALSE)
  p_vel <- ggplot(data_subset, aes(x=peak_pos, y=weeks_on_chart)) + 
    geom_bin2d(bins=30, color="black", linewidth=0.1) + 
    scale_fill_gradient(low="#ccff00", high="#4b0082") + scale_x_reverse(limits=c(100, 1)) +
    geom_text_repel(data = top_outlier, aes(label = paste0(performer, " - ", title, "\n(", weeks_on_chart, " wks)")), 
                    nudge_x = -25, nudge_y = 5, color = "black", family = "clean_font", size = 3.5, fontface = "bold", segment.color = NA) +
    theme_custom + 
    labs(title="Songs Trajectory", subtitle="Mapping Song Rank vs. Time on the Chart. Darker spots = most songs drop from the list quickly.", x="Peak Rank", y="Weeks")
  
  # E. WAVE 
  top_4_wave <- data_subset %>% filter(parent_genre != "Other") %>% group_by(parent_genre) %>% summarize(n = n_distinct(paste(title, performer))) %>% slice_max(n, n=4) %>% pull(parent_genre)
  wave_data <- data_subset %>% filter(parent_genre %in% top_4_wave) %>% group_by(year, parent_genre) %>% summarize(n = n_distinct(paste(title, performer)), .groups="drop")
  
  p_wave <- ggplot(wave_data, aes(x=year, y=n, fill=parent_genre)) +
    geom_stream(type="mirror", bw=0.6, color="black", linewidth=0.3) + 
    scale_fill_manual(values=genre_colors, name="Genre") +
    scale_x_continuous(breaks=x_breaks) + 
    theme_custom + 
    theme(
      axis.text.y=element_blank(),
      legend.position = "bottom",          
      legend.title = element_blank(),      
      legend.text = element_text(size=12, family="clean_font"),
      legend.margin = margin(t=-10)
    ) +
    labs(title="THE VIBE SHIFT", subtitle="Tracking the rise and fall of genres. Thicker streams = more market dominance.", x="", y="")
  
  # F. DISTRIBUTION OF PEAK POSITIONS
  # ----------------------------------------------------------------------------
  top5_long <- data_subset %>% filter(parent_genre != "Other") %>% group_by(parent_genre) %>% summarize(n = n_distinct(paste(title, performer))) %>% slice_max(n, n=5) %>% pull(parent_genre)
  stats_long <- data_subset %>% filter(parent_genre %in% top5_long) %>% group_by(parent_genre) %>% summarize(med = median(peak_pos, na.rm=T)) %>% arrange(med)
  
  p_ridge <- ggplot(data_subset %>% filter(parent_genre %in% top5_long), aes(x=peak_pos, y=fct_reorder(parent_genre, peak_pos, .fun=median, .desc=TRUE), fill=parent_genre)) +
    geom_density_ridges(alpha=0.9, color="black", scale=1.3) + 
    scale_fill_manual(values=genre_colors) + 
    scale_x_continuous(limits = c(100, 1), breaks = c(100, 75, 50, 25, 1)) +
    annotate("text", x = 100, y = 5.2, label = glue("Best Median: {stats_long$parent_genre[1]} (Rank {stats_long$med[1]})"), 
             family="clean_font", fontface="bold", size=4, hjust=1) +
    theme_custom + 
    labs(title="CHARTING SUCCESS", subtitle="Where do genres typically peak? Peaks further right mean higher average chart ranks.", x="Peak Rank", y="")
  
  # G. METRONOME (REALISTIC)
  p_metronome <- create_metronome_grob(data_subset, accent_color, current_bg)
  
  layout <- "11\n23\n45\n67"
  (p_title + p_artist + p_popular + p_vel + p_wave + p_ridge + p_metronome) + plot_layout(design=layout, heights=c(0.35, 1, 1, 1.3))
}

# 6. EXECUTION LOOP
# ------------------------------------------------------------------------------
decades <- c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")

for(d in decades) {
  subset_df <- master_df %>% filter(year >= as.numeric(gsub("s", "", d)) & year <= as.numeric(gsub("s", "", d)) + 9)
  p <- create_dashboard(paste0("Billboard Hot 100: ", d), subset_df, decade_styles[[d]], is_master=FALSE)
  fname <- paste0(output_dir, "Dashboard_", d, ".png")
  agg_png(fname, width=15, height=15, units="in", res=150, scaling=1.5)
  print(p); dev.off()
  message(paste("Saved:", fname))
}

# Master (All-time)
p_master <- create_dashboard("Billboard Hot 100: 1960 - 2025", master_df, decade_styles[["1960-2025"]], is_master=TRUE)
agg_png(paste0(output_dir, "Dashboard_All_Time.png"), width=20, height=24, units="in", res=150, scaling=2.0) 
print(p_master); dev.off()
message("Saved: Dashboard_All_Time.png")
