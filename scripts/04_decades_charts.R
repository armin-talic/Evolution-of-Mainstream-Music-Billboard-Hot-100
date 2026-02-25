# ==============================================================================
# DECADES VIZ 
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
font_add_google("Shrikhand", "beatles_font")
font_add_google("Montserrat", "clean_font") 
showtext_auto()

# output_dir <- 
if(!dir.exists(output_dir)) dir.create(output_dir, recursive=TRUE)

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

# 3. Dynamic labels
# ------------------------------------------------------------------------------
create_metronome_grob <- function(data_subset, accent_color, current_bg) {
  stats <- data_subset %>%
    summarize(
      avg_tempo = mean(audio_tempo, na.rm=TRUE),
      min_tempo = quantile(audio_tempo, 0.05, na.rm=TRUE),
      max_tempo = quantile(audio_tempo, 0.95, na.rm=TRUE)
    )
  
  min_scale <- 0; max_scale <- 220
  bpm_to_coord <- function(bpm) {
    norm <- (pmax(min_scale, pmin(max_scale, bpm)) - min_scale) / (max_scale - min_scale)
    angle <- pi * (1 - norm)
    list(x = cos(angle), y = sin(angle))
  }
  
  dens <- density(data_subset$audio_tempo, from=min_scale, to=max_scale, n=200, na.rm=TRUE)
  heatmap_data <- tibble(bpm = dens$x, density = dens$y) %>%
    mutate(alpha_val = density / max(density), 
           start_angle = pi * (1 - (bpm - min_scale)/(max_scale - min_scale)),
           end_angle = lag(start_angle, default = pi)) %>%
    filter(bpm <= max_scale & bpm >= min_scale)
  
  poly_list <- list()
  for(i in 1:nrow(heatmap_data)) {
    row <- heatmap_data[i,]; x1 <- cos(row$start_angle); y1 <- sin(row$start_angle)
    x2 <- cos(row$end_angle); y2 <- sin(row$end_angle)
    poly_list[[i]] <- tibble(group = i, x = c(0, x1, x2), y = c(0, y1, y2), intensity = row$alpha_val)
  }
  heat_poly <- bind_rows(poly_list)
  
  avg_pos <- bpm_to_coord(stats$avg_tempo); min_pos <- bpm_to_coord(stats$min_tempo); max_pos <- bpm_to_coord(stats$max_tempo)
  
  ggplot() +
    geom_polygon(data = heat_poly, aes(x=x, y=y, group=group, fill=intensity), color=NA) +
    scale_fill_gradient(low = current_bg, high = accent_color) + 
    annotate("path", x = seq(-1, 1, length.out=100), y = sqrt(1 - seq(-1, 1, length.out=100)^2), color = "#333333", linewidth = 1.5) +
    annotate("segment", x=-1, y=0, xend=1, yend=0, color="#333333", linewidth=1.5) +
    annotate("text", x = -1.15, y = 0, label = "0", family="clean_font", size=5, color="#555555") +
    annotate("text", x = 1.15, y = 0, label = "220", family="clean_font", size=5, color="#555555") +
    # Dynamic Average Label following the Red Line
    annotate("text", x = avg_pos$x, y = 1.15, label = paste0(round(stats$avg_tempo), " BPM"), 
             family="clean_font", size=6, fontface="bold", color="#e74c3c") +
    annotate("segment", x=0, y=0, xend=min_pos$x*0.9, yend=min_pos$y*0.9, color="#555555", linetype="dashed", linewidth=1) +
    annotate("text", x=min_pos$x*1.1, y=min_pos$y*1.1 + 0.1, label=paste0(round(stats$min_tempo)), family="clean_font", size=3.5, color="#555555") +
    annotate("segment", x=0, y=0, xend=max_pos$x*0.9, yend=max_pos$y*0.9, color="#555555", linetype="dashed", linewidth=1) +
    annotate("text", x=max_pos$x*1.1, y=max_pos$y*1.1 + 0.1, label=paste0(round(stats$max_tempo)), family="clean_font", size=3.5, color="#555555") +
    annotate("segment", x=0, y=0, xend=avg_pos$x*0.95, yend=avg_pos$y*0.95, color="#e74c3c", linewidth=2) +
    annotate("point", x=0, y=0, size=5, color="#333333") +
    coord_fixed(clip = "off", xlim=c(-1.5, 1.5), ylim=c(0, 1.5)) +
    theme_void() + labs(title="TEMPO ANALYSIS") +
    theme(plot.title = element_text(family="beatles_font", size=24, hjust=0.5),
          plot.background = element_rect(fill = current_bg, color = NA), 
          panel.background = element_rect(fill = current_bg, color = NA),
          legend.position = "none",
          plot.margin = margin(20, 20, 20, 20))
}

# 4. DASHBOARD GENERATOR
# ------------------------------------------------------------------------------
create_dashboard <- function(title_text, data_subset, style_list, is_master = FALSE) {
  
  current_bg <- style_list$bg
  accent_color <- style_list$accent
  
  min_yr <- min(data_subset$year); max_yr <- max(data_subset$year)
  x_breaks <- if(is_master) seq(1960, 2020, by=10) else seq(min_yr, max_yr, by=5)
  p95 <- quantile(data_subset$weeks_on_chart, 0.95, na.rm=TRUE)
  longevity_max <- case_when(p95 <= 20 ~ 20, p95 <= 30 ~ 30, p95 <= 40 ~ 40, p95 <= 50 ~ 50, TRUE ~ 60)
  
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
  
  # B. TOP ARTIST (STRICT TOP 1 FOR BOTH MASTER & DECADES)
  leaders <- data_subset %>% group_by(year, performer) %>% summarize(wks = sum(weeks_on_chart, na.rm=T), .groups="drop") %>% group_by(year) %>% slice_max(wks, n=1, with_ties=F)
  artist_genres <- data_subset %>% group_by(year, performer) %>% slice_max(weeks_on_chart, n=1, with_ties=FALSE) %>% select(year, performer, parent_genre)
  leaders <- leaders %>% left_join(artist_genres, by=c("year", "performer"))
  
  # Strictly only show the Top 1 artist by total weeks in the subset
  top_1_perf <- leaders %>% group_by(performer) %>% summarize(total_wks = sum(wks)) %>% slice_max(total_wks, n=1, with_ties=FALSE) %>% pull(performer)
  
  if(is_master) {
    leaders <- leaders %>% mutate(fill_col = if_else(performer == top_1_perf, parent_genre, "Default"))
    legend_artist_html <- glue("<span style='color:{genre_colors[artist_genres$parent_genre[artist_genres$performer == top_1_perf][1]]};'>● {top_1_perf}</span>")
    
    p_artist <- ggplot(leaders, aes(x=year, y=wks, fill=fill_col)) + 
      geom_col(color="black", linewidth=0.2) + 
      scale_fill_manual(values = c(genre_colors, "Default" = "#CCCCCC")) + 
      scale_x_continuous(breaks=x_breaks) + scale_y_continuous(expand=expansion(mult=c(0, 0.3))) + 
      theme_custom + labs(title="TOP ARTIST BY YEAR", subtitle=paste0("Leader: ", legend_artist_html), x="", y="Weeks")
  } else {
    leaders <- leaders %>% mutate(label_txt = if_else(performer == top_1_perf, str_wrap(performer, 10), NA_character_))
    p_artist <- ggplot(leaders, aes(x=year, y=wks)) + 
      geom_col(fill=accent_color, color="black", linewidth=0.5) +
      geom_text_repel(aes(label=label_txt), nudge_y=10, direction="y", size=3, fontface="bold", family="clean_font", max.overlaps=Inf) +
      scale_x_continuous(breaks=x_breaks) + scale_y_continuous(expand=expansion(mult=c(0, 0.6))) + 
      theme_custom + labs(title="TOP ARTIST BY YEAR", subtitle="Top 1 Labeled", x="", y="Weeks")
  }
  
  # C. MOST POPULAR SONGS 
  top_songs <- data_subset %>% filter(peak_pos == 1) %>% group_by(year, title, performer, parent_genre) %>% summarize(wks = max(weeks_on_chart), .groups="drop") %>% group_by(year) %>% slice_max(wks, n=1, with_ties=FALSE)
  
  # Strictly only one label for the all-time peak hit
  top_single_labeled <- top_songs %>% slice_max(wks, n=1, with_ties=FALSE) 
  
  # C. MOST POPULAR SONG (STRICT 10 BARS + 1 LABEL)
  # ------------------------------------------------------------------------------
  top_songs_all <- data_subset %>% 
    filter(peak_pos == 1) %>% 
    group_by(year, title, performer, parent_genre) %>% 
    summarize(wks = max(weeks_on_chart), .groups="drop") %>% 
    slice_max(wks, n=10, with_ties=FALSE) # Limit to Top 10 Bars total
  
  # Strictly only one label for the single all-time peak hit in this set
  top_single_labeled <- top_songs_all %>% 
    slice_max(wks, n=1, with_ties=FALSE) 
  
  
  
  
  # C. MOST POPULAR SONG (ONE BAR PER YEAR + ONE TEXT LABEL)
  # ------------------------------------------------------------------------------
  top_songs_all <- data_subset %>% 
    filter(peak_pos == 1) %>% 
    mutate(title_clean = str_remove_all(title, "\\s*\\(.*?\\)")) %>% # Remove brackets
    group_by(year) %>% 
    # Pick the #1 song that stayed longest for EACH year
    slice_max(weeks_on_chart, n = 1, with_ties = FALSE) %>% 
    ungroup() %>%
    select(year, title_clean, performer, parent_genre, wks = weeks_on_chart)
  
  # Strictly only one label for the single all-time peak hit in this dashboard
  top_single_labeled <- top_songs_all %>% 
    slice_max(wks, n = 1, with_ties = FALSE) 
  
  p_popular <- ggplot(top_songs_all, aes(x = year, y = wks, fill = parent_genre)) +
    geom_col(color = "black", alpha = 0.9, width = 0.8) + 
    scale_fill_manual(values = genre_colors, drop = TRUE) +
    # Tilted label for the overall leader of the sheet
    geom_text(data = top_single_labeled, 
              aes(label = paste0(performer, "\n—\n", title_clean)),
              vjust = -0.5, angle = 15, size = 4.5, fontface = "bold", 
              family = "clean_font", lineheight = 0.9) +
    # X-axis: Every second year for readability
    scale_x_continuous(breaks = seq(min_yr, max_yr, by = 5)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.8))) + 
    theme_custom +
    labs(title = "MOST POPULAR SONG PER YEAR", 
         subtitle = "The longest-running #1 hit for every year", 
         x = "", y = "Weeks")
  
  # D. VELOCITY (BLACK LINE REMOVED)
  # ------------------------------------------------------------------------------
  top_outlier <- data_subset %>% slice_max(weeks_on_chart, n=1, with_ties=FALSE)
  
  p_vel <- ggplot(data_subset, aes(x=peak_pos, y=weeks_on_chart)) + 
    geom_bin2d(bins=30, color="black", linewidth=0.1) + 
    scale_fill_gradient(low="#ccff00", high="#4b0082") + scale_x_reverse(limits=c(100, 1)) +
    geom_text_repel(
      data = top_outlier, 
      aes(label = paste0(performer, " - ", title, "\n(", weeks_on_chart, " wks)")), 
      nudge_x = -25, 
      nudge_y = 5, 
      color = "black", 
      family = "clean_font", 
      size = 3.5, 
      fontface = "bold",
      segment.color = NA  # This removes the connecting line
    ) +
    theme_custom + 
    labs(title="CHART VELOCITY", subtitle="Radiant density", x="Peak Rank", y="Weeks")
  
  # E. WAVE
  top_4_wave <- data_subset %>% filter(parent_genre != "Other") %>% group_by(parent_genre) %>% summarize(n = n_distinct(paste(title, performer))) %>% slice_max(n, n=4) %>% pull(parent_genre)
  wave_data <- data_subset %>% filter(parent_genre %in% top_4_wave) %>% group_by(year, parent_genre) %>% summarize(n = n_distinct(paste(title, performer)), .groups="drop")
  legend_wave_html <- paste(map_chr(top_4_wave, ~glue("<span style='color:{genre_colors[.x]};'>● {.x}</span>")), collapse="   ")
  
  p_wave <- ggplot(wave_data, aes(x=year, y=n, fill=parent_genre)) +
    geom_stream(type="mirror", bw=0.6, color="black", linewidth=0.3) + scale_fill_manual(values=genre_colors) +
    scale_x_continuous(breaks=x_breaks) + theme_custom + theme(axis.text.y=element_blank()) +
    labs(title="GENRES WAVE", subtitle=legend_wave_html, x="", y="")
  
  # F. LONGEVITY
  top5_long <- data_subset %>% filter(parent_genre != "Other") %>% group_by(parent_genre) %>% summarize(n = n_distinct(paste(title, performer))) %>% slice_max(n, n=5) %>% pull(parent_genre)
  stats_long <- data_subset %>% filter(parent_genre %in% top5_long) %>% group_by(parent_genre) %>% summarize(med = median(weeks_on_chart, na.rm=T)) %>% arrange(desc(med))
  
  p_ridge <- ggplot(data_subset %>% filter(parent_genre %in% top5_long), aes(x=weeks_on_chart, y=fct_reorder(parent_genre, weeks_on_chart, .fun=median), fill=parent_genre)) +
    geom_density_ridges(alpha=0.9, color="black", scale=1.3) + scale_fill_manual(values=genre_colors) + scale_x_continuous(limits = c(0, longevity_max + 10)) +
    annotate("text", x = longevity_max, y = 5.2, label = glue("Leader: {stats_long$parent_genre[1]} ({stats_long$med[1]} wks)"), 
             family="clean_font", fontface="bold", size=4, hjust=1) +
    theme_custom + labs(title="LONGEVITY", subtitle="Median weeks by genre", x="Weeks", y="")
  
  # G. METRONOME
  p_metronome <- create_metronome_grob(data_subset, accent_color, current_bg)
  
  layout <- "11\n23\n45\n67"
  (p_title + p_artist + p_popular + p_vel + p_wave + p_ridge + p_metronome) + plot_layout(design=layout, heights=c(0.35, 1, 1, 1.3))
}

# 5. EXECUTION
# ------------------------------------------------------------------------------
decades <- c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")

for(d in decades) {
  subset_df <- master_df %>% filter(year >= as.numeric(gsub("s", "", d)) & year <= as.numeric(gsub("s", "", d)) + 9)
  p <- create_dashboard(paste0("Billboard Hot 100: ", d), subset_df, decade_styles[[d]], is_master=FALSE)
  fname <- paste0(output_dir, "Dashboard_", d, ".png")
  agg_png(fname, width=15, height=15, units="in", res=150, scaling=1.5)
  print(p); dev.off()
}

p_master <- create_dashboard("Billboard Hot 100: 1960 - 2025", master_df, decade_styles[["1960-2025"]], is_master=TRUE)
agg_png(paste0(output_dir, "Dashboard_All_Time.png"), width=20, height=24, units="in", res=150, scaling=2.0) 
print(p_master); dev.off()