# ==============================================================================
# SCRIPT 2: VISUALIZATION & EXPORT
# ==============================================================================
library(tidyverse)
library(patchwork)
library(ggrepel)
library(showtext)
library(ragg)

# 1. SETUP & AESTHETICS
# ------------------------------------------------------------------------------
options(ragg.max_dim = 200000)
font_add_google("Shrikhand", "beatles_font")
font_add_google("Montserrat", "clean_font") 
showtext_auto()

# Load Processed Data
master <- read_csv("master.csv", show_col_types = FALSE)

# Color & Style
txt_col       <- "#1B1F17"        
bar_border    <- "#111111"        
vinyl_red_label <- "#eb5757"      
vinyl_hole      <- "#ffffff"      
grad_low        <- "#f5c518"      
grad_high       <- "#eb5757"      

# 2. THE CHART FUNCTION
# ------------------------------------------------------------------------------
create_warhol_chart <- function(data_source, title_text, bg_hex) {
  
  # A. Local Data Calculation
  fixed_groove_max    <- 100 
  fixed_label_y       <- 215  
  fixed_outer_limit   <- 260  # Optimized for text visibility
  
  local_grooves <- tibble(y = seq(0, fixed_groove_max, by = 5)) %>% 
    crossing(x = seq(1960, 2026, length.out = 100)) %>% mutate(group = y)
  
  local_annotes <- data_source %>%
    mutate(decade = floor(year/10)*10) %>% 
    group_by(decade) %>% 
    slice_max(weeks_on_chart, n=1, with_ties=F) %>% 
    ungroup() %>%
    mutate(
      clean_performer = str_remove(performer, regex(" ?featuring.*", ignore_case = TRUE)),
      label = paste0(title, " - ", clean_performer, "\n", year, " : ", weeks_on_chart, " weeks"),
      
      # Text Positioning
      x_text_pos = case_when(
        decade == 1960 ~ 1965, 
        decade == 1970 ~ 1975, 
        decade == 1980 ~ 1985, 
        decade == 1990 ~ 1995, 
        decade == 2000 ~ 2005, 
        decade == 2010 ~ 2015, 
        decade == 2020 ~ 2020, 
        TRUE ~ year
      ),
      y_text_pos = case_when(
        # Fix for "Walkin' in the rain" text overlap
        str_detect(title, regex("Walkin' in the rain", ignore_case = TRUE)) ~ fixed_label_y + 45,
        decade == 2020 ~ 210,  
        TRUE ~ fixed_label_y             
      )
    )
  
  # Separate Teddy Swims for special handling
  main_annotes <- local_annotes %>% filter(!str_detect(performer, "Teddy Swims"))
  teddy_annote <- local_annotes %>% filter(str_detect(performer, "Teddy Swims"))
  
  # Decade Markers
  decade_markers_filtered <- tibble(
    year = seq(1970, 2020, 10), 
    label = paste0(seq(1970, 2020, 10), "s"),
    y_pos = c(150, 155, 145, 155, 150, 145) 
  )
  
  # B. The Plot Construction
  p <- ggplot(data_source, aes(x = year)) +
    # Vinyl Layers
    annotate("rect", xmin = 1960, xmax = 2025.5, ymin = 0, ymax = fixed_groove_max, fill = "#111111") + 
    geom_path(data = local_grooves, aes(x = x, y = y, group = group), 
              color = "white", linewidth = 0.4, alpha = 0.15) + 
    annotate("rect", xmin = 1960, xmax = 2025.5, ymin = -35, ymax = 0, fill = vinyl_red_label) + 
    annotate("rect", xmin = 1960, xmax = 2025.5, ymin = -35, ymax = -25, fill = vinyl_hole) + 
    
    # Data Bars
    geom_col(aes(y = weeks_on_chart, fill = weeks_on_chart), width = 1.0, alpha = 1, 
             color = bar_border, linewidth = 0.15) + 
    scale_fill_gradient(low = grad_low, high = grad_high, guide = "none") + 
    
    # Regular Labels
    geom_text(data = main_annotes, aes(x = x_text_pos, y = y_text_pos, label = label),
              color = txt_col, size = 11, fontface = "bold", family = "clean_font", lineheight = 1.0) + 
    
    # Teddy Swims Label
    geom_label(data = teddy_annote, aes(x = x_text_pos, y = y_text_pos, label = label),
               color = txt_col, fill = bg_hex, size = 11, fontface = "bold", 
               family = "clean_font", lineheight = 1.0, label.size = 1.5, label.padding = unit(0.3, "lines")) +
    
    # Teddy Swims Curved Arrow (Updated Logic)
    geom_curve(data = teddy_annote, 
               aes(x = year, y = weeks_on_chart + 2, 
                   xend = x_text_pos - 1,    # Tilted left
                   yend = y_text_pos - 90),  # Stops before border
               curvature = -0.3,             # Slight curve
               arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
               color = txt_col, size = 1.2, lineend = "round") +
    
    # Decade Markers
    geom_text(data = decade_markers_filtered, aes(x = year, y = y_pos, label = label), 
              color = txt_col, size = 32, family = "beatles_font") +
    
    # Theme
    coord_polar(start = 0, clip = "off") + 
    scale_x_continuous(limits = c(1960, 2026), breaks = NULL) +
    ylim(-35, fixed_outer_limit) + 
    theme_void() + 
    theme(
      plot.background = element_rect(fill = bg_hex, color = NA), 
      plot.title = element_text(family = "beatles_font", color = txt_col, face = "bold", 
                                hjust = 0.5, size = 145, margin = margin(t = 30, b = 10)),
      plot.subtitle = element_text(family = "clean_font", color = txt_col, 
                                   hjust = 0.5, size = 45, margin = margin(b = 15)),
      plot.caption = element_text(family = "clean_font", color = "grey40", face = "italic", 
                                  hjust = 0.5, size = 30, margin = margin(t = 10, b = 20)),
      plot.margin = margin(0,0,0,0)
    ) +
    labs(
      title = toupper(title_text),
      subtitle = "Billboard Hot 100: Top Song per Decade by Longevity",
      caption = "Longevity is measured by the total accumulated weeks the song was in the top 100"
    )
  return(p)
}

# 3. CHART DATA SUBSETS
# ------------------------------------------------------------------------------
master_winners <- master %>% group_by(year) %>% slice_max(weeks_on_chart, n=1, with_ties=F) %>% ungroup()
p_master <- create_warhol_chart(master_winners, "MASTER RECORD", "#f5c518")

rock_data <- master %>% 
  filter(parent_genre == "Rock") %>% 
  filter(!str_detect(genres, regex("pop", ignore_case = TRUE)) | is.na(genres),
         !str_detect(strGenre, regex("pop", ignore_case = TRUE)) | is.na(strGenre)) %>%
  group_by(year) %>% slice_max(weeks_on_chart, n = 1, with_ties = FALSE) %>% ungroup()
p_rock <- create_warhol_chart(rock_data, "ROCK RECORD", "#eb5757")

pop_data <- master %>% 
  filter(parent_genre %in% c("Pop", "R&B/Soul")) %>% 
  group_by(year) %>% slice_max(weeks_on_chart, n=1, with_ties=FALSE) %>% ungroup()
p_pop <- create_warhol_chart(pop_data, "POP & SOUL RECORD", "#d560ba")

hip_data <- master %>% 
  filter(parent_genre == "Hip-Hop" & year >= 1970) %>% 
  filter(!str_detect(performer, regex("Post Malone", ignore_case = TRUE))) %>% # Post Malone Filter
  group_by(year) %>% slice_max(weeks_on_chart, n=1, with_ties=F) %>% ungroup()
p_hiphop <- create_warhol_chart(hip_data, "HIP-HOP RECORD", "#2d9cdb")

# 4. EXPORT
# ------------------------------------------------------------------------------
# Set your path
output_dir <- ""
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

agg_png(paste0(output_dir, "Master_Record.png"), width = 12, height = 12, units = "in", res = 300)
print(p_master)
invisible(dev.off())

agg_png(paste0(output_dir, "Rock_Record.png"), width = 12, height = 12, units = "in", res = 300)
print(p_rock)
invisible(dev.off())

agg_png(paste0(output_dir, "Pop_Soul_Record.png"), width = 12, height = 12, units = "in", res = 300)
print(p_pop)
invisible(dev.off())

agg_png(paste0(output_dir, "HipHop_Record.png"), width = 12, height = 12, units = "in", res = 300)
print(p_hiphop)
invisible(dev.off())

message("Charts successfully saved to: ", output_dir)