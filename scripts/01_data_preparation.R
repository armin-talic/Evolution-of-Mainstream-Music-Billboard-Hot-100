# ==============================================================================
# FILE 1: DATA PREPARATION, API FETCHING & DATA ENRICHMENT
# ==============================================================================
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

# Suppress warnings for clean console output
options(warn = -1)

# Set your working directory
setwd("") # set up your wd

# ------------------------------------------------------------------------------
# STEP 1: LOAD RAW BILLBOARD DATA
# ------------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/utdata/rwd-billboard-data/main/data-out/hot-100-current.csv"
raw_data <- read_csv(url, show_col_types = FALSE)
write_csv(raw_data, "raw_data.csv")

# Prepare basic weekly timeline for Visualization
df_fixed <- raw_data %>%
  mutate(
    chart_week = ymd(chart_week),
    year = year(chart_week),
    decade = paste0(floor(year / 10) * 10, "s"),
    is_new = is.na(last_week)
  )

all_artists <- unique(raw_data$performer)

# ------------------------------------------------------------------------------
# STEP 2: MUSICBRAINZ API FETCH 
# ------------------------------------------------------------------------------
cat("\n--- STEP 2: Fetching MusicBrainz API Metadata ---\n")

get_mb_with_relations <- function(artist_name) {
  ua <- user_agent("VinylProject/1.0 (research@example.com)")
  
  Sys.sleep(1.2) # Mandatory API Rate Limit
  search_url <- paste0("https://musicbrainz.org/ws/2/artist?query=artist:", URLencode(artist_name), "&fmt=json")
  
  tryCatch({
    resp <- GET(search_url, ua)
    if (status_code(resp) != 200) return(NULL)
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if (length(data$artists) > 0) {
      art <- data$artists[1, , drop = FALSE]
      mbid <- art$id[1]
      if (is.null(mbid) || is.na(mbid)) return(NULL)
      
      safe_chr <- function(col) if (col %in% names(art) && length(art[[col]]) > 0 && !is.na(art[[col]][1])) as.character(art[[col]][1]) else NA_character_
      
      t1<-NA; t2<-NA; t3<-NA
      if ("tags" %in% names(art) && is.data.frame(art$tags[[1]])) {
        tags <- art$tags[[1]]
        if("count" %in% names(tags)) tags <- tags[order(-as.numeric(tags$count)), ]
        if(nrow(tags)>=1) t1 <- tags$name[1]
        if(nrow(tags)>=2) t2 <- tags$name[2]
        if(nrow(tags)>=3) t3 <- tags$name[3]
      }
      
      # Combine top tags into a single string
      genres_combined <- paste(na.omit(c(t1, t2, t3)), collapse = ", ")
      if(genres_combined == "") genres_combined <- NA_character_
      
      # Call 2: Band Members Lookup
      Sys.sleep(1.2) 
      lookup_url <- paste0("https://musicbrainz.org/ws/2/artist/", mbid, "?inc=artist-rels&fmt=json")
      resp_rel <- GET(lookup_url, ua)
      
      band_members <- NA_character_
      if (status_code(resp_rel) == 200) {
        rel_data <- fromJSON(content(resp_rel, "text", encoding = "UTF-8"))
        if ("relations" %in% names(rel_data) && is.data.frame(rel_data$relations)) {
          rels <- rel_data$relations
          if ("type" %in% names(rels) && "artist" %in% names(rels)) {
            members <- rels[rels$type == "member of band", ]
            if (nrow(members) > 0 && "name" %in% names(members$artist)) {
              band_members <- paste(members$artist$name, collapse = " | ")
            }
          }
        }
      }
      
      return(data.frame(
        performer = artist_name, mb_id = mbid, genres = genres_combined, 
        mb_type = safe_chr("type"), mb_gender = safe_chr("gender"), 
        mb_country = safe_chr("country"), mb_tag_1 = t1, mb_tag_2 = t2, mb_tag_3 = t3,
        mb_band_members = band_members, 
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) return(NULL))
  return(NULL)
}

mb_output_file <- "artist_metadata_full.csv"

if (file.exists(mb_output_file)) {
  mb_cache <- read_csv(mb_output_file, show_col_types = FALSE)
  fetched_mb <- unique(mb_cache$performer)
} else { fetched_mb <- character(0) }

mb_to_fetch <- setdiff(all_artists, fetched_mb)

if (length(mb_to_fetch) > 0) {
  cat("Fetching MusicBrainz for", length(mb_to_fetch), "artists...\n")
  for (i in seq_along(mb_to_fetch)) {
    artist <- mb_to_fetch[i]
    if (i %% 50 == 0 || i == 1) cat(sprintf("[MusicBrainz %d/%d]: %s\n", i, length(mb_to_fetch), artist))
    res <- get_mb_with_relations(artist)
    if (!is.null(res)) {
      write_csv(res, mb_output_file, append = file.exists(mb_output_file))
    } else {

      bad_row <- data.frame(
        performer = artist, mb_id = "FAILED", genres = NA, mb_type = NA, 
        mb_gender = NA, mb_country = NA, mb_tag_1 = NA, mb_tag_2 = NA, mb_tag_3 = NA, 
        mb_band_members = NA
      )
      write_csv(bad_row, mb_output_file, append = file.exists(mb_output_file))
    }
  }
} else { cat("MusicBrainz data is fully fetched.\n") }

# ------------------------------------------------------------------------------
# STEP 3: THEAUDIODB API FETCH 
# ------------------------------------------------------------------------------
cat("\n--- STEP 3: Fetching TheAudioDB API Metadata ---\n")

get_adb_data <- function(artist_name) {
  ua <- user_agent("VinylProject/1.0 (research@example.com)")
  Sys.sleep(0.5) # AudioDB allows faster requests
  
  search_url <- paste0("https://www.theaudiodb.com/api/v1/json/2/search.php?s=", URLencode(artist_name))
  
  tryCatch({
    resp <- GET(search_url, ua)
    if (status_code(resp) == 200) {
      data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      
      if (!is.null(data$artists) && is.data.frame(data$artists)) {
        art <- data$artists[1, ]
        safe_val <- function(val) if(is.null(val) || is.na(val) || val == "") NA_character_ else as.character(val)
        
        return(data.frame(
          performer = artist_name, strGenre = safe_val(art$strGenre),
          strMood = safe_val(art$strMood), strStyle = safe_val(art$strStyle),
          strGender = safe_val(art$strGender), strCountry = safe_val(art$strCountry),
          intMembers = as.numeric(safe_val(art$intMembers)),
          stringsAsFactors = FALSE
        ))
      }
    }
  }, error = function(e) return(NULL))
  return(NULL)
}

adb_output_file <- "theaudiodb_metadata_full.csv"

if (file.exists(adb_output_file)) {
  adb_cache <- read_csv(adb_output_file, show_col_types = FALSE)
  fetched_adb <- unique(adb_cache$performer)
} else { fetched_adb <- character(0) }

adb_to_fetch <- setdiff(all_artists, fetched_adb)

if (length(adb_to_fetch) > 0) {
  cat("Fetching TheAudioDB for", length(adb_to_fetch), "artists...\n")
  for (i in seq_along(adb_to_fetch)) {
    artist <- adb_to_fetch[i]
    if (i %% 50 == 0 || i == 1) cat(sprintf("[AudioDB %d/%d]: %s\n", i, length(adb_to_fetch), artist))
    res <- get_adb_data(artist)
    if (!is.null(res)) {
      write_csv(res, adb_output_file, append = file.exists(adb_output_file))
    } else {
      bad_row <- data.frame(performer = artist, strGenre = NA, strMood = NA, strStyle = NA, strGender = NA, strCountry = NA, intMembers = NA)
      write_csv(bad_row, adb_output_file, append = file.exists(adb_output_file))
    }
  }
} else { cat("TheAudioDB data is fully fetched.\n") }

# ------------------------------------------------------------------------------
# STEP 4: MASTER DATAFRAME CREATION
# ------------------------------------------------------------------------------
cat("\n--- STEP 4: Creating Master Dataframe ---\n")

mb_metadata <- read_csv("artist_metadata_full.csv", show_col_types = FALSE)
adb_metadata <- read_csv("theaudiodb_metadata_full.csv", show_col_types = FALSE)

master <- df_fixed %>%
  filter(year >= 1960) %>%
  group_by(year, decade, performer, title) %>%
  summarize(
    weeks_on_chart = max(wks_on_chart, na.rm = TRUE),
    peak_pos = min(current_week, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mb_metadata, by = "performer") %>%
  left_join(adb_metadata, by = "performer")

# ------------------------------------------------------------------------------
# STEP 5: GENRE CLEANING & ENRICHMENT
# ------------------------------------------------------------------------------
tags_to_drop <- c(
  "other", "na", "american", "british", "uk", "usa", "german", "germany", "english",
  "scottish", "australian", "canadian", "italian", "european", "hawaiian", "chinese", 
  "french", "austrian", "danish", "brazilian", "américain", "south african", "irish", "south korea",
  "actor", "songwriter", "singer", "singers", "lyricist", "composer", "compositeur", 
  "arrangers", "pianist", "saxophonist", "dj", "rapper", "female vocalist", 
  "background vocalist", "audiobook", "special purpose", "session", "compilation",
  "drama", "crime", "video game", "spoken word", "comedy", "occult", "anime", 
  "50s", "60s", "70s", "80s", "90s", "2010s", "2020s", "1960s", "fictitious artist", 
  "fictitious-artist", "aln-sh", "2008 universal fire victim", "death by automobile", 
  "death by heart attack", "death by heart failure", "death by heroin", "death by overdose", 
  "austrian composer", "need attention", "death by brain aneurysm", "death by suicide", 
  "disambiguate", "death by covid-19", "27 club", "the gentle giant", "death by stroke", 
  "golden boy of song", "check it", "death by pneumonia", "johnnie has gone for a solider", 
  "andrews sisters", "60th anniversary", "likedis auto", "death by bronchial infection", 
  "death by murder", "asian", "death by taxes", "châtelet 87 volume 2", "jamaican singer", 
  "jamaican songwriter", "german audiobook reader", "german voice actor", "comedian", 
  "death by gun", "i m a nut", "death by prostate cancer", "welsh", "death by diabetes", 
  "sesame street", "muppet character", "awesome", "franklin square", "long island", "ny", 
  "eddie rubin", "1970s", "extremely long tracks", "marillion", "conductors", "film composer", 
  "special purpose artist", "epic", "madison", "wisconsin", "ohio", "polly browne", 
  "sweet dreams", "author", "audio drama", "satanism", "nl", "you've got a friend", 
  "death by a fall", "musical director", "musician", "has german audiobooks", "italian descent", 
  "conductor", "honey honey 2009", "pickettywitch", "awesomename", "'70s", "winter wonderland", 
  "soprano", "metabrainz board member", "non-music", "united kingdom", "葉加瀬太郎", 
  "art-garfunkel", "death by cocaine", "scientology", "japanese", "kid blue", "star wars", 
  "british orchestra", "death by breast cancer", "ray parker jr", 
  "http://musicbrainz.org/release-group/5716ae91-5a28-3f07-b167-7c50dfe0e107", "us", "2000s", 
  "english singer/songwriter; ex-member of 404", "latoya-jackson", "sillyname", 
  "academy award winner", "producer", "puerto rican", "fixme or cleanup", "scotland", "nim", 
  "rozpadniemy", "ktp", "queer", "1980s", "c81", "childs", "toni", "england", "stretford", 
  "norwegian", "fixme", "1989", "supergroup", "stevie ray vaughan", "united states", "odm", 
  "dttx", "ost", "fictional", "series title as artist", "lgbtqi", "1990s", "hit squad", 
  "whistle register", "female vocalists", "african", "senegalese", "changing faces", "far away", 
  "bay area", "n2", "female vocals", "korean", "utaite", "detroit", "algeria", "rapist", "fav", 
  "american idol", "bork", "not an artist", "50 cent", "youngbloodz", "youngbloodz new music", 
  "wisin y yandel", "yolandita monge", "sexy", "side project", "cast", "server name", 
  "fixme artist credits", "music for soccer moms", "alliteration", "nashville star", 
  "nashville star 2008", "nickelodeon", "seen live", "vbyjtiicc a6pamrvplxdepb02k-", "indian", 
  "needs disambiguation/annotation", "michael jackson", "romanian", "romanian singer", 
  "nepo baby", "new hollow", "mc", "actress", "fictional artist", "pro trump", "the sims", 
  "score", "oakland", "various artists", "nuno", "_edit", "death by fall", "toronto", "children", 
  "anti vax", "lflores077@gmail.com", "vtuber", "eurovision 2023 artists", "gen z", "cologne", 
  "4ad", "text line: 615-819-4399", "drag queen", "liverpool", "contralto", "mezzo-soprano"
)

artist_genre_map <- master %>%
  mutate(merged_genres = coalesce(strGenre, genres)) %>% 
  separate_longer_delim(merged_genres, delim = ", ") %>%
  mutate(genres_clean = str_to_title(str_trim(merged_genres))) %>%
  mutate(genres_clean = ifelse(str_detect(tolower(genres_clean), "rock & roll|rock and roll"), "Rock", genres_clean)) %>%
  filter(!is.na(genres_clean) & !(tolower(genres_clean) %in% tags_to_drop)) %>%
  select(performer, genres = genres_clean) %>% 
  distinct() %>%
  mutate(
    parent_genre = case_when(
      str_detect(tolower(genres), "rock|metal|punk|grunge|aor|rockabilly|skiffle|beat|merseybeat|post-hardcore|metalcore|djent|surf|emo|easycore|mod|indie") ~ "Rock & Metal",
      str_detect(tolower(genres), "hip hop|hip-hop|rap|trap|drill|g-funk|boom bap|horrorcore|ratchet|rage") ~ "Hip-Hop",
      str_detect(tolower(genres), "pop|schlager|bubblegum|yé-yé|k-pop|kpop|j-pop|eurovision|boy group|boy band|girl group|chanson|dansband|adult contemporary|easy listening") ~ "Pop",
      str_detect(tolower(genres), "r&b|rnb|soul|funk|motown|doo-wop|rhythm & blues|quiet storm|neo soul|new jack swing") ~ "R&B/Soul",
      str_detect(tolower(genres), "country|folk|americana|bluegrass|western|nashville|honky tonk|cowboy|singer-songwriter|swamp|tex-mex|stomp and holler|acoustic") ~ "Country/Folk",
      str_detect(tolower(genres), "dance|electronic|house|techno|trance|edm|future bass|jungle|drum and bass|eurodance|club|krushclub|lo-fi|electronica") ~ "Electronic/Dance",
      str_detect(tolower(genres), "jazz|blues|swing|big band|bebop|bossa nova|stride|dixieland|cabaret|lounge") ~ "Jazz/Blues",
      str_detect(tolower(genres), "latin|reggaeton|salsa|bachata|mambo|mariachi|bolero|flamenco|norteño|corrido|mexicano|tropical|ranchera") ~ "Latin",
      str_detect(tolower(genres), "reggae|ska|rocksteady|dancehall|dub") ~ "Reggae/Ska",
      TRUE ~ "Other"
    )
  )

# ------------------------------------------------------------------------------
# STEP 6: Export Files
# ------------------------------------------------------------------------------
cat("\n--- STEP 6: Saving Flattened Files ---\n")

top_10_list <- artist_genre_map %>% count(genres, sort = TRUE) %>% head(10) %>% pull(genres)
artist_genre_map <- artist_genre_map %>%
  mutate(is_top_10_genre = genres %in% top_10_list)

write_csv(master, "prep_master_songs.csv")
write_csv(artist_genre_map, "prep_artist_genres.csv")
write_csv(df_fixed, "prep_weekly_timeline.csv")

