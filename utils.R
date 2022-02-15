# 1) Scrape chart data ----
# Create function to scrape chart data
scrape_chart_data <- function(url) {
  
  # Only read HTML once to save time
  # Run in tryCatch to avoid breaking loop if URLs are invalid (some weeks
  # there is no chart data)
  tryCatch({
    table_body <- url %>% 
      read_html() %>% 
      html_element(".chart-table") %>% 
      html_element("tbody")
    
    # Get artist names from top 200 
    artists <- table_body %>% 
      html_elements(".chart-table-track") %>% 
      html_elements("span") %>% # generic text holder
      html_text() %>%  
      str_remove("^by ") %>% 
      str_trim()
    
    # Get track names from top 200 
    track_names <- table_body %>% 
      html_elements(".chart-table-track") %>% 
      html_elements("strong") %>%  # bold
      html_text() %>% 
      str_trim()
    
    # Get track IDs from top 200 
    track_ids <- table_body %>% 
      html_elements(".chart-table-image") %>% 
      html_elements("a") %>% 
      html_attr("href") %>% 
      str_remove("^https://open.spotify.com/track/")
    
    
    # EXERCISE
    track_streams <- 0L
    # END EXERCISE
    
    
    # Return tibble
    tibble(
      "track_name" = track_names,
      "artist" = artists,
      "track_id" = track_ids,
      "track_streams" = track_streams
    )
  }, error = function(e) {NULL}
  )
}


# Separator using for collapsing artist genre list to a single character string
GENRE_SEP <- "||"
# Same but as regex
GENRE_SEP_REGEX <- str_c("\\Q", GENRE_SEP, "\\E")


# Split vector into list containing chunks of size `n`.
# Last element of list might have fewer than `n` elements.
split_every <- function(x, n) {
  # Find split number each element belongs to
  split_id <- x %>%
    seq_along() %>%
    divide_by(n) %>%
    ceiling()
  
  # Perform split
  splits <- x %>%
    split(split_id) %>%
    unname()
  splits
}


# 2) Scrape lyrics ----
display_lyrics_coverage <- function(tracks) {
  n_lyrics <- tracks %>%
    filter(!is.na(lyrics)) %>%
    nrow()
  n_tracks <- tracks %>%
    nrow()
  
  glue("We currently have {n_lyrics} lyrics ",
       "out of {n_tracks} songs ({round(n_lyrics/n_tracks * 100, 1)}%)")
}


# Get lyrics URL from song title/artist for various websites
# Examples:
# - Song "levitating" by "Dua Lipa":
#   - https://www.metrolyrics.com/levitating-lyrics-dua-lipa.html
# - Song "MONTERO (Call Me By Your Name)" by "Lil Nas X":
#   - https://genius.com/Lil-nas-x-montero-call-me-by-your-name-lyrics
# - Song "Peaches (feat. Daniel Caesar & Giveon)" by "Justin Bieber":
#   - https://www.azlyrics.com/lyrics/justinbieber/peaches.html
# - Song "Save Your Tears" by "The Weeknd":
#   - http://www.absolutelyrics.com/lyrics/view/the_weeknd/save_your_tears
get_lyrics_url <- function(song, artist, website = "genius") {
  # Make everything lowercase
  song <- song %>%
    str_to_lower()
  artist <- artist %>%
    str_to_lower()
  
  # Remove all "(feat...)" / "[feat...]" / "(with...)"/ "(with...)" noise from
  # title/artist
  song <- song %>%
    str_remove_all("\\((feat[^\\)]*)\\)") %>%
    str_remove_all("\\[feat([^\\]]*)\\]") %>%
    str_remove_all("\\((with[^\\)]*)\\)") %>%
    str_remove_all("\\[with([^\\]]*)\\]") %>%
    str_squish()
  
  artist <- artist %>%
    str_remove_all("\\(([^\\)]*)\\)") %>%
    str_remove_all("\\[([^\\]]*)\\]") %>%
    str_squish()
  
  # Remove anything following " - " from title (such as " - remix")
  song <- song %>%
    str_remove_all(" - .*") %>%
    str_squish()
  
  # Remove special characters for certain websites
  if (website == "genius") {
    song <- song %>%
      str_remove_all("'|,|\\\\|\\.|&|\\*|!") %>%
      str_squish()
    
    artist <- artist %>%
      str_remove_all("'|,|\\\\|\\.|&|\\*|!") %>%
      str_squish()
  }
  
  # Replace special characters for certain websites
  if (website == "genius") {
    song <- song %>%
      str_replace_all("/", "-") %>%
      str_squish()
    
    artist <- artist %>%
      str_replace_all("/", "-") %>%
      str_squish()
  }
  
  # Build URL skeleton
  lyrics_url <- website %>%
    switch(
      "genius" = glue(
        "https://genius.com/{artist}-{song}-lyrics"
      ),
      "metrolyrics" = glue(
        "https://www.metrolyrics.com/{song}-lyrics-{artist}.html"
      ),
      "azlyrics" = glue(
        ""
      ),
      "absolutelyrics" = glue(
        "http://www.absolutelyrics.com/lyrics/view/{artist}/{song}"
      ),
      "vagalume" = glue(
        "https://www.vagalume.com.br/{artist}/{song}.html"
      )
    )
  
  # Character to replace white spaces
  space_replacement <- case_when(website == "azlyrics" ~ "",
                                 website == "absolutelyrics" ~ "_",
                                 website == "genius" ~ "-",
                                 website == "vagalume" ~ "-",
                                 TRUE ~ "")
  
  lyrics_url %>%
    str_replace_all("[[:space:]]", space_replacement) %>%
    # Remove all parentheses
    str_remove_all("[\\(\\)]") %>%
    # Convert text to ASCII to get rid of special characters (e.g. "í" -> "i", "’" -> "'")
    stri_trans_general("Any-Latin") %>%
    stri_trans_general("Latin-ASCII")
}


# Extract website domain from full URL
get_website_from_url <- function(url) {
  # Extract everything between "://" and ".com":
  #  - positive lookbehind for "://
  #  - non-greedy match for content
  #  - positive lookahead for ".com"
  url %>%
    str_extract("(?<=://)(.*?)(?=\\.com)") %>%
    str_remove("www\\.")
}


# Scrape lyrics from given URL
scrape_lyrics <- function(lyrics_url, show_progress = TRUE, timeout_ = 3.0) {
  
  # Increment progress bar (if requested)
  if (show_progress) {
    pb$tick(tokens = list(what = lyrics_url))
  }
  
  # Extract website domain from full URL
  website <- get_website_from_url(lyrics_url)
  
  # Get HTML from URL; error if response violates timeout
  lyrics_html <- lyrics_url %>%
    httr::GET(., httr::timeout(timeout_)) %>%
    read_html()
  
  # Do actual scraping
  if (website == "genius") {
    lyrics_html %>%
      # Elements whose class begins with "Lyrics__Container"
      html_elements("[class^=Lyrics__Container]") %>%
      # NOTE: html_text2() captures the line breaks correctly, unlike html_text()
      html_text2() %>%
      str_c(collapse = "\n")
    
  } else if (website == "azlyrics") {
    ""
    
  } else if (website == "absolutelyrics") {
    lyrics_html %>%
      html_element("#view_lyrics") %>% 
      html_text()
    
  } else if (website == "metrolyrics") {
    lyrics_html %>%
      html_elements("#lyrics-body-text p") %>%
      html_text() %>%
      str_c(collapse = "\n")
    
  } else if (website == "vagalume") {
    lyrics_html %>%
      html_element("#lyrics") %>%
      html_text2()
    
  } else {
    stop("Invalid website!")
  }
}



# 4) Pre-process ----
get_lyrics_from_track_id <- function(df, my_track_ids) {
  df %>%
    filter(track_id %in% my_track_ids) %>%
    pull(lyrics, track_name) %>%
    as.list()
}


get_lyrics_from_track_name <- function(df, my_track_names) {
  
  map(my_track_names, ~ df %>% 
        filter(track_name %in% .x)
  ) %>%
    bind_rows() %>% 
    pull(lyrics, track_name) %>%
    as.list()
}

# Censor profane words before output
censor_output <- function(df, word_var = word, is_profane_var = is_profane) {
  df %>%
    mutate(
      word = if_else({{ is_profane_var }},
                     # Replace vowels in profane words by *
                     str_replace_all({{ word_var }}, "[aeiou, @]", "*"),
                     {{ word_var }})
    )
}


# 8) Word embeddings ----
# Get names of n most and least similar songs to a reference song, according to
# their average word embeddings
find_similar <- function(cossim, my_track_name, n = 5) {
  similarities <- cossim %>%
    filter(
      track_name == my_track_name
    ) %>%
    # Select top track in case of multiple with same name
    slice(1L) %>%
    # Pull cosine similarities for all other tracks against track of interest
    select(starts_with("V")) %>%
    unlist() %>%
    set_names(cossim$track_name) %>%
    # Remove similarities of 100% -- should be song against itself!
    `[`(. != 1.0)
  
  # Get most and least similar
  most_similar <- similarities %>%
    sort(decreasing = TRUE) %>%
    head(n)
  least_similar <- similarities %>%
    sort(decreasing = FALSE) %>%
    head(n)
  
  # Return as tibble
  c(most_similar, least_similar) %>%
    enframe(name = "track_name", value = "similarity") %>% 
    mutate("Position" = c(rep(glue::glue("Top {n}"), n),
                          rep(glue::glue("Bottom {n}"), n))) %>% 
    relocate(Position)
}

