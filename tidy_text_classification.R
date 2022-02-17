library(tidymodels)
library(tidyverse)
library(cld3)
library(tidytext)
library(textrecipes)
library(naivebayes)
library(discrim)
library(textdata)
library(stopwords)
library(SnowballC)
library(lsa)
library(vip)
source("utils.R")


# Tidy Text Classification ------------------------------------------------

## 1) Create genres (our target variable) ----

# Read track information 
tracks <- read_csv("data/tracks.csv.gz") %>% 
  select(-is_explicit)

# Read lyrics currently scraped - these will be appended to the tracks
lyrics <- read_csv("data/lyrics.csv.gz") %>%
  filter(!is.na(lyrics))

# Find maximum number of genres for an artist
max_genres <- tracks %>%
  pull(artist_genres) %>%
  str_count(GENRE_SEP_REGEX) %>%
  replace_na(0L) %>%
  # Maximum number of genre separator characters + 1
  max() + 1L

# Create genre column names
genre_col_names <- str_c("artist_genre_", as.character(1:max_genres))

# Split artist genres column into multiple character columns, each with a single
# genre
tracks <- tracks %>%
  separate(artist_genres, sep = GENRE_SEP_REGEX,
           into = genre_col_names, fill = "right")


tracks <- tracks %>%
  # Replace NA genres with missing string ("")
  mutate(across(starts_with("artist_genre"), ~replace_na(.x, ""))) %>%
  mutate(
    is_genre_dm = if_any(starts_with("artist_genre"), ~str_detect(.x, "death metal")),
    is_genre_bm = if_any(starts_with("artist_genre"), ~str_detect(.x, "black metal")),
    is_genre_pm = if_any(starts_with("artist_genre"), ~str_detect(.x, "power metal"))
  ) %>% 
  rowwise() %>% 
  mutate(n_cats = sum(c_across(matches("is_genre")))) %>% 
  ungroup() %>% 
  filter(n_cats == 1) %>% 
  mutate(genre = factor(case_when(is_genre_bm ~ "black metal",
                                  is_genre_dm ~ "death metal",
                                  is_genre_pm ~ "power metal",
                                  TRUE ~ NA_character_
  ))) %>% 
  select(-contains("artist_genre"), -starts_with("is_genre"))


## 2) Pre-processing ----

# Remove everything in square brackets, e.g. "[Intro]" ...
lyrics <- lyrics %>%
  mutate(lyrics = str_remove_all(lyrics, "\\[([^\\]]*)\\]"))

# Append lyrics onto tracks
lyrics <- lyrics %>%
  left_join(tracks, by = "track_id") %>% 
  drop_na()


# Remove non-English lyrics. Language detection using Google's Compact Language
# Detector 3
lyrics <- lyrics %>%
  mutate(language = cld3::detect_language(lyrics)) %>%
  # Keep only English ones
  filter(language == "en")

# See how many tracks are for each genre!
lyrics %>% count(genre)

## 3) {tidymodels} introduction ----

# Split into training and testing datasets (70% training, 30% testing)
# (set random number generator seed for reproducibility)
set.seed(1234)
lyrics_split <- initial_split(lyrics, prop = 0.7, strata = genre)

lyrics_train <- training(lyrics_split)
lyrics_test <- testing(lyrics_split)


# Here we use cross-validation, will allow us to tune without
# leaking data to the test set.
set.seed(234)
lyrics_folds <- vfold_cv(lyrics_train, v = 3, strata = "genre")
lyrics_folds


## 4) TFIDF ----

# Let's tokenize (at word level)

# If we aren't fitting a model we can use unnest_tokens (tidytext)
tidy_lyrics <- lyrics %>%
  select(-lyrics_url) %>%
  unnest_tokens(word, lyrics, to_lower = TRUE)


# First recipe. We tokenize - taking the 250 most common tokens
# We then create a tfidf score for each of these tokens, for each of the 
# "documents" (the songs). This results in 250 feature columns.
tfidf_rec <- recipe(genre ~ lyrics, data = lyrics_train) %>%
  step_tokenize(lyrics) %>%
  step_tokenfilter(lyrics, max_tokens = 250) %>% # referring to lyrics column
  step_tfidf(lyrics) # gives us 250 features for each document (i.e. song)

# Let's have a quick look at what this is doing
tfidf_rec %>% 
  prep(lyrics_train) %>%
  bake(lyrics_train)

# Create a workflow - allows for neater code when working in
# tidymodels
tfidf_wf <- workflow() %>%
  add_recipe(tfidf_rec)

# Naive Bayes model
# Assumes each of the words in each document are independent from e/o
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
nb_spec


# Create nb_wf which we will fit to each of the resamples.
tfidf_nb_wf <- tfidf_wf %>%
  add_model(nb_spec)

# Fit to each of the folds, allowing 1 fold to be the test set 
# each time
# Using `save_pred = TRUE` allows us to save predictions from
# each of the folds.
tfidf_nb_fit <- fit_resamples(
  tfidf_nb_wf,
  lyrics_folds,
  control = control_resamples(save_pred = TRUE) 
)

# Collect metrics from our fitted model
tfidf_nb_metrics <- collect_metrics(tfidf_nb_fit)
tfidf_nb_predictions <- collect_predictions(tfidf_nb_fit)
tfidf_nb_metrics

# Get a confusion matrix averaged over each of the folds
conf_mat_resampled(tfidf_nb_fit, tidy = FALSE) %>%
  autoplot(type = "heatmap")

# Stack 3 one-versus-all ROC curves
tfidf_nb_predictions %>% 
  roc_curve(truth = genre, ends_with("metal")) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_line(aes(col = .level), size = 1) +
  geom_abline(lty = 2)


## Exercise (1) ----

# Now we have gone through one algorithm - let's try a new one via an exercise:
# 1. Keeping the same recipe - now try fitting a different algorithm, this time 
# a random forest with the "randomForest" engine. Make sure you use the same 
# resampling folds as before.
# 2. Compare this new model with the naive Bayes model in terms of accuracy.
# 3. Create a confusion matrix for the resamples. Compare this to the confusion
# matrix for the naive bayes model.

# Check against the code in the answers folder when you are done.


## 5) Stopwords ------------------------------

# Now, let's see whether stemming and/or removing stopwords will 
# improve our model.

# Get stopwords from all sources containing English stopwords, using functions
# from packages {stopwords} and {tidytext}

# Get stopwords from all sources that are english.
my_stopwords <- stopwords::stopwords_getsources() %>%
  purrr::set_names() %>%
  map(stopwords_getlanguages) %>%
  keep(~ "en" %in% .x) %>%
  names() %>%
  map_dfr(~ get_stopwords(language = "en", source = .x)) %>%
  distinct(word)

# Add lyrics-specific stop words: vocalisations etc
my_stopwords <- my_stopwords %>%
  add_row(
    word = c("ah", "ahh", "ayy", "ba", "da", "de", "doo", "em", "ha", "hey", 
             "huh", "la", "mm", "mmm", "na", "nah", "ooh", "uh", "woah", "woo", 
             "ya", "yah", "yeah")
  ) %>%
  distinct(word)


# Create vector of stopwords
sw_vec <- my_stopwords %>% pull(word)


# If we weren't fitting a model - we would remove them with anti_join()
dim(tidy_lyrics)
tidy_lyrics <- tidy_lyrics %>%
  anti_join(my_stopwords)

dim(tidy_lyrics)

# New recipe using stopwords
tfidf_sw_rec <- recipe(genre ~ lyrics, data = lyrics_train) %>%
  step_tokenize(lyrics) %>%
  step_stopwords(lyrics, custom_stopword_source = sw_vec) %>% 
  step_tokenfilter(lyrics, max_tokens = 250) %>% # referring to lyrics column
  step_tfidf(lyrics)

# We now follow the same steps as before, but with the new recipe

# Let's have a quick look at what this is doing
# Before removing stopwords
tfidf_rec %>% 
  prep(lyrics_train) %>%
  bake(lyrics_train)

# After removing stopwords
tfidf_sw_rec %>% 
  prep(lyrics_train) %>%
  bake(lyrics_train) 

# Create a workflow - allows for neater code when working in
# tidymodels
tfidf_sw_wf <- workflow() %>%
  add_recipe(tfidf_sw_rec)

# We will use the random forest spec from the exercises, as this 
# was superior to the naive Bayes model
rand_spec <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")

tfidf_sw_wf <- tfidf_sw_wf %>% 
  add_model(rand_spec)

# Fit to each of the folds, allowing 1 fold to be the test set 
# each time
# Using `save_pred = TRUE` allows us to save predictions from
# each of the folds. 
tfidf_sw_fit <- fit_resamples(
  tfidf_sw_wf,
  lyrics_folds,
  control = control_resamples(save_pred = TRUE) 
)

# Collect metrics from our fitted model
tfidf_sw_metrics <- collect_metrics(tfidf_sw_fit)
tfidf_sw_predictions <- collect_predictions(tfidf_sw_fit)
tfidf_sw_metrics

# Get a confusion matrix averaged over each of the folds
conf_mat_resampled(tfidf_sw_fit, tidy = FALSE) %>%
  autoplot(type = "heatmap")

# We didn't see any improvement here. Instead, let's try doing another form
# of pre-processing, in an exercise.

## 6) Stemming words ----
# Another type of pre-processing that we might do is stemming words.
# This will group words with the same/nearly the same meaning and so may
# make our TFIDF features more informative. 
# E.g. Happy/happiness/happier -> "happi"

wordStem("happy") # SnowballC package
wordStem("feeling")

# And so to apply it to a dataframe, we use wordStem() inside a mutate():
tidy_lyrics <- tidy_lyrics %>%
  mutate(word_stemmed = wordStem(word))

# This can also be done via a recipe step from textrecipes, which we
# will try in the exercise


## Exercise (2) ----

# 1. Fit another random forest model - but this time adding a word stemming
# step in addition to the removal of stopwords. Consider the order in which
# these two steps should be done.
# 2. How does the accuracy compare to the random forest in exercise 1?
# 3. In which, if any, of the categories did we see an improvement? Hint:
# create a confusion matrix.

# Other cool steps from {textrecipes} include:
# - step_ngram() 
# - step_tf()
# - step_lda()

## 7) Variable Importance Plots ----

# If we want to calculate variable importance, we will need to refit
# to create a single model (as opposed to fitting a model to each of the
# resampling folds with fit_resamples).

full_rf_fit <- fit(tfidf_sw_wf, lyrics_train)

# We can then find which of the features were most important when classifying
# genre. This can be done using the {vip} package.
full_rf_fit %>%
  extract_fit_parsnip() %>%
  vip(scale = TRUE) +
  labs(title = "Variable Importance Scores for random forest model")


full_rf_fit %>%
  predict(lyrics_test) %>%
  bind_cols(lyrics_test) %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(my_stopwords) %>%
  count(.pred_class, word) %>%
  group_by(.pred_class) %>%
  slice_max(n = 10, order_by = n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  facet_wrap(~.pred_class, scales = "free")


## 8) Word embeddings ----

# Can compare the lyrics too to see whether they are in fact similar
get_lyrics_from_track_name(lyrics, "Horrendous Member Dismemberment") %>% 
  first() %>% 
  cat()

# Get pre-trained GloVe word embeddings
glove6b <- embedding_glove6b(dimensions = 50)
glove6b

# Let's tokenize (at word level)
tidy_lyrics <- lyrics %>%
  select(-lyrics_url) %>%
  unnest_tokens(word, lyrics, to_lower = TRUE)
tidy_lyrics

## Compute average embedding per song - takes a little bit of time
# Each word has an embedding - so each song has an average embedding
set.seed(2020)

track_embeddings <- tidy_lyrics %>%
  left_join(glove6b, by = c("word" = "token")) %>%
  group_by(track_id) %>%
  summarise(across(
    matches("^d[0-9]*$"),
    mean, na.rm = TRUE
  )) %>% 
  slice_sample(n = 1000) # just for the sake of demonstration

# Set track IDs aside for later
track_ids <- track_embeddings %>%
  select(track_id)

# Compute cosine similarity per song - takes a little bit of time
cossim <- track_embeddings %>%
  select(-track_id) %>%
  as.matrix() %>%
  # Each row currently contains the track embeddings vector; the function below
  # computes cosine similarity between the *columns* of a matrix, so we transpose
  # ours
  t() %>%
  lsa::cosine() %>%
  as_tibble()


# Bring back track IDs and names
cossim <- cossim %>%
  bind_cols(
    track_id = track_ids
  ) %>%
  left_join(
    tracks %>% select(track_id, track_name),
    by = "track_id"
  ) %>%
  relocate(track_id, track_name)

# Can now use cosine similarity to find similar tracks to a target track

# Some examples
similar_tracks <- cossim %>%
  find_similar("Horrendous Member Dismemberment", n = 3)
similar_tracks


similar_tracks %>%
  pull(track_name) %>%
  get_lyrics_from_track_name(lyrics, .)



# Finally we fit a model using the pre-trained word embeddings.
# Instead of using TFIDF scores for our features, we will now use
# the mean embedding vector for each song as our feature vector.
# Embedding vector dimension 50 => 50 features


embed_rec <- recipe(genre ~ lyrics, data = lyrics_train) %>%
  #step_rm(lyrics_url, language, track_id, track_name, artist_id, artist) %>% 
  step_tokenize(lyrics) %>%
  step_word_embeddings(lyrics, embeddings = glove6b, aggregation = "mean") 

# Let's have a quick look at what this is doing
test_embeddings <- embed_rec %>% 
  prep(lyrics_train) %>%
  bake(lyrics_train)

test_embeddings

# Create a workflow - allows for neater code when working in
# tidymodels
embed_wf <- workflow() %>%
  add_recipe(embed_rec) %>% 
  add_model(rand_spec)

embed_rf_fit <- fit_resamples(
  embed_wf,
  lyrics_folds,
  control = control_resamples(save_pred = TRUE) 
)

# Collect metrics from our fitted model
embed_metrics <- collect_metrics(embed_rf_fit)
embed_predictions <- collect_predictions(embed_rf_fit)

# Check metrics
embed_metrics

# Get a confusion matrix averaged over each of the folds
conf_mat_resampled(embed_rf_fit, tidy = FALSE) %>%
  autoplot(type = "heatmap")

# Stack 3 one-versus-all ROC curves - one for each of the levels
embed_predictions %>% 
  roc_curve(truth = genre, ends_with("metal")) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_line(aes(col = .level), size = 1) +
  geom_abline(lty = 2)

## Exercise (3) ----
# 1. What would happen if we refit and again made a variable importance 
# plot? Why would the result be difficult/impossible to interpret?

## 9) Hyperparameter tuning --------------------------------------------------

tune_embed_rec <- recipe(genre ~ lyrics, data = lyrics_train) %>%
  step_tokenize(lyrics) %>%
  step_word_embeddings(lyrics, embeddings = glove6b, aggregation = "mean")

rf_tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")


# Create a grid of hyperparameters to tune over
# mtry - number of features 
hyp_grid <- grid_regular(
  mtry(range = c(5, 50)), # sensible range provided by tidymodels
  trees(range = c(500, 4000)), # sensible range provided by tidymodels
  levels = 5 # in reality would go for a much higher number of levels
)
hyp_grid

# Create new workflow with new recipe and new model specification
tune_wf <- workflow() %>%
  add_recipe(tune_embed_rec) %>%
  add_model(rf_tune_spec)

# Tune over hyperparams (equivalent to fit_resamples)
# We now give it a grid to tune over too.
set.seed(1871)

# tune_rs <- tune_grid(
#   tune_wf,
#   lyrics_folds,
#   grid = hyp_grid,
#   control = control_resamples(save_pred = TRUE)
# )

# Here is one we made earlier
tune_rs <- readRDS("tune_rs.rds")

# Collect metrics as before
collect_metrics(tune_rs)

# We can autoplot a tuned model to see the performance over 
# the tuning parameters
autoplot(tune_rs) +
  labs(
    title = "Random Forest performance across feature count and tree count range",
    subtitle = "Performance metrics can be used to identity the best combination of hyperparameters"
  )

# You can also extract the best values in a dataframe 
tune_rs %>%
  show_best("roc_auc")
tune_rs %>%
  show_best("accuracy")

# To actually select the final tuning parameters, use a `select_*` 
# function (often `select_best`)
# We have to define ourselves what "simple" means (low trees, low mtry)
chosen_hyps <- tune_rs %>%
  select_by_one_std_err(metric = "roc_auc", trees, mtry)

chosen_hyps

# Use best hyperparameters (chosen by ROC_AUC score) to create
# a final workflow
final_rf <- finalize_workflow(tune_wf, chosen_hyps)

# Now train workflow on ALL available training data, and 
# test on hold- out set
final_fit <- final_rf %>% 
  last_fit(lyrics_split)

# Collect final metrics
final_fit %>% 
  collect_metrics()

# Create final ROC curve
final_fit %>% 
  collect_predictions() %>% 
  roc_curve(truth = genre, ends_with("metal")) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_line(aes(col = .level), size = 1) +
  geom_abline(lty = 2) 


# Compare to previous models (note we now use autoplot() and so the 
# colours are different models, not different genres)
final_fit %>% 
  collect_predictions() %>% 
  mutate(model = "Tuned Random Forest") %>% 
  bind_rows(mutate(embed_predictions, model = "Untuned Random Forest")) %>% 
  bind_rows(mutate(tfidf_nb_predictions, model = "Untuned Naive Bayes")) %>%
  group_by(model) %>% 
  roc_curve(truth = genre, ends_with("metal")) %>% 
  autoplot()


