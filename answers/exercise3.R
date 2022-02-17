# The features in this model are just elements of a word embedding vector.
# Feature N is just the value of the Nth dimension of this vector. This 
# embedding vector doesn't mean anything to us as it is the aggregation of multiple
# words in the lyrics. Therefore looking at the 'most important' feature is not 
# going to tell us anything about specific words in the lyrics, unlike for 
# TFIDF features where each value was related to a specific word.

embed_rf_fit <- fit(embed_wf, lyrics_train)

# We can then find which of the features were most important when classifying
# genre. This can be done using the {vip} package.

embed_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(scale = TRUE) + 
  labs(title = "Variable Importance Scores for random forest model")