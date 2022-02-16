# The features in this model are just elements of a word embedding vector.
# Feature N is just the value of the Nth dimension of this vector. This 
# embedding vector doesn't mean anything to us as it is the aggregation of multiple
# words in the lyrics. Therefore looking at the 'most important' feature is not 
# going to tell us anything about specific words in the lyrics, unlike for 
# TFIDF features where each value was related to a specific word.