# Install all packages
all_pkgs <- c("tidymodels", "tidyverse", "cld3", "tidytext", "textrecipes", 
              "naivebayes", "discrim", "textdata", "stopwords", "SnowballC",
              "lsa", "vip")

install.packages(all_pkgs)

# Check we can load all packages
check_pks <- vapply(all_pkgs, function(s) {
  library(s, logical.return = TRUE, character.only = TRUE)
}, logical(1))

if (!all(check_pks)) {
  print(check_pks[check_pks != TRUE])
  stop("The above packages failed to load.")
}


message("Packages successfully installed and loaded.")


# Download word embeddings
glove6b <- embedding_glove6b(dimensions = 50)

