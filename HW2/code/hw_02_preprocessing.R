# *********************************************************************** #
#                         ** Statistical Learning **                      #
#                       ** HW-02 - Pre-processing **                      #
# *********************************************************************** #

#' ---
#' title: "Statistical Learning"
#' author: "Pierpaolo Brutti"
#' date: "HW-02 - Pre-processing"
#' ---



# Packages ----------------------------------------------------------------
require(readr)
require(cleanNLP)  # https://statsmaths.github.io/cleanNLP/
require(stringi)
require(caret)

# Load & Look -------------------------------------------------------------
amazon <- read_csv("amazon.csv")
str(amazon)
table(amazon$class)
stri_wrap(amazon$text[amazon$class == "book"][1:10])

# Remove NA otherwise troubles with <clnp_annotates> below
len <- stri_length(amazon$text)
hist(len, breaks = 1000)
summary(len)  # wow! min length = 2, average length = 917, max length...30k!! XD
quantile(len, seq(0,1,.1), na.rm = T)

idx_na <- which(is.na(len))
amazon <- amazon[-idx_na,]

# Clean -------------------------------------------------------------------
?cnlp_init_stringi
?cnlp_annotate

cnlp_init_stringi()  # initialize tokenizer backend
anno <- cnlp_annotate(amazon, text_name = "text")   # take some time...

# tf–idf score ------------------------------------------------------------

# See our notes: Kernel for Text (pp. 142-147)
# https://elearning.uniroma1.it/pluginfile.php/1029332/mod_folder/intro/Lecture_11.pdf
?cnlp_utils_tfidf

# The options in the call determine what words are included:
# a word must be used in at least <mid_df> percent of documents 
# but not in more than <max_df> documents
X <- cnlp_utils_tfidf(anno$token, 
                      min_df = 0.01, max_df = 0.5, 
                      tf_weight = "raw")

# Take a look 
dim(X)
colnames(X)
round(as.matrix(X[1:10, 1:10]), 2)

# Train-Test split --------------------------------------------------------

y <- amazon$class
X <- as.matrix(X)

set.seed(124) # for reproducibility
idx_tr <- createDataPartition(y = y, p = .75, list = F)

X_tr <- X[ idx_tr, ]; X_te <- X[-idx_tr, ]
y_tr <- y[ idx_tr ]; y_te <- y[-idx_tr]

dim(X_tr); dim(X_te); length(y_tr); length(y_te)

