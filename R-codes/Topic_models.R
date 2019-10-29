#### Another tutorial
## https://rpubs.com/Junny31/318845

########## Test dataset ############

# all_docs = c(
#   "Favorite dog fluffy tan",
#   "Dog brown cat brown",
#   "Favorite hat brown coat pink",
#   "Dog hat leash",
#   "Fluffy coat brown coat",
#   "Dog brown fluffy brown coat",
#   "Dog white brown spot",
#   "White dog pink coat brown dog",
#   "fluffy",
#   "Fluffy dog brown hat favorite",
#   "Fluffy dog white coat hat"
# )

########## Test dataset ############

all_docs = c(
  rep("Probabilistic topic model", 7)
  ,rep("famous fashion model", 3)
  ,rep("my new topic model", 2)
)
########### Data preparation ################3

library(tm)
docs <- Corpus(VectorSource(all_docs))
inspect(docs)
docs <- tm_map(docs, content_transformer(tolower))        # Convert the text to lower case

######### Data Exploration ###############
library(Matrix)
dtm <- TermDocumentMatrix(docs)
m <- sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v, dimnames = dtm$dimnames)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)



############## Model training ################
text_dtm <- DocumentTermMatrix(docs)
text_dtm
findFreqTerms(text_dtm, lowfreq = 1)
termFreq(all_docs)
our_names <- rownames(m)
image(t(m), ylab = rownames(m)) ## Change


############################### CTM ################################
library(topicmodels);library(tidytext); library(ggplot2);library(dplyr);library(tidyr)
dtm <- DocumentTermMatrix(docs)   ## Technically the previous one is TermDocumentMatrix
?LDA
our_k = 3


text_ctm <- CTM(dtm, k = our_k)
text_ctm
text_topics <- tidy(text_ctm, matrix = "beta")
text_topics
apply(terms(text_ctm, 5), MARGIN = 2, paste, collapse = ", ")

text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms #event_beta_measured_CTM.csv

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+
  ggtitle("CTM(raw): beta:parameters of the word distribution for each topic")


text_docs <- tidy(text_ctm, matrix = "gamma")
text_docs
text_top_topics <- text_docs %>%
  group_by(document) %>%
  top_n(our_k , gamma) %>%
  ungroup() %>%
  arrange(document, -gamma)

View(text_top_topics)  ### event_gamma_measured_CTM.csv

text_top_topics %>%
  spread(key=topic, value = gamma)

text_top_topics %>%
  spread(key=topic, value = gamma) %>%
  arrange(as.numeric(document))











#################### textmineR ##############################
library(textmineR)
# Check
# https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html


# load nih_sample data set from textmineR
data(nih_sample)
str(nih_sample)


# create a document term matrix 
dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT, # character vector of documents
                 doc_names = nih_sample$APPLICATION_ID, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2]



# Fit a Latent Dirichlet Allocation model
# note the number of topics is arbitrary here
# see extensions for more info
set.seed(12345)
model <- FitLdaModel(dtm = dtm, 
                     k = 20,
                     iterations = 500, # Usually at least 500 iterations or more is recommended
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 


# # R-squared 
# # - only works for probabilistic models like LDA and CTM
# model$r2
# #> [1] 0.2747765

# log Likelihood (does not consider the prior) 
plot(model$log_likelihood, type = "l")


## Next, we turn our attention to topic quality. 
## There are many “topic coherence” metrics available in the literature. For example, 
## https://svn.aksw.org/papers/2015/WSDM_Topic_Evaluation/public.pdf
## https://mimno.infosci.cornell.edu/nips2013ws/nips2013tm_submission_7.pdf


# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
head(t(model$top_terms))


# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)
  
# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)

model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]




