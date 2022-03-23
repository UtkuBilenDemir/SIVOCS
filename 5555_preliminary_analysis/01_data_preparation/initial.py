import pandas as pd
import pickle
import pprint
from sklearn.datasets import load_files
from sklearn.feature_extraction.text import CountVectorizer
import np
import matplotlib as plt
import os

import re
import pandas as pd
from pprint import pprint

# Gensim
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel

# spacy for lemmatization
import spacy

# Plotting tools
import pyLDAvis
import pyLDAvis.gensim  # don't skip this
import matplotlib.pyplot as plt
import os

os.getcwd()

snsf_ge_abs_df = pd.read_csv("data/P3_GrantExport_with_abstracts.csv", sep=";")
snsf_ge_abs_df.keys()
len([x for x in snsf_ge_abs_df["Abstract"] if str(x) == "nan"]) / len(snsf_ge_abs_df["Abstract"])
len([x for x in snsf_ge_abs_df["Project Title English"] if str(x) == "nan"]) / len(
    snsf_ge_abs_df["Project Title English"])
len([x for x in snsf_ge_abs_df["Project Title"] if str(x) == "nan"]) / len(snsf_ge_abs_df["Project Title"])

abstracts = [x for x in snsf_ge_abs_df["Abstract"] if str(x) != "nan"]
stopwords_extra = pickle.load(open("../../Current/topic_modelling/ext_sources/extra_stopwords.p", "rb"))
de_fr_stopwords = [
    "de",
    "dans",
    "une",
    "sur",
    "qui",
    "pour",
    "ce",
    "avec",
    "sont",
    "aux",
    "cette",
    "leur",
    "ainsi",
    "comme",
    "recherche",
    "entre",
    "suisse",
    "etude",
    "analyse",
    "projet",
    "histoire",
    "son",
    "question",
    "elle",
    "leurs",
    "travail",
    "deux",
    "dont",
    "pratiques",
    "mais",
    "der",
    "und",
    "die",
    "de",
    "von",
    "den",
    "mit",
    "fur",
    "ist",
    "zu",
    "da",
    "dem",
    "eine",
    "wird",
    "sowie",
    "werden",
    "sind",
    "ein",
    "auch",
    "au",
    "al",
    "tagung",
    "zur",
    "auf",
    "forschung",
    "durch",
    "einer",
    "wurde",
    "hat",
    "soll",
    "und",
    "der",
    "die",
    "de",
    "von",
    "al",
    "den",
    "da",
    "sich",
    "auf",
    "wie",
    "zu",
    "eine",
    "mit",
    "einer",
    "ist",
    "auch",
    "sie",
    "dem",
    "ein",
    "wird",
    "oder",
    "diese",
    "durch",
    "dabei",
    "au",
    "das",
    "fur",
    "nicht",
    "zwischen",
    "und",
    "die",
    "der",
    "von",
    "werden",
    "den",
    "fur",
    "mit",
    "auf",
    "de",
    "zu",
    "ist",
    "da",
    "wird",
    "eine",
    "sind",
    "wie",
    "sowie",
    "auch",
    "al",
    "einer",
    "sich",
    "welche",
    "schweiz",
    "diese",
    "oder",
    "durch",
    "ein",
    "zur",
    "au"
]
stopwords_extra = stopwords_extra + de_fr_stopwords
len(stopwords_extra)
stopwords_extra[len(stopwords_extra) -10 :len(stopwords_extra)]
# --------------------------------------- i.) Token

def sent_to_words(sentences):
    for sentence in sentences:
        yield (gensim.utils.simple_preprocess(str(sentence), deacc=True))  # deacc=True removes punctuations


token_abs = list(sent_to_words(abstracts))
token_abs[6]


## ubd: This one is definitely a better introduction

# --------------------------------------- ii.) Remove stopwords
# Define function for stopwords
def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stopwords_extra] for doc in texts]


# Remove Stop Words
token_abs_key_nostop = remove_stopwords(token_abs)
print("With stop words: {} words\nWithout stop words: {} words\nDiscarded words: {}".format(len(token_abs[6]),
                                                                                            len(token_abs_key_nostop[
                                                                                                    6]),
                                                                                            [i for i in token_abs[6] if
                                                                                             i not in
                                                                                             token_abs_key_nostop[6]]))
## [print(i + "|" + j) for i, j in zip(token_abs_key[6], token_abs_key_nostop[6])]
## print("\n".join("{} || {}".format(x, y) for x, y in zip(token_abs_key[6], token_abs_key_nostop[6])))


# --------------------------------------- iii.) Form Bigrams
bigram = gensim.models.Phrases(token_abs_key_nostop, min_count=5, threshold=100)  # higher threshold fewer phrases.
bigram_mod = gensim.models.phrases.Phraser(bigram)


def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]


abs_key_bigrams = make_bigrams(token_abs_key_nostop)
# TODO: Here are some duplicates, what's up with those??
# TODO solved: Those appear a few times on the text, nothing to worry about
len(abs_key_bigrams[6])

trigram = gensim.models.Phrases(bigram[abs_key_bigrams], threshold=100)
trigram_mod = gensim.models.phrases.Phraser(trigram)


def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]


abs_key_trigrams = make_trigrams(abs_key_bigrams)

len(abs_key_trigrams[6])
abs_key_trigrams = [[word for word in simple_preprocess(str(doc)) if word != "rights_reserved"] for doc in
                    abs_key_trigrams]

len(abs_key_trigrams[6])

# --------------------------------------- iv.) Lemmatize

from nltk.stem.wordnet import WordNetLemmatizer

# Lemmatizer 2
#
lemmatizer = WordNetLemmatizer()
#
abs_key_lemmatized = [[lemmatizer.lemmatize(token) for token in doc] for doc in abs_key_trigrams]
#


# --------------------------------------- 2. Create the Dictionary and Corpus needed for Topic Modeling
# Create Dictionary
# TODO: WHICH ONE IS BETTER
id2word = corpora.Dictionary(abs_key_lemmatized)
### id2word = corpora.Dictionary(abs_key_bigrams)
# TODO: Better? We remove rare words and common words based on their document frequency.
#  Below we remove words that appear in less than 20 documents or in more than 50% of the documents.
#  Consider trying to remove words only based on their frequency, or maybe combining that with this approach.
id2word.filter_extremes(no_above=0.25)

# Create Corpus
texts = abs_key_lemmatized
### texts = abs_key_bigrams

# Term Document Frequency
corpus = [id2word.doc2bow(text) for text in texts]

# View
print(corpus)

# Gensim creates a unique id for each word in the document. The produced corpus shown above is a mapping of (word_id, word_frequency).
#
# For example, (0, 1) above implies, word id 0 occurs once in the first document. Likewise, word id 1 occurs twice and so on.
#
# This is used as the input by the LDA model.
#
# If you want to see what word a given id corresponds to, pass the id as a key to the dictionary.

id2word[327]

# Human readable format of corpus (term-frequency)
[[(id2word[id], freq) for id, freq in cp] for cp in [corpus[6]]]

corpus
# --------------------------------------- 5. Building LDA Mallet Model INSTEAD
mallet_path = '../../Current/topic_modelling/ext_sources/dependencies/mallet-2.0.8/bin/mallet'  # update this path
# TODO: TRY DIFFERENT CORPORA
ldamallet = gensim.models.wrappers.LdaMallet(mallet_path, corpus=corpus, num_topics=20, id2word=id2word)
# Show Topics
pprint(ldamallet.show_topics(formatted=False))

# Compute Coherence Score
coherence_model_ldamallet = CoherenceModel(model=ldamallet, texts=abs_key_lemmatized, dictionary=id2word,
                                           coherence='c_v')
### coherence_model_ldamallet = CoherenceModel(model=ldamallet, texts=abs_key_bigrams, dictionary=id2word, coherence='c_v')
coherence_ldamallet = coherence_model_ldamallet.get_coherence()
print('\nCoherence Score: ', coherence_ldamallet)


# --------------------------------------- 6. Find the optimal number of topics

def compute_coherence_values(dictionary, corpus, texts, limit, start=2, step=3):
    """
    Compute c_v coherence for various number of topics

    Parameters:
    ----------
    dictionary : Gensim dictionary
    corpus : Gensim corpus
    texts : List of input texts
    limit : Max num of topics

    Returns:
    -------
    model_list : List of LDA topic models
    coherence_values : Coherence values corresponding to the LDA model with respective number of topics
    """
    coherence_values = []
    model_list = []
    for num_topics in range(start, limit, step):
        model = gensim.models.wrappers.LdaMallet(mallet_path, corpus=corpus, num_topics=num_topics, id2word=id2word)
        model_list.append(model)
        coherencemodel = CoherenceModel(model=model, texts=texts, dictionary=dictionary, coherence='c_v')
        coherence_values.append(coherencemodel.get_coherence())

    return model_list, coherence_values


import timeit

start = timeit.default_timer()
# Can take a long time to run.
model_list, coherence_values = compute_coherence_values(dictionary=id2word, corpus=corpus, texts=abs_key_lemmatized,
                                                        start=2, limit=40, step=2)
### model_list, coherence_values = compute_coherence_values(dictionary=id2word, corpus=corpus, texts=abs_key_bigrams, start=2, limit=40, step=4)
stop = timeit.default_timer()
print('Time: ', stop - start)

2 + 2

## pickle.dump(model_list,  open('model_list', 'wb'))


# Show graph
limit = 40;
start = 2;
step = 2;
x = range(start, limit, step)

coherence_values[10] = 0.5840828
coherence_values[11] = 0.5745439

plt.plot(x, coherence_values)
plt.xlabel("Num Topics")
plt.ylabel("Coherence score")
plt.legend(("coherence_values"), loc='best')
plt.show()
# Print the coherence scores
for m, cv in zip(x, coherence_values):
    print("Num Topics =", m, " has Coherence Value of", round(cv, 4))
##
## import plotly.graph_objects as go
## import numpy as np
## fig = go.Figure(data=go.Scatter(x=list(x), y=coherence_values, line=dict(color="#DA4E52")))
## fig.update_layout( # plot_bgcolor="#F0F1EB",
##                    paper_bgcolor='rgba(0,0,0,0)',
##                    # plot_bgcolor='rgba(0,0,0,0)',
##                    xaxis_title='Num. of Topics',
##                    yaxis_title='Coherence score')
## fig.show()
## fig.write_html("./presentation/reveal.js/visualizations/coherence.html")
# --------------------------------------- 7. Select the model and print the topics
optimal_model = model_list[9]
model_topics = optimal_model.show_topics(formatted=False)
pprint(optimal_model.print_topics(num_words=10))

# --------------------------------------- 8. Visualize the topics
import pyLDAvis
import pyLDAvis.gensim as gensimvis

pyLDAvis.disable_notebook()
optimal_model_gensim = gensim.models.wrappers.ldamallet.malletmodel2ldamodel(optimal_model)
vis = pyLDAvis.gensim.prepare(optimal_model_gensim, corpus, id2word)
# pyLDAvis.show(vis)
# pyLDAvis.prepared_data_to_html(vis)
pyLDAvis.save_html(vis, "snsf_top_mod2.html")

list(optimal_model.load_document_topics())[2003]
optimal_model.print_topics()

## unified_abs_key[2003]
2+2
pickle.dump(model_list, open("model_list.p", "wb"))
pickle.dump(coherence_values, open("coherence_values.p", "wb"))

list(enumerate(coherence_values))


optimal_model.num_topics