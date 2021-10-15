#!/usr/bin/env python3

import logging
from gensim import utils
from gensim import models
from gensim.corpora.textcorpus import TextCorpus, strip_multiple_whitespaces, remove_stopwords
from gensim.models.phrases import Phrases, Phraser
from pprint import pprint

from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet as wn

import pandas as pd
import psutil
import sys
import os

logger = logging.getLogger(__name__)

#json_file = './head.json'
json_file = './2000.json'
#json_file = './reviews_Books_5.json'

bigram_model = None

def memory_usage_psutil():
    # return the memory usage in MB
    import psutil
    process = psutil.Process(os.getpid())
    mem = process.memory_info()[0] / float(2 ** 20)
    return mem

def to_unicode(text, encoding='utf8', errors='strict'):
    """ adapted from TextCorpus.lower_to_unicode (just unicode)

    """
    return utils.to_unicode(text, encoding, errors)

wn_lemm = WordNetLemmatizer()
def lemmatize(tokens):
    return [wn_lemm.lemmatize(token) for token in tokens]

def lemmatize2(tokens):
    r = []

    for token in tokens:
        lem = wn.morphy(token)
        if lem:
            r.append(lem)
        else:
            r.append(token)

    return r

def apply_bigrams(tokens):
    if bigram_model is None:
        logger.error("no bigram model")

    return bigram_model[tokens]

def lowercase(tokens):
    """Remove stopwords using list from `gensim.parsing.preprocessing.STOPWORDS`.
    """
    return [token.lower() for token in tokens]

def split_tokenizer(text):
    for token in text.split():
        yield token

# not yet: bigrams and lemmatization
class CorpusTest(TextCorpus):
    #stopwords = set('for a of the and to in on'.split())
    filename = json_file

    def get_texts(self):
        reader = pd.read_json(self.filename, lines=True, chunksize=1)
        for chunk in reader:
            # more input modifications?
            yield self.preprocess_text(chunk.reviewText.values[0])
            #yield utils.to_unicode(chunk.reviewText.values[0]).split()

        #for doc in self.getstream():
        #    yield [word for word in utils.to_unicode(doc).lower().split() if word not in self.stopwords]

    def __len__(self):
        self.length = sum(1 for _ in self.get_texts())
        return self.length

    def gen_bigram_model(self):
        bigrams = Phrases(iter(self.get_texts()))
        print(memory_usage_psutil())
        return Phraser(bigrams)


corpus = CorpusTest(input="not_used",
                    character_filters=[to_unicode, strip_multiple_whitespaces],
                    token_filters=[lemmatize2, lowercase, remove_stopwords],
                    tokenizer=split_tokenizer)

# create bigram model
bigram_model = corpus.gen_bigram_model()

bicorpus = CorpusTest(input="not_used",
                      character_filters=[to_unicode, strip_multiple_whitespaces],
                      token_filters=[lemmatize2, lowercase, remove_stopwords, apply_bigrams],
                      tokenizer=split_tokenizer)


lda_model = models.LdaModel(bicorpus, id2word=bicorpus.dictionary, num_topics=100)

pprint(lda_model.print_topics())
doc_lda = lda_model[corpus]

print("done")
