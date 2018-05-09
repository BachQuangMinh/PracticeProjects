# -*- coding: utf-8 -*-
"""
Created on Sun Nov  5 15:14:25 2017

@author: team MTQ
"""

import os
from sumeval.metrics.rouge import RougeCalculator

BASE_DIR='C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\Hackathon2018_textsummarization\\Final version'

def read_data(filename):
    filename = os.path.join(BASE_DIR, 'DUC2003', filename)

    with open(filename) as f:
        content = f.read()
    return content


def cal_rouge(summa, refer):
    ##compare evaluate a summary with a given refer
    rouge = RougeCalculator(stopwords=True, lang="en")

    rouge_1 = rouge.rouge_n(
                summary=summa,
                references=refer,
                n=1)
    
    rouge_2 = rouge.rouge_n(
                summary=summa,
                references=[refer],
                n=2)
    
    rouge_l = rouge.rouge_l(
                summary=summa,
                references=[refer])

    print("ROUGE-1: {}, ROUGE-2: {}, ROUGE-L: {}".format(
        rouge_1, rouge_2, rouge_l
    ).replace(", ", "\n"))


#Textrank task ###################################

content = read_data(filename='input.txt')

from summa import summarizer
summary_textrank = summarizer.summarize(content, ratio=0.2)


#Gensim BM25 textrank task #########################

from gensim.summarization.summarizer import summarize

summary_bm25 = summarize(content, ratio=0.2) 


#Evaluation task ##################################

reference01 = read_data(filename='ref0.txt')
reference02 = read_data(filename='ref1.txt')

# Textrank:
print('Textrank score:')
print('- Ref0:')
cal_rouge(summa=summary_textrank, refer=reference01)
print('- Ref1:')
cal_rouge(summa=summary_textrank, refer=reference02)

# BM25:
print('BM25 score:')
print('- Ref0:')
cal_rouge(summa=summary_bm25, refer=reference01)
print('- Ref1:')
cal_rouge(summa=summary_bm25, refer=reference02)





