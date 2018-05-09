# -*- coding: utf-8 -*-
"""
Created on Sun Nov  5 15:14:25 2017

@author: Team MTQ
"""

import textrank


textrank.setup_environment()


#Textrank task ###################################

filename = "D:\\rouge\\input.txt"
content = ''

with open(filename) as f:
    content = f.read()
    print(content)
s = textrank.extractSentences(content)

dir(textrank)


text = '''
Text summarization is a problem that has been studied for many years and various solutions and systems have been proposed and implemented. the task can be described as building a machine that is capable of making a summary when given a text. The key idea is to find a “smaller” set of data that contains the information of the entire data set. 
There are two main approaches to this problem: Extraction and Abstraction. Extraction methods build the summary by selecting sentences having rich information in the given text. Meanwhile, abstractive methods aim at creating a summary using key ideas from the text by building an internal semantic representation and then use natural language generation techniques. Beside these methods, machine learning techniques in information retrieval and text mining have also been used to develop systems that aid users with the task of summarization. Examples for such systems include MAHS = Machine Aided Human Summarization, which highlights candidates to be included in the summary, and systems that depend on post-processing by a human (HAMS = Human Aided Machine Summarization)

'''
s = textrank.extractSentences(text)
print(s)



#Gensim BM25 textrank task #########################

from gensim.summarization.summarizer import summarize
text = '''
Text summarization is a problem that has been studied for many years and various solutions and systems have been proposed and implemented. the task can be described as building a machine that is capable of making a summary when given a text. The key idea is to find a “smaller” set of data that contains the information of the entire data set. 
There are two main approaches to this problem: Extraction and Abstraction. Extraction methods build the summary by selecting sentences having rich information in the given text. Meanwhile, abstractive methods aim at creating a summary using key ideas from the text by building an internal semantic representation and then use natural language generation techniques. Beside these methods, machine learning techniques in information retrieval and text mining have also been used to develop systems that aid users with the task of summarization. Examples for such systems include MAHS = Machine Aided Human Summarization, which highlights candidates to be included in the summary, and systems that depend on post-processing by a human (HAMS = Human Aided Machine Summarization)

'''
put = summarize(text, ratio = 0.6)

print(put)






#Evaluation task ##################################

from sumeval.metrics.rouge import RougeCalculator


rouge = RougeCalculator(stopwords=True, lang="en")

rouge_1 = rouge.rouge_n(
            summary="I went to the Mars from my living town.",
            references="I went to Mars",
            n=1)

rouge_2 = rouge.rouge_n(
            summary="I went to the Mars from my living town.",
            references=["I went to Mars", "It's my living town"],
            n=2)

rouge_l = rouge.rouge_l(
            summary="I went to the Mars from my living town.",
            references=["I went to Mars", "It's my living town"])

# You need spaCy to calculate ROUGE-BE

rouge_be = rouge.rouge_be(
            summary="I went to the Mars from my living town.",
            references=["I went to Mars", "It's my living town"])

print("ROUGE-1: {}, ROUGE-2: {}, ROUGE-L: {}, ROUGE-BE: {}".format(
    rouge_1, rouge_2, rouge_l, rouge_be
).replace(", ", "\n"))


from sumeval.metrics.bleu import BLEUCalculator


bleu = BLEUCalculator()
score = bleu.bleu("I am waiting on the beach",
                  "He is walking on the beach")

bleu_ja = BLEUCalculator(lang="en")