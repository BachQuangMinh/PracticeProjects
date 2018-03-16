# Hackathon 2


The project contains 4 main components:

* DUC2003: data (2003 Document Understanding Conference)
* summa: implement basic textrank
* main.py: python file to run this project
* report.docx: report method

## Installation Guide


 * Sumeval
    - https://github.com/chakki-works/sumeval
        * pip install sumeval
    - After installation you need to download a language model:
        * python -m spacy download en
 * Gensim
    - https://github.com/RaRe-Technologies/gensim
        * The simple way to install gensim is:
            * pip install -U gensim
        * Or, if you have instead downloaded and unzipped the source tar.gz package, you’d run:
            * python setup.py test
            * python setup.py install

# Run
 Run file main.py to get result


# Note
 Summa textrank package can cause conflicts with Python 3, so you should uninstall summa package in your computer and directly use summa folder in this project.