# Project 4: Document Classification
# Attached Files:
#  File Project 4 Grading Rubric.pdf (46.902 KB)
# It can be useful to be able to classify new "test" documents using already classified "training" documents.  A common example is using a corpus of labeled spam and ham (non-spam) e-mails to predict whether or not a new document is spam.  

# For this project, you can start with a spam/ham dataset, then predict the class of new documents (either withheld from the training dataset or from another source such as your own spam folder).   One example corpus:   https://spamassassin.apache.org/old/publiccorpus/
  
#  Here are two short videos that you may find helpful.

# The first video shows how to unzip the provided files.
# 
# https://spamassassin.apache.org/old/publiccorpus/
###=========================================================================
easy_ham_url  = 'https://spamassassin.apache.org/old/publiccorpus/20030228_easy_ham.tar.bz2'
some <- read.table(easy_ham_url)

