import 

import enchant

## load spell checkers
D_US = enchant.Dict('en_US')
D_GB = enchant.Dict('en_GB')


for i, doc in enumerate(corpus):
    fpath_words = os.path.join(sys.argv[1], 'doc'+str(i)+'words')
    fpath_lemmas = os.path.join(sys.argv[1], 'doc'+str(i)+'words')
    
    f_words = open(fpath_words, 'w')
    f_lemmas = open(fpath_lemmas, 'w')
        for j, sent in enumerate(doc):
            words = sent.words()
            lemmas = sent.lemmas()

            
        
