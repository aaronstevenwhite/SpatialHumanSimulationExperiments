import sys, copy
import random

sys.path.append('/home/aaronsteven/software/CHILDESPy')

from childes_corpus_loader import *
from string import Template


def get_sents(corpus_search_term, words, lower=2):

    print corpus_search_term

    unstemmed_sentences = CHILDESSentences(corpora, corpus_search_term, tagged=False)
    stemmed_sentences = CHILDESStemmedSentences(corpora, corpus_search_term, tagged=True, strip_affix=False)

    finished = defaultdict(list)

    sents_gen = itertools.izip(unstemmed_sentences, stemmed_sentences)

    for unstemmed_transcript, stemmed_transcript in sents_gen:
        sent_gen = itertools.izip(unstemmed_transcript.sentences, stemmed_transcript.sentences)

        for unstemmed, stemmed in sent_gen:
            unstemmed_sent, stemmed_sent = unstemmed.sentence, stemmed.sentence

            stemmed_sent_words_only = [word_tag[0] for word_tag in stemmed_sent]
            stemmed_sent_stems_only = [word.split('-')[0] for word in stemmed_sent_words_only]

            lex_indices = [i for i, word_tag in enumerate(stemmed_sent) if word_tag[1] in ['v', 'n', 'adj'] if word_tag[0].split('-')[0] not in ['do', 'have', 'be']]

            for word in words:
                try:
                    word_index = stemmed_sent_stems_only.index(word)

                    if 'xxx' not in stemmed_sent_stems_only and len(stemmed_sent) > lower:
                        stemmed_sent_bracketed = copy.copy(stemmed_sent_words_only)
                        stemmed_sent_bracketed[word_index] = '[{}]'.format(stemmed_sent_bracketed[word_index])

                        for index in lex_indices:
                            if index != word_index:
                                stemmed_sent_bracketed[index] = '|{}|'.format(stemmed_sent_bracketed[index])

                        finished[word].append((stemmed_sent_bracketed, unstemmed_sent))

                except ValueError:
                    pass


    for verb in finished:
        random.shuffle(finished[verb])

    return finished


## 

corpora = main()

verbs_file = open('/home/aaronsteven/experiments/ProjectionExperiments/materials/triad/verbs.list')
verbs = [verb_line.strip() for verb_line in verbs_file]

subcorpora = ['dinner', 'mother', 'father']

stemmed_sents_by_verb_by_subcorp = defaultdict(dict)
unstemmed_sents_by_verb_by_subcorp = defaultdict(dict)

for subcorp in subcorpora:
    verb_lists = get_sents('gleason/'+subcorp, verbs)

    for verb, vlist in verb_lists.iteritems():
        stemmed_sents_by_verb_by_subcorp[subcorp][verb] = [vsent[0] for vsent in vlist]
        unstemmed_sents_by_verb_by_subcorp[subcorp][verb] = [vsent[1] for vsent in vlist]

stemmed_sents_by_verb_by_subcorp_counts = {subcorp : defaultdict(int, {verb : len(sentlist) for verb, sentlist in sbdict.iteritems()}) for subcorp, sbdict in stemmed_sents_by_verb_by_subcorp.iteritems()}

print stemmed_sents_by_verb_by_subcorp_counts

for subcorp, verbdict in stemmed_sents_by_verb_by_subcorp.iteritems():
    for verb, sentlist in verbdict.iteritems():
        if stemmed_sents_by_verb_by_subcorp_counts['dinner'][verb] > 20:
            if stemmed_sents_by_verb_by_subcorp_counts['mother'][verb] + stemmed_sents_by_verb_by_subcorp_counts['father'][verb] > 20:
                if subcorp == 'dinner':
                    fname = verb + '_' + subcorp
                else:
                    fname = verb + '_play'

                with open(fname, 'a') as f:

                    for sent in sentlist[:30]:
                        sentstr = ' '.join(sent)
                        sentstr = sentstr.capitalize() + '.'

                        f.write(sentstr + '\n')

                with open(fname+'_raw', 'a') as f:
                    sentlist_raw = unstemmed_sents_by_verb_by_subcorp[subcorp][verb]

                    for sent in sentlist_raw[:30]:
                        sentstr = ' '.join(sent)

                        f.write(sentstr + '\n')
