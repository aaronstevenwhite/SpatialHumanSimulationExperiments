import argparse

from numpy import log

## initialize parser
parser = argparse.ArgumentParser(description='Preprocess human simulation norming data')

## file handling
parser.add_argument('--data', 
                    type=str, 
                    default='../../data/norming/new/results.ibex')
parser.add_argument('--truewords', 
                    type=str,
                    default='../../materials/norming/item_order')
# parser.add_argument('--mturk', 
#                     type=str, 
#                     default='../data/norming/turk')
parser.add_argument('--lemmafreqs', 
                    type=str, 
                    default='./sorted.uk.lemma.unigrams')
parser.add_argument('--wordfreqs', 
                    type=str, 
                    default='./sorted.uk.word.unigrams')
parser.add_argument('--dataout', 
                    type=str, 
                    default='../../data/norming/new/results.preprocessed')
# parser.add_argument('--demoout', 
#                     type=str, 
#                     default='../data/demographics')


## parse arguments
args = parser.parse_args()

with open(args.data) as f:
        linelist = [line.strip().split(',') for line in f if line[0] != '#']

metadata = [line for line in linelist if line[2] == 'Form']
data = [line for line in linelist if line[2] != 'Form' or line[7] == 'initials']

def convert_morph_parse(morphparse):
        regular = ['guess', 'remember', 'want', 'need']

        regular_mapping = {'3s' : 's',
                           'past' : 'ed',
                           'presp' : 'ing',
                           'pastp' : 'ed'}
        irregular_mapping = {'say' : {'3s' : 'says',
                                      'past' : 'said',
                                      'presp' : 'saying',
                                      'pastp' : 'said'},
                             'think' : {'3s' : 'thinks',
                                      'past' : 'thought',
                                      'presp' : 'thinking',
                                      'pastp' : 'thought'},
                             'see' : {'3s' : 'sees',
                                      'past' : 'saw',
                                      'presp' : 'seeing',
                                      'pastp' : 'seen'},
                             'hear' : {'3s' : 'hears',
                                      'past' : 'heard',
                                      'presp' : 'hearing',
                                      'pastp' : 'heard'},
                             'know' : {'3s' : 'knows',
                                      'past' : 'knew',
                                      'presp' : 'knowing',
                                      'pastp' : 'known'},
                             'tell' : {'3s' : 'tells',
                                      'past' : 'told',
                                      'presp' : 'telling',
                                      'pastp' : 'told'},}
        
        morphparse_split = morphparse.split('-')

        word = morphparse_split[0]
        
        if len(morphparse_split) == 1:
                return word
        else:
                bound_morpheme = morphparse_split[1]
                if word in regular:
                        return word + regular_mapping[bound_morpheme] 
                else:
                        return irregular_mapping[word][bound_morpheme]
        
true_words = [convert_morph_parse(line.strip()) for line in open(args.truewords)]
        
if args.lemmafreqs:
        lemma_freqs = {}

        for line in open(args.lemmafreqs):
                try:
                        freq, lemma = line.strip().split()
                        lemma_freqs[lemma] = str(log(int(freq))) 
                except:
                        pass

if args.wordfreqs:
        word_freqs = {}

        for line in open(args.wordfreqs):
                try:
                        freq, word = line.strip().split()
                        word_freqs[word] = str(log(int(freq))) 
                except:
                        pass

data_processed = []
current_datum = []

id_item_pairs = []

for i, line in enumerate(data):
        if line[5] != 'practice':
                if line[2] == 'Form' and line[7] == 'initials':
                        workerid = line[8]
                        continue
                if line[2] == 'FocusForm':
                        if line[-2] == 'response':
                                if current_datum:
                                        if len(current_datum) == 12:
                                                current_datum += ['NA']*2
                                        if curr_id_item_pair not in id_item_pairs:
                                                data_processed.append(current_datum)
                                                id_item_pairs.append(curr_id_item_pair)


                                ident = workerid
                                item_index = int(line[3])-9

                                curr_id_item_pair = (ident, item_index)
                                
                                verb, context, lexical = line[5].split('_')
                                response = line[8].lower()
                                trueinflected = true_words[item_index]
                                responseaccuracy = str(1) if response == trueinflected else str(0)

                                current_datum = [ident, str(item_index),
                                                 verb, context, lexical, 
                                                 response, trueinflected,
                                                 responseaccuracy]
                                
                                if args.lemmafreqs:
                                        current_datum += [lemma_freqs[verb]] 
                                if args.wordfreqs:
                                        current_datum += [word_freqs[verb]]

                                        try:
                                                current_datum += [word_freqs[response]]
                                        except KeyError:
                                                current_datum += ['NA']

                        else:
                                current_datum += [line[-1]]
                else:
                        current_datum += [line[-2], line[-1]]

with open(args.dataout, 'w') as f:
	f.write('\t'.join(['id', 'item', 'verb', 'context', 'lexical', 
                           'response', 'verbinflected', 'responseaccuracy', 
                           'truelemmafreq', 'truewordfreq', 'responsewordfreq', 
                           'responseRT', 'questionaccuracy', 'questionaccuracyRT'])+'\n')

	lines_catted = ['\t'.join(line) for line in data_processed]
	f.write('\n'.join(lines_catted))
