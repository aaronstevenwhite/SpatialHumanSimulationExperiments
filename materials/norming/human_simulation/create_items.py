import sys, os, re, copy, argparse
import random
import enchant

from string import Template, punctuation
from collections import defaultdict
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

random.seed(300)

##################
## argument parser
##################

## initialize parser
parser = argparse.ArgumentParser(description='Create human simulation experiment')

## file handling
parser.add_argument('--sentencedir', 
                    type=str, 
                    default='./sentences')
parser.add_argument('--outputname', 
                    type=str, 
                    default='./experiment.js')
parser.add_argument('--nonwords',
                    type=str,
                    default='./nonwords')
parser.add_argument('--bracketed', 
                    type=str, 
                    default='./bracketed')

## parse arguments
args = parser.parse_args()

fnames = os.listdir(args.sentencedir)
verbs = list(set([fn.split('_')[0] for fn in fnames]))

stopwords = stopwords.words('english')

def tokenize(sent):
    sent = sent.strip()
    sent = re.findall('\|[^\|]*\||\([^\)]*\)|\[[^\]]*\]|\{[^\}]*\}|\S+', sent[:-1]) + [sent[-1]]
    
    print sent

    return sent

def get_lexical_items(sent, include_affix=False):

    word_affixes = []

    for word_affix in re.findall('\|(.+?)\|', sent):
        word_affix = word_affix.split('-')

        if len(word_affix) == 1:
            word_affixes.append(word_affix + ['zero'])
        else:
            word_affixes.append(word_affix)

    if include_affix:        
        return word_affixes
    else:
        return [word_affix[0] for word_affix in word_affixes]

def get_word(word):
    lexical_item = get_lexical_items(word)

    if lexical_item:
        return lexical_item[0]
    else:
        if '{' not in word:
            return word
        else:
            return re.findall('\{(.+?)\}', word)[0]

def get_vocabulary(directory):
    fpaths = [os.path.join(directory, fname) for fname in os.listdir(directory)]
    files = [open(fpath) for fpath in fpaths]
    lexical_items = [word for f in files for sentline in f for word in get_lexical_items(sentline) if word not in punctuation]

    return set(lexical_items)

vocabulary = get_vocabulary(args.sentencedir)

us_dict = enchant.Dict('en_US')

def dict_check(word):
    try:
        if us_dict.check(word):
            return True
        else:
            return False
    except:
        return False
        

nonwords = [word.strip() for word in open(args.nonwords)]
nonwords = filter(lambda word: not dict_check(word), nonwords)

nonwords_vocabulary_dict = dict(zip(list(vocabulary), nonwords))

affix_dict = {'presp' : 'ing',
              'pastp' : 'ed',
              'past'  : 'ed',
              '3s'    : 's',
              'pl'    : 's',
              'plposs': 's\'',
              'cp'    : 'er',
              'adv'   : 'ly',
              'zero'  : ''}

vowels = ['a', 'e', 'i', 'o', 'u']

def affix_map(word, affix):
    affix = affix_dict[affix]

    if not affix:
        return word
    elif word[-1] in vowels and affix[0] in vowels:
        return word + 't' + affix
    elif word[-1] == 's' and affix[0] not in vowels:
        return word + 'e' + affix
    else:
        return word + affix

def nonwords_vocabulary_map(word):
    try:
        word, affix = get_lexical_items(word, include_affix=True)[0]
    except (IndexError, ValueError):
        return get_word(word)
        
    nonce_word = nonwords_vocabulary_dict[word]

    return affix_map(nonce_word, affix)

def controller_formatter(subcorpus, verb_index, popped_out, nonce):
    sent_joined = ' '.join(popped_out[:-1]) + popped_out[-1]

    if (verb_index+1) % 4:
        return '[["'+subcorpus+'", '+str(verb_index)+'], "FocusForm", {html : "<script>$(\'input:text:visible:first\').focus();</script>'+sent_joined+'"}]'
    else:
        question = create_question(popped_out)

        return '[["'+subcorpus+'", '+str(verb_index)+'], "FocusForm", {html : "<script>$(\'input:text:visible:first\').focus();</script>'+sent_joined+'"},\n\t\t\t'+question+']'

def create_question(popped_out):
    sent_filtered = [word for word in popped_out if not re.match('^<input', word)]
    random_words_quoted = create_random_words(sent_filtered, vocabulary)

    random_words_joined = ', '.join(random_words_quoted)

    return '"Question", {q: "Which word was in the previous sentence? (press a number or click)", as: ['+random_words_joined+']}'


def create_random_words(sentence, vocabulary):
    random_sentence = copy.copy(sentence[:-1])
    random_vocab = list(vocabulary.difference(set(random_sentence)))

    stopword_filter = lambda word: word.lower().split("'")[0] not in stopwords

    random_sentence_filtered = filter(stopword_filter, random_sentence)

    if not len(random_sentence_filtered):
        random_sentence_filtered = random_sentence

    random.shuffle(random_vocab)
    random.shuffle(random_sentence_filtered)

    if not random_sentence_filtered:
        raise ValueError, 'all words in sentence filtered: ' + ' '.join(sentence)

    random_words = random_sentence_filtered[:1] + random_vocab[:4]

    random_words_quoted = []

    for word in random_words:
        word = word.lower().strip(',').strip('.') if word != 'I' else word.strip(',').strip('.')
        random_words_quoted.append('"{}"'.format(word))

    return random_words_quoted


def create_controller(subcorpus, sentence, verb_index, nonce):
    popped_out = []

    good = False
    
    for word in sentence:
        bracketed = re.findall('\[([a-z]+?-?[a-z1-9]*?)\]', word)
        if bracketed:
            true_word = bracketed[0]

            bracketed_split = bracketed[0].split('-')
            
            if len(bracketed_split) > 1:
                nonce_word = 'florp' + affix_dict[bracketed_split[1]]
            else:
                nonce_word = 'florp'
                
            if word[:2] == '[[':
                raise ValueError, 'double bracket: ' + subcorpus + '  ' + ' '.join(sentence)

            popped_out.append("<input placeholder='"+ nonce_word +"' name='response' type='text' size='10' class='obligatory' />")
            good = True
        else:
            if nonce:
                word = nonwords_vocabulary_map(word)
            else:
                word = get_word(word)

            if not popped_out:
                word = word.capitalize()

            popped_out.append(word)

    if not good:
        raise ValueError, 'no bracket: ' + subcorpus + '  ' + ' '.join(sentence)

    controller = controller_formatter(subcorpus, verb_index, popped_out, nonce)

    return controller, true_word


def create_controllers_for(fname):
    fpath = os.path.join(args.sentencedir, fname)
    f = open(fpath)

    verb, subcorp = fname.split('_')

    sents = []

    for sent in f:
        if not sent[0] == '*':
            sent_split = tokenize(sent)

            if len(sent_split) > 2:
                sents.append(sent_split)

    random.shuffle(sents)
    sampled_sents = sents[:20]

    controllers = defaultdict(list)
    bracketed = defaultdict(list)

    subcorp_ind = 0 if subcorp == 'dinner' else 2
    verb_ind = verbs.index(verb)

    verb_ind += 3*verb_ind + 1 + subcorp_ind

    for sentence in sampled_sents:
        real_controller, brack = create_controller(fname+'_real', sentence, verb_ind, nonce=False)
        nonce_controller, _ = create_controller(fname+'_nonce', sentence, verb_ind+1, nonce=True)

        controllers[verb_ind] += [real_controller]
        controllers[verb_ind+1] += [nonce_controller]
        
        bracketed[verb_ind] += [brack]
        bracketed[verb_ind+1] += [brack]

    return controllers, bracketed


conf_temp = lambda items, controller_names, autofocus: Template('''
var shuffleSequence = seq("consent", "setcounter", "intro", "practice", "begin", rshuffle($controller_names), "sr", "debrief");
var practiceItemTypes = ["practice"];

var manualSendResults = true;

var defaults = [
    "Message", {hideProgressBar: true},
    "Form", { 
        hideProgressBar: true,
        continueOnReturn: false
    },
    "Question", {hasCorrect: true}

];

var items = [
	["consent", "Form", {
        html: { include: "consent.html" },
		validators: {age: function (s) { if (s.match(/^\d+$$/)) return true;
							else return "Bad value for age"; }}
    } ],

        ["setcounter", "__SetCounter__", { }],
        ["sr", "__SendResults__", { }],

	["intro", "Message", {html: { include: "intro.html" }}],

	["practice", "FocusForm", {html : "$autofocus John <input placeholder='florped' name='response' type='text' size='10' class='obligatory' /> the ball very hard."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["ball", "bat", "swing", "soft", "a"]}],
	["practice", "FocusForm", {html : "$autofocus Stacy went to the grocery store to <input placeholder='florp' name='response' type='text' size='10' class='obligatory' /> food."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["grocery", "vegetable", "fruit", "eat", "yesterday"]}],
	["practice", "FocusForm", {html : "$autofocus Ben <input placeholder='florped' name='response' type='text' size='10' class='obligatory' /> some money in his bank account."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["money", "cash", "savings", "half", "safe"]}],

	["begin", "Message", {
				html: { include: "begin.html" },
				} ],

    ["sep", "Separator", { }],

    $items

];
''').substitute(items=items, controller_names=controller_names, autofocus=autofocus)

####################################################################################

controllers = {}
bracketed = {}

for fname in fnames:
    conts, bracked = create_controllers_for(fname)

    controllers.update(conts)
    bracketed.update(bracked)

verb_inds_sorted = sorted(controllers.keys())

controllers = [cont for i in verb_inds_sorted for cont in controllers[i]]
bracketed = [cont for i in verb_inds_sorted for cont in bracketed[i]]

with open(args.bracketed, 'w') as g:
    g.write('\n'.join(bracketed))

def create_debrief_controller(size=15):
    alpha = [str(i) for i in range(10)] + ['A', 'B', 'C', 'D', 'E', 'F']
    code = ''.join(random.sample(alpha, size))
    return '[["debrief", 0], "Message", {html: "<p><center>Please enter the following code into Mechanical Turk.</center></p><p><center><b>'+code+'</b></center></p>"}]'

controllers += [create_debrief_controller() for _ in range(100)]

controller_names = ['"' + verb+'_'+subcorp+'_'+lex_items + '"' for lex_items in ['real', 'nonce'] for subcorp in ['dinner', 'play'] for verb in verbs]
controller_names_str = ', '.join(controller_names)

controllers_str = ',\n'.join(controllers)

with open(args.outputname, 'w') as f:
    f.write(conf_temp(controllers_str, controller_names_str, "<script>$('input:text:visible:first').focus();</script>"))
