import os, re, sys, copy, string, argparse
import random
import enchant

from collections import defaultdict
#from boto.mturk.connection import MTurkConnection
#from boto.mturk.question import QuestionContent,Question,QuestionForm,Overview,AnswerSpecification,SelectionAnswer,FormattedContent,FreeTextAnswer


## 

##################
## argument parser
##################

## initialize parser
parser = argparse.ArgumentParser(description='Load data and run likert factor analysis.')

## file handling
parser.add_argument('--mastertemplate', 
                    type=str, 
                    default='./master.template')
parser.add_argument('--instructions', 
                    type=str, 
                    default='./instructions')
parser.add_argument('--confjs', 
                    type=str, 
                    default='./confjs')
parser.add_argument('--css', 
                    type=str, 
                    default='./css')
parser.add_argument('--staticjs', 
                    type=str, 
                    default='./staticjs')
parser.add_argument('--mainjs', 
                    type=str, 
                    default='./mainjs')
parser.add_argument('--normingitems', 
                    type=str, 
                    default='/home/aaronsteven/experiments/HumanSimulation/materials/norming/human_simulation/experiment.js')
parser.add_argument('--itemorder', 
                    type=str, 
                    default='/home/aaronsteven/experiments/HumanSimulation/materials/norming/human_simulation/item_order')
parser.add_argument('--informativity', 
                    type=str, 
                    default='/home/aaronsteven/experiments/HumanSimulation/analysis/norming/similarity_order')
parser.add_argument('--verbs', 
                    type=str, 
                    default='/home/aaronsteven/experiments/ProjectionExperiments/materials/triad/verbs.list')
parser.add_argument('--realverbpairs', 
                    type=str, 
                    default='/home/aaronsteven/experiments/HumanSimulation/analysis/norming/real_verb_pairs')
parser.add_argument('--outputfile', 
                    type=str, 
                    default='item_order')
parser.add_argument('--outputdir', 
                    type=str, 
                    default='./lists')

## parse arguments
args = parser.parse_args()

## load master template and static content
instructions = open(args.instructions).read()
confjs = open(args.confjs).read()
css = open(args.css).read()
staticjs = open(args.staticjs).read()
mainjs = open(args.mainjs).read()

mastertemplate = lambda items: string.Template(open(args.mastertemplate).read()).substitute(instructions=instructions, 
                                                                               conf=confjs,
                                                                               css=css,
                                                                               staticjs=staticjs,
                                                                               mainjs=mainjs,
                                                                               items=items)

## load spell checkers
D_US = enchant.Dict('en_US')
D_GB = enchant.Dict('en_GB')

def dict_check(word):
    try:
        return D_US.check(word) or D_GB.check(word)
    except:
        return False

real_nonce_mapping = {'hear' : 'jilp',
                      'want' : 'quarv',
                      'think' : 'blarp',
                      'tell' : 'spard',
                      'see' : 'grolp',
                      'remember' : 'dax',
                      'need' : 'ralp',
                      'say' : 'florp',
                      'guess' : 'blick',
                      'know' : 'crix'}

affix_mapping = {'presp' : 'ing',
                 'pastp' : 'ed',
                 'past'  : 'ed',
                 '3s'    : 's',
                 'pl'    : 's',
                 'plposs': 's\'',
                 'cp'    : 'er',
                 'adv'   : 'ly',
                 'zero'  : ''}

def create_nonce_verb_form(line):
    morph_parse = line.strip().split('-')

    root = real_nonce_mapping[morph_parse[0]]
    
    if len(morph_parse) == 1:
        return root
    else:
        affix = affix_mapping[morph_parse[1]]

        if root[-1] == 'x' and affix == 's':
            return root + 'e' + affix
        else:
            return root + affix

strip_punct = lambda word: word.strip('.').strip('?').strip(',')

sentence_regex = re.compile('[0-9]].*?</script>(.*)?"')
blank_regex = re.compile("<input placeholder='florp.*?' name='response' type='text' size='10' class='obligatory' />")

with open(args.normingitems) as f:
    items_file = f.read()

nonce_verbs = [create_nonce_verb_form(line) for line in open(args.itemorder)]

items = sentence_regex.findall(items_file)
items_cleaned = [blank_regex.sub(nonce_verbs[i], item) for i, item in enumerate(items)]

all_real_words = set([strip_punct(word) for sentence in items_cleaned for word in sentence.split() if dict_check(strip_punct(word)) and len(strip_punct(word)) > 3])
all_nonce_words = set([strip_punct(word) for sentence in items_cleaned for word in sentence.split() if not dict_check(strip_punct(word)) and len(strip_punct(word)) > 3])

with open(args.informativity) as f:
    ordering = [line.strip().split(',') for i, line in enumerate(f) if i > 0]

def create_question(sentence, lexical):
    possible_words = set([strip_punct(word) for word in sentence.split() if strip_punct(word) not in nonce_verbs and len(word) > 3])

    true_word = random.choice(list(possible_words)).lower()

    if lexical == 'real':
        other_words = [random.choice(list(all_real_words)).lower() for i in range(4)]
    elif lexical == 'nonce':
        other_words = [random.choice(list(all_nonce_words)).lower() for i in range(4)]

    choices = ['"' + word + '"' for word in [true_word] + other_words]
    choices_joined = ','.join(choices)

    return '"Question", {q: "Which word was in the previous sentence? (press a number or click)", as: ['+choices_joined+']}'

controllers = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
verbs = []

verb_counter = defaultdict(int)

for j, datum in enumerate(ordering):
    verb, context, lexical, i, sim = datum

    sentence = items_cleaned[int(i)].capitalize()
    ordering[j].append(sentence)
    ident = verb+'_'+context+'_'+lexical

    group = 'low' if (verb_counter[ident] / 10) else 'high'
    verb_counter[ident] += 1

    if len(sentence.split()) > 3:
        try:
            question = create_question(sentence, lexical)
            controller = '["training", "FlashSentence", {s : "'+sentence+'"},\n\t\t'+question+']'

            controllers[ident][group]['question'].append(controller)
        except IndexError:
            controllers[ident][group]['question'].append(None)
    else:
        controllers[ident][group]['question'].append(None)

    controller = '["training", "FlashSentence", {s : "'+sentence+'"}]'
    controllers[ident][group]['noquestion'].append(controller)

ordering_dict = {int(datum[3]) : datum[0:3]+datum[4:] for datum in ordering}
    
# with open(args.outputfile, 'w') as f:
#     for i, d in ordering_dict.iteritems():
#         if d[2] == 'real':
#             sim_diff = float(ordering_dict[i+20][3]) - float(d[3])
#             f.write(':'.join(d[:2]+[d[3], str(sim_diff)]+d[4:])+'\n')
    
##
        
verbs = [line.strip() for line in open(args.verbs)] + ['know']

def create_nonce_test_list(ident, group, nonce):
    return ['["nonce_'+ident+'_'+group+'", "AcceptabilityJudgment", {q: \''+nonce+' | '+v+'\'}]' for v in verbs]

## real

def create_true_test_list(ident, group):
    pairs = [line.strip().split(',') for line in open(args.realverbpairs)]

    return ['["true_'+ident+'_'+group+'", "AcceptabilityJudgment", {q: \''+verb1+' | '+verb2+'\'}]' for verb1, verb2 in pairs]

## template

begin_temp = lambda nonce: '["begin", "Message", { html: "<p>The practice is over. You will now receive sentences containing the novel word <b>'+nonce+'</b>. After you see these sentences, you will be asked questions about the similarity between the meaning of <b>'+nonce+'</b> and the meaning of other words you already know.</p>"} ]'
begin_message = {real : begin_temp(nonce) for real, nonce in real_nonce_mapping.iteritems()}

begin_noncesim_temp = lambda nonce: '["beginnoncesim", "Message", {html: "<p>You will now be asked about the similarity in meaning between <b>'+ nonce + '</b> and a word you already knew.</p>"}]'
begin_noncesim_message = {real : begin_noncesim_temp(nonce) for real, nonce in real_nonce_mapping.iteritems()}

config_temp = lambda simident, begin, begin_noncesim, items: string.Template('''
var shuffleSequence = seq("consent", "intro", "practice", "begin", randomize("training"), "beginnoncesim", randomize("nonce_$simident"), "begintruesim", randomize("true_$simident"));
var practiceItemTypes = ["practice"];

var defaults = [
    "Message", {hideProgressBar: true},
    "Form", { 
        hideProgressBar: true,
        continueOnReturn: false
    },
    "AcceptabilityJudgment", {
        s: 'How similar are the meanings of these two words?',
        as: ["1", "2", "3", "4", "5", "6", "7"],
        presentAsScale: true,
        instructions: "Use number keys or click boxes to answer.",
        leftComment: "Very dissimilar", rightComment: "Very similar"
    },
    "FlashSentence", {timeout : 6000},
    "Question", {hasCorrect: true}

];

var items = [
	["consent", "Form", {
        html: { include: "consent.html" },
		validators: {age: function (s) { if (s.match(/^\d+$$/)) return true;
							else return "Bad value for age"; }}
    } ],

	["intro", "Message", {html: { include: "intro.html" }}],

	["practice", "FlashSentence", {s : "John paktisted the ball out the window."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["ball", "bat", "swing", "soft", "a"]}],
	["practice", "FlashSentence", {s : "Stacy paktisted John five dollars."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["dollars", "cash", "savings", "half", "full"]}],
	["practice", "FlashSentence", {s : "Guy paktisted his hands in the air."},
                     "Question", {q : "Which word was in the previous sentence? (press a number or click)", as: ["hands", "kick", "punch", "gold", "ground"]}],
	["practice", "AcceptabilityJudgment", {q: "paktist | break"}],
	["practice", "AcceptabilityJudgment", {q: "paktist | send"}],
	["practice", "AcceptabilityJudgment", {q: "paktist | find"}],

    $begin,

    $begin_noncesim,

    ["begintruesim", "Message", {html: "<p>You will now be asked about the similarity in meaning between pairs of words you knew before the experiment began.</p>"}],

    ["sep", "Separator", { }],

    $items

];
''').substitute(simident=simident, begin=begin, begin_noncesim=begin_noncesim, items=items)

#HOST = 'mechanicalturk.sandbox.amazonaws.com'
#mturk = MTurkConnection(host=HOST)

#title = 'Word Learning Task (~8 minutes)'
#description = ('Learn a word then answer questions about it.')
#keywords = 'experiment, linguistics, language, learning, sentence, read, reading, science'

#hit_html = {}

# for ident, ident_dict in controllers.iteritems():
#     verb, _, _ = ident.split('_')
#     for group, group_dict in ident_dict.iteritems():
#         nonce_training_controllers = ''
#         has_question = [i for i, question in enumerate(group_dict['question']) if question]
#         use_question = random.sample(has_question, 4)

#         nonce_training_controllers = []
        
#         for i in range(len(group_dict['question'])):
#             if i in has_question:
#                 nonce_training_controllers.append(group_dict['question'][i])
#             else:
#                 nonce_training_controllers.append(group_dict['noquestion'][i])

#         nonce_training_controllers_big = ',\n'.join(nonce_training_controllers)

#         if group == 'high':
#             nonce_training_controllers_small = ',\n'.join(nonce_training_controllers[-5:])
#         else:
#             nonce_training_controllers_small = ',\n'.join(nonce_training_controllers[:5])
                
#         nonce_word = real_nonce_mapping[verb]
                
#         nonce_test_big = ',\n'.join(create_nonce_test_list(ident, group+'_big', nonce_word))
#         nonce_test_small = ',\n'.join(create_nonce_test_list(ident, group+'_small', nonce_word))

#         true_test_big = ',\n'.join(create_true_test_list(ident, group+'_big'))
#         true_test_small = ',\n'.join(create_true_test_list(ident, group+'_small'))
        
#         beg_mess = begin_message[verb]
#         beg_nsim_mess = begin_noncesim_message[verb]

#         config_big = config_temp(ident+'_'+group+'_big', beg_mess, beg_nsim_mess, nonce_training_controllers_big+',\n'+nonce_test_big+',\n'+true_test_big)
#         config_small = config_temp(ident+'_'+group+'_small', beg_mess, beg_nsim_mess, nonce_training_controllers_small+',\n'+nonce_test_small+',\n'+true_test_small)
        
#         listpath_big = os.path.join(args.outputdir, ident+'_'+group+'_big')
#         listpath_small = os.path.join(args.outputdir, ident+'_'+group+'_small')

#         hit_html[ident+'_'+group+'_big'] = mastertemplate(config_big)
#         hit_html[ident+'_'+group+'_small'] = mastertemplate(config_small)

#         # overview = Overview()
#         # overview.append_field('Title', title)
#         # overview.append(FormattedContent(mastertemplate(config)))

#         # question_form = QuestionForm()
#         # question_form.append(overview)
        
#         # mtc.create_hit(questions=question_form,
#         #        max_assignments=1,
#         #        title=title,
#         #        description=description,
#         #        keywords=keywords,
#         #        duration = 60*5,
#         #        reward=0.05)
        
#         with open(listpath_big, 'w') as f:
#             f.write(mastertemplate(config_big))

#         with open(listpath_small, 'w') as f:
#             f.write(mastertemplate(config_small))

        
