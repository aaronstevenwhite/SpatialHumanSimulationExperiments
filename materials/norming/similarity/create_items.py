import os, string, argparse, random
from collections import defaultdict

##################
## argument parser
##################

random.seed(300)

## initialize parser
parser = argparse.ArgumentParser(description='Create human simulation experiment')


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
parser.add_argument('--wordpairs', 
                    type=str, 
                    default='../../../analysis/norming/human_simulation/unique_truelemma_responselemma_pairs')
parser.add_argument('--itemsperlist', 
                    type=int, 
                    default=60)
parser.add_argument('--experimentdir', 
                    type=str, 
                    default='.')

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

## build experiment
wordpairs = [line.strip().split(';') for i, line in enumerate(open(args.wordpairs)) if i > 0]
pairdict = defaultdict(list)

for verblemma, responselemma in wordpairs:
    if verblemma != responselemma:
        pairdict[verblemma].append(responselemma)
        random.shuffle(pairdict[verblemma])

length = {verblemma : len(responselemmalist) for verblemma, responselemmalist in pairdict.iteritems()}
num_per_list = {verblemma : int(round(args.itemsperlist*float(verblength) / sum(length.values()), 0)) for verblemma, verblength in length.iteritems()}

def create_controller(verb_pair):
    verb1, verb2 = verb_pair

    return '["similarity", "AcceptabilityJudgment", {q: \''+verb1+' | '+verb2+'\'}]'

num_of_lists = (sum(length.values()) / args.itemsperlist) - 2

item_lists = defaultdict(list)

for l in range(num_of_lists):
    for verblemma, responselemmalist in pairdict.iteritems():
        end = num_per_list[verblemma]
        list_addition = [create_controller([verblemma, responselemma]) for responselemma in responselemmalist[:end]]
        item_lists[l].extend(list_addition)
        pairdict[verblemma] = responselemmalist[end:]
    random.shuffle(item_lists[l])

l = 0
for verblemma, responselemmalist in pairdict.iteritems():
    for responselemma in responselemmalist:
        item_lists[l].append(create_controller([verblemma, responselemma]))
        l = (l+1) % num_of_lists

conf_temp = lambda items: string.Template('''
var shuffleSequence = seq("consent", "intro", "practice", "begin", sepWith("sep", randomize("similarity")));
var practiceItemTypes = ["practice"];

var defaults = [
    "Separator", {
        transfer: 500,
        hideProgressBar: true,
        normalMessage: "+"
    },
    "Message", {
        hideProgressBar: true
    },
    "AcceptabilityJudgment", {
        s: 'How similar are these two verbs?',
        as: ["1", "2", "3", "4", "5", "6", "7"],
        presentAsScale: true,
        instructions: "Use number keys or click boxes to answer.",
        leftComment: "Very dissimilar", rightComment: "Very similar"
    },
    "Form", { 
        hideProgressBar: true,
        continueOnReturn: false   
    }

];

var items = [
	["consent", "Form", {
        html: { include: "consent.html" },
		validators: {age: function (s) { if (s.match(/^\d+$$/)) return true;
							else return "Bad value for age"; }}
    } ],

	["intro", "Message", {html: { include: "intro.html" }}],

	["practice", "AcceptabilityJudgment", {q: "crack | break"}],
	["practice", "AcceptabilityJudgment", {q: "eat | fail"}],
	["practice", "AcceptabilityJudgment", {q: "stand | sit"}],
	["practice", "AcceptabilityJudgment", {q: "tease | gouge"}],
	["practice", "AcceptabilityJudgment", {q: "park | drive"}],

	["begin", "Message", {
				html: { include: "begin.html" },
				} ],

    ["sep", "Separator", { }],

    $items

];
''').substitute(items=items)

master = defaultdict(str)

for l in item_lists:
    master[l] = mastertemplate(conf_temp(',\n'.join(item_lists[l])))

    with open(os.path.join(args.experimentdir, 'lists/list'+str(l)), 'w') as f:
        f.write(master[l])
