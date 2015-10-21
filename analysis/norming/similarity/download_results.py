import re, argparse

from boto.mturk.connection import MTurkConnection

## initialize parser
parser = argparse.ArgumentParser(description='Preprocess human simulation norming data')

parser.add_argument('--dataout', 
                    type=str, 
                    default='../../../data/norming/similarity/results.preprocessed')

## parse arguments
args = parser.parse_args()


mturk = MTurkConnection()
hits = mturk.get_all_hits()

with open(args.dataout, 'w') as f:
    f.write('id,verb0,verb1,rating,rt\n')
    for h in hits:
        if h.Title == 'Word Similarity Rating Task (~8 minutes)':
            hid = h.HITId
            assignments = mturk.get_assignments(hid)

            for assign in assignments:
                ## get worker id and ibex responses
                wid = assign.WorkerId
                answer = assign.answers[0][0].fields[0]

                ## make evaluable by python
                answer = re.sub('""', '"', answer)
                answer = re.sub('true', 'True', answer)
                answer = re.sub('false', 'False', answer)

                ## evaluate answer string to list
                answer = eval(answer)[3]

                for line in answer:
                    if len(line) == 9 and line[3][1] != 'practice':
                        verb0, verb1 = line[5][1].split(' | ')
                        rating = line[6][1]
                        rt = str(line[8][1])

                        line_string = ','.join([wid, verb0, verb1, rating, rt])
                        f.write(line_string+'\n')
