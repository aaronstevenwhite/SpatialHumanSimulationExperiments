import os, re, csv, argparse

from boto.mturk.connection import MTurkConnection

## initialize parser
parser = argparse.ArgumentParser(description='Preprocess human simulation learning data')

parser.add_argument('--datadir', 
                    type=str, 
                    default='../../data/learning/raw')

parser.add_argument('--dataout', 
                    type=str, 
                    default='../../data/learning/results.preprocessed')

parser.add_argument('--memoryout', 
                    type=str, 
                    default='../../data/learning/memory.preprocessed')

## parse arguments
args = parser.parse_args()

hits = [(batch, open(os.path.join(args.datadir, batch))) for batch in os.listdir(args.datadir)]

# ['HITId', 'HITTypeId', 'Title', 'Description', 'Keywords', 'Reward', 'CreationTime', 'MaxAssignments', 'RequesterAnnotation', 'AssignmentDurationInSeconds', 'AutoApprovalDelayInSeconds', 'Expiration', 'NumberOfSimilarHITs', 'LifetimeInSeconds', 'AssignmentId', 'WorkerId', 'AssignmentStatus', 'AcceptTime', 'SubmitTime', 'AutoApprovalTime', 'ApprovalTime', 'RejectionTime', 'RequesterFeedback', 'WorkTimeInSeconds', 'LifetimeApprovalRate', 'Last30DaysApprovalRate', 'Last7DaysApprovalRate', 'Answer.Finished', 'Answer.responses', 'Approve', 'Reject']


with open(args.dataout, 'w') as f:
    with open(args.memoryout, 'w') as g:
        f.write('id,verb,context,lexical,comptype,informativity,trainingsize,verb0,verb1,rating,rt\n')
        g.write('id,verb,context,lexical,informativity,trainingsize,accuracy,rt\n')

        for batch, h in hits:
            print batch
            assignments = csv.reader(h, delimiter=',', quotechar='"')
            assignments.next() # pop off header
            for assign in assignments:
                ## get worker id and ibex responses
                wid = assign[15]
                answer = assign[-1]

                ## make evaluable by python
                # answer = re.sub('""', '"', answer)
                answer = re.sub('true', 'True', answer)
                answer = re.sub('false', 'False', answer)

                ## evaluate answer string to list
                answer = eval(answer)[3]

                memory_task = []

                for line in answer:
                    if line[3][1] == 'training' and line[0][1] == 'Question':
                        accuracy = str(line[7][1])
                        rt = str(line[8][1])

                        memory_task.append(accuracy+','+rt)

                    elif len(line) == 9 and line[0][1] == 'AcceptabilityJudgment' and line[3][1] != 'practice':
                        comp_type, verb, context, lexical, informativity, training_size = line[3][1].split('_')

                        if memory_task:
                            for datum in memory_task:
                                line_string = ','.join([wid, verb, context, lexical,
                                                        informativity, training_size,
                                                        datum])

                                g.write(line_string+'\n')

                            memory_task = []

                        verb0, verb1 = line[5][1].split(' | ')
                        rating = line[6][1]
                        rt = str(line[8][1])

                        line_string = ','.join([wid, verb, context, lexical,
                                                comp_type, informativity, training_size,
                                                verb0, verb1, rating, rt])
                        f.write(line_string+'\n')

                    #else: print line
