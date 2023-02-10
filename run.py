import glob
import os
from pprint import pprint
import re
import sys
import time

from dotenv import dotenv_values
import example
from epidoctokenizer import tokenize_file, tokenize_string

config = dotenv_values('.env')
DDBDP_PATH = config['DDBDP_PATH']

if __name__ == '__main__':

    if sys.argv[1] == '-f':
        tokenizer = tokenize_file(sys.argv[2])
        meta = tokenizer['meta']
        tokens = tokenizer['tokens']()
        print(tokenizer['edition_xml'])
        for t in tokens:
            print(t)
            print(t['orig']['form'])


    elif sys.argv[1] == 'example':
        tokenizer = tokenize_string(example.xml_str)
        print(tokenizer['edition_xml'])

        tokens = tokenizer['tokens']()
        pprint(tokens)

    elif sys.argv[1] == 'tests':
        #test_result = tests('/Users/erikhenriksson/Documents/GitHub/idp.data/DDB_EpiDoc_XML/bgu/bgu.4/bgu.4.1113dupl.xml', 0)
        #print(test_result)
        #exit()
        sus = 0
        prs = 0
        comp = 0
        total = len(glob.glob(f'{DDBDP_PATH}/**/*.xml', recursive=True))
        c = 0
        for root, dirs, files in os.walk(DDBDP_PATH):
            dirs.sort()
            for name in sorted(files):
                if name.endswith('xml'):
                    c+=1
                    startTime = time.time()
                    #print(os.path.join(root, name))
                    test_result = tests(os.path.join(root, name))

                    sus += test_result[1]
                    prs += test_result[0]
                    comp += test_result[2]

                    #print(test_result)
                    print(f'{(c/total):.2f}', end="\r")
                    #if not c % 1000:
                    #    print(sus, prs,comp)
        print(sus, prs)

    elif sys.argv[1] == 'IDP':
        total = len(glob.glob(f'{DDBDP_PATH}/**/*.xml', recursive=True))
        c = 0
        var_types = set()
        for root, dirs, files in os.walk(DDBDP_PATH):
            dirs.sort()
            for name in sorted(files):
                if name.endswith('xml'):
                    c+=1
                    startTime = time.time()
                    #print(os.path.join(root, name))
                    tokenizer = tokenize_file(os.path.join(root, name))
                    tokens = tokenizer['tokens']()
                    #executionTime = (time.time() - startTime)
                    #print('Took ' + str(executionTime))
                    print(f'{(c/total):.2f}', end="\r")
                    '''
                    for t in tokens:
                        if type(t) == list:
                            for l in t:
                                parts = l.split('⧽')
                                for p in parts:
                                    m = re.findall('>[a-z]+(?=\[[0-9]+\])', p)
                                    var_type = ''.join(m)
                                    var_types.add(var_type)
                    '''
                    #print(f'{(c/total):.2f}', end="\r")
                    #if not c % 100:
                    #    pprint(sorted(var_types))
                    #print(tokenizer['edition_xml'])
                    #for t in tokens:
                    #    if type(t) == list:
                    #        print(t)
                            

                      
                    '''
                    for key, val in tokens.items():
                        TOTAL_CHOICES += 1
                        if key not in ALL_CHOICES:
                            ALL_CHOICES[key] = 1
                        else:
                            ALL_CHOICES[key] += 1

                
                    
                    print(f'{(c/total):.2f}', end="\r")
                    '''
        pprint(sorted(var_types))
        #result()


