import os
import json
from system_call_util import *
import matplotlib.pyplot as plt

def construct_full_qptl(ltl):
    prefix = ''
    for a in ltl['outputs']:
        prefix = 'E ' + a + '. ' + prefix
    for a in ltl['inputs']:
        prefix = 'A ' + a + '. ' + prefix

    return prefix + ltl['formula']

def translate_formulas(formulas : list[str], goal_solvers : list[str], timeout):
    results = []
    for (i, f) in enumerate(formulas):
        print(i + 1, '/', len(formulas))
        print('Formula:', f)

        instance = dict()

        print('Call', 'HQPTL')
        res = call_hqptl_to_nba(formula=f, timeout=timeout)
        if res == None:
            print('TO by', 'HQPTL')
            instance['HQPTL'] = None 
        else:
            instance['HQPTL'] = {'time': res['time']}

        for goalSolver in goal_solvers:
            print('Call', goalSolver)
            res = call_goal(formula=f, mode=goalSolver, timeout=timeout)
            if res == None:
                print('TO by', goalSolver)
                instance[goalSolver] = None 
            else:
                instance[goalSolver] = {'time': res['time']}

        results.append(instance)

    return results


def generate_survival_plot_times(data : list, solvers: list[str], output_path : str):
    results = dict()
    for s in solvers:
        results[s] = []

    for instance in data:
        for s in solvers:
            if s not in instance:
                continue

            if instance[s] != None:
                results[s].append(instance[s]['time'])

    
    fig, axs = plt.subplots()
    for k in solvers:
        times = results[k].copy()
        times.sort()

        x = [*range(1, len(times) + 1)]
        y = []
        count = 0
        for t in times:
            count += t
            y.append(count)

        axs.plot(x, y,label=k, alpha=1.0, linewidth=2)

    axs.legend()

    axs.set_xlabel('Instances') 
    axs.set_ylabel('Time (s)') 

    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    plt.savefig(output_path)  


#goal_solvers = ['tableau', 'inctableau', 'temporaltester', 'gpvw', 'gpvw+', 'ltl2aut', 'ltl2aut+', 'ltl2ba', 'pltl2ba', 'couvreur', 'ltl2buchi', 'modella', 'kp02']
goal_solvers = ['ltl2aut', 'ltl2aut+', 'ltl2ba', 'pltl2ba', 'couvreur', 'ltl2buchi']

# Load the instances
with open('./eval/data/formula_instances_syntcomp.json', 'r') as f:
    data_string = f.read()
instances = json.loads(data_string)

# Construct the full qptl instance for each formula
formulas = [construct_full_qptl(instance) for instance in instances]

results_json_path = './eval/out/translation_results_syntcomp.json'

# Solve each instance
data = translate_formulas(formulas=formulas, goal_solvers=goal_solvers,timeout=20)

os.makedirs(os.path.dirname(results_json_path), exist_ok=True)
data_string = json.dumps(data, indent=4)
with open(results_json_path, 'w') as f:
    f.write(data_string)

generate_survival_plot_times(data=data, solvers=['HQPTL'] + goal_solvers, output_path='./eval/out/translation_results_syntcomp.pdf')
