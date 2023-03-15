from system_call_util import *
from statistics import mean 
import matplotlib.pyplot as plt
import json
import os

def verify_promptness_on_random_formulas(instances : list, number_of_promptness_queries : int, timeout = None):  
    results = []
    for (i, instance) in enumerate(instances):
        print(i+1, '/', len(instances), ':', instance['name'])

        system = instance['ts'] 
        aps = [ap.strip() for ap in system.split('\n')[0].split(' ')[1:] if ap.strip() != '']
        inputs = ['{' + a + '}' + '_pi' for a in instance['inputs'] if '"' + a + '"' in aps]
        outputs = ['{' + a + '}' + '_pi' for a in instance['outputs'] if '"' + a + '"' in aps]

        # If there are no inputs or outputs, there is nothing to verify
        if len(inputs) == 0 or len(outputs) == 0:
            results.append(None)
            continue
        
        system_size = (len(system.split('\n')) - 4) / 2

        input_formulas = call_randltl(seed=random.randint(0, 10**8), number_of_formulas=number_of_promptness_queries, sizeL=10, sizeU=20,aps=inputs)
        output_formulas = call_randltl(seed=random.randint(0, 10**8), number_of_formulas=number_of_promptness_queries, sizeL=10, sizeU=20,aps=outputs)

        input_formulas = list(map(lambda x: x.replace('"', '').replace('{', '"').replace('}', '"'), input_formulas))
        output_formulas = list(map(lambda x: x.replace('"', '').replace('{', '"').replace('}', '"'), output_formulas))


        local_times = []
        local_sat = []

        for (input_f, output_f) in zip(input_formulas, output_formulas):

            pos = '(p -> (p U (!p U (' + output_f + '))))'
            neg = '(!p -> (!p U (p U (' + output_f + '))))'

            promptness_formula = 'E p. forall pi. (G F p) & (G F !p) & G( (' + input_f + ') -> (' + pos + ' & ' + neg + '))'

            res = call_hqptl_mc(system=system, formula=promptness_formula,timeout=timeout)

            if res == None:
                # Timeout or Error
                ()
            else:
                if 'UNSAT' in res['result']:
                    local_sat.append(False)
                    local_times.append(res['time'])
                elif 'SAT' in res['result']:
                    local_sat.append(True)
                    local_times.append(res['time'])
                else:
                    print('Unexpected output by HQPTL', res)

        if len(local_times) == 0:
            results.append(None)
        else: 
            avg_time = mean(local_times)
            instance = {'time': avg_time, 'size': system_size, 'local_times': local_times, 'local_sat': local_sat}
            results.append(instance)
                
    return results


def plot_times(data : dict, output_path : str):
    fig, axs = plt.subplots()
    
    x = []
    y = []

    for entry in data:
        if entry:
            # Generate Table
            x.append(entry['size'])
            y.append(entry['time'])
        
    axs.scatter(x, y, marker='o', alpha=0.4, s=15 )

    axs.set_xlabel('System Size') 
    axs.set_ylabel('Time (s)') 

    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    plt.savefig(output_path)  


# Set a random seed
random.seed(0)

######################################################################
instances_json_path = './eval/data/system_instances_syntcomp.json'

with open(instances_json_path, 'r') as f:
    instances_string = f.read()
instances = json.loads(instances_string)

print('Number Of Instances:', len(instances))
######################################################################

######################################################################
data_json_path = './eval/out/mc_results_promptness_syntcomp.json'

data = verify_promptness_on_random_formulas(instances=instances,number_of_promptness_queries=5,timeout=60)

data_string = json.dumps(data, indent=4)
os.makedirs(os.path.dirname(data_json_path), exist_ok=True)
with open(data_json_path, 'w') as f:
    f.write(data_string)

######################################################################

plot_times(data, './eval/out/mc_results_promptness_syntcomp.pdf')
