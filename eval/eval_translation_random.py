import os
import json
from system_call_util import *
import matplotlib.pyplot as plt

def eval_data_random(seed, goalSolver : str,number_of_formulas,timeout,number_of_quantifiers):
    numberOfAPs = 7
    sizeL = 20
    sizeU = 70

    aps = [(i+1) * 'a' for i in range(numberOfAPs)]

    formulas = get_random_qptl_formulas(seed=seed, number_of_formulas=number_of_formulas, sizeL=sizeL, sizeU=sizeU, aps=aps, number_of_quantifiers=number_of_quantifiers)
    results = []

    for (i, f) in enumerate(formulas):
        print(i+1, '/', len(formulas))
        print('Formula:', f)
        res_hqptl = call_hqptl_to_nba(formula=f, timeout=timeout)
        res_goal = call_goal(formula=f, mode=goalSolver, timeout=timeout)

        if res_hqptl == None:
            print('TO by HQPTL')

        if res_goal == None:
            print('TO by GOAL')
        
        instance = {'formula': f, 'hqptl': res_hqptl, 'goal': res_goal}

        results.append(instance)

    return results

def plot_time(data : list, timeout, output_path : str):
    fig, axs = plt.subplots()
    
    x = []
    y = []

    to_time = 1.15 * timeout

    for entry in data:
        if entry['hqptl']:
            x.append(entry['hqptl']['time'])
        else:
            x.append(to_time)

        if entry['goal']:
            y.append(entry['goal']['time'])
        else:
            y.append(to_time)

    axs.axline((0, 0), (1, 1), c = 'black', linewidth=1)

    axs.axvspan(1.1 * timeout, 1.2 * timeout, 0, 1.2 * timeout, facecolor='lightgrey')
    axs.axhspan(1.1 * timeout, 1.2 * timeout, 0, 1.2 * timeout, facecolor='lightgrey')

    axs.scatter(x, y, marker='x', c = 'blue', alpha=0.6)

    axs.set_xlabel('HQPTL (s)') 
    axs.set_ylabel('GOAL (s)') 
    axs.set_aspect('equal', adjustable='box')

    plt.xlim(0, 1.2 * timeout)
    plt.ylim(0, 1.2 * timeout)

    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    plt.savefig(output_path)  


def plot_size(data : list, output_path : str):
    all_sizes = []
    for entry in data:
        if entry['hqptl']:
            all_sizes.append(entry['hqptl']['size'])
        
        if entry['goal']:
            all_sizes.append(entry['goal']['size'])

    max_size = max(all_sizes) if len(all_sizes) != 0 else 1
    max_size_with_timeout = 10**1.3 * max_size
    min_size_with_timeout = 10**1.1 * max_size
    to_size = 10**1.2 * max_size

    fig, axs = plt.subplots()
    
    x = []
    y = []

    for entry in data:
        if entry['hqptl']:
            x.append(entry['hqptl']['size'])
        else:
            x.append(to_size)

        if entry['goal']:
            y.append(entry['goal']['size'])
        else:
            y.append(to_size)

    # Draw Diagonal Line
    axs.axline((0, 0), (1, 1), c = 'black', linewidth=1)

    axs.axvspan(min_size_with_timeout, max_size_with_timeout, 0, max_size_with_timeout, facecolor='lightgrey')
    axs.axhspan(min_size_with_timeout, max_size_with_timeout, 0, max_size_with_timeout, facecolor='lightgrey')

    axs.set_xscale('log')
    axs.set_yscale('log')
    

    axs.scatter(x, y, marker='x', c = 'blue', alpha=0.8)

    axs.set_xlabel('Size HQPTL') 
    axs.set_ylabel('Size GOAL') 
    axs.set_aspect('equal', adjustable='box')

    plt.xlim(1, max_size_with_timeout)
    plt.ylim(1, max_size_with_timeout)


    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    plt.savefig(output_path)  


random.seed(0)

number_of_quantifiers_list = [0, 3]
timeout = 20
number_of_formulas = 50

for number_of_quantifiers in number_of_quantifiers_list:
    print('Number of Quantifiers: ', number_of_quantifiers)

    data = eval_data_random(seed=0, goalSolver='couvreur', number_of_formulas=number_of_formulas, timeout=timeout, number_of_quantifiers=number_of_quantifiers)

    data_json_path = './eval/out/translation_results_random_' + str(number_of_quantifiers) + '.json'
    data_string = json.dumps(data, indent=4)
    os.makedirs(os.path.dirname(data_json_path), exist_ok=True)
    with open(data_json_path, 'w') as f:
        f.write(data_string)
   
    plot_time(data=data, timeout=timeout, output_path='./eval/out/translation_results_random_time_' + str(number_of_quantifiers) + '.pdf')
    plot_size(data=data, output_path='./eval/out/translation_results_random_size_' + str(number_of_quantifiers) + '.pdf')


