import time 
import sys
import subprocess
from subprocess import TimeoutExpired
import random

def system_call_docker(cmd : str, timeout_sec=None):
    docker_container_name = 'c' + str(random.randint(0, 10**6))

    # First, we ensure that no container with this name already exists
    rmProc = subprocess.Popen('docker rm -f ' + docker_container_name, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    rmProc.wait()

    proc = subprocess.Popen('docker run --rm --name ' + docker_container_name + ' -it ' + cmd, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

    try:
        stdout, stderr = proc.communicate(timeout=timeout_sec)
    except TimeoutExpired:
        rmProc = subprocess.Popen('docker rm -f ' + docker_container_name, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
        rmProc.wait()
        proc.kill()
        return None, '', ''
   
    return proc.returncode, stdout.decode('utf-8').strip(), stderr.decode('utf-8').strip()

def call_hqptl_to_nba(formula : str, timeout : int):
    fileName = './eval/prop.txt'
    outFileName = './eval/out.hoa'

    with open(fileName, 'w') as file:
        file.write(formula)

    args = [
            '--mount', 
            'src=\"$(pwd)/eval\",target=/home/eval,type=bind',
            'qptltrans',
            '--qptl-to-nba',
            fileName,
            '-o',
            outFileName
        ]

    startTime = time.time()
    (code, out, err) = system_call_docker(' '.join(args), timeout_sec=timeout)
    endTime = time.time()
    et = endTime - startTime 

    if code == None:
        # Timeout
        return None


    if code != 0 or err != '': 
        print ('Error in hqptl: ', err, out, err, file=sys.stderr)
        return None

    # Parse the number of states in the generated automaton
    with open(outFileName) as f:
        out = f.read()
    numberOfStates = int((out.split('States: ')[1]).split('\n')[0])
    
    return {'time': et, 'size': numberOfStates}


def call_hqptl_mc(system : str, formula : str, timeout : int):
    prop_file_name = './eval/prop.txt'
    system_file_name = './eval/ts.txt'

    with open(prop_file_name, 'w') as f:
        f.write(formula)
    with open(system_file_name, 'w') as f:
        f.write(system)

    args = [
            '--mount', 
            'src=\"$(pwd)/eval\",target=/home/eval,type=bind',
            'frontend',
            '--verify',
            '--explicit',
            system_file_name,
            prop_file_name,
        ]

    startTime = time.time()
    (code, out, err) = system_call_docker(' '.join(args), timeout_sec=timeout)
    endTime = time.time()
    et = endTime - startTime 

    if code == None:
        # Timeout
        return None


    if code != 0 or err != '': 
        print ('Error in hqptl: ', err, out, err)
        return None

    return {'time': et, 'result': out}

def call_goal (formula : str, mode : str, timeout : int):
    modified_formula = formula.replace('0', 'false').replace('1', 'true').replace('.', ':')

    scriptFileName = './eval/goalScript.gs'
    outFileName = './eval/outGoal.hoa'

    scriptContent = '$aut = translate -m \"' + mode + '\" \"' + modified_formula + '\";\nsave $aut -c \"HOAF\" \"' + outFileName + '\";'

    with open(scriptFileName, 'w') as file:
        file.write(scriptContent)

    args = [
            '--mount', 
            'src=\"$(pwd)/eval\",target=/home/eval,type=bind',
            'goal',
            'batch',
            scriptFileName
        ]

    startTime = time.time()
    (code, out, err) = system_call_docker(' '.join(args),timeout_sec=timeout)
    endTime = time.time()
    et = endTime - startTime 

    if code == None:
        return None

    if code != 0 or err != '': 
        print ('Error in GOAL: ', out, err, file=sys.stderr)
        return None

    try:
        with open(outFileName) as f:
            out = f.read()
        numberOfStates = int((out.split('States: ')[1]).split('\n')[0])
    except:
        print('Could not parse number of states')
        return None
    
    return {'time': et, 'size': numberOfStates}



def call_randltl(seed : int, number_of_formulas : int, sizeL : int, sizeU : int, aps : list[str]):
    args = [
        'randltl',
        '--seed',
        str(seed),
        '--tree-size=' + str(sizeL) + '..' + str(sizeU),
        '--ltl-priorities \"xor=0, M=0\"',
        '-p',
        '-n',
        str(number_of_formulas)
    ] + aps

    (code, out, err) = system_call_docker(' '.join(args))

    if code == None or code != 0 or err != '':
        print('Error in spot\'s randltl', code, out, err)
        return []

    return [l for l in out.split('\n') if l != '']


def get_random_qptl_formulas(seed : int, number_of_formulas : int, sizeL : int, sizeU : int, aps, number_of_quantifiers):
    if number_of_quantifiers > len(aps):
        print('Not enough APs for all the quantifiers')
        exit(0)

    ltl_formulas = call_randltl(seed=seed, number_of_formulas=number_of_formulas, sizeL=sizeL, sizeU=sizeU, aps=aps)

    results = []

    for ltl in ltl_formulas:
        shuffledAps = random.sample(aps, k=len(aps))

        prefix = ''
        for i in range(number_of_quantifiers):
            a = shuffledAps[i]
            if i % 2 == 0:
                prefix = prefix + 'A ' + a + '. ' 
            else:
                prefix = prefix + 'E ' + a + '. ' 

        results.append(prefix + ' ' + ltl)
       
    return results