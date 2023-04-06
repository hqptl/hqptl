# HQPTL: A Model Checker for HyperQPTL

This repository contains HQPTL - a fully-automatic model checker for HyperQPTL on finite-state systems.
In addition, HQPTL features a QPTL-to-automata translation. 


## Structure 

- `src/` contains the source code of HQPTL (written in F#). HQPTL consists of two executables: a Frontend, which handles the model-checking and a tool called QPTLTrans which can be used for QPTL-to-automata translations. 
- `app/` is the target folder for the build. The final HQPTL executable will be placed here.
- `examples/` contains a few examples which can be used as a smoke test
- `docker/` contains various files needed to construct Docker containers containing HQPTL and GOAL
- `eval/` contains evaluation scripts (written in python) and the data needed for evaluation


The following sections contain instructions on how to build HQPTL and how to use it. 
To reproduce the experimental results from the paper, you do not need to build HQPTL yourself and instead construct a Docker image. 
Details on how to build the images and reproduce the results are given in `Reproduce_Experiments.md`.

## Build HQPTL

### Dependencies

We require the following dependencies:

- [.NET 7 SDK](https://dotnet.microsoft.com/en-us/download) (tested with version 7.0.201)
- [spot](https://spot.lrde.epita.fr/) (tested with version 2.11.4)

Install the .NET 7 SDK (see [here](https://dotnet.microsoft.com/en-us/download) for details).
Download and build spot (details can be found [here](https://spot.lrde.epita.fr/)). You can place the spot executables in any location of your choosing. 
HQPTL requires the *absolute* path to spot (see details below).

### Build HQPTL

To build HQPTL (which consists of a Frontend and a QPTL translation tool) run the following (when in the main directory of this repository).

```shell
cd src/Frontend
dotnet build -c "release" -o ../../app
cd ../..
```
and 
```shell
cd src/QPTLTrans
dotnet build -c "release" -o ../../app
cd ../..
```

Afterward, the HQPTL executables `Frontend` and `QPTLTrans` are located in the `app/` folder.
The `Frontend` tool handles the main feature of HQPTL: Model checking of HyperQPTL.
The `QPTLTrans` reuses the logic of HQPTL to translate QPTL formulas into Omega-automata.

### Connect spot to HQPTL

HQPTL requires the *autfilt* and *ltl2tgba* tools from the spot library.
HQPTL is designed such that it only needs the **absolute** path to these executables, so they can be installed and placed at whatever locations fits best.
The absolute paths are specified in a `paths.json` configuration file. 
This file must be located in the *same* directory as the HQPTL executables (this convention makes it easy to find the config file, independent of the relative path HQPTL is called from). 
We already provide a template file `app/paths.json` that *needs to be modified*. 
After having built spot, paste the absolute path to the *autfilt* and *ltl2tgba* executables to the `paths.json` file. 
For example, if `/usr/bin/autfilt` and `/usr/bin/ltl2tgba` are the *autfilt* and *ltl2tgba* executables, the content of `app/paths.json` should be

```
{
    "autfilt": "/usr/bin/autfilt",
    "ltl2tgba": "/usr/bin/ltl2tgba"
}
```

### A First Example

To test that the paths have been setup correctly, we can verify our first HyperQPTL property by running the following (from the main directory of this repository)
```shell
app/Frontend --explicit ./examples/example_system.txt ./examples/example_hyperqptl.txt
```
which should output `SAT`.


Similarly, we can translate an example QPTL formula into an NBA by running
```shell
app/QPTLTrans --qptl-to-nba ./examples/example_qptl.txt
```
which should output an automaton in the [HANOI](http://adl.github.io/hoaf/) format.


## Model Checking with the HQPTL-Frontend

In this section, we first discuss the command-line options of the HQPTL-Frontend (used for HyperQPTL model checking), followed by the structure of supported input. 

### Command-line Arguments

The HQPTL-Frontend supports several command-line options.
We focus on the verification of explicit-state systems by calling
```
app/Frontend --explicit <systemPath(s)> <propPath>
```
where `<systemPath(s)>` is either a single path to the system or multiple such paths and `<propPath>` is the path to the property.
In case  `<systemPath(s)>` is only a single path, we use the system at this path to resolve all quantifiers. 
In case `<systemPath(s)>` are multiple paths, their number must match the trace quantifier prefix in the HyperQPTL property.

In addition to explicit state systems, HQPTL also features support for NuSMV models and automata as models. They will not be needed for the evaluation.

For details on how the system and property are specified, we refer to the following sections.   

Additional (optional) command-line options include

- `-t` sets the timeout for all internal systemcalls (not the call itself) in milliseconds. If not set, we use no timeout.


### Specifying HyperQPTL Properties

The specifications checked by HQPTL are written in HyperQPTL.
A HyperQPTL formula consists of an LTL-like body, preceded by a quantifier prefix. 
Formulas have the form `<prefix> <body>`.

Here `<body>` can be one of the following:
- `1`: specifies the boolean constant true
- `0`: specifies the boolean constant false
- `"<AP>"_<TVAR>` where `<AP>` is an atomic proposition (any sequence of letters that does not contain `"`) and `<TVAR>` is a trace variable which is any string consisting of letters, digits, and `-` (starting with a letter). This atomic formula holds if the atomic proposition `<AP>` holds in the current step on trace `<TVAR>`. Note that atomic proposition name is escaped in `"`s.
- `<PVAR>` where `<PVAR>` is a propositional variable which is any string consisting of letters, digits, `-`, `_`, and `@` (starting with a letter).
- `(<body>)`: Parenthesis
- `<body> & <body>`: Conjunction
- `<body> | <body>`: Disjunction
- `<body> -> <body>`: Implication
- `<body> <-> <body>`: Equivalence
- `<body> U <body>`: Until
- `<body> W <body>`: Weak Until
- `<body> R <body>`: Release
- `F <body>`: Eventually
- `G <body>`: Globally
- `X <body>`: Next
- `! <body>`: Negation

The operators follow the usual operator precedences. To avoid ambiguity, we recommend always placing parenthesis around each construct. 

The quantifier prefix `<prefix>` can be one of the following:
- The empty string
- Universal trace quantification `forall <TVAR>. <prefix>`
- Existential trace quantification `exists <TVAR>. <prefix>`
- Universal propositional quantification `A <PVAR>. <prefix>`
- Existential propositional quantification `E <PVAR>. <prefix>`

where `<TVAR>` is a trace variable and `<PVAR>` is a propositional variable.

### Specifying Explicit-state Transition Systems

An explicit-state system has the form 

```
aps "<AP>" ... "<AP>"
init <stateID> ... <stateID> 
--BODY-- 
<stateDefinition>
...
<stateDefinition>
```

Here, `<AP>` is an atomic proposition. This can be any string not containing `"`. Note that all atomic propositions are escaped.
`<stateID>` is any natural number specifying a state. 
The header specifies which states are initial (there must be at least one initial state) and which APs are used in the system.

A `<stateDefinition>` has the form 
```
State: <stateID> <apEval>
<stateID> ... <stateID>
```

It specifies which state we are defining and the evaluation of the atomic propositions in that state. 
The `<apEval>` has the form `[(t|f) ... (t|f)]` and specifies if each atomic proposition holds (`t`) or does not hold `f`. The length of this list must match the number of atomic propositions listed in the header. 
The second line lists all successors of that state.
Every state must have at least one successor state.

Consider the following example:

```
aps "x" "y"
init 0 1
--BODY-- 
State: 0 [f f]
0 2 3
State: 1 [f t]
0 1 2
State: 2 [t f]
0 2 3
State: 3 [t t]
2 3
```

This specification declares states `0` and  `1` as initial states and `"x"` and `"y"` as APs.
For each state, we give the evaluation of the atomic propositions as a list of booleans (either `f`, or `t`).
For example, in state `1`, AP `"x"` does not hold but `"y"` does.
Each state lists all successors of that node. 
For example, the successor states of state `0` are states `0`, `2`, and `3`.

An example property on the above example system would be:

```
forall A. exists B. X (G ("x"_A <-> "y"_B))
```


## QPTL Translations with QPTLTrans

HQPTL can translate QPTL formulas (in prenex normal form) into omega-automata.
This functionality is bundled in the commandline tool `QPTLTrans`.

The supported commandline options are 

- `--qptl-to-nba <propPath>` to translate into an NBA
- `--qptl-to-gnba <propPath>` to translate into a generalized NBA

where `<propPath>` is a file that contains the QPTL formula.

For details on the QPTL syntax we refer to the following sections.   

Additional (optional) command-line options include

- `-t` sets the timeout for all internal systemcalls (not the call itself) in milliseconds. If not set, we use no timeout.
- `-o <outpulFile>` sets the output file. If set, the final automataon is written to `<outpulFile>`. If not used, the automaton is printed to stdout.


### QPTL Syntax 
A QPTL formula has the form `<prefix> <body>` where:

`<body>` can be one of the following:
- `1`: specifies the boolean constant true
- `0`: specifies the boolean constant false
- `<PVAR>` where `<PVAR>` is a propositional variable which is any string consisting of letters, digits, `-`, `_`, and `@` (starting with a letter).
- `(<body>)`: Parenthesis
- `<body> & <body>`: Conjunction
- `<body> | <body>`: Disjunction
- `<body> -> <body>`: Implication
- `<body> <-> <body>`: Equivalence
- `<body> U <body>`: Until
- `<body> W <body>`: Weak Until
- `<body> R <body>`: Release
- `F <body>`: Eventually
- `G <body>`: Globally
- `X <body>`: Next
- `! <body>`: Negation

and `<prefix>` can be one of the following:
- The empty string
- Universal propositional quantification `A <PVAR>. <prefix>`
- Existential propositional quantification `E <PVAR>. <prefix>`

The operators follow the usual operator precedences. To avoid ambiguity, we recommend always placing parenthesis around each construct. 

QPTLTrans will construct an NBA (or GNBA) over all propositional variables used within the formula which are not quantified in the prefix. 

An example QPTL property would be
```
A r_m. E g_m. ((G (F (! (r_m))))) -> ((1)&(G ((! (g_m)) | (! (g_0))))&(G ((r_0) -> (F (g_0))))&(G ((r_m) -> (X ((! (g_0)) U (g_m))))))
```