#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-22 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(DiagrammeR)

#      Functions                                                            ####

#      Data                                                                 ####

###############################################################################
#   [Diagram]                                                               ####

grViz(diagram = "digraph flowchart {
 
  node [fontname = arial, shape = oval]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  
  #Chapter 1 Nodes
  node [fontname = arial, shape = oval]
  tab4 [label = '@@4']
  tab6 [label = '@@6']
  tab7 [label = '@@7']
  tab8 [label = '@@8']
  tab9 [label = '@@9']
  tab10 [label = '@@10']
  tab11 [label = '@@11']
  tab12 [label = '@@12']
  tab13 [label = '@@13']
  tab14 [label = '@@14']
  tab15 [label = '@@15']
  
  #Chapter 2 Nodes
  node [fontname = arial, shape = oval]
  tab5 [label = '@@5']
  
  #Base
  tab1 -> tab2;
  tab1 -> tab3;
  tab2 -> tab4;
  tab3 -> tab5
  
  #Chapter 1
  tab4 -> tab6
  tab4 -> tab7
  tab6 -> tab8
  tab6 -> tab9
  tab7 -> tab10
  tab7 -> tab11
  tab8 -> tab12
  tab9 -> tab13
  tab10 -> tab14
  tab11 -> tab15
  
  #Chapter 2
}
  
  [1]: 'Telemetry Data'
  [2]: 'Chapter 1: Resource Selection'    
  [3]: 'Chapter 2: Survival' 
  [4]: 'Bayesian Discrete Choice Model with DHGLM'
  [5]: 'Bayesian Cox Proportional Hazard Model'
  [6]: 'Mean Model'
  [7]: 'Dispersion Model'
  [8]: 'Behavioral Type'
  [9]: 'Behavioral Plasticity'
  [10]: 'Behavioral Predictability'
  [11]: 'Behavioral Syndrome'
  [12]: 'Method: Mixed Effects RSF with Random Intercept'
  [13]: 'Method: Mixed Effects RSF with Random Slopes'
  [14]: 'Method: Mixed Effects on Mean model residuals'
  [15]: 'Correlation between any of the behaviors'
  ")

