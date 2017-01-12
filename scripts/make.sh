#!/bin/bash  
#Currect dirs
  scrDir=$(pwd)
  echo $scrDir
# Sources
  source ./lib/compile.sh

   
## COMPILATION
echo --- Compilation started::
#
  delBuild



  copyD 'lib/cuddBDD/' 		'CUDD'

  compD 'src/support/' 		'file' # file support 
  compD 'src/support/' 		'strg' # string support 

#   compD 'src/parsers/cir/' 	'itcBEN'
  
# IO user-defined lib
  compD 'src/parsers/cir/' 	'itcBEN' 
  copyD 'lib/ocamlgraph/' 	'Graph'
  compD 'src/ioPattern/' 	'ioType' # IO patt

  compD 'src/ioLib/'		'ioBits16'
  compD 'src/ioLib/'		'ioPrecise'
  compD 'src/ioLib/'		'ioTB'
#   compD 'src/ioPattern/lib/'	'autom_lib'
#   compD 'src/ioPattern/lib/'	'b02_lib'
  # there are others -add them   

  compD 'src/infCollect/' 	'timingGl'
  compD 'src/graph/' 		'cirG' # combin graph
  compD 'src/support/' 		'lst' # list support fun graph
  compD 'src/graph/' 		'printGr' # combin graph
  compD 'src/graph/' 		'mvfs' # combin graph


  ## BDD start
  compD 'src/bdd/' 		'strucAlloc'

 # cir Bdd struct
  compD 'src/bdd/meths/' 	'commOp' # St sPace
  compD 'src/bdd/meths/'	'logOpM1' # M1 log.oper - TB01
  compD 'src/bdd/meths/'	'logOpM2' # M2 log.oper - precise
  compD 'src/bdd/meths/'	'logOpM16' # 16 value logic
  compD 'src/bdd/meths/'	'methChoice' 

#printing cubes for debugging 
  compD 'src/bdd/printRead/'	'printCube'
  compD 'src/bdd/printRead/'	'formPairs'
# choose logic.operators
  compD 'src/bdd/transBuild/'	'bddShrink' # 
  compD 'src/bdd/transBuild/'	'varResolv' # 

   # IO patt
  compD 'src/bdd/ssWork/'	'ssManip' #
  compD 'src/ioPattern/' 	'num2Bdd' # IO patt
  compD 'src/ioPattern/' 	'inpAutom'
  compD 'src/ioPattern/' 	'ioAutom'
#   SS manipulations


  # full BDD building
  compD 'src/bdd/'		'cir2bdd' #

  

# Transition function
  compD 'src/support/' 		'bddTr' 
  compD 'src/err/check/' 	'contamin'
  compD 'src/bdd/step/'		'stopCond' 
  compD 'src/bdd/step/' 	'step' 
# 1 cycle trans and others funs
  compD 'src/bdd/step/'		'genFun' #
  
  compD 'src/err/injPriority/' 'fanInOut'  
  compD 'src/err/cirModif/' 	'cirType'  

  compD 'src/err/SET/' 		'model'  

  compD 'src/err/cirModif/' 	'votSuppr'   
  compD 'src/err/cirModif/' 	'buildCir' 
  compD 'src/err/cirModif/' 	'accumAn'
  compD 'src/err/cirModif/' 	'propagAn'
  
  compD 'src/top/' 		'top'

 


  #Top file


  #LINKING in Build folder
  cd $buildDir
#   echo $accumLink 
  $ocamlRun -g -I $ocamlTop_lib $accumLink top.cmo -o   top.exe
  check
#
echo ::Compilation DONE ---
#   ./top.exe
# echo ::Execution DONE ---





































