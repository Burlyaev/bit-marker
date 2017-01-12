#Main Directories location
  cd .. #now at the root
  topDir=$(pwd) #save root's address
  #echo Top Directory:: $topDir
  buildDir=$topDir'/Build/' # build folder
  libDir=$topDir'/lib/' #3rd-pqrty library dir
  srcDir=$topDir'/src/' #scr directory
  
#batch compilation
  ocamlRun=ocamlc 
  ocamlTop_lib='usr/lib/ocaml/'
# third party libs needed
  accumLink=$accumLink' graph.cma  cudd.cma str.cma' 
##Functions
function cpUse  { #copy compiled files to Build
    cp *.* $buildDir$1
    cp *.* $buildDir$1
    cp *.* $buildDir$1
  }
function mv2Build  { #copy compiled files to Build
    mv $1.cmo $buildDir
    mv $1.cmi $buildDir
    mv $1.cma $buildDir
    cp $1.ml  $buildDir
  }

function cleaning { # deleting previously compiled files
    rm $1'.cma' > null
    rm $1'.cmo' > null
    rm $1'.cmi' > null
  }

function check { #chech if files are really compiles 
  if [ $? -ne 0 ]
    then
	echo [Err] COMPILATION FAILED 
	exit 1
    fi
  } 

function comp { # compile inside the folder of scr
  #cleaning $1 > null 
  $ocamlRun -g -c  -I $buildDir  $1'.ml' 
  check
  $ocamlRun -g -a  -I $buildDir  $1'.cmo' -o $1'.cma'
  check
  #accumLink=$accumLink' '$1'.cma'
 }
 
function compD { #compile and copy to Build
    echo Compilation $2 
    comp $1$2
    accumLink=$accumLink' '$2'.cma'
    mv2Build $1$2 
  }

function copyD { #just copy to Build
    echo Copying $2 
    cd $1
    cpUse
    cd $topDir
 }

# Deleting all files in Build	
function delBuild {  
    echo Deleting Old Build
    cd $buildDir 
    rm *.* > null
    echo ...  Done
    cd $topDir
}