#! /bin/bash

#set -x

#if [$# -ne 3]
#then
#  echo "Usage: `basename $0` {acmofile} {xvariable} {yvariable} {group1} {group2} {pngoutput}"
#  exit -1
#fi

acmocsv=$1
format=$2
X=$3
Y=$4
group1=$5
group2=$6
pngoutput=$7

command -v R >/dev/null 2>&1 || { echo >&2 "'R' is required by this tool but was not found on past";exit 1;}

INSTALL_DIR="$( cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd )"
rcorrelation=$INSTALL_DIR/Correlation.r
#echo "$2"



xvfb-run R --no-save --vanilla --slave --args $acmocsv $format $X $Y $group1 $group2 $pngoutput< $rcorrelation

exit
