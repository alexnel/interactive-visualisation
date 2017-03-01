format=$2
file=$2

$1 -Tplain -O $2
$1 -Tpng -O $2
stack build
stack exec interactive-visualisation $2