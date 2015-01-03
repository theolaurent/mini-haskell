command="./petitghc"

test_good() {
    for f in $@ ;
    do
	$command $f || echo "Good test fail: $f" ;
    done
}
test_bad() {
    for f in $@ ;
    do
	$command $f && echo "Bad test pass: $f" ;
    done
}



compile_and_exec() {
    file=$1
    execfile=${file/%.hs/.s}
    outfile=${file/%.hs/.out}
    outfile0="${outfile}0"
    outfile1="${outfile}1"
    ./petitghc $file ;
    spim -ldata 2000000 -f $execfile > $outfile0 ;
    sed '1,5d' $outfile0 > $outfile1 ;
    diff -q $outfile $outfile1
}

case $1 in
    syntax)
	command="${command} --parse-only" ;
	test_good tests/syntax/good/*.hs tests/typing/*/*.hs tests/exec*/*.hs ;
	test_bad tests/syntax/bad/*.hs ;;
    typing)
	command="${command} --type-only" ;
	test_good tests/typing/good/*.hs tests/exec*/*.hs ;
	test_bad tests/typing/bad/*.hs ;;
    execution)
	command="compile_and_exec" ;
	test_good tests/exec/*.hs
#	test_good tests/exec*/*.hs ;
#	test_bad tests/typing/bad/*.hs ;;
esac 
       
	
