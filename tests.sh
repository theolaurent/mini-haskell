command="./main.native"

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

case $1 in
    syntax)
	command="./main.native --parse-only" ;
	test_good tests/syntax/good/*.hs tests/typing/*/*.hs tests/exec*/*.hs ;
	test_bad tests/syntax/bad/*.hs ;;
    typing)
	command="./main.native --type-only" ;
	test_good tests/typing/good/*.hs tests/exec*/*.hs ;
	test_bad tests/typing/bad/*.hs ;;
esac 
       
	
