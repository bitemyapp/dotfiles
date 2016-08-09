#!/bin/bash

genp () {
	ghc-mod browse -dp Prelude
}

arr () {
	cut -d' ' -f1 | sed -r "1 s/^(.*)$/    '\1'/; 2,$ s/^(.*)$/  , '\1'/"
}

exps=""

run () {
	[ -n "$exps" ] && exps="$exps, $1" || exps="$1"
	echo "$1 = ["
	shift
	genp | grep "$@" | arr
	echo '  ]'
}

run classes 'class'
run funct '^[a-z]'
run constr '^[A-Z].*from:'
run types -e 'data' -e 'type'
echo "strange = ["
genp | grep "^[A-Z]" | grep -v '::' | grep -v -e 'True' -e 'False' | arr
echo '  ]'
echo "truefalse = [ 'True', 'False' ]"

echo "constr.push(truefalse...)"
echo "types.push(strange...)"

echo "module.exports = { $exps }"
