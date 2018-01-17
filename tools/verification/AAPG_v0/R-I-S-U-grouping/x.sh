#!/bin/bash
rm vishnu.txt
rm types.txt
find . -path \*.txt >> types.txt
while read line
do
	#echo $line
	printf "\n###################### \n$line : \n" >> vishnu.txt
	grep . $line >> vishnu.txt
	printf "\n###################### \n" >> vishnu.txt
done < types.txt
