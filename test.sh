#!/bin/bash
chords="./allChords.txt"
guesses="./guesses.txt"

count=0
totalChords=$(wc -l < $chords)

#flash guesses.txt
echo "" > $guesses

while read chord
do
	# find guesses
	./Proj1Test $chord | grep -Po "[0-9]+(?= guesses)" >> $guesses
	
	# inc the counter
	count=$[count+1]
	
	# update the progress bar
	avg=$(awk "BEGIN {printf \"%.2f\",100*${count}/${totalChords}}")
	echo -ne 'Please wait ('$avg'%)\r'
done < "$chords"

# flush the progress bar
echo ''
# print results
awk '{s += $1} END {print "Average Guesses:", s / NR}' $guesses
