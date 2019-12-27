function run_amplifier_set () {
    if [ $# != 5 ]; then
        echo "need 5 args"
        exit 1
    fi

    INTCODE_EXEC="../../lib/bs/native/intcode2.native"
    PROGRAM_FILE="./input.txt"

    AMP1_OUTPUT="$(echo "$1\n0" | $INTCODE_EXEC $PROGRAM_FILE)"
    AMP2_OUTPUT="$(echo "$2\n$AMP1_OUTPUT" | $INTCODE_EXEC $PROGRAM_FILE)"
    AMP3_OUTPUT="$(echo "$3\n$AMP2_OUTPUT" | $INTCODE_EXEC $PROGRAM_FILE)"
    AMP4_OUTPUT="$(echo "$4\n$AMP3_OUTPUT" | $INTCODE_EXEC $PROGRAM_FILE)"
    AMP5_OUTPUT="$(echo "$5\n$AMP4_OUTPUT" | $INTCODE_EXEC $PROGRAM_FILE)"
    echo $AMP5_OUTPUT
}

OUTPUTS=""

# pipe this output to sort -n to find the max
cat ./permutations.txt | while read line; do
    NEW_OUTPUT="$(run_amplifier_set $line)"
    echo $NEW_OUTPUT
done
