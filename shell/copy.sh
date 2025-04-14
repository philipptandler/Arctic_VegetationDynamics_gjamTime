FOLDERS=(
	# "probe1_highorder1"
	"probe1_highorder2"
)

for arg in "${FOLDERS[@]}"; do
	scp tandlerp@euler.ethz.ch:/cluster/scratch/tandlerp/analysis/"$arg"/* analysis/"$arg"/
done
