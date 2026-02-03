for setting in {1..4}; do
for curve in {1..4}; do
for seed in {001..100}; do
if ! test -f "bars_result/bars_${seed}_${setting}${curve}.rda"; then
echo "Doing bars_${seed}_${setting}${curve}.rda"
Rscript "main_bars.R" $seed $setting $curve
fi
done
done
done