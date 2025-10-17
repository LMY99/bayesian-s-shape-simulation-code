#!/usr/bin/env bash

module load R/4.3
for setting in {1..3} do
for curve in {1..4} do
for seed in {001..100}; do
  if ! test -f "flex_CIs_${seed}_${setting}${curve}.rda"; then
    sbatch --export="seed=${seed},setting=${setting},curve=${curve}}," -J "flexA${seed}${setting}${curve}" --time=7-00:00:00 run_flex.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "sflex_CIs_${seed}_${setting}${curve}.rda"; then
    sbatch --export="seed=${seed},setting=${setting},curve=${curve}}" -J "sflexA${seed}${setting}${curve}" --time=7-00:00:00 run_flex_short.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "S_CIs_${seed}_${setting}${curve}.rda"; then
    sbatch --export="seed=${seed},setting=${setting},curve=${curve}}" -J "SA${seed}${setting}${curve}" --time=7-00:00:00 run_S.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "sS_CIs_${seed}_${setting}${curve}.rda"; then
    sbatch --export="seed=${seed},setting=${setting},curve=${curve}}" -J "sSA${seed}${setting}${curve}" --time=7-00:00:00 run_S_short.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "para_CIs_${seed}_${setting}${curve}.rda"; then
    sbatch --export="seed=${seed},setting=${setting},curve=${curve}}" -J "paraA${seed}${setting}${curve}" --time=7-00:00:00 run_para.sh
  fi
done
done
done