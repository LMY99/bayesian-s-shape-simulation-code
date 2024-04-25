#!/usr/bin/env bash


for seed in {001..100}; do
  if ! test -f "flex_CIs_${seed}.rda"; then
    sbatch --export="seed=${seed}" -J "flexL${seed}" --time=7-00:00:00 --mem=2G run_flex.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "sflex_CIs_${seed}.rda"; then
    sbatch --export="seed=${seed}" -J "sflexL${seed}" --time=7-00:00:00 --mem=2G run_flex_short.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "S_CIs_${seed}.rda"; then
    sbatch --export="seed=${seed}" -J "SL${seed}" --time=7-00:00:00 --mem=2G run_S.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "sS_CIs_${seed}.rda"; then
    sbatch --export="seed=${seed}" -J "sSL${seed}" --time=7-00:00:00 --mem=2G run_S_short.sh
  fi
done
for seed in {001..100}; do
  if ! test -f "para_CIs_${seed}.rda"; then
    sbatch --export="seed=${seed}" -J "paraL${seed}" --time=7-00:00:00 --mem=2G run_para.sh
  fi
done
