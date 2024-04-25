#!/usr/bin/env bash


for seed in {001..100}; do
  if ! test -f "flex_CIs_${seed}.rda"; then
<<<<<<< HEAD
    sbatch --export="seed=${seed}" -J "flexA${seed}" --time=7-00:00:00 run_flex.sh
=======
    sbatch --export="seed=${seed}" -J "flexL${seed}" --time=7-00:00:00 --mem=2G run_flex.sh
>>>>>>> aa966e1 (Specify memory alloc on JHPCE)
  fi
done
for seed in {001..100}; do
  if ! test -f "sflex_CIs_${seed}.rda"; then
<<<<<<< HEAD
    sbatch --export="seed=${seed}" -J "sflexA${seed}" --time=7-00:00:00 run_flex_short.sh
=======
    sbatch --export="seed=${seed}" -J "sflexL${seed}" --time=7-00:00:00 --mem=2G run_flex_short.sh
>>>>>>> aa966e1 (Specify memory alloc on JHPCE)
  fi
done
for seed in {001..100}; do
  if ! test -f "S_CIs_${seed}.rda"; then
<<<<<<< HEAD
    sbatch --export="seed=${seed}" -J "SA${seed}" --time=7-00:00:00 run_S.sh
=======
    sbatch --export="seed=${seed}" -J "SL${seed}" --time=7-00:00:00 --mem=2G run_S.sh
>>>>>>> aa966e1 (Specify memory alloc on JHPCE)
  fi
done
for seed in {001..100}; do
  if ! test -f "sS_CIs_${seed}.rda"; then
<<<<<<< HEAD
    sbatch --export="seed=${seed}" -J "sSA${seed}" --time=7-00:00:00 run_S_short.sh
=======
    sbatch --export="seed=${seed}" -J "sSL${seed}" --time=7-00:00:00 --mem=2G run_S_short.sh
>>>>>>> aa966e1 (Specify memory alloc on JHPCE)
  fi
done
for seed in {001..100}; do
  if ! test -f "para_CIs_${seed}.rda"; then
<<<<<<< HEAD
    sbatch --export="seed=${seed}" -J "paraA${seed}" --time=7-00:00:00 run_para.sh
=======
    sbatch --export="seed=${seed}" -J "paraL${seed}" --time=7-00:00:00 --mem=2G run_para.sh
>>>>>>> aa966e1 (Specify memory alloc on JHPCE)
  fi
done
