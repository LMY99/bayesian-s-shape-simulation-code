#!/usr/bin/env bash

module load conda_R/4.3
Rscript "main_S.R" $seed
