#!/usr/bin/env bash

module load conda_R/4.3.x
Rscript "main_para.R" $seed
