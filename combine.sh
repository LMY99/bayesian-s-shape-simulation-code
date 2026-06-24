#!/usr/bin/env bash

module load conda_R
Rscript combine.R
Rscript combine_threshold.R