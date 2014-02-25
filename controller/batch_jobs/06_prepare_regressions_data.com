
cd ~/projects/exp1.1/DataPrep
#$ -S /bin/bash
#$ -l h_vmem=4.0G
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q serial
#$ -N mjw_job1

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

Rscript --vanilla 06_prepare_regression_data.R "beer"

Rscript --vanilla 06_prepare_regression_data.R "milk"

Rscript --vanilla 06_prepare_regression_data.R "carbbev"
