
cd ~/projects/exp1.1/DataPrep
#$ -S /bin/bash
#$ -l h_vmem=8.0G
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q test
#$ -N mjw_job1

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

Rscript --vanilla 02_aggregate_category.R "diapers"
