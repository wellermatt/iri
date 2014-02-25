
cd ~/projects/exp1.1/DataPrep
#$ -S /bin/bash
#$ -l h_vmem=20.0G
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q serial
#$ -N mjw_job1

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

Rscript --vanilla 01_consolidate_category.R "saltsnck"

Rscript --vanilla 01_consolidate_category.R "carbbev"

Rscript --vanilla 01_consolidate_category.R "fzdinent"

Rscript --vanilla 01_consolidate_category.R "milk"