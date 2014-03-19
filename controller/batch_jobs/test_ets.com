
cd ~/projects/iri/model/ModelFitting/ets/
#$ -S /bin/bash
#$ -l h_vmem=4.0G
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q test
#$ -N mjw_job1

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

Rscript --vanilla ets_testing.R




