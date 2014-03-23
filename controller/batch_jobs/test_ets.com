
cd ~/projects/iri/model/ModelFitting/ets/
#$ -S /bin/bash
#$ -l h_vmem=2.0G
#$ -pe make 6
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q night
#$ -N ETS_NIGHT

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

R CMD BATCH --no-save ets_testing.R ~/output/ets_testing.output

mail -s "ets" wellermatt@hotmail.com < ~/output/ets_testing.output




