
cd ~/projects/iri/model/ModelFitting/Regression/
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

R CMD BATCH --no-save 10_regression_roll.R ~/output/reg_roll_testing.output

mail -s "reg roll" wellermatt@hotmail.com < ~/output/reg_roll_testing.output




