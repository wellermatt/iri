
cd ~/projects/iri/model/ModelFitting/RegressionModelling/
#$ -S /bin/bash
#$ -l h_vmem=5.0G
#$ -pe make 8
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q parallel
#$ -N REG_ROLL_TEST

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

R CMD BATCH --no-save "--args milk 3 8" 10_regression_roll.R ~/output/reg_roll_milk.output
R CMD BATCH --no-save "--args carbbev 3 8" 10_regression_roll.R ~/output/reg_roll_carbbev.output
R CMD BATCH --no-save "--args beer 3 8" 10_regression_roll.R ~/output/reg_roll_beer.output





