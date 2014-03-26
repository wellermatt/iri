
cd ~/projects/iri/model/ModelFitting/RegressionModelling/
#$ -S /bin/bash
#$ -l h_vmem=4.0G
#$ -pe make 4
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q parallel
#$ -N REG_ROLL_PARALLEL_ALL_4

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

R CMD BATCH --no-save "--args milk 3 4" 10_regression_roll.R ~/output/reg_roll_milk_all.output
R CMD BATCH --no-save "--args carbbev 3 4" 10_regression_roll.R ~/output/reg_roll_carbbev_all.output
R CMD BATCH --no-save "--args beer 3 4" 10_regression_roll.R ~/output/reg_roll_beer_all.output





