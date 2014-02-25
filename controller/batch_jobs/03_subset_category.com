
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

R CMD BATCH --no-save 03_subset_topn_category.R ../output/03_subset_topn_category.output
mail -s "step 3 done" wellermatt@hotmail.com matt.weller@lancaster.ac.uk < ../output/03_subset_topn_category.output


# mail -s "beer done" wellermatt@hotmail.com matt.weller@lancaster.ac.uk





