
cd ~/projects/iri/data/DataPrep

#$ -S /bin/bash
#$ -l h_vmem=15.0G
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q serial
#$ -N mjw_job1

source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

R CMD BATCH --no-save 05_transform_sales_promos.R ~/output/05_transform_sales_promos.output

mail -s "Stage 5" wellermatt@hotmail.com matt.weller@lancaster.ac.uk < ~/output/05_transform_sales_promos.output


