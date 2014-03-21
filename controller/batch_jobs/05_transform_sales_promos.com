
#$ -S /bin/bash
#$ -N step_5
#$ -l h_vmem=15.0G
#$ -o '~/output/qsub.out'
#$ -e '~/output/qsub.err'
#$ -m e
#$ -M wellermatt@hotmail.com
#$ -q test


source /etc/profile

module add intel
module add R

echo Job running on compute node `uname -n`

cd ~/projects/iri/data/DataPrep
R CMD BATCH --no-save 05_transform_sales_promos.R ~/output/05_transform_sales_promos.output

mail -s "Stage 5" wellermatt@hotmail.com matt.weller@lancaster.ac.uk < ~/output/05_transform_sales_promos.output

