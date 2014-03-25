cd e:\Git\iri\model\ModelFitting\RegressionModelling

R CMD BATCH --vanilla "--args milk 1 8" 10_regression_roll.R e:\output\reg_roll_testing_milk.output
#R CMD BATCH --vanilla "--args carbbev 1 8" 10_regression_roll.R e:\output\reg_roll_testing_carbbev.output

pause




