cd e:\Git\iri\model\ModelFitting\RegressionModelling
R CMD BATCH --vanilla "--args carbbev 1 6" 10_regression_roll.R e:\output\reg_roll_testing_carbbev.output
R CMD BATCH --vanilla "--args milk 1 6" 10_regression_roll.R e:\output\reg_roll_testing_milk.output
pause




