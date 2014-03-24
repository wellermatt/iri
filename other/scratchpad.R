
f_errors.calculate = function(dt)
{
    # receives a data.table and adds the errors in various guises
    dt[,e:=act-fc]
    dt[,ae:abs(e)]
    dt[,sape:(2*ae)/(act+fc)]
}


