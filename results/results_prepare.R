library(reshape2)
## FUNCTIONS FOR RERIEVING & FORMATTING RESULTS


f_results.load = function(fil=NULL)
{
    library("stringr")
    res = readRDS(fil)
    res = f_errors.calculate(res)
    res[,`:=`(fc.item = factor(fc.item),
              lvl = factor(str_count( fc.item, "/") + 1)) ]
    
    res[,Level:=factor(lvl, levels = c(1,2,3),labels = c("ITEM", "CHAIN", "STORE"),ordered=TRUE)]    
    setkeyv(res,cols=c("lvl","fc.item","o","h"))
}

f_errors.calculate = function(dt)
{
    # receives a data.table and adds the errors in various guises
    dt[,e:=fc-act]
    dt[,ae:=abs(e)]
    dt[,`:=`(ape = ae/act,
             sape = (2*ae)/(act+fc),            
             rae.snaive = abs(fc-act)/abs(fc.snaive-act),
             rae.naive = abs(fc-act)/abs(fc.naive-act),
             ase.naive = ae/mae.naive,
             ase.snaive = ae/mae.snaive)]
}


f_results.summarise = function(res) {
    
    # mdAPE, MAPE, sMAPE, MASE
    #dcast(data=res, lvl+fc.item+h~.,fun.aggregate=median,na.rm=TRUE,value.var="ase.snaive")
    
    res.summary = res[, j = list(mape = mean(ape, na.rm=TRUE),
                                 mdape = median(ape, na.rm=T),
                                 smape = mean(sape, na.rm=TRUE),
                                 mdrae.n = median(rae.naive),
                                 mdrae.sn = median(rae.snaive),
                                 mase.n = mean(ase.naive, na.rm=TRUE),
                                 mase.sn = mean(ase.snaive, na.rm=TRUE)),
                      by=list(Level,fc.item, lvl, method, freq, freq.cycle)]

}

res.files = c("E:/data/errors/ets_12_12_3_beer_00-01-18200-53030.rds",
              "E:/data/errors/ets_52_12_3_beer_00-01-18200-53030.rds",
              "E:/data/errors/ets_52_52_3_beer_00-01-18200-53030.rds",
              "E:/data/errors/reg_12_12_3_beer_00-01-18200-53030.rds",
              "E:/data/errors/reg_52_12_3_beer_00-01-18200-53030.rds",
              "E:/data/errors/reg_52_52_3_beer_00-01-18200-53030.rds")

res.files = c("E:/data/errors/ets_12_12_3_beer_.rds",
              "E:/data/errors/ets_52_12_3_beer_.rds",
              "E:/data/errors/ets_52_52_3_beer_.rds",
              "E:/data/errors/reg_12_12_3_beer_.rds",
              "E:/data/errors/reg_52_12_3_beer_.rds",
              "E:/data/errors/reg_52_52_3_beer_.rds")

res = rbindlist(lapply(res.files,function(x) {
    vars = strsplit(strsplit(x,"/")[[1]][4],"_")[[1]][1:3]
    data.table(method = vars[1], freq = vars[2], freq.cycle = vars[3],f_results.load(x))}))[!(freq.cycle!=freq)]

saveRDS(res, "E:/data/errors/all.rds")

res[,`:=`(freq = factor(freq, levels = c(12,52),labels=c("MONTH", "WEEK")),
          freq.cycle = factor(freq.cycle,levels = c(12,52),labels=c("MONTH", "WEEK")))]


res[,.N,by=list(method,freq,freq.cycle, Level)]

 24*13*2700
dcast(res, Level+fc.item~method+freq+freq.cycle, fun.aggregate= median,na.rm=TRUE, value.var="ape")

dcast(res, freq+Level~method, fun.aggregate= median,na.rm=TRUE, value.var="ape")
res.summary = f_results.summarise(res)


library(ggplot2)
ggplot(data = res.summary[lvl==1], aes(x = method, y = mdape)) + geom_point() +  coord_flip() + facet_wrap(~freq)+
    ylim(0,0.5)


ggplot(data=res[lvl==1], aes(x= fc.item, y=ase.naive)) + geom_boxplot() + coord_flip()
ggplot(data=res[lvl==1], aes(x= fc.item, y=rae.naive)) + geom_boxplot() + coord_flip()


#res=f_results.load("E:/data/errors/reg_52_52_beer_3_.rds")
#res.summary = f_results.summarise(res)



res.summary[Level=="ITEM"]
