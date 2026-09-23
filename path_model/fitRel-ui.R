ui <- function () 
{
    ini({
        tka <- 0.472088188962616
        tcl <- c(-Inf, 1.01261572377496, 4.60517018598809)
        tv <- 3.45959323989827
        add.sd <- c(0, 0.695451582847587)
        eta.ka ~ 0.39790253245538
        eta.cl ~ 0.0702730989050519
        eta.v ~ 0.0190886806158593
    })
    model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
    })
}
ui <- rxode2::rxode2(ui)
ui <- rxode2::rxUiDecompress(ui)
assign("modelName", "one.cmt", envir=, ui)
rm("model", envir=ui)
ui <- rxode2::rxUiCompress(ui)

