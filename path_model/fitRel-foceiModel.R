foceiModel <- list()

foceiModel[["inner"]] <- rxode2::rxode2("param(THETA[1],THETA[2],THETA[3],THETA[4],ETA[1],ETA[2],ETA[3]);\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_expr_0~ETA[2]+THETA[2];\nrx_expr_1~ETA[3]+THETA[3];\nrx_expr_2~ETA[1]+THETA[1];\nrx_expr_3~exp(rx_expr_0);\nrx_expr_4~exp(rx_expr_1);\nrx_expr_5~exp(rx_expr_2);\nrx_pred_=linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,rx_expr_3,rx_expr_4,0,0,0,0,rx_expr_5);\nrx__sens_rx_pred__BY_ETA_1___=rx_expr_5*((rx__sens_central_BY_ka)/(rx_expr_4));\nrx__sens_rx_pred__BY_ETA_2___=rx_expr_3*((rx__sens_central_BY_p1)/(rx_expr_4));\nrx__sens_rx_pred__BY_ETA_3___=rx_expr_4*(-(central)/((rx_expr_4)*(rx_expr_4))+(rx__sens_central_BY_v1)/(rx_expr_4));\nrx_r_=Rx_pow_di(THETA[4],2);\nrx__sens_rx_r__BY_ETA_1___=0;\nrx__sens_rx_r__BY_ETA_2___=0;\nrx__sens_rx_r__BY_ETA_3___=0;\ncmt(rxLinCmt);\ndvid(3);\n")

foceiModel[["innerHess2"]] <- NULL

foceiModel[["innerOeta"]] <- "rx_yj_~2\nrx_lambda_~1\nrx_hi_~1\nrx_low_~0\nrx_expr_0~ETA[2]+THETA[2]\nrx_expr_1~ETA[3]+THETA[3]\nrx_expr_2~ETA[1]+THETA[1]\nrx_expr_3~exp(rx_expr_0)\nrx_expr_4~exp(rx_expr_1)\nrx_expr_5~exp(rx_expr_2)\nrx_pred_=linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5)\nrx__sens_rx_pred__BY_ETA_1___=rx_expr_5*((rx__sens_central_BY_ka)/(rx_expr_4))\nrx__sens_rx_pred__BY_ETA_2___=rx_expr_3*((rx__sens_central_BY_p1)/(rx_expr_4))\nrx__sens_rx_pred__BY_ETA_3___=rx_expr_4*(-(central)/((rx_expr_4)*(rx_expr_4))+(rx__sens_central_BY_v1)/(rx_expr_4))\nrx_r_=Rx_pow_di(THETA[4], 2)\nrx__sens_rx_r__BY_ETA_1___=0\nrx__sens_rx_r__BY_ETA_2___=0\nrx__sens_rx_r__BY_ETA_3___=0\nrx__ETA1=ETA[1]\nrx__ETA2=ETA[2]\nrx__ETA3=ETA[3]"

foceiModel[["predOnly"]] <- rxode2::rxode2("param(THETA[1],THETA[2],THETA[3],THETA[4],ETA[1],ETA[2],ETA[3]);\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_expr_0~ETA[2]+THETA[2];\nrx_expr_1~ETA[3]+THETA[3];\nrx_expr_2~ETA[1]+THETA[1];\nrx_expr_3~exp(rx_expr_0);\nrx_expr_4~exp(rx_expr_1);\nrx_expr_5~exp(rx_expr_2);\nrx_pred_=linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,rx_expr_3,rx_expr_4,0,0,0,0,rx_expr_5);\nrx_r_=Rx_pow_di(THETA[4],2);\ntka=THETA[1];\ntcl=THETA[2];\ntv=THETA[3];\nadd.sd=THETA[4];\neta.ka=ETA[1];\neta.cl=ETA[2];\neta.v=ETA[3];\nka=rx_expr_5;\ncl=rx_expr_3;\nv=rx_expr_4;\ntad=tad();\ndosenum=dosenum();\ncmt(rxLinCmt);\ndvid(3);\n")

foceiModel[["extra.pars"]] <- NULL

foceiModel[["outer"]] <- NULL

foceiModel[["outerMeta"]] <- NULL

foceiModel[["outerPoolOk"]] <- TRUE

foceiModel[["outerNode"]] <- NULL

foceiModel[["outerNodeMeta"]] <- NULL

foceiModel[["predNoLhs"]] <- rxode2::rxode2("param(THETA[1],THETA[2],THETA[3],THETA[4],ETA[1],ETA[2],ETA[3]);\nrx_yj_~2;\nrx_lambda_~1;\nrx_hi_~1;\nrx_low_~0;\nrx_pred_=linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,exp(ETA[2]+THETA[2]),exp(ETA[3]+THETA[3]),0,0,0,0,exp(ETA[1]+THETA[1]));\nrx_r_=Rx_pow_di(THETA[4],2);\ncmt(rxLinCmt);\ndvid(3);\n")

foceiModel[["theta"]] <- NULL

foceiModel[["pred.minus.dv"]] <- TRUE

foceiModel[["log.thetas"]] <- integer(0)

foceiModel[["log.etas"]] <- integer(0)

foceiModel[["extraProps"]] <- list()

foceiModel[["eventTheta"]] <- c(0L, 0L, 0L, 0L)

foceiModel[["eventEta"]] <- c(0L, 0L, 0L)

foceiModel[["eventEtaAll"]] <- c(0L, 0L, 0L)

class(foceiModel) <- "foceiModelList"

