intAct2Sys <- function(prot2Sys, bpSysL) {
    ind <- bpSysL[[prot2Sys]]
    bpnames <- names(ind)
    if (!all(is.null(bpnames)) && !all(is.na(bpnames))) {
        intAct2Sys(names(bpSysL[[prot2Sys]])[1], bpSysL[[prot2Sys]])
    } else {
        if (!is.null(ind) && all(ind != "ND")) {
            ind[1]
        } else {
            prot2Sys
        }
    }
}
