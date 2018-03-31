.getDescend <- function(tree, node, curr = NULL) {
    # Code from Ruud Scharn
    if (is.null(curr)) {
        curr <- vector()
    }
    daughters <- tree$edge[which(tree$edge[, 1] == node), 2]
    curr <- c(curr, daughters)
    w <- which(daughters >= length(tree$tip))
    if (length(w) > 0) {
        for (i in 1:length(w)) {
            curr <- .getDescend(tree, daughters[w[i]], curr)
        }
    }
    return(curr)
}
