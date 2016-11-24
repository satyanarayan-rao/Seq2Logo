
#' Title
#'
#' @param sequence
#' @param image_file_name
#' @param encodingFile
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
generateSeqLogoForAsequence <- function (sequence = "AAACCTAA",
                                         image_file_name = "pwm.png",
                                         encodingFile = NULL,
                                         dpi = 150){
    seq = sequence
    if (is.null(encodingFile)) {
        encodingFile = system.file("extdata", "oneHotEncode.txt", package = "Seq2Logo")
    }
    encodedMatrix = OneHotEncode(seq, encodingFile)
    pwm = seqLogo::makePWM(encodedMatrix)
    png (filename = image_file_name,
         res = dpi)
    seqLogo(pwm)
    dev.off()
}
OneHotEncode <- function (sequence, encodingFile){
    encodingdf = read.csv(encodingFile,
                          sep = "",
                          comment.char = "#",
                          colClasses = "character")

    rownames(encodingdf) = encodingdf [["Letter"]]
    encodedString = matrix(nrow = dim(encodingdf)[1])
    encodedString = encodedString[apply (encodedString,
                                         2,
                                         function(x) all(is.finite(x)))]
    sequence = toupper(sequence)
    splittedseq = base::unlist( base::strsplit(sequence, split = ""))
    #print (splittedseq)
    for (letter in splittedseq){
        column = as.matrix(as.numeric (unlist(strsplit(encodingdf[letter, "Encoding"], split = ""))))
        encodedString = cbind(encodedString, column)
    }
    encodedString = as.data.frame(encodedString )
    #print (encodedString)
    return (encodedString)

}
