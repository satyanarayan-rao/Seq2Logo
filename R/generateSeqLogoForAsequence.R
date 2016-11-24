
#' Title
#'
#' @param sequence
#' @param image_file_name
#' @param dpi
#' @param save_to_dir
#' @param encodingFile
#' @param height
#' @param width
#'
#' @return
#' @export
#'
#' @examples
generateSeqLogoForAsequence <- function (sequence = "AAACCTAA",
                                         image_file_name = "pwm.png",
                                         save_to_dir = "~/",
                                         encodingFile = NULL,
                                         height = 4,
                                         width = 6.48, # golen ratio * height
                                         dpi = 150,
                                         onlyLetters = TRUE){
    seq = sequence
    if (is.null(encodingFile)) {
        encodingFile = system.file("extdata", "oneHotEncode.txt", package = "Seq2Logo")
    }
    encodedMatrix = OneHotEncode(seq, encodingFile)
    pwm = seqLogo::makePWM(encodedMatrix)
    pngfileName = paste (save_to_dir, image_file_name, sep = "/")
    png (filename = pngfileName,
         height = 4,
         width = 7,
         units = "in",
         res = dpi)
    if (onlyLetters == TRUE) {
        seqLogo::seqLogo(pwm = pwm,
                         xaxis = FALSE, yaxis = FALSE,
                         ic.scale = FALSE)
    } else {
        seqLogo::seqLogo(pwm = pwm)
    }

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
