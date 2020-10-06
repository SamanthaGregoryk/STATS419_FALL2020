library(devtools);

detach(package:humanVerseWSU);  # if currently in use

install_github("MonteShaffer/humanVerseWSU/humanVerseWSU");  # This requires Rtools to be installed (https://cran.r-project.org/bin/windows/Rtools/)

library(humanVerseWSU);

?removeAllColumnsBut  # if you have the latest version, this new function will be available to you.

