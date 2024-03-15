pdfseparate -f %1 -l %1 %4_html.pdf Sigma16UserGuide-images\lateximagetemp-%%d.pdf
pdfcrop --hires --margins "0 1 0 0" Sigma16UserGuide-images\lateximagetemp-%1.pdf Sigma16UserGuide-images\%3.pdf
pdftocairo -svg -noshrink Sigma16UserGuide-images\%3.pdf Sigma16UserGuide-images\%3.svg
del Sigma16UserGuide-images\%3.pdf
del Sigma16UserGuide-images\lateximagetemp-%1.pdf
exit
