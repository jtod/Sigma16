(TeX-add-style-hook
 "M1_System_Circuit"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:orgf4224c8"
    "sec:org123fdb7"
    "sec:org4f15872"
    "sec:org7ad5d3a"
    "sec:org78ee250"
    "sec:org1853d9b"
    "sec:org94a495b"
    "sec:orga8861eb"
    "sec:org272c6e7"
    "sec:org9182d3c"
    "sec:org89ea49c"
    "sec:org2ff0a37"
    "sec:org6c98e44"
    "sec:org8779f76"
    "sec:org0f3e9d7"
    "sec:orgd5c7fdb"
    "sec:orgd158294"
    "sec:org87adaae"
    "sec:orgd5100f6"
    "sec:org648e636"
    "sec:orgc1afc92"))
 :latex)

