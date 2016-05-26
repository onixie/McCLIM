
(defsystem #:scigraph
    :depends-on (#:mcclim #:scrigraph/dwim)
    :serial t
    :components ((:file "scigraph/package")
                 (:file "scigraph/copy")
                 (:file "scigraph/dump")
                 (:file "scigraph/duplicate")
                 (:file "scigraph/random")
                 (:file "scigraph/menu-tools")
                 (:file "scigraph/basic-classes")
                 (:file "scigraph/draw")
                 (:file "scigraph/mouse")
                 (:file "scigraph/color")
                 (:file "scigraph/basic-graph")
                 (:file "scigraph/graph-mixins")
                 (:file "scigraph/axis")
                 (:file "scigraph/moving-object")
                 (:file "scigraph/symbol")
                 (:file "scigraph/graph-data")
                 (:file "scigraph/legend")
                 (:file "scigraph/graph-classes")
                 (:file "scigraph/present")
                 (:file "scigraph/annotations")
                 (:file "scigraph/annotated-graph")
                 (:file "scigraph/contour")
                 (:file "scigraph/equation")
                 (:file "scigraph/popup-accept")
                 (:file "scigraph/popup-accept-methods")
                 (:file "scigraph/duplicate-methods")
                 (:file "scigraph/frame")
                 (:file "scigraph/export")
                 (:file "scigraph/demo-frame")))

(defsystem #:scrigraph/dwim
  :components ((:file "dwim/package")
               (:file "dwim/feature-case")
               (:file "dwim/macros")
               (:file "dwim/tv")
               (:file "dwim/draw")
               (:file "dwim/present")
               (:file "dwim/extensions")
               (:file "dwim/wholine")
               (:file "dwim/export")))
