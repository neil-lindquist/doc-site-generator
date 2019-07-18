
(asdf:defsystem "doc-site-generator"
  :description "Tools for generating the pages on neil-lindquist.github.io"
  :version "0.0.0"
  :author "Neil Lindquist <NeilLindquist5@gmail.com>"
  :licence "MIT"

  :depends-on ("cl-ppcre"
               "sb-introspect")
  :components ((:file "docs")))
