(bps-load-file (make-bps-path "ltms") "ltre" :action :compile)
(compile-ltre)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)
;;do NOT load -- causes trouble with JTRE!
;;(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)
(bps-load-file (make-bps-path "ltms") "dds" :action :compile)
(bps-load-file (make-bps-path "ltms") "carnival" :action :compile)
(bps-load-file (make-bps-path "ltms") "carnival-ltre" :action :compile)
(bps-load-file (make-bps-path "ltms") "carnival-ex" :action :compile)
(bps-load-file (make-bps-path "ltms") "carnival-play" :action :compile)
