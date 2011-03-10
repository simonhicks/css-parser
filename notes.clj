can treat multiplication and % operations the same as literals/single args
addition can then be calculated with each side either being a single arg or a multiplication/%/bracketed operation
division can be given special treatment
  included in other calcs, but not in simple args


should model each operation seperately as a two arg thing

eg. 
(def simple-plus
  (complex [f (alt non-ratio-arg division)
            _ plus-sign
            s (alt non-ratio-arg division)]
   (make-node...)))

and then include all of the operations in non-ratio-arg except for division


