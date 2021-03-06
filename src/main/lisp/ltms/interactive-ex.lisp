(setq *ltms* (create-ltms "Explain Example"))
(setq x (tms-create-node *ltms* "x" :ASSUMPTIONP t)
      y (tms-create-node *ltms* "y")
      z (tms-create-node *ltms* "z")
      r (tms-create-node *ltms* "r"))
(add-formula *ltms* `(:OR ,x ,y))
(add-formula *ltms* `(:OR (:NOT ,y) ,z))
(add-formula *ltms* `(:OR (:NOT ,z) ,r))
(enable-assumption x :FALSE)
(explain-node r)
(support-for-node r)
(assumptions-of-node r)
(signed-node-string r)
(node-consequences x)
(setq clause (tms-node-support r))
(clause-antecedents clause)
(show-node-consequences x)



