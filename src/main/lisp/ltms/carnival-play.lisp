(new-carnival "test")
(load "../ltms/carnival-test.lisp")
(solve)
(check-consistency)
(what-node 'NS1)
(what-node 'PIK3CA)
(what-node 'MYC)
