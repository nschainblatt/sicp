(controller
  (assign product (const 1))
  (assign counter (const 1))
  test-counter
  (test (op >) (reg counter) (reg n))
  (branch (label fact-done))
  (assign product (op mul) (reg product) (reg counter))
  (assign counter (op add) (reg counter) (const 1))
  (goto (label test-counter))
  fact-done)
