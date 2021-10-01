harmonic n | n < 2 = error "values must be >= 2"
harmonic 2          = 0.5
harmonic n          = (1/n) + harmonic (n-1)