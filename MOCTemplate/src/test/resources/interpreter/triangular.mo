DEF MAIN:nat == triangular(2000)

DEF triang2(sum:nat, idx:nat, n:nat):nat == IF or(lt(idx, n), eq(idx, n)) THEN triang2(add(sum, idx), add(idx, 1), n) ELSE sum FI

DEF triangular(n:nat):nat == triang2(0, 0, n)
