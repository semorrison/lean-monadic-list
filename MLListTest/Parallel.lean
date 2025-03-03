import MLList.Parallel

open Lean

partial def collatz (n : Nat) : Nat :=
  go 0 n
where go (s n : Nat) :=
  if n ≤ 1 then
    s
  else
    go (s+1) (if n % 2 = 0 then n / 2 else 3 * n + 1)

-- Pretty underwhelming profiling results:

set_option profiler true in -- 28
#eval (Array.range 1000000 |>.map collatz).size

set_option profiler true in -- 36
#eval do
  let R : MLList MetaM Nat := MLList.range |>.map collatz
  _ ← R.takeAsArray 1000000

set_option profiler true in -- 27
#eval do
  let R : MLList MetaM Nat := MLList.range |>.parallelMap collatz (chunkSize := 1000)
  _ ← R.takeAsArray 1000000
