/-
Copyright (c) 2023 Kim Morrison. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import Std.Data.HashSet
import Batteries.Data.MLList.Basic
/-!
# Lazy deduplication of lazy lists
-/

namespace MLList

variable {α β : Type} {m : Type → Type} [Monad m] [BEq β] [Hashable β]

/-- Lazily deduplicate a lazy list, using a stored `HashMap`. -/
-- We choose `HashMap` here instead of `RBSet` as the initial application is `Expr`.
def dedupBy (L : MLList m α) (f : α → m β) : MLList m α :=
  ((L.liftM : MLList (StateT (Std.HashMap β Unit) m) α) >>= fun a => do
      let b ← f a
      guard !(← get).contains b
      modify fun s => s.insert b ()
      pure a)
  |>.runState' ∅

/-- Lazily deduplicate a lazy list, using a stored `HashMap`. -/
def dedup (L : MLList m β) : MLList m β :=
  L.dedupBy (fun b => pure b)

end MLList
