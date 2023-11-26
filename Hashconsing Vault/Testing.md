
###  Test Case 1: Basic Interning (Done)
- **Objective:** Ensure that identical values are interned correctly.
- **Test Steps:**
  1. Intern a value.
  2. Intern the same value again.
  3. Assert that the two interned values have the same memory address.

### Test Case 2: Cleanup Effectiveness (Done)
- **Objective:** Test if the cleanup method effectively removes unreferenced entries.
- **Test Steps:**
  1. Intern several values.
  2. Drop all external references to some of the interned values.
  3. Call `cleanup` method on the table.
  4. Assert that the size of the table has decreased appropriately.

### Test Case 3: Drop Behavior (Done)
- **Objective:** Ensure values are removed from the table when they are dropped.
- **Test Steps:**
  1. Intern a value and keep a reference to it.
  2. Drop the reference to the interned value.
  3. Assert that the value is no longer in the table.

### Test Case 4: Hash Collisions (Working) -Use a bad hasher
- **Objective:** Ensure the library handles hash collisions gracefully.
- **Test Steps:**
  1. Intern two distinct values that intentionally collide in hash space.
  2. Assert that both values are retained correctly and distinguishable.

### Test Case 5: Memory Usage (Done)
- **Objective:** Evaluate the memory efficiency of hash consing.
- **Test Steps:**
  1. Generate a large amount of data with repeated values.
  2. Intern all data within the library.
  3. Measure the memory usage.
  4. Compare against memory usage when not using the hash consing library.

### Test Case 6: Stress Test (Done)
- **Objective:** Ensure stability under heavy usage.
- **Test Steps:**
  1. Intern a large number of values in a loop.
  2. Periodically call `cleanup`.
  3. Assert no panics occur, and the table remains consistent.

### Test Case 7: Multithread Safety (Done)
- **Objective:** Ensure no undefined behavior in a multithreaded context.
- **Test Steps:**
  1. Attempt to use the library across multiple threads
  2. Assert the behavior is as expected or a proper error is thrown.

### Test Case 8: Re-entrant Drop (Done)
- **Objective:** Ensure that the `Drop` implementation does not cause undefined behavior due to re-entrant calls.
- **Test Steps:**
  1. Create a structure that panics on drop.
  2. Intern the structure.
  3. Trigger a drop and assert that no re-entrant behavior occurs.


### Test Case 9: Large Scale Uniqueness Test (Done)
- **Objective:** Ensure unique interning at a large scale.
- **Test Steps:**
  1. Intern a large number of unique values.
  2. Assert that each value retrieved from the table is unique and correct.

