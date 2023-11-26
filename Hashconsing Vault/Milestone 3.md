

## Intro

Good Morning everyone!
I'm Karan Ahluwalia and today I'm  going to share with you the progress of my project under the guidance of Prof. Matthew Fluet. Our focus is on developing a Type-Safe Modular Hash-Consing Library in Rust and Haskell.

## HashConisng

Hash-Consing is a technique that optimizes memory usage by sharing instances of immutable values. It's a crucial strategy in programming, especially when dealing with large, complex data structures. Our goal with this project was to leverage the unique capabilities of Rust and Haskell to create a robust hash-consing library."

## Milestone

In this milestone, our primary objectives were to implement the core features of the Type-Safe Modular Hash-Consing (TSMHC) library in both languages, ensuring type safety, modularity, and efficient memory management. We also focused on comprehensive documentation and testing strategies to ensure the reliability and effectiveness of our libraries.


## Progress

Progress has been significant. We've successfully designed and implemented a hash-consing library in Rust, including both a single-threaded and a thread-safe version. This dual approach allows us to cater to different application needs, ensuring flexibility and performance optimization.


## Rust


Rust, as many of you know, emphasizes memory safety without relying on a garbage collector. This aspect is crucial in system-level programming, where performance and safety are paramount. Our library leverages Rust's strengths in handling concurrency and memory management, ensuring that our hash-consing approach is both efficient and safe.



## Implementation


In Rust, we've developed two versions of the library: a single-threaded and a multi-threaded version. The single-threaded version is straightforward, focusing on memory safety and efficient data sharing. For example, our `hashcons` function in Rust uses a combination of `Rc` (Reference Counted) and `RefCell` for mutable shared state, typical of single-threaded scenarios.



For the multi-threaded version in Rust, we utilized `Arc` (Atomic Reference Counted) and `Mutex` to safely manage shared data across threads. This is critical for applications requiring concurrent data processing, ensuring thread safety while maintaining efficient memory usage.






## Challenges

One key challenge was determining the appropriate data structures for the library. We also had to ensure that the library architecture aligns seamlessly with Rust’s strengths and capabilities





## Outcomes

Expected outcomes are promising. We anticipate that programs utilizing our hash-consing library will have a smaller memory footprint compared to those without it. For single-threaded applications, the single-threaded version of our library should offer faster performance



## Future Work

Looking ahead, our future work includes automating cleanup processes and creating custom hash maps to further enhance performance and efficiency. These steps are critical in ensuring that our library not only reduces memory usage but also boosts the overall speed of computations.


