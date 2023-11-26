#![allow(warnings)]
mod ast;
mod asthc;
mod bdd;
mod parser;

use ast::{parse_bool_expr, BoolExpr};
use asthc::{parse_hc_bool_expr, BoolExprHc};
use hash_cons::{Hc, HcTable};
use rand::Rng;
use std::thread;

use stats_alloc::{Region, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;
use std::fs::File;
use std::rc::Rc;
use std::time::Instant;

use std::io::{self, BufRead, BufReader, Write};
#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

fn main() {
    println!("Thread_safe Auto-Cleanup disabled Hash-Consing Benchmarks\n");
    let input1 = "out1.txt";
    println!("1% unique");
    bench_unique_thread_safe(input1);
    bench_unique_hc(input1);
    bench_unique(input1);

    let input33 = "out33.txt";
    println!("\n\n33% unique\n");
    bench_unique_thread_safe(input33);
    bench_unique_hc(input33);
    bench_unique(input33);

    let input50 = "out50.txt";
    println!("\n\n50% unique\n");
    bench_unique_thread_safe(input50);
    bench_unique_hc(input50);
    bench_unique(input50);

    let input67 = "out67.txt";
    println!("\n\n67% unique\n");
    bench_unique_thread_safe(input67);
    bench_unique_hc(input67);
    bench_unique(input67);
}

fn bench_unique(input_file_path: &str) {
    //For Non-Hash-Consed BoolExpr
    let mut my_vec: Vec<BoolExpr> = Vec::new();

    // Open the input file in read-only mode
    let input_file = File::open(input_file_path).unwrap();
    let reader = BufReader::new(input_file);

    // initialize region(for memory allocation testing)
    let reg = Region::new(&GLOBAL);

    // start timer
    let start = Instant::now();

    // non-hashconsed boolexpr
    for line in reader.lines() {
        let k = parse_bool_expr(&line.unwrap()).unwrap();
        my_vec.push(k);
    }
    //  end timer
    let duration = start.elapsed();

    // get memory allocation stats
    let stats = reg.change();

    // print results
    println!("\nPrinting for Non-Hash-Consed BoolExpr\n");
    println!("Time taken: {:?}", duration);
    println!("Bytes allocated: {}", stats.bytes_allocated);
    println!("Bytes deallocated: {}", stats.bytes_deallocated);
    println!("Bytes reallocated: {}", stats.bytes_reallocated);
    println!(
        "Bytes utilized: {}",
        stats.bytes_allocated - stats.bytes_deallocated
    );
    println!("vector length: {}", my_vec.len());
}

fn bench_unique_hc(input_file_path: &str) {
    let mut my_hc_vec: Vec<BoolExprHc> = Vec::new();

    // Open the input file in read-only mode
    let input_file = File::open(input_file_path).unwrap();
    let reader = BufReader::new(input_file);

    // iniitalize hashcons table
    let table: HcTable<BoolExprHc> = HcTable::new();

    // initialize region(for memory allocation testing)
    let reg = Region::new(&GLOBAL);

    // start timer
    let start = Instant::now();

    // hashconsed boolexpr
    for line in reader.lines() {
        let k = parse_hc_bool_expr(&line.unwrap(), table.clone()).unwrap();
        my_hc_vec.push(k);
    }

    // end timer
    let duration = start.elapsed();

    // get memory allocation stats
    let stats = reg.change();

    // print results
    println!("\nPrinting for Hash-Consed BoolExpr\n");
    println!("Time taken: {:?}", duration);
    println!("Bytes allocated: {}", stats.bytes_allocated);
    println!("Bytes deallocated: {}", stats.bytes_deallocated);
    println!("Bytes reallocated: {}", stats.bytes_reallocated);
    println!(
        "Bytes utilized: {}",
        stats.bytes_allocated - stats.bytes_deallocated
    );
    println!("vector length: {}", my_hc_vec.len());
    println!("table length: {}", table.len());
}

fn bench_unique_thread_safe(input_file_path: &str) {
    let mut my_hc_vec: Vec<BoolExprHc> = Vec::new();

    // Open the input file in read-only mode
    let input_file = File::open(input_file_path).unwrap();
    let reader = BufReader::new(input_file);

    // iniitalize hashcons table
    let table_original: HcTable<BoolExprHc> = HcTable::new();

    // initialize region(for memory allocation testing)
    let reg = Region::new(&GLOBAL);

    // start timer
    let start = Instant::now();

    let table_clone = table_original.clone();
    // hashconsed boolexpr
    for line in reader.lines() {
        let table = table_clone.clone();
        let thread_handle_hc =
            thread::spawn(move || parse_hc_bool_expr(&line.unwrap(), table.clone()).unwrap());
        let hc_val = thread_handle_hc
            .join()
            .expect("Thread should finish and return  `BoolExprHc` without panicking");

        my_hc_vec.push(hc_val);
    }

    // end timer
    let duration = start.elapsed();

    // get memory allocation stats
    let stats = reg.change();

    // print results
    println!("\nPrinting for multi-threaded inserted Hash-Consed BoolExpr\n");
    println!("Time taken: {:?}", duration);
    println!("Bytes allocated: {}", stats.bytes_allocated);
    println!("Bytes deallocated: {}", stats.bytes_deallocated);
    println!("Bytes reallocated: {}", stats.bytes_reallocated);
    println!(
        "Bytes utilized: {}",
        stats.bytes_allocated - stats.bytes_deallocated
    );
    println!("vector length: {}", my_hc_vec.len());
    println!("table length: {}", table_original.len());
}
