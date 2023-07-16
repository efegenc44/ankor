use std::{ops::Range, collections::HashMap};

use crate::{span::Span, error::Error};

pub struct Reporter {
    pub lineses: HashMap<String, String>
}

impl Reporter {
    pub fn new() -> Self {
        Self { lineses: HashMap::new() }
    }
    
    #[inline]
    fn print_multiple(s: &str, range: Range<usize>) {
        for _ in range {
            eprint!("{s}");
        }
    }

    pub fn report_file_error(error: std::io::Error, path: &str) {
        eprintln!("\n  Error | {error}"); 
        eprintln!("        |");
        eprintln!("   File | {path}\n");
        std::process::exit(1);
    }

    pub fn report(&mut self, source_name: &str, err: Error, stage: &str) {
        let lines = match self.lineses.get(source_name) {
            Some(lines) => lines.lines(),
            None => {
                dbg!(source_name);
                let file = std::fs::read_to_string(source_name).expect("Error reading a file.");
                self.lineses.insert(source_name.to_string(), file);
                self.lineses.get(source_name).unwrap().lines()
            },
        };
        
        let Span { line_start, line_end, start, end } = err.span;
        
        eprintln!("\n  Error | [{source_name}:{line_start}:{start}] (at {stage})"); 
        eprintln!("        |");
        
        let fline = lines.clone().nth(line_start - 1).unwrap();
        eprintln!("   {line_start:>4} | {fline}"); 
        eprint!  ("        | ");
        
        Self::print_multiple(" ", 1..start);
        
        if line_end == line_start {
            Self::print_multiple("^", start..end);
            eprintln!();
            eprintln!("        | {msg}\n", msg = err.msg); 

            if let Some((err, source)) = err.origin {
                eprintln!("      ! | Originates from");
                self.report(&source, *err, stage)
            }
    
            return
        }
        
        // First Line
        Self::print_multiple("^", start..fline.len()+1);
        eprintln!();

        // Middle Lines if there are
        for line_no2 in line_start+1..line_end {
            let line = lines.clone().nth(line_no2 - 1).unwrap();
            eprintln!("   {line_no2:>4} | {line}");
            eprint!  ("        | ");
            Self::print_multiple("^", 1..line.len()+1);
            eprintln!();
        }
        
        // Last Line
        let lline = lines.clone().nth(line_end - 1).unwrap();
        eprintln!("   {line_end:>4} | {lline}"); 
        eprint!  ("        | ");
        Self::print_multiple("^", 1..end);
        eprintln!();
        eprintln!("        | {msg}\n", msg = err.msg);

        if let Some((err, source)) = err.origin {
            eprintln!("      ! | Originates from");
            self.report(&source, *err, stage)
        }
    }
}