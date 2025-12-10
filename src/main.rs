#[macro_use]
extern crate tracing;

use scripting::{executor::Executor, parser::Parser};
use tracing_subscriber::EnvFilter;

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let input_path = "program.txt";
    info!("Reading input script from: {}", input_path);

    let input = std::fs::read_to_string(input_path)
        .expect(&format!("Failed to read input script from: {}", input_path));
    info!("Input script read successfully.");
    debug!("Input script content:\n{}", input);

    let mut parser = Parser::new(&input);
    let module = parser.parse_module().expect(&format!(
        "Failed to parse module at position {}",
        parser.position()
    ));
    info!("Module parsed successfully.");
    debug!("Parsed module: {:#?}", module);

    let mut executor = Executor::new();
    executor.load_standard_library();
    info!("Standard library loaded.");

    info!("Starting module execution...");
    executor
        .execute_module(module)
        .expect(&format!("Failed to execute module"));
    info!("Execution completed successfully.");

    //debug!("Final executor state: {:#?}", executor);
}
