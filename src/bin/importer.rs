use anyhow::{anyhow, Context, Result};
use renderer::mapcss::filterer::Filterer;
use renderer::mapcss::parser::parse_file;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use tini::Ini;

fn import(input: &Path, tmp_output: &Path, output: &Path, filterer: Option<&Filterer>) -> Result<()> {
    println!("Importing OSM data from {}", input.to_string_lossy());
    renderer::geodata::importer::import(input, tmp_output, filterer)?;
    fs::rename(tmp_output, output)?;

    Ok(())
}

fn get_value_from_config(config: &Ini, section: &str, name: &str) -> String {
    match config.get(section, name) {
        Some(value) => value,
        _ => {
            eprintln!("Property {} is missing in section [{}]", name, section);
            std::process::exit(1);
        }
    }
}

fn split_stylesheet_path(file_path: &str) -> Result<(PathBuf, String)> {
    let mut result = PathBuf::from(file_path);
    let file_name = result
        .file_name()
        .and_then(|x| x.to_str().map(ToString::to_string))
        .ok_or_else(|| anyhow!("Failed to extract the file name for {}", file_path))?;
    result.pop();
    Ok((result, file_name))
}

fn load_filter(config_path: &PathBuf) -> Result<Filterer> {
    let config = match Ini::from_file(config_path) {
        Ok(config) => config,
        Err(err) => {
            eprintln!("Failed to parse config from {}: {}", config_path.display(), err);
            std::process::exit(1);
        }
    };

    let style_section = "style";
    let stylesheet_file = get_value_from_config(&config, style_section, "file");
    let (base_path, file_name) = split_stylesheet_path(&stylesheet_file)?;
    let rules = parse_file(&base_path, &file_name).context("Failed to parse the stylesheet file")?;

    Ok(Filterer::new(rules))
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut filterer: Option<Filterer> = None;

    if args.len() != 3 && args.len() != 4 {
        let bin_name = args.first().map(String::as_str).unwrap_or("importer");
        eprintln!("Usage: {} INPUT OUTPUT [CONFIG]", bin_name);
        std::process::exit(1);
    }

    let input = PathBuf::from(&args[1]);
    let output = PathBuf::from(&args[2]);

    if args.len() == 4 {
        let config_path = PathBuf::from(&args[3]);
        filterer = load_filter(&config_path).ok();

        filterer.as_ref().expect("Filed to load mapcss filters").dump_selectors();
    }

    let mut tmp_output = output.clone();
    tmp_output.set_extension("tmp");

    match import(&input, &tmp_output, &output, filterer.as_ref()) {
        Ok(_) => println!("Successfully imported OSM data to {}", output.to_string_lossy()),
        Err(err) => {
            // Make a best-effort attempt to remove the unfinished mess
            // we may have potentially left behind, deliberately ignoring
            // the error.
            let _ = fs::remove_file(tmp_output);

            for cause in err.chain() {
                eprintln!("{}", cause);
            }
            std::process::exit(1);
        }
    }
}
