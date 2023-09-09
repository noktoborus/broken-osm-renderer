use crate::geodata::importer::ParsedTags;
use crate::mapcss::parser::{
    BinaryNumericTestType, BinaryStringTestType, ObjectType, PropertyValue, Rule, Selector, Test, UnaryTestType,
};

struct Filter {
    selectors: Vec<Selector>,
    properties: Vec<String>,
}

pub struct Filterer {
    filters: Vec<Filter>,
}

fn match_by_tag(key: &String, tag_val: &String, test: &Test) -> bool {
    let is_true_value = |x| x == "yes" || x == "true" || x == "1";
    let is_false_value = |x| x == "no" || x == "false" || x == "0";

    match *test {
        Test::Unary {
            ref tag_name,
            ref test_type,
        } => {
            if tag_name != key {
                return false;
            };
            match *test_type {
                UnaryTestType::Exists => true,
                UnaryTestType::NotExists => false,
                UnaryTestType::True => is_true_value(tag_val),
                UnaryTestType::False => is_false_value(tag_val),
            }
        }
        Test::BinaryStringCompare {
            ref tag_name,
            ref value,
            ref test_type,
        } => {
            if tag_name != key {
                return false;
            };
            match *test_type {
                BinaryStringTestType::Equal => tag_val == value,
                BinaryStringTestType::NotEqual => tag_val != value,
            }
        }
        Test::BinaryNumericCompare {
            ref tag_name,
            ref value,
            ref test_type,
        } => {
            if tag_name != key {
                return false;
            };
            let tag_floatval = match str::parse::<f64>(tag_val) {
                Err(_) => return false,
                Ok(val) => val,
            };

            match *test_type {
                BinaryNumericTestType::Less => tag_floatval < *value,
                BinaryNumericTestType::LessOrEqual => tag_floatval <= *value,
                BinaryNumericTestType::Greater => tag_floatval > *value,
                BinaryNumericTestType::GreaterOrEqual => tag_floatval >= *value,
            }
        }
    }
}

impl Filterer {
    pub fn new(rules: Vec<Rule>) -> Filterer {
        let mut filters: Vec<Filter> = Vec::new();

        for rule in rules {
            let mut selectors: Vec<Selector> = Vec::new();
            let mut properties: Vec<String> = Vec::new();

            for sel in rule.selectors {
                if sel.tests.is_empty() {
                    continue;
                }

                selectors.push(sel);
            }
            for propval in rule.properties.iter().map(|prop| &prop.value) {
                match *propval {
                    PropertyValue::Identifier(ref id) => {
                        properties.push(id.to_string());
                    }
                    _ => {}
                }
            }

            filters.push(Filter { selectors, properties });
        }

        Filterer { filters }
    }

    pub fn dump_selectors(&self) {
        println!("Filters:");
        for filter in self.filters.iter() {
            filter.selectors.iter().for_each(|selector| {
                println!(
                    "{}{} -> {}",
                    selector.object_type,
                    selector
                        .tests
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<_>>()
                        .join(""),
                    filter.properties.join(", ")
                )
            });
        }
    }

    pub fn filter_tags(&self, object_types: Vec<ObjectType>, tags: &ParsedTags) -> ParsedTags {
        let mut filtered = ParsedTags::new();

        for filter in self.filters.iter() {
            let mut filter_selected = ParsedTags::new();

            for selector in filter
                .selectors
                .iter()
                .filter(|selector| object_types.contains(&selector.object_type))
            {
                for (tag_key, tag_val) in tags {
                    if selector.tests.iter().all(|test| match_by_tag(tag_key, tag_val, test)) {
                        filter_selected.insert(tag_key.to_string(), tag_val.to_string());
                    }
                }
            }

            if !filter_selected.is_empty() {
                for (k, v) in tags.iter().filter(|(tag_key, _)| filter.properties.contains(tag_key)) {
                    filter_selected.insert(k.to_string(), v.to_string());
                }
            }

            for (k, v) in filter_selected {
                if !filtered.contains_key(&k) {
                    filtered.insert(k, v);
                }
            }
        }
        return filtered;
    }
}
