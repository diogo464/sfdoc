use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use sfdoc::Hook;

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Snippets(HashMap<String, Snippet>);

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Snippet {
    pub prefix: String,
    pub body: Vec<String>,
    pub description: String,
}

impl Snippets {
    pub fn add(&mut self, hook: &Hook) {
        let name = hook.name();
        let prefix = format!("hook{}", hook.name());
        let description = hook.description().to_owned();
        let mut body = Vec::new();

        let mut add_comma = false;
        let mut main_line = format!("hook.add(\"{name}\", \"${{0:{name}}}\", function(");
        for arg in hook.parameters() {
            if add_comma {
                main_line.push_str(", ");
            } else {
                add_comma = true;
            }
            main_line.push_str(&format!(
                "{} --[[@cast {} {}]]",
                arg.name(),
                arg.name(),
                arg.types()
            ));
        }
        main_line.push(')');

        body.push(main_line);
        body.push("\t$1".into());
        body.push("end)".into());

        self.0.insert(
            hook.name().to_owned(),
            Snippet {
                prefix,
                body,
                description,
            },
        );
    }
}
