FROM rust:1.33

LABEL "com.github.actions.name"="Rust"
LABEL "com.github.actions.description"="Build Rust"
LABEL "com.github.actions.icon"="code"
LABEL "com.github.actions.color"="orange"

LABEL "repository"="http://github.com/lpil/gleam"
LABEL "maintainer"="Louis Pilfold <louis@lpil.uk>"

ADD bin/* /bin/

ENTRYPOINT ["/bin/sh"]
