import gleam/option.{Option}
import gleam/list
import gleam/string_builder.{StringBuilder}

pub const encoding = "
<meta charset=\"utf-8\" />
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
"

pub type Document {
  Document(
    doctype: Option(String),
    html_attrs: StringBuilder,
    body_attrs: StringBuilder,
    head: StringBuilder,
    body: StringBuilder,
    scripts: List(String),
  )
}

pub fn new() {
  Document(
    doctype: option.None,
    html_attrs: string_builder.new(),
    body_attrs: string_builder.new(),
    head: string_builder.new(),
    body: string_builder.new(),
    scripts: [],
  )
}

pub fn merge(self: Document, new: Document) -> Document {
  Document(
    // Overwrite the doctype with a newer one, unless the newer one is `None`
    doctype: option.or(new.doctype, self.doctype),
    html_attrs: string_builder.append_builder(self.html_attrs, new.html_attrs),
    body_attrs: string_builder.append_builder(self.body_attrs, new.body_attrs),
    head: string_builder.append_builder(self.head, new.head),
    body: string_builder.append_builder(self.body, new.body),
    scripts: list.append(self.scripts, new.scripts),
  )
}

pub fn concat(docs: List(Document)) -> Document {
  docs
  |> list.fold(new(), merge)
}

pub fn from_doctype(doctype: String) -> Document {
  Document(..new(), doctype: option.Some(doctype))
}

pub fn append_html_attrs(self: Document, html_attrs: StringBuilder) -> Document {
  Document(
    ..self,
    html_attrs: string_builder.append_builder(self.html_attrs, html_attrs),
  )
}

pub fn append_body_attrs(self: Document, body_attrs: StringBuilder) -> Document {
  Document(
    ..self,
    body_attrs: string_builder.append_builder(self.body_attrs, body_attrs),
  )
}

pub fn from_head(head: StringBuilder) -> Document {
  Document(..new(), head: head)
}

pub fn append_head(self: Document, head: StringBuilder) -> Document {
  Document(..self, head: string_builder.append_builder(self.head, head))
}

pub fn from_body(body: StringBuilder) -> Document {
  Document(..new(), body: body)
}

pub fn append_body(self: Document, body: StringBuilder) -> Document {
  Document(..self, body: string_builder.append_builder(self.body, body))
}

pub fn replace_body(self: Document, body: StringBuilder) -> Document {
  Document(..self, body: body)
}

pub fn from_script(script: String) -> Document {
  Document(..new(), scripts: [script])
}

pub fn into_head(state: Document) -> Document {
  Document(
    ..state,
    head: string_builder.append_builder(state.head, state.body),
    body: string_builder.new(),
  )
}
