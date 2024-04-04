import project

pub fn main() {
  let location = project.main()
  let assert "http://localhost:8080/" = location.href
  let assert "http://localhost:8080" = location.origin
  let assert "http:" = location.protocol
  let assert "localhost:8080" = location.host
  let assert "localhost" = location.hostname
  let assert "8080" = location.port
  let assert "/" = location.pathname
  let assert "" = location.search
  let assert "" = location.hash
}
