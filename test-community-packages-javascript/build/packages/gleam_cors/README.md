# gleam_cors

[![Package Version](https://img.shields.io/hexpm/v/gleam_cors)](https://hex.pm/packages/gleam_cors)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_cors/)

Unofficial CORS middleware for the [`gleam_http`](https://hexdocs.pm/gleam_http/index.html) library.

## Installation

```sh
gleam add gleam_cors
```

## Usage

Use the `middleware` function to set up CORS for your application. This middleware should be
placed early in your middleware stack (late in the pipeline).

```diff
+import gleam/http/cors
+import gleam/http
 import myproject/web/middleware

 pub fn stack() {
   service
   |> middleware.rescue
   |> middleware.log
+  |> cors.middleware(
+    origins: ["http://localhost:8000"],
+    methods: [http.Get, http.Post, http.Delete],
+    headers: ["Authorization", "Content-Type"],
+  )
 }
```

## Changelog

See [CHANGELOG.md](CHANGELOG.md) in the project repository
