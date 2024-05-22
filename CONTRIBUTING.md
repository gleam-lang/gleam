# How to Contribute

## Adding a New API Function

1. Figure out what you want to do
    - Go to https://hexdocs.pm/hex/Mix.Tasks.Hex.html and find what you want to do
2. Once you find the page, click on the code icon on the top-right to go to the corresponding source code like so: https://github.com/hexpm/hex/blob/main/lib/mix/tasks/hex.owner.ex#L125
```elixir
  defp transfer_owner(organization, package, owner) do
    auth = Mix.Tasks.Hex.auth_info(:write)
    Hex.Shell.info("Transferring ownership to #{owner} for #{package}")

    case Hex.API.Package.Owner.add(organization, package, owner, "full", true, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Transferring ownership failed")
        Hex.Utils.print_error_result(other)
    end
  end
```

3. The API function this is calling is `Hex.API.Package.Owner.add` so we go to https://github.com/hexpm/hex/blob/main/lib/hex/api/package.ex, scroll down to the defmodule for Owner and then find the `add` function:
```elixir
    def add(repo, package, owner, level, transfer, auth) when package != "" do
      Hex.API.check_write_api()

      owner = URI.encode_www_form(owner)
      path = "packages/#{URI.encode(package)}/owners/#{URI.encode(owner)}"
      params = %{level: level, transfer: transfer}
      API.erlang_put_request(repo, path, params, auth)
    end
```
`path` tells us what path and variables we will be using while `params` tells us what the body of the request should contain. The `API.erlang_put_request` at the bottom of this tells us the method of our request needs to be `PUT`. Now we have all of the information we need to create our request function like so:
```elixir
pub fn transfer_owner_request(
    package_name: &str,
    owner: &str,
    api_key: &str,
    config: &Config,
) -> http::Request<Vec<u8>> {
    let body = json!({
        "level": OwnerLevel::Full.to_string(),
        "transfer": true,
    });

    config
        .api_request(
            Method::PUT,
            &format!("packages/{}/owners/{}", package_name, owner),
            Some(api_key),
        )
        .body(body.to_string().into_bytes())
        .expect("transfer_owner_request request")
}
```
Note that the `api_key` and `config` fields will always be present in these request functions while the other fields are tailored to the specific request we want to make.

4. TODO: How to figure out what to write for the response function?