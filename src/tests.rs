use super::*;
use mockito::Matcher;
use serde_json::json;

#[tokio::test]
async fn authenticate_test_success() {
    let username = "me@example.com";
    let password = "password";
    let name = "authenticate_test_1";
    let expected_secret = "some-secret-here";

    let resp_body = json!({
        "authing_key": false,
        "inserted_at": "2020-05-02T17:18:23.336328Z",
        "name": "authenticate_test_1",
        "permissions": [{"domain": "api", "resource": "write"}],
        "revoked_at": null,
        "secret": expected_secret,
        "updated_at": "2020-05-02T17: 18: 23.336328Z",
        "url": "https: //hex.pm/api/keys/authenticate_test_1"
    });

    let mock = mockito::mock("POST", "/keys")
        .expect(1)
        .match_header("authorization", "Basic bWVAZXhhbXBsZS5jb206cGFzc3dvcmQ=")
        .match_header("content-type", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(201)
        .with_body(resp_body.to_string())
        .create();

    let mut client = UnauthenticatedClient::new();
    client.api_base_url = url::Url::parse(&mockito::server_url()).unwrap();

    let authed_client = client
        .authenticate(username, password, name)
        .await
        .expect("should be ok");

    assert_eq!(expected_secret, authed_client.api_token);
    assert_eq!(
        url::Url::parse(&mockito::server_url()).unwrap(),
        authed_client.api_base_url
    );
    mock.assert();
}

#[tokio::test]
async fn authenticate_test_rate_limted() {
    let username = "me@example.com";
    let password = "password";
    let name = "authenticate_test_2";

    let mock = mockito::mock("POST", "/keys")
        .expect(1)
        .match_header("authorization", "Basic bWVAZXhhbXBsZS5jb206cGFzc3dvcmQ=")
        .match_header("content-type", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(429)
        .create();

    let mut client = UnauthenticatedClient::new();
    client.api_base_url = url::Url::parse(&mockito::server_url()).unwrap();

    match client.authenticate(username, password, name).await {
        Err(AuthenticateError::RateLimited) => (),
        result => panic!(
            "expected Err(AuthenticateError::RateLimited), got {:?}",
            result
        ),
    }

    mock.assert();
}

#[tokio::test]
async fn authenticate_test_bad_creds() {
    let username = "me@example.com";
    let password = "password";
    let name = "authenticate_test_3";

    let resp_body = json!({
        "message": "invalid usernameand password combination",
        "status": 401,
    });

    let mock = mockito::mock("POST", "/keys")
        .expect(1)
        .match_header("authorization", "Basic bWVAZXhhbXBsZS5jb206cGFzc3dvcmQ=")
        .match_header("content-type", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(401)
        .with_body(resp_body.to_string())
        .create();

    let mut client = UnauthenticatedClient::new();
    client.api_base_url = url::Url::parse(&mockito::server_url()).unwrap();

    match client.authenticate(username, password, name).await {
        Err(AuthenticateError::InvalidCredentials) => (),
        result => panic!(
            "expected Err(AuthenticateError::InvalidCredentials), got {:?}",
            result
        ),
    }

    mock.assert();
}
