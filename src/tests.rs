// TODO: remove all the async stuff and mockito server. The library is pure now
// so it isn't needed.

use std::{convert::TryFrom, str::FromStr};

use super::*;
use mockito::Matcher;
use serde_json::json;

async fn http_send<Body: Into<reqwest::Body>>(
    request: http::Request<Body>,
) -> Result<http::Response<Vec<u8>>, reqwest::Error> {
    // Make the request
    let mut response = reqwest::Client::new()
        .execute(request.try_into().unwrap())
        .await?;
    // Convert to http::Response
    let mut builder = http::Response::builder()
        .status(response.status())
        .version(response.version());
    std::mem::swap(builder.headers_mut().unwrap(), response.headers_mut());
    Ok(builder
        .body(response.bytes().await.unwrap().to_vec())
        .unwrap())
}

#[tokio::test]
async fn authenticate_test_success() {
    let username = "me@example.com";
    let password = "password";
    let name = "louis-test";
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
        .match_header("accept", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(201)
        .with_body(resp_body.to_string())
        .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let secret = crate::create_api_token_response(
        http_send(crate::create_api_token_request(
            username, password, name, &config,
        ))
        .await
        .unwrap(),
    )
    .unwrap();

    assert_eq!(expected_secret, secret);
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
        .match_header("accept", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(429)
        .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::create_api_token_response(
        http_send(crate::create_api_token_request(
            username, password, name, &config,
        ))
        .await
        .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::RateLimited => (),
        result => panic!("expected RateLimited, got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn authenticate_test_bad_creds() {
    let username = "me@example.com";
    let password = "password";
    let name = "authenticate_test_3";

    let resp_body = json!({
        "message": "invalid username and password combination",
        "status": 401,
    });

    let mock = mockito::mock("POST", "/keys")
        .expect(1)
        .match_header("authorization", "Basic bWVAZXhhbXBsZS5jb206cGFzc3dvcmQ=")
        .match_header("content-type", "application/json")
        .match_header("accept", "application/json")
        .match_body(Matcher::Json(json!({
            "name": name,
            "permissions":[{ "domain": "api", "resource": "write" }]
        })))
        .with_status(401)
        .with_body(resp_body.to_string())
        .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::create_api_token_response(
        http_send(crate::create_api_token_request(
            username, password, name, &config,
        ))
        .await
        .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::InvalidCredentials => (),
        result => panic!("expected InvalidCredentials, got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn remove_docs_success() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";

    let mock = mockito::mock(
        "DELETE",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(204)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::remove_docs_response(
        http_send(crate::remove_docs_request(package, version, token, &config).unwrap())
            .await
            .unwrap(),
    )
    .unwrap();

    assert_eq!(result, ());
    mock.assert();
}

#[tokio::test]
async fn remove_docs_unknown_package_version() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib_this_does_not_exist";
    let version = "0.8.0";

    let mock = mockito::mock(
        "DELETE",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(404)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::remove_docs_response(
        http_send(crate::remove_docs_request(package, version, token, &config).unwrap())
            .await
            .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::NotFound => (),
        result => panic!("expected ApiError::NotFound got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn remove_docs_rate_limted() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";

    let mock = mockito::mock(
        "DELETE",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(429)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::remove_docs_response(
        http_send(crate::remove_docs_request(package, version, token, &config).unwrap())
            .await
            .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::RateLimited => (),
        result => panic!("expected ApiError::RateLimited got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn remove_docs_invalid_token() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";

    let mock = mockito::mock(
        "DELETE",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(401)
    .with_body(
        json!({
            "message": "invalid API key",
            "status": 401,
        })
        .to_string(),
    )
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::remove_docs_response(
        http_send(crate::remove_docs_request(package, version, token, &config).unwrap())
            .await
            .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::InvalidApiKey => (),
        result => panic!("expected ApiError::InvalidApiKey got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn remove_docs_forbidden() {
    let token = "my-api-token-here";
    let package = "jason";
    let version = "1.2.0";

    let mock = mockito::mock(
        "DELETE",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(403)
    .with_body(
        json!({
            "message": "account is not authorized for this action",
            "status": 403,
        })
        .to_string(),
    )
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::remove_docs_response(
        http_send(crate::remove_docs_request(package, version, token, &config).unwrap())
            .await
            .unwrap(),
    )
    .unwrap_err();

    match result {
        ApiError::Forbidden => (),
        result => panic!("expected ApiError::Forbidden got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn remove_docs_bad_package_name() {
    let token = "my-api-token-here";
    let package = "not valid";
    let version = "1.2.0";

    let config = Config::new();

    match crate::remove_docs_request(package, version, token, &config).unwrap_err() {
        ApiError::InvalidPackageNameFormat(p) if p == package => (),
        result => panic!("expected Err(ApiError::BadPackage), got {:?}", result),
    }
}

#[tokio::test]
async fn publish_docs_success() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let mock = mockito::mock(
        "POST",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(201)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::publish_docs_response(
        http_send(crate::publish_docs_request(package, version, tarball, token, &config).unwrap())
            .await
            .unwrap(),
    );

    match result {
        Ok(()) => (),
        result => panic!("expected Ok(()), got {:?}", result),
    }

    mock.assert()
}

#[tokio::test]
async fn publish_docs_bad_package_name() {
    let token = "my-api-token-here";
    let package = "not valid";
    let version = "1.2.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();

    match crate::publish_docs_request(package, version, tarball, token, &config).unwrap_err() {
        ApiError::InvalidPackageNameFormat(p) if p == package => (),
        result => panic!("expected Err(ApiError::BadPackage), got {:?}", result),
    }
}

#[tokio::test]
async fn publish_docs_bad_package_version() {
    let token = "my-api-token-here";
    let package = "name";
    let version = "invalid version";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();

    match crate::publish_docs_request(package, version, tarball, token, &config).unwrap_err() {
        ApiError::InvalidVersionFormat(v) if v == version => (),
        result => panic!("expected ApiError::BadPackage, got {:?}", result),
    }
}

#[tokio::test]
async fn publish_docs_not_found() {
    let token = "my-api-token-here";
    let package = "name";
    let version = "1.1.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let mock = mockito::mock(
        "POST",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(404)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::publish_docs_response(
        http_send(crate::publish_docs_request(package, version, tarball, token, &config).unwrap())
            .await
            .unwrap(),
    );

    match result {
        Err(ApiError::NotFound) => (),
        result => panic!("expected ApiError::NotFound, got {:?}", result),
    }

    mock.assert()
}

#[tokio::test]
async fn publish_docs_rate_limit() {
    let token = "my-api-token-here";
    let package = "name";
    let version = "1.1.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let mock = mockito::mock(
        "POST",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(429)
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::publish_docs_response(
        http_send(crate::publish_docs_request(package, version, tarball, token, &config).unwrap())
            .await
            .unwrap(),
    );

    match result {
        Err(ApiError::RateLimited) => (),
        result => panic!("expected ApiError::RateLimited, got {:?}", result),
    }

    mock.assert()
}

#[tokio::test]
async fn publish_docs_invalid_api_token() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let mock = mockito::mock(
        "POST",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(401)
    .with_body(
        json!({
            "message": "invalid API key",
            "status": 401,
        })
        .to_string(),
    )
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::publish_docs_response(
        http_send(crate::publish_docs_request(package, version, tarball, token, &config).unwrap())
            .await
            .unwrap(),
    );

    match result {
        Err(ApiError::InvalidApiKey) => (),
        result => panic!("expected Err(ApiError::InvalidApiKey), got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn publish_docs_forbidden() {
    let token = "my-api-token-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let mock = mockito::mock(
        "POST",
        format!("/packages/{}/releases/{}/docs", package, version).as_str(),
    )
    .expect(1)
    .match_header("authorization", token)
    .match_header("accept", "application/json")
    .with_status(403)
    .with_body(
        json!({
            "message": "account is not authorized for this action",
            "status": 403,
        })
        .to_string(),
    )
    .create();

    let mut config = Config::new();
    config.api_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let result = crate::publish_docs_response(
        http_send(crate::publish_docs_request(package, version, tarball, token, &config).unwrap())
            .await
            .unwrap(),
    );

    match result {
        Err(ApiError::Forbidden) => (),
        result => panic!("expected Err(ApiError::Forbidden), got {:?}", result),
    }

    mock.assert();
}

#[tokio::test]
async fn get_package_ok_test() {
    let response_body = std::include_bytes!("../test/package_exfmt");

    // Set up test server
    let mock = mockito::mock("GET", "/packages/exfmt")
        .expect(1)
        .with_status(200)
        .with_body(&response_body[..])
        .create();

    // Test!
    let mut config = Config::new();
    config.repository_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let package = crate::get_package_response(
        http_send(crate::get_package_request("exfmt", None, &config))
            .await
            .unwrap(),
        std::include_bytes!("../test/public_key"),
    )
    .unwrap();

    assert_eq!(
        Package {
            name: "exfmt".to_string(),
            repository: "hexpm".to_string(),
            releases: vec![
                Release {
                    version: Version::try_from("0.0.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        82, 48, 191, 145, 92, 172, 0, 108, 238, 71, 57, 23, 101, 177, 161, 83, 91,
                        182, 18, 232, 249, 225, 29, 12, 246, 5, 215, 165, 32, 57, 179, 110
                    ]
                },
                Release {
                    version: Version::try_from("0.1.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        111, 246, 240, 176, 118, 229, 12, 15, 164, 61, 186, 3, 89, 106, 153, 225,
                        247, 52, 245, 8, 216, 139, 21, 232, 200, 16, 214, 59, 241, 188, 9, 6
                    ]
                },
                Release {
                    version: Version::try_from("0.2.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        149, 9, 192, 229, 84, 162, 110, 207, 161, 43, 31, 0, 126, 168, 14, 243, 31,
                        43, 195, 238, 100, 91, 78, 100, 213, 181, 101, 154, 106, 168, 170, 107
                    ]
                },
                Release {
                    version: Version::try_from("0.2.1").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        157, 229, 28, 212, 92, 249, 14, 240, 235, 104, 31, 12, 160, 199, 83, 195,
                        154, 105, 222, 37, 221, 80, 181, 183, 113, 240, 234, 107, 144, 85, 255, 65
                    ]
                },
                Release {
                    version: Version::try_from("0.2.2").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        112, 250, 133, 189, 183, 192, 54, 218, 115, 55, 216, 97, 204, 201, 191,
                        168, 250, 133, 138, 252, 202, 240, 74, 197, 228, 235, 81, 18, 241, 7, 155,
                        38
                    ]
                },
                Release {
                    version: Version::try_from("0.2.3").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        131, 20, 29, 160, 171, 124, 7, 125, 210, 88, 17, 189, 199, 49, 191, 190,
                        14, 162, 38, 247, 52, 176, 189, 17, 7, 188, 151, 152, 24, 64, 170, 29
                    ]
                },
                Release {
                    version: Version::try_from("0.2.4").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        109, 162, 185, 169, 26, 4, 62, 60, 167, 54, 182, 161, 140, 197, 75, 113,
                        183, 117, 247, 201, 218, 228, 14, 160, 115, 157, 196, 51, 108, 16, 96, 217
                    ]
                },
                Release {
                    version: Version::try_from("0.3.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        97, 50, 95, 212, 242, 59, 245, 177, 140, 78, 79, 180, 108, 174, 119, 176,
                        24, 80, 218, 152, 178, 227, 152, 242, 32, 126, 72, 67, 222, 0, 173, 170
                    ]
                },
                Release {
                    version: Version::try_from("0.4.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        246, 178, 237, 214, 217, 158, 143, 52, 130, 186, 64, 50, 94, 175, 161, 81,
                        68, 186, 4, 73, 53, 226, 235, 144, 209, 84, 231, 136, 165, 119, 122, 126
                    ]
                },
                Release {
                    version: Version::try_from("0.5.0").unwrap(),
                    dependencies: vec![],
                    retirement_status: None,
                    outer_checksum: vec![
                        151, 86, 157, 218, 218, 131, 240, 119, 198, 216, 202, 240, 65, 17, 57, 228,
                        84, 252, 59, 207, 246, 49, 22, 21, 52, 47, 51, 139, 190, 9, 95, 109
                    ]
                }
            ],
        },
        package,
    );

    mock.assert();
}

#[tokio::test]
async fn get_package_not_found() {
    let config = Config::new();
    let error = crate::get_package_response(
        http_send(crate::get_package_request(
            "louissaysthispackagedoesnotexist",
            None,
            &config,
        ))
        .await
        .unwrap(),
        std::include_bytes!("../test/public_key"),
    )
    .unwrap_err();

    assert!(error.is_not_found());
}

#[tokio::test]
async fn get_repository_versions_ok_test() {
    let response_body = std::include_bytes!("../test/versions");

    // Set up test server
    let mock = mockito::mock("GET", "/versions")
        .expect(1)
        .with_status(200)
        .with_body(&response_body[..])
        .create();

    // Test!
    let mut config = Config::new();
    config.repository_base = http::Uri::from_str(&mockito::server_url()).unwrap();

    let versions = crate::get_repository_versions_response(
        http_send(crate::get_repository_versions_request(None, &config))
            .await
            .unwrap(),
        std::include_bytes!("../test/public_key"),
    );

    assert_eq!(
        &vec![
            Version::parse("0.0.0").unwrap(),
            Version::parse("0.1.0").unwrap(),
            Version::parse("0.2.0").unwrap(),
            Version::parse("0.2.1").unwrap(),
            Version::parse("0.2.2").unwrap(),
            Version::parse("0.2.3").unwrap(),
            Version::parse("0.2.4").unwrap(),
            Version::parse("0.3.0").unwrap(),
            Version::parse("0.4.0").unwrap(),
            Version::parse("0.5.0").unwrap(),
        ],
        versions.unwrap().get("exfmt").unwrap(),
    );

    mock.assert();
}

#[tokio::test]
async fn get_repository_tarball_ok_test() {
    let config = Config::new();
    let checksum =
        base16::decode("9107f6a859cb96945ad9a099085db028ca2bebb3c8ea42eec227b51c614cc2e0").unwrap();

    let downloaded = crate::get_package_tarball_response(
        http_send(crate::get_package_tarball_request(
            "gleam_stdlib",
            "0.14.0",
            None,
            &config,
        ))
        .await
        .unwrap(),
        &checksum,
    )
    .unwrap();

    assert_eq!(
        &downloaded,
        std::include_bytes!("../test/gleam_stdlib-0.14.0.tar")
    );
}

#[tokio::test]
async fn get_repository_tarball_bad_checksum_test() {
    let config = Config::new();
    let checksum = vec![1, 2, 3, 4, 5];

    let err = crate::get_package_tarball_response(
        http_send(crate::get_package_tarball_request(
            "gleam_stdlib",
            "0.14.0",
            None,
            &config,
        ))
        .await
        .unwrap(),
        &checksum,
    )
    .unwrap_err();

    assert_eq!(
        err.to_string(),
        "the downloaded data did not have the expected checksum"
    );
}

#[tokio::test]
async fn get_repository_tarball_not_found_test() {
    let config = Config::new();
    let checksum = vec![1, 2, 3, 4, 5];

    let err = crate::get_package_tarball_response(
        http_send(crate::get_package_tarball_request(
            "gleam_stdlib",
            "unknown-version",
            None,
            &config,
        ))
        .await
        .unwrap(),
        &checksum,
    )
    .unwrap_err();

    assert_eq!(err.to_string(), "resource was not found");
}
