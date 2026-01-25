use std::{convert::TryFrom, io::Cursor};

use super::*;
use serde_json::json;

fn make_response(status: u16, body: Vec<u8>) -> http::Response<Vec<u8>> {
    http::Response::builder().status(status).body(body).unwrap()
}

fn make_json_response(status: u16, body: serde_json::Value) -> http::Response<Vec<u8>> {
    make_response(status, body.to_string().into_bytes())
}

#[test]
fn authenticate_request() {
    let username = "me@example.com";
    let password = "password";
    let name = "louis-test";

    let config = Config::new();
    let request = crate::api_create_api_key_request(username, password, name, &config);

    assert_eq!(request.method(), http::Method::POST);
    assert_eq!(request.uri().path(), "/api/keys");
    assert_eq!(
        request.headers().get("authorization").unwrap(),
        "Basic bWVAZXhhbXBsZS5jb206cGFzc3dvcmQ="
    );
    assert_eq!(
        request.headers().get("content-type").unwrap(),
        "application/json"
    );
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");

    let body: serde_json::Value = serde_json::from_slice(request.body()).unwrap();
    assert_eq!(body["name"], "louis-test");
    assert_eq!(body["permissions"][0]["domain"], "api");
    assert_eq!(body["permissions"][0]["resource"], "write");
}

#[test]
fn authenticate_response_success() {
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

    let response = make_json_response(201, resp_body);
    let secret = crate::api_create_api_key_response(response).unwrap();

    assert_eq!(expected_secret, secret);
}

#[test]
fn authenticate_response_rate_limited() {
    let response = make_response(429, vec![]);
    let result = crate::api_create_api_key_response(response).unwrap_err();

    match result {
        ApiError::RateLimited => (),
        result => panic!("expected RateLimited, got {:?}", result),
    }
}

#[test]
fn authenticate_response_bad_creds() {
    let resp_body = json!({
        "message": "invalid username and password combination",
        "status": 401,
    });

    let response = make_json_response(401, resp_body);
    let result = crate::api_create_api_key_response(response).unwrap_err();

    match result {
        ApiError::InvalidCredentials => (),
        result => panic!("expected InvalidCredentials, got {:?}", result),
    }
}

#[test]
fn remove_docs_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";

    let config = Config::new();
    let request = crate::api_remove_docs_request(package, version, key, &config).unwrap();

    assert_eq!(request.method(), http::Method::DELETE);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib/releases/0.8.0/docs"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn remove_docs_response_success() {
    let response = make_response(204, vec![]);
    let result = crate::api_remove_docs_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn revert_release_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib";
    let version = "0.8.0";

    let config = Config::new();
    let request = crate::api_revert_release_request(package, version, key, &config).unwrap();

    assert_eq!(request.method(), http::Method::DELETE);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib/releases/0.8.0"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn revert_release_response_success() {
    let response = make_response(204, vec![]);
    let result = crate::api_revert_release_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn add_owner_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib";
    let owner = "lpil";
    let level = OwnerLevel::Maintainer;

    let config = Config::new();
    let request = crate::api_add_owner_request(package, owner, level, key, &config);

    assert_eq!(request.method(), http::Method::PUT);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib/owners/lpil"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");

    let body: serde_json::Value = serde_json::from_slice(request.body()).unwrap();
    assert_eq!(body["level"], "maintainer");
    assert_eq!(body["transfer"], false);
}

#[test]
fn add_owner_response_success() {
    let response = make_response(204, vec![]);
    let result = crate::api_add_owner_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn transfer_owner_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib";
    let owner = "lpil";

    let config = Config::new();
    let request = crate::api_transfer_owner_request(package, owner, key, &config);

    assert_eq!(request.method(), http::Method::PUT);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib/owners/lpil"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");

    let body: serde_json::Value = serde_json::from_slice(request.body()).unwrap();
    assert_eq!(body["level"], "full");
    assert_eq!(body["transfer"], true);
}

#[test]
fn transfer_owner_response_success() {
    let response = make_response(204, vec![]);
    let result = crate::api_transfer_owner_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn remove_owner_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib";
    let owner = "lpil";

    let config = Config::new();
    let request = crate::api_remove_owner_request(package, owner, key, &config);

    assert_eq!(request.method(), http::Method::DELETE);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib/owners/lpil"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn remove_owner_response_success() {
    let response = make_response(204, vec![]);
    let result = crate::api_remove_owner_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn remove_key_request() {
    let name = "some-key-name";
    let key = "my-api-key-here";

    let config = Config::new();
    let request = crate::api_remove_api_key_request(name, key, &config);

    assert_eq!(request.method(), http::Method::DELETE);
    assert_eq!(request.uri().path(), "/api/keys/some-key-name");
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn remove_key_response_success_204() {
    let response = make_response(204, vec![]);
    let result = crate::api_remove_api_key_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn remove_key_response_success_200() {
    let response = make_response(200, vec![]);
    let result = crate::api_remove_api_key_response(response).unwrap();
    assert_eq!(result, ());
}

#[test]
fn remove_docs_response_not_found() {
    let response = make_response(404, vec![]);
    let result = crate::api_remove_docs_response(response).unwrap_err();

    match result {
        ApiError::NotFound => (),
        result => panic!("expected ApiError::NotFound got {:?}", result),
    }
}

#[test]
fn remove_docs_response_rate_limited() {
    let response = make_response(429, vec![]);
    let result = crate::api_remove_docs_response(response).unwrap_err();

    match result {
        ApiError::RateLimited => (),
        result => panic!("expected ApiError::RateLimited got {:?}", result),
    }
}

#[test]
fn remove_docs_response_invalid_key() {
    let resp_body = json!({
        "message": "invalid API key",
        "status": 401,
    });
    let response = make_json_response(401, resp_body);
    let result = crate::api_remove_docs_response(response).unwrap_err();

    match result {
        ApiError::InvalidApiKey => (),
        result => panic!("expected ApiError::InvalidApiKey got {:?}", result),
    }
}

#[test]
fn remove_docs_response_forbidden() {
    let resp_body = json!({
        "message": "account is not authorized for this action",
        "status": 403,
    });
    let response = make_json_response(403, resp_body);
    let result = crate::api_remove_docs_response(response).unwrap_err();

    match result {
        ApiError::Forbidden => (),
        result => panic!("expected ApiError::Forbidden got {:?}", result),
    }
}

#[test]
fn remove_docs_bad_package_name() {
    let key = "my-api-key-here";
    let package = "not valid";
    let version = "1.2.0";

    let config = Config::new();

    match crate::api_remove_docs_request(package, version, key, &config).unwrap_err() {
        ApiError::InvalidPackageNameFormat(p) if p == package => (),
        result => panic!("expected Err(ApiError::BadPackage), got {:?}", result),
    }
}

#[test]
fn publish_docs_request() {
    let key = "my-api-key-here";
    let package = "gleam_experimental_stdlib_123";
    let version = "0.8.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();
    let request =
        crate::api_publish_docs_request(package, version, tarball.clone(), key, &config).unwrap();

    assert_eq!(request.method(), http::Method::POST);
    assert_eq!(
        request.uri().path(),
        "/api/packages/gleam_experimental_stdlib_123/releases/0.8.0/docs"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
    assert_eq!(
        request.headers().get("content-type").unwrap(),
        "application/x-tar"
    );
    assert_eq!(request.headers().get("content-encoding").unwrap(), "x-gzip");
    assert_eq!(request.body(), &tarball);
}

#[test]
fn publish_docs_response_success() {
    let response = make_response(201, vec![]);
    let result = crate::api_publish_docs_response(response);

    match result {
        Ok(()) => (),
        result => panic!("expected Ok(()), got {:?}", result),
    }
}

#[test]
fn publish_docs_bad_package_name() {
    let key = "my-api-key-here";
    let package = "not valid";
    let version = "1.2.0";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();

    match crate::api_publish_docs_request(package, version, tarball, key, &config).unwrap_err() {
        ApiError::InvalidPackageNameFormat(p) if p == package => (),
        result => panic!("expected Err(ApiError::BadPackage), got {:?}", result),
    }
}

#[test]
fn publish_docs_bad_package_version() {
    let key = "my-api-key-here";
    let package = "name";
    let version = "invalid version";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();

    match crate::api_publish_docs_request(package, version, tarball, key, &config).unwrap_err() {
        ApiError::InvalidVersionFormat(v) if v == version => (),
        result => panic!("expected ApiError::BadPackage, got {:?}", result),
    }
}

#[test]
fn publish_docs_response_not_found() {
    let response = make_response(404, vec![]);
    let result = crate::api_publish_docs_response(response);

    match result {
        Err(ApiError::NotFound) => (),
        result => panic!("expected ApiError::NotFound, got {:?}", result),
    }
}

#[test]
fn publish_docs_response_rate_limit() {
    let response = make_response(429, vec![]);
    let result = crate::api_publish_docs_response(response);

    match result {
        Err(ApiError::RateLimited) => (),
        result => panic!("expected ApiError::RateLimited, got {:?}", result),
    }
}

#[test]
fn publish_docs_response_invalid_api_key() {
    let resp_body = json!({
        "message": "invalid API key",
        "status": 401,
    });
    let response = make_json_response(401, resp_body);
    let result = crate::api_publish_docs_response(response);

    match result {
        Err(ApiError::InvalidApiKey) => (),
        result => panic!("expected Err(ApiError::InvalidApiKey), got {:?}", result),
    }
}

#[test]
fn publish_docs_response_forbidden() {
    let resp_body = json!({
        "message": "account is not authorized for this action",
        "status": 403,
    });
    let response = make_json_response(403, resp_body);
    let result = crate::api_publish_docs_response(response);

    match result {
        Err(ApiError::Forbidden) => (),
        result => panic!("expected Err(ApiError::Forbidden), got {:?}", result),
    }
}

fn expected_package_exfmt() -> Package {
    Package {
        name: "exfmt".to_string(),
        repository: "hexpm".to_string(),
        releases: vec![
            Release {
                version: Version::try_from("0.0.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    82, 48, 191, 145, 92, 172, 0, 108, 238, 71, 57, 23, 101, 177, 161, 83, 91, 182,
                    18, 232, 249, 225, 29, 12, 246, 5, 215, 165, 32, 57, 179, 110,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.1.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    111, 246, 240, 176, 118, 229, 12, 15, 164, 61, 186, 3, 89, 106, 153, 225, 247,
                    52, 245, 8, 216, 139, 21, 232, 200, 16, 214, 59, 241, 188, 9, 6,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.2.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    149, 9, 192, 229, 84, 162, 110, 207, 161, 43, 31, 0, 126, 168, 14, 243, 31, 43,
                    195, 238, 100, 91, 78, 100, 213, 181, 101, 154, 106, 168, 170, 107,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.2.1").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    157, 229, 28, 212, 92, 249, 14, 240, 235, 104, 31, 12, 160, 199, 83, 195, 154,
                    105, 222, 37, 221, 80, 181, 183, 113, 240, 234, 107, 144, 85, 255, 65,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.2.2").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    112, 250, 133, 189, 183, 192, 54, 218, 115, 55, 216, 97, 204, 201, 191, 168,
                    250, 133, 138, 252, 202, 240, 74, 197, 228, 235, 81, 18, 241, 7, 155, 38,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.2.3").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    131, 20, 29, 160, 171, 124, 7, 125, 210, 88, 17, 189, 199, 49, 191, 190, 14,
                    162, 38, 247, 52, 176, 189, 17, 7, 188, 151, 152, 24, 64, 170, 29,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.2.4").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    109, 162, 185, 169, 26, 4, 62, 60, 167, 54, 182, 161, 140, 197, 75, 113, 183,
                    117, 247, 201, 218, 228, 14, 160, 115, 157, 196, 51, 108, 16, 96, 217,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.3.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    97, 50, 95, 212, 242, 59, 245, 177, 140, 78, 79, 180, 108, 174, 119, 176, 24,
                    80, 218, 152, 178, 227, 152, 242, 32, 126, 72, 67, 222, 0, 173, 170,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.4.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    246, 178, 237, 214, 217, 158, 143, 52, 130, 186, 64, 50, 94, 175, 161, 81, 68,
                    186, 4, 73, 53, 226, 235, 144, 209, 84, 231, 136, 165, 119, 122, 126,
                ],
                meta: (),
            },
            Release {
                version: Version::try_from("0.5.0").unwrap(),
                requirements: [].into(),
                retirement_status: None,
                outer_checksum: vec![
                    151, 86, 157, 218, 218, 131, 240, 119, 198, 216, 202, 240, 65, 17, 57, 228, 84,
                    252, 59, 207, 246, 49, 22, 21, 52, 47, 51, 139, 190, 9, 95, 109,
                ],
                meta: (),
            },
        ],
    }
}

#[test]
fn get_package_request() {
    let config = Config::new();
    let request = crate::repository_v2_get_package_request("exfmt", None, &config);

    assert_eq!(request.method(), http::Method::GET);
    assert_eq!(request.uri().path(), "/packages/exfmt");
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn get_package_response_ok() {
    let response_body = std::include_bytes!("../test/package_exfmt");
    let response = make_response(200, response_body.to_vec());

    let package = crate::repository_v2_get_package_response(
        response,
        std::include_bytes!("../test/public_key"),
    )
    .unwrap();

    assert_eq!(expected_package_exfmt(), package);
}

#[test]
fn get_package_response_not_found() {
    let response = make_response(404, vec![]);
    let error = crate::repository_v2_get_package_response(
        response,
        std::include_bytes!("../test/public_key"),
    )
    .unwrap_err();

    assert!(error.is_not_found());
}

#[test]
fn get_package_from_bytes_ok() {
    let response_body = std::include_bytes!("../test/package_exfmt");
    let mut uncompressed = Vec::new();
    let mut decoder = GzDecoder::new(Cursor::new(response_body));
    let _ = decoder
        .read_to_end(&mut uncompressed)
        .expect("failed to decompress body");

    let package = crate::repository_v2_package_parse_body(
        &uncompressed,
        std::include_bytes!("../test/public_key"),
    )
    .expect("package failed to parse");

    assert_eq!(expected_package_exfmt(), package);
}

#[test]
fn get_package_from_bytes_malformed() {
    // public key should not be a valid protobuf and should therefore fail
    let bytes = std::include_bytes!("../test/public_key").to_vec();
    let package_error = crate::repository_v2_package_parse_body(&bytes, &bytes)
        .expect_err("parsing failed to fail");

    assert!(package_error.is_invalid_protobuf());
}

#[test]
fn get_repository_versions_request() {
    let config = Config::new();
    let request = crate::repository_v2_get_versions_request(None, &config);

    assert_eq!(request.method(), http::Method::GET);
    assert_eq!(request.uri().path(), "/versions");
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn get_repository_versions_response_ok() {
    let response_body = std::include_bytes!("../test/versions");
    let response = make_response(200, response_body.to_vec());

    let versions = crate::repository_v2_get_versions_response(
        response,
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
}

#[test]
fn get_repository_versions_from_bytes_ok() {
    let response_body = std::include_bytes!("../test/versions");
    let mut uncompressed = Vec::new();
    let mut decoder = GzDecoder::new(Cursor::new(response_body));
    let _ = decoder
        .read_to_end(&mut uncompressed)
        .expect("failed to decompress body");

    let versions = crate::repository_v2_get_versions_body(
        &uncompressed,
        std::include_bytes!("../test/public_key"),
    )
    .expect("versions failed to parse");

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
        versions.get("exfmt").unwrap(),
    );
}

#[test]
fn get_repository_versions_from_bytes_malformed() {
    // public key should not be a valid protobuf and should therefore fail
    let bytes = std::include_bytes!("../test/public_key").to_vec();
    let versions_error =
        crate::repository_v2_get_versions_body(&bytes, &bytes).expect_err("parsing failed to fail");

    assert!(versions_error.is_invalid_protobuf());
}

#[test]
fn get_repository_tarball_request() {
    let config = Config::new();
    let request =
        crate::repository_get_package_tarball_request("gleam_stdlib", "0.14.0", None, &config);

    assert_eq!(request.method(), http::Method::GET);
    assert_eq!(request.uri().path(), "/tarballs/gleam_stdlib-0.14.0.tar");
    assert_eq!(
        request.headers().get("accept").unwrap(),
        "application/x-tar"
    );
}

#[test]
fn get_repository_tarball_response_ok() {
    let tarball_bytes = std::include_bytes!("../test/gleam_stdlib-0.14.0.tar");
    let checksum =
        base16::decode("9107f6a859cb96945ad9a099085db028ca2bebb3c8ea42eec227b51c614cc2e0").unwrap();

    let response = make_response(200, tarball_bytes.to_vec());
    let downloaded = crate::repository_get_package_tarball_response(response, &checksum).unwrap();

    assert_eq!(&downloaded, tarball_bytes);
}

#[test]
fn get_repository_tarball_response_bad_checksum() {
    let tarball_bytes = std::include_bytes!("../test/gleam_stdlib-0.14.0.tar");
    let checksum = vec![1, 2, 3, 4, 5];

    let response = make_response(200, tarball_bytes.to_vec());
    let err = crate::repository_get_package_tarball_response(response, &checksum).unwrap_err();

    assert_eq!(
        err.to_string(),
        "the downloaded data did not have the expected checksum"
    );
}

#[test]
fn get_repository_tarball_response_not_found() {
    let checksum = vec![1, 2, 3, 4, 5];

    let response = make_response(404, vec![]);
    let err = crate::repository_get_package_tarball_response(response, &checksum).unwrap_err();

    assert_eq!(err.to_string(), "resource was not found");
}

#[test]
fn publish_package_request() {
    let key = "my-api-key-here";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();
    let request = crate::api_publish_package_request(tarball.clone(), key, &config, false);

    assert_eq!(request.method(), http::Method::POST);
    assert_eq!(
        request.uri().path_and_query().unwrap(),
        "/api/publish?replace=false"
    );
    assert_eq!(request.headers().get("authorization").unwrap(), key);
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
    assert_eq!(
        request.headers().get("content-type").unwrap(),
        "application/x-tar"
    );
    assert_eq!(request.body(), &tarball);
}

#[test]
fn publish_package_request_replace() {
    let key = "my-api-key-here";
    let tarball = std::include_bytes!("../test/example.tar.gz").to_vec();

    let config = Config::new();
    let request = crate::api_publish_package_request(tarball.clone(), key, &config, true);

    assert_eq!(
        request.uri().path_and_query().unwrap(),
        "/api/publish?replace=true"
    );
}

#[test]
fn publish_package_response_success() {
    let response = make_response(201, vec![]);
    let result = crate::api_publish_package_response(response);

    match result {
        Ok(()) => (),
        result => panic!("expected Ok(()), got {:?}", result),
    }
}

#[test]
fn modify_package_late() {
    let resp_body = json!({
        "errors": {"inserted_at": "can only modify a release up to one hour after publication"},
        "message": "Validation error(s)",
        "status": 422,
    });
    let response = make_json_response(422, resp_body);
    let result = crate::api_publish_package_response(response);

    match result {
        Err(ApiError::LateModification) => (),
        result => panic!("expected Err(ApiError::LateModification), got {:?}", result),
    }
}

#[test]
fn not_replacing() {
    let resp_body = json!({
        "errors": {"inserted_at": "must include the --replace flag to update an existing release"},
        "message": "Validation error(s)",
        "status": 422,
    });
    let response = make_json_response(422, resp_body);
    let result = crate::api_publish_package_response(response);

    match result {
        Err(ApiError::NotReplacing) => (),
        result => panic!("expected Err(ApiError::NotReplacing), got {:?}", result),
    }
}

#[test]
fn get_package_release_request() {
    let config = Config::new();
    let request = crate::api_get_package_release_request("clint", "0.0.1", None, &config);

    assert_eq!(request.method(), http::Method::GET);
    assert_eq!(request.uri().path(), "/api/packages/clint/releases/0.0.1");
    assert_eq!(request.headers().get("accept").unwrap(), "application/json");
}

#[test]
fn get_package_release_response_not_found() {
    let response = make_response(404, vec![]);
    let error = crate::api_get_package_release_response(response).unwrap_err();

    assert!(error.is_not_found());
}

#[test]
fn get_package_release_response_ok() {
    let resp_body = json!({
        "version": "0.0.1",
        "checksum": "41C6781B5F4B986BCE14C3578D39C497BCB8427F1D36D8CDE5FCAA6E03CAE2B1",
        "requirements": {
            "plug": {
                "requirement": "~>0.11.0",
                "optional": false,
                "app": "plug"
            },
            "cowboy": {
                "requirement": "~>1.0.0",
                "optional": false,
                "app": "cowboy"
            }
        },
        "meta": {
            "app": "clint",
            "build_tools": ["mix"]
        }
    });
    let response = make_json_response(200, resp_body);
    let resp = crate::api_get_package_release_response(response).unwrap();

    assert_eq!(
        resp,
        Release {
            version: Version::new(0, 0, 1),
            requirements: [
                (
                    "plug".into(),
                    Dependency {
                        requirement: Range::new("~>0.11.0".into()).unwrap(),
                        optional: false,
                        app: Some("plug".into()),
                        repository: None
                    }
                ),
                (
                    "cowboy".into(),
                    Dependency {
                        requirement: Range::new("~>1.0.0".into()).unwrap(),
                        optional: false,
                        app: Some("cowboy".into()),
                        repository: None
                    }
                )
            ]
            .into(),
            retirement_status: None,
            outer_checksum: vec![
                65, 198, 120, 27, 95, 75, 152, 107, 206, 20, 195, 87, 141, 57, 196, 151, 188, 184,
                66, 127, 29, 54, 216, 205, 229, 252, 170, 110, 3, 202, 226, 177
            ],
            meta: ReleaseMeta {
                app: "clint".into(),
                build_tools: vec!["mix".into()]
            }
        }
    )
}

#[test]
fn make_request_base_trailing_slash_is_optional() {
    let slash = http::Uri::from_static("http://host/path/");
    let no_slash = http::Uri::from_static("http://host/path");
    let suffix = "suffix";
    let expect = "/path/suffix";

    let slash = make_request(slash, http::Method::GET, suffix, None);
    assert_eq!(slash.uri_ref().unwrap().path(), expect);

    let no_slash = make_request(no_slash, http::Method::GET, suffix, None);
    assert_eq!(no_slash.uri_ref().unwrap().path(), expect);
}
