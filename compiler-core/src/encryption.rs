// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

pub fn encrypt_with_passphrase(
    message: &[u8],
    passphrase: &str,
) -> Result<String, age::EncryptError> {
    let passphrase = age::secrecy::SecretString::from(passphrase);
    let recipient = age::scrypt::Recipient::new(passphrase);

    let encrypted = age::encrypt_and_armor(&recipient, message)?;

    Ok(encrypted)
}

// the function `decrypt_with_passphrase` has two possible failure cases:
// - when decryption fails
// - when the data was decrypted succesfully but the result is not UTF-8 valid
#[derive(Debug)]
pub enum DecryptError {
    Decrypt(age::DecryptError),
    Io(std::string::FromUtf8Error),
}

pub fn decrypt_with_passphrase(
    encrypted_message: &[u8],
    passphrase: &str,
) -> Result<String, DecryptError> {
    let passphrase = age::secrecy::SecretString::from(passphrase);
    let identity = age::scrypt::Identity::new(passphrase);

    let decrypted = age::decrypt(&identity, encrypted_message).map_err(DecryptError::Decrypt)?;
    let decrypted = String::from_utf8(decrypted).map_err(DecryptError::Io)?;

    Ok(decrypted)
}
