use crate::Result;

pub fn encrypt_with_passphrase(message: &[u8], passphrase: &str) -> Result<String> {
    let passphrase = age::secrecy::SecretString::from(passphrase);
    let recipient = age::scrypt::Recipient::new(passphrase.clone());
    let encrypted = age::encrypt_and_armor(&recipient, message).unwrap();
    Ok(encrypted)
}

pub fn decrypt_with_passphrase(encrypted_message: &[u8], passphrase: &str) -> Result<String> {
    let passphrase = age::secrecy::SecretString::from(passphrase);
    let identity = age::scrypt::Identity::new(passphrase);
    let decrypted = age::decrypt(&identity, encrypted_message).unwrap();
    let decrypted = String::from_utf8(decrypted).unwrap();
    Ok(decrypted)
}
