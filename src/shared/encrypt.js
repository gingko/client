const config = require("../../config.js");

let publicKey;
const algorithm = {
  name: "RSA-OAEP",
  hash: "SHA-256",
}

async function importPublicKey(jwk) {
  return window.crypto.subtle.importKey(
    "jwk",
    jwk,
    algorithm,
    true,
    ["encrypt"]
  );
}

function arrayBufferToBase64String(buffer) {
  let binary = "";
  const bytes = new Uint8Array(buffer);
  const len = bytes.byteLength;
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return window.btoa(binary);
}

setTimeout(async () => {
  publicKey = await importPublicKey(config.PUBLIC_KEY);
  //console.log("public key imported", publicKey);
}, 8000);

module.exports = {
  encrypt: async function (plaintext) {
    if (!publicKey) {
      publicKey = await importPublicKey(config.PUBLIC_KEY);
    }

    // Generate AES key
    const aesKey = await window.crypto.subtle.generateKey(
      {
        name: "AES-GCM",
        length: 256,
      },
      true,
      ["encrypt", "decrypt"]
    );
    const aesKeyExported = await window.crypto.subtle.exportKey(
      "jwk",
      aesKey
    );
    const aesKeyBuffer = new TextEncoder().encode(JSON.stringify(aesKeyExported));
    const encryptedAesKey = await crypto.subtle.encrypt(
      {
        name: "RSA-OAEP",
        hash: "SHA-256",
      },
      publicKey,
      aesKeyBuffer
    );
    const encryptedAesArray = new Uint8Array(encryptedAesKey);
    const encryptedAesBase64 = arrayBufferToBase64String(encryptedAesArray);

    // Encrypt plaintext
    const iv = window.crypto.getRandomValues(new Uint8Array(12));
    const encrypted = await window.crypto.subtle.encrypt(
      {
        name: "AES-GCM",
        iv: iv,
      },
      aesKey,
      new TextEncoder().encode(plaintext)
    );
    const encryptedArray = new Uint8Array(encrypted);
    const encryptedBase64 = arrayBufferToBase64String(encryptedArray);
    const ivBase64 = arrayBufferToBase64String(iv);
    return [encryptedAesBase64, ivBase64, encryptedBase64].join(":");
  }
}