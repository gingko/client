import config from "../config.js";
import crypto from "node:crypto";

// Import private key from config
const privateKey = await crypto.subtle.importKey(
    "jwk",
    config.PRIVATE_KEY,
    {
        name: "RSA-OAEP",
        hash: "SHA-256",
    },
    true,
    ["decrypt"]
);

// Get encrypted message from stdin
const encryptedMessage = await new Promise((resolve) => {
    let data = "";
    process.stdin.on("data", (chunk) => {
        data += chunk;
    });
    process.stdin.on("end", () => {
        resolve(data);
    });
});

const encryptedAesKey = encryptedMessage.split(":")[0];
const aesIv = encryptedMessage.split(":")[1];
const encryptedText = encryptedMessage.split(":")[2];

// Decrypt AES key
const decryptedAesKey = await crypto.subtle.decrypt(
    {
        name: "RSA-OAEP",
        hash: "SHA-256",
    },
    privateKey,
    Buffer.from(encryptedAesKey, "base64")
);

const aesKey = await crypto.subtle.importKey(
    "jwk",
    JSON.parse(new TextDecoder().decode(decryptedAesKey)),
    {
        name: "AES-GCM",
        length: 256,
    },
    true,
    ["encrypt", "decrypt"]
);


// Decrypt text
const decryptedText = await crypto.subtle.decrypt(
    {
        name: "AES-GCM",
        iv: Buffer.from(aesIv, "base64"),
    },
    aesKey,
    Buffer.from(encryptedText, "base64")
);

// output to stdout
process.stdout.write(new TextDecoder().decode(decryptedText));