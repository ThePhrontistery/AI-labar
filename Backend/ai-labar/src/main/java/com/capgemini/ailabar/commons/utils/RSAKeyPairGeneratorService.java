package com.capgemini.ailabar.commons.utils;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import java.security.*;
import java.util.Base64;

@Service
public class RSAKeyPairGeneratorService {
    private KeyPair keyPair;

    public void generateKeys() throws NoSuchAlgorithmException, NoSuchProviderException {
        Security.addProvider(new BouncyCastleProvider());
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA", "BC");
        generator.initialize(2048);
        this.keyPair = generator.generateKeyPair();
    }

    public String publicKeyToString() {
        if (keyPair == null) {
            throw new IllegalStateException("Key pair has not been generated. Call generateKeys() first.");
        }
        return Base64.getEncoder().encodeToString(keyPair.getPublic().getEncoded());
    }

    public PrivateKey getPrivateKey() {
        if (keyPair == null) {
            throw new IllegalStateException("Key pair has not been generated. Call generateKeys() first.");
        }
        return keyPair.getPrivate();
    }

    public String decryptWithPrivateKey(String mensajeCifrado, PrivateKey clavePrivada) throws Exception {
        Cipher descifrador = Cipher.getInstance("RSA");
        descifrador.init(Cipher.DECRYPT_MODE, clavePrivada);
        byte[] textoDescifrado = descifrador.doFinal(Base64.getDecoder().decode(mensajeCifrado));
        return new String(textoDescifrado);
    }
}
