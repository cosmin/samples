/* Copyright notice
# Copyright (C) 2007, Cosmin Stejerean (http://www.offbytwo.com)
#
# You are free to use this code under the terms of the Creative Commons Attribution license
# available at http://creativecommons.org/licenses/by/3.0/
# so long as you include the following notice 'includes code from Cosmin Stejerean (http://www.offbytwo.com)'
*/

import java.security.Key;
import java.security.MessageDigest;
import java.security.spec.KeySpec;
import java.sql.Date;
import java.sql.Time;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.StringTokenizer;

import javax.crypto.Cipher;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESedeKeySpec;


//The shared 3DES key is itself encrypted using the SHA hash value of the LTPA password (padded with 0x0 upto 24 bytes).

public class LtpaUtils {

    public static void main(String[] args)
    {
        // If the key contains \= please replace it with =
        String ltpa3DESKey = "YOURBASE64ENCODEDKEYHERE"; // you can get this from your Websphere configuration
        String ltpaPassword = "the password protecting the 3DES key"; // you can also get this from your Websphere cofiguration
        
        String tokenCipher = "";
        
        try{
            String option = args[0];
            if(option.compareToIgnoreCase("-d") == 0)
            {
                tokenCipher = args[1];
            }
            else
            {
                throw new Exception("Unhandled parameter.");
            }
        }
        catch(Exception e)
        {
            System.out.println("Usage: java LtpaUtils -d <encryptedLtpaToken> - Takes an encrypted base64 ASCII token and decrypts it");
            return;
        }
                
        try{
            byte[] secretKey = getSecretKey(ltpa3DESKey, ltpaPassword);
            String ltpaPlaintext = new String(decryptLtpaToken(tokenCipher, secretKey));
            displayTokenData(ltpaPlaintext);
        }
        catch(Exception e)
        {
            System.out.println("Caught inner: " + e);
        }
    }
    
    private static void displayTokenData(String token)
    {
        System.out.println("\n");
        StringTokenizer st = new StringTokenizer(token, "%");
        String userInfo = st.nextToken();
        String expires = st.nextToken();
        String signature = st.nextToken();
        
        Date d = new Date(Long.parseLong(expires));
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss z");
        System.out.println("Token is for: " + userInfo);
        System.out.println("Token expires at: " + sdf.format(d));
        System.out.println("\n\nFull token string : " + token);
    }
    
    private static byte[] getSecretKey(String shared3DES, String password) throws Exception
    {
        MessageDigest md = MessageDigest.getInstance("SHA");
        md.update(password.getBytes());
        byte[] hash3DES = new byte[24];
        System.arraycopy(md.digest(), 0, hash3DES, 0, 20);
        Arrays.fill(hash3DES, 20, 24, (byte) 0);
        // decrypt the real key and return it
        return decrypt(Base64.decode(shared3DES), hash3DES);
    }
    
    public static byte[] decryptLtpaToken(String encryptedLtpaToken, byte[] key) throws Exception 
    {
        final byte[] ltpaByteArray = Base64.decode(encryptedLtpaToken);
        return decrypt(ltpaByteArray, key);
    }

    public static byte[] decrypt(byte[] ciphertext, byte[] key) throws Exception {
        final Cipher cipher = Cipher.getInstance("DESede/ECB/PKCS5Padding");
        final KeySpec keySpec = new DESedeKeySpec(key);
        final Key secretKey = SecretKeyFactory.getInstance("TripleDES").generateSecret(keySpec);

        cipher.init(Cipher.DECRYPT_MODE, secretKey);
        return cipher.doFinal(ciphertext);
    }
}
