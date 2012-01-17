// $Id: TestRSA.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestRSA.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.13  2000/08/17 11:41:02  edwin
// java.* -> xjava.*
//
// Revision 1.12  1999/07/26 16:12:48  iang
// Now catches Throwable, as unknown class errors were sneaking through
//
// Revision 1.11  1999/07/12 20:37:38  edwin
// renaming java.security.interfaces.RSAPrivateKey and RSAPublicKey to CryptixRSAPrivateKey and CryptixRSAPublicKey. This is one more step to JDK1.2 compatibility.
//
// Revision 1.10  1998/02/01 05:08:44  hopwood
// + Committed changes below.
//
// Revision 1.9.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.9  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.8  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.7  1997/12/07 06:42:21  hopwood
// + Added TestElGamal, various other changes.
//
// Revision 1.6  1997/11/29 05:12:22  hopwood
// + Changes to use new test API (BaseTest).
//
// Revision 1.5  1997/11/22 07:05:41  raif
// + Updated the dox.
// + Replaced use of getU() with getInverseOfQModP().
//
// Revision 1.4  1997/11/20 22:31:26  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.3  1997/11/07 05:53:26  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/05 08:01:56  raif
// *** empty log message ***
//
// Revision 1.1.1.1  1997/11/03 22:36:55  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.0  1997/07/23  R. Naffah
// + original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.util.core.BI;
import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import xjava.security.Cipher;
import xjava.security.interfaces.CryptixRSAPublicKey;
import xjava.security.interfaces.CryptixRSAPrivateKey;
import xjava.security.interfaces.RSAFactors;

/**
 * This object tests the RawRSA* and RSA_*_PKCS1 classes by generating
 * 4 keypairs with varying strength (384- to 768-bit in 128-bit increments).
 * It then tests each newly created keypair by signing a fixed string with
 * the private key and verifying the obtained signature with the public one.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @since   Cryptix 2.2.2
 * @author  Raif S. Naffah
 */
public class TestRSA
extends BaseTest
{

// Constants and variables
//................................................................................

    private static final byte[] message = (
        "Je ne veux que du magnifique, et je ne travaille pas pour le " +
        "vulgaire des lecteurs --Giambattista BODONI (1740-1813)").getBytes();
    private static final SecureRandom prng = new SecureRandom();


// Test methods
//................................................................................

    public static void main (String[] args) {
        new TestRSA().commandline(args);
    }

    protected void engineTest() throws Exception {
        int k = 4;
        setExpectedPasses(k*9);

        KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA");
        Signature[] sigs = new Signature[] {
            Signature.getInstance("MD2/RSA/PKCS#1"),
            Signature.getInstance("MD5/RSA/PKCS#1"),
            Signature.getInstance("SHA-1/RSA/PKCS#1"),
            Signature.getInstance("RIPEMD160/RSA/PKCS#1"),
        };
        Cipher cipher = Cipher.getInstance("RSA");
        int s;  // key strength

        for (int i = 0; i < k; i++) {
            s = 384 + 128 * i;
            out.println("\nTest #" + (i + 1) + " (" + s + "-bit modulus)\n");

            out.print("  Generating keypair ");
            kpg.initialize(s, prng);
            out.print(". ");
            KeyPair pair = kpg.generateKeyPair();
            out.print(". ");
            PrivateKey sk = pair.getPrivate();
            out.print(". ");
            PublicKey pk = pair.getPublic();
            out.println(". Done!\n");

            // out.print("  Testing Signatures ");
            for (int j = 0; j < sigs.length; j++)
            {
                testSignature(s, pk, sk, sigs[j]);
                // out.print(". ");
            }
            // out.println("Done!\n");

            testEncryption(s, pk, sk, cipher);
        }
    }

    private void testSignature(int s, PublicKey pk, PrivateKey sk, Signature alg) {
        try {
            out.println("  Signing with a " + s + "-bit key using " + alg.getAlgorithm()
                + "...");
            alg.initSign(sk);
            alg.update(message);
            byte[] signature = alg.sign();
                
            out.println("  Verifying with same " + s + "-bit key using " + alg.getAlgorithm()
                + "...");
            alg.initVerify(pk);
            alg.update(message);
            boolean ok = alg.verify(signature);
            passIf(ok, "Signature verification");

            if (!ok) {
                out.println("---- begin debugging -----\n");
                out.println("Computed signature: " + Hex.dumpString(signature));

                out.println("RSA parameters:");

                BigInteger m = ((CryptixRSAPublicKey) pk).getModulus();
                BigInteger e = ((CryptixRSAPublicKey) pk).getExponent();
                out.println("   Public key material:");
                out.println("   n: " + BI.dumpString(m));
                out.println("   e: " + BI.dumpString(e));

                BigInteger n = ((CryptixRSAPrivateKey) sk).getModulus();
                BigInteger d = ((CryptixRSAPrivateKey) sk).getExponent();
                BigInteger p = ((RSAFactors) sk).getP();
                BigInteger q = ((RSAFactors) sk).getQ();
                BigInteger u = ((RSAFactors) sk).getInverseOfQModP();
                out.println("   Private key material:");
                out.println("   n: " + BI.dumpString(n));
                out.println("   d: " + BI.dumpString(d));
                out.println("   p: " + BI.dumpString(p));
                out.println("   q: " + BI.dumpString(q));
                out.println("   u: " + BI.dumpString(u));

                BigInteger x = new BigInteger(signature);
                out.println("   The signature as a BigInteger:");
                out.println("   x: " + BI.dumpString(x));

                out.println("RSA correctness tests:");
                try {
                    // 1. same modulus for both public and private keys
                    boolean yes = m.compareTo(n) == 0;
                    out.println("\t1. Same modulus? " + yes);
                    if (!yes) throw new RuntimeException();

                    // 2. n = pq
                    yes = p.multiply(q).compareTo(n) == 0;
                    out.println("\t2. n = pq? " + yes);
                    if (!yes) throw new RuntimeException();

                    // 3. x = (x ** ed) mod n
                    BigInteger y = x.modPow(e, n);
                    BigInteger z = y.modPow(d, n);
                    yes = z.compareTo(x) == 0;
                    out.println("\t3. x = (x ** ed) mod n? " + yes);
                    if (!yes) throw new RuntimeException();

                    // 4. same with CRT-based formula
                    BigInteger ONE = BigInteger.valueOf(1L);
                    BigInteger ep = d.mod(p.subtract(ONE));
                    BigInteger eq = d.mod(q.subtract(ONE));
                    BigInteger p2 = y.mod(p).modPow(ep, p);
                    BigInteger q2 = y.mod(q).modPow(eq, q);
                    q2 = q2.subtract(p2);
                    if (q2.signum() == -1) q2 = q2.add(q);  // 0 <= q2 - p2 < q
                    z = p2.add(p.multiply(q2.multiply(u).mod(q)));
                    yes = z.compareTo(x) == 0;
                    out.println("\t4. (x ** e) mod n = (y ** d) mod pq? " + yes);
                    if (!yes) throw new RuntimeException();
                } catch (Throwable ex) {
                    error(ex);
                }
                // passed RSA tests - must be a problem with formatting
                out.println("---- end debugging -----");
            }

            // flip a bit.
            signature[0] ^= 1;
            alg.initVerify(pk);
            alg.update(message);
            ok = !alg.verify(signature);
            passIf(ok, "Incorrect signature should not verify");

        } catch (Throwable e) {
            error(e);
        }
    }

    private void testEncryption(int s, PublicKey pk, PrivateKey sk, Cipher alg) {
        skip("Encryption test not implemented");
    }

/* old methods
    public void self_test() throws Exception {
        KeyPairGenerator keygen = KeyPairGenerator.getInstance("RSA", "Cryptix");
        SecureRandom random = new SecureRandom();

        long start = System.currentTimeMillis();
        keygen.initialize(1024, random);
        KeyPair keypair = keygen.generateKeyPair();
        long duration = System.currentTimeMillis() - start;

        out.println("Keygen: " + (float)duration/1000 + " seconds");

        //RawRSACipher raw = new RawRSACipher();
        //raw.test(keypair, random);
    }

    private void testEncryption(KeyPair keypair, SecureRandom random, Cipher cipher)
    throws KeyException {
        RSAPrivateKey privateKey = (RSAPrivateKey) (keypair.getPrivate());
        RSAPublicKey publicKey = (RSAPublicKey) (keypair.getPublic());

        byte[] M = new byte[blockSize()];
        random.nextBytes(M);
        M[0] = 0; // make sure input is within range

        long start = System.currentTimeMillis();
        cipher.initEncrypt(publicKey);
        BigInteger e = exp;
        
        byte[] C = crypt(M);
        long midpoint = System.currentTimeMillis();
        cipher.initDecrypt(privateKey);
        byte[] Mdash = crypt(C);
        long end = System.currentTimeMillis();

        out.println("         n = " + BI.dumpString(n));
        out.println("         e = " + BI.dumpString(e));
        out.println("         d = " + BI.dumpString(exp));
        out.println("         p = " + BI.dumpString(p));
        out.println("         q = " + BI.dumpString(q));
        out.println("q^-1 mod p = " + BI.dumpString(u));
        out.println(" plaintext = " + Hex.toString(M) + "\n");
        out.println("ciphertext = " + Hex.toString(C) + "\n");

        boolean ok = ArrayUtil.areEqual(M, Mdash);
        if (!ok) out.println("  computed = " + Hex.toString(Mdash) + "\n");
        passIf(ok, "Computed matches original?");

        out.println("Encrypt: " + ((float) (midpoint - start) / 1000) + " seconds");
        out.println("Decrypt: " + ((float) (end - midpoint) / 1000) + " seconds");
    }
*/
}
