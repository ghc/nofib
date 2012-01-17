// $Id: TestElGamal.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestElGamal.java,v $
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
// Revision 1.8  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.7  1998/02/01 05:08:45  hopwood
// + Committed changes below.
//
// Revision 1.6.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.6  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.5  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.4  1997/12/15 02:40:54  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/12/15  hopwood
// + Speed up test by only doing 512 and 768-bit primes for the time being.
//
// Revision 1.3  1997/12/14 18:00:10  hopwood
// + Committed changes below.
//
// Revision 1.2.1  1997/12/11  hopwood
// + Changed step for modulus to 256, so that the pre-computed parameters
//   will be used (when they have been computed).
// + Added new test for 384-bit parameter generation.
//
// Revision 1.2  1997/12/08 16:31:29  hopwood
// + Fix variable name that was used twice.
//
// Revision 1.1  1997/12/07 06:42:21  hopwood
// + Added TestElGamal, various other changes.
//
// Revision 0.1.0  1997/12/04  David Hopwood
// + Original version (based on TestRSA).
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
import xjava.security.interfaces.ElGamalPublicKey;
import xjava.security.interfaces.ElGamalPrivateKey;

/**
 * This object tests the RawElGamal* and *_ElGamal_PKCS1 classes, by
 * generating 6 keypairs with varying strength (384- to 1024-bit in 128-bit
 * increments). It then tests each newly created keypair by:
 * <ul>
 *   <li> signing a fixed string with the private key and verifying the
 *        obtained signature with the public one.
 *   <li> checking that a slightly-modified signature will not pass verification.
 *   <li> encrypting a random string, decrypting it, and checking that the
 *        result matches the original.
 * </ul>
 * Note: we should really test against fixed conformance data, to ensure
 * interoperability with other implementations. Randomised tests are useful but
 * not sufficient.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 * @author  Raif S. Naffah
 * @since   Cryptix 2.2.2
 */
public class TestElGamal
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
        new TestElGamal().commandline(args);
    }

    protected void engineTest() throws Exception {
        int k = 2;
        setExpectedPasses(k*9);

        KeyPairGenerator kpg = KeyPairGenerator.getInstance("ElGamal");
        Signature[] sigs = new Signature[] {
            Signature.getInstance("MD2/ElGamal/PKCS#1"),
            Signature.getInstance("MD5/ElGamal/PKCS#1"),
            Signature.getInstance("SHA-1/ElGamal/PKCS#1"),
            Signature.getInstance("RIPEMD160/ElGamal/PKCS#1"),
        };
        Cipher cipher = null; //Cipher.getInstance("ElGamal");
        int s;  // key strength

        for (int i = 0; i < k; i++) {
            s = 384 + 128 * i;
            out.println("\nTest #" + (i + 1) + " (" + s + "-bit modulus)\n");

            out.println("  Generating keypair...\n");
            kpg.initialize(s, prng);
            KeyPair pair = kpg.generateKeyPair();
            PrivateKey sk = pair.getPrivate();
            PublicKey pk = pair.getPublic();

            for (int j = 0; j < sigs.length; j++)
                testSignature(s, pk, sk, sigs[j]);

            testEncryption(s, pk, sk, cipher);
        }
    }

    private void testSignature(int s, PublicKey pk, PrivateKey sk, Signature alg) {
        try {
            out.println("\n  Signing with a " + s + "-bit key using " + alg.getAlgorithm()
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

                out.println("ElGamal parameters:");

                BigInteger p = ((ElGamalPublicKey) pk).getP();
                BigInteger g = ((ElGamalPublicKey) pk).getG();
                BigInteger y = ((ElGamalPublicKey) pk).getY();
                out.println("  Public key material:");
                out.println("    p: " + BI.dumpString(p));
                out.println("    g: " + BI.dumpString(g));
                out.println("    y: " + BI.dumpString(y));

                BigInteger x = ((ElGamalPrivateKey) sk).getX();
                out.println("  Private key material:");
                out.println("    x: " + BI.dumpString(x));

                BigInteger sig = new BigInteger(signature);
                out.println("  The signature as a BigInteger:");
                out.println(" sig: " + BI.dumpString(sig));

                // need correctness tests here, as for TestRSA.
                out.println("---- end debugging -----");
            }

            // flip a bit.
            signature[0] ^= 1;
            alg.initVerify(pk);
            alg.update(message);
            ok = !alg.verify(signature);
            passIf(ok, "Incorrect signature should not verify");

        } catch (Exception e) {
            error(e);
        }
    }

    private void testEncryption(int s, PublicKey pk, PrivateKey sk, Cipher alg) {
        skip("Encryption test not implemented");
    }
}
