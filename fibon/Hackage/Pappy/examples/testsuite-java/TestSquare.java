// $Id: TestSquare.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestSquare.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:04  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.9  2000/08/17 11:41:02  edwin
// java.* -> xjava.*
//
// Revision 1.8  1998/02/21 19:19:44  iang
// Added comment on the validation set.  Not tested yet, should be.
//
// Revision 1.7  1998/01/28 05:40:02  hopwood
// + Major update of test classes.
//
// Revision 1.6.1  1998/01/28  hopwood
// + Removed dependency on Mode class.
// + Changed to use BaseTest API.
// + Cosmetics.
//
// Revision 1.6  1998/01/21 23:13:58  iang
// + Added System.exit, Exception on bad data.
// + Where does the conformance data come from? Need Formal Ref.
//
// Revision 1.5  1997/11/27 05:41:31  iang
// + Add static to block - crashes javadoc without.
//
// Revision 1.4  1997/11/20 22:31:26  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.3.1  1997/11/17  David Hopwood
// + Changed occurrence of
//   "if (x.length < 33) .. Hex.toString(x) .. else .. Hex.dumpString(x)"
//   to "Hex.dumpString(x)". Hex.dumpString has been changed so that
//   this will have the same effect.
//
// Revision 1.3  1997/11/07 05:53:26  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/05 08:01:57  raif
// *** empty log message ***
//
// Revision 0.1.0.2  1997/08/20  R. Naffah
// + Use RawKeyGenerator and SquareKeyGenerator classes.
//
// Revision 0.1.0.1  1997/08/13  R. Naffah
// + Modified to use/test new Mode implementation.
// + Use RawSecretKey instead of ByteArrayKey.
// + Use cryptix.util.
//
// Revision 0.1.0.0  1997/07/30  R. Naffah
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.provider.key.RawKeyGenerator;
import cryptix.provider.key.RawSecretKey;
import cryptix.util.core.ArrayUtil;
import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import xjava.security.Cipher;
import xjava.security.FeedbackCipher;
import xjava.security.KeyGenerator;
import xjava.security.SecretKey;

/**
 * Tests the output of the Square cipher algorithm implementation against
 * certified pre-computed values for a given set of reference input.
 * <P>
 * Refs:
 * <UL>
 *   <LI>The
 *     <a href="http://www.esat.kuleuven.ac.be/%7Erijmen/square/">
 *     Square Home Page</a>,
 *     especially, the
 *     <a href="http://www.esat.kuleuven.ac.be/%7Erijmen/downloadable/square/vdata">
 *     validation data set v1.0</a>.
 * </UL>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 * @author  David Hopwood
 */
class TestSquare
extends BaseTest
{

// Variables and constants
//...........................................................................

    private static final byte[] key = new byte[16];
    static {
        for (int i = 0; i < 16; i++) key[i] = (byte)i;
    }
    private SecretKey aKey;


// Test methods
//...........................................................................

    public static void main(String[] args) {
        new TestSquare().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(10);

//        aKey = new RawSecretKey("Square", key);
        RawKeyGenerator rkg = (RawKeyGenerator)
            KeyGenerator.getInstance("Square", "Cryptix");
        aKey = rkg.generateKey(key);

        test1();
        test2();
        test3();
        test4();
        test5();
    }
    
    private void test1() throws Exception {
        byte[] input = new byte[16];
        for (int i = 0; i < 16; i++)
            input[i] = (byte) i;

	// where did this data come from?
        byte[] output = {
            (byte)0x7C, (byte)0x34, (byte)0x91, (byte)0xD9,
            (byte)0x49, (byte)0x94, (byte)0xE7, (byte)0x0F,
            (byte)0x0E, (byte)0xC2, (byte)0xE7, (byte)0xA5,
            (byte)0xCC, (byte)0xB5, (byte)0xA1, (byte)0x4F
        };
        out.println("\nTest vector (raw/ECB):\nEncrypting:");
        Cipher alg = Cipher.getInstance("Square", "Cryptix");

        alg.initEncrypt(aKey);
        compareIt(alg.crypt(input), output);

        out.println("\nDecrypting:");
        alg.initDecrypt(aKey);
        compareIt(alg.crypt(output), input);
    }
    
    private void test2() throws Exception {
        byte[] input = new byte[32];
        for (int i = 0; i < 16; i++)
            input[i] = input[i + 16] = (byte) i;

        byte[] output = {
            (byte)0x7C, (byte)0x34, (byte)0x91, (byte)0xD9,
            (byte)0x49, (byte)0x94, (byte)0xE7, (byte)0x0F,
            (byte)0x0E, (byte)0xC2, (byte)0xE7, (byte)0xA5,
            (byte)0xCC, (byte)0xB5, (byte)0xA1, (byte)0x4F,

            (byte)0x7C, (byte)0x34, (byte)0x91, (byte)0xD9,
            (byte)0x49, (byte)0x94, (byte)0xE7, (byte)0x0F,
            (byte)0x0E, (byte)0xC2, (byte)0xE7, (byte)0xA5,
            (byte)0xCC, (byte)0xB5, (byte)0xA1, (byte)0x4F
        };
        out.println("\nTest vector (ECB):\nEncrypting:");
        Cipher alg = Cipher.getInstance("Square/ECB", "Cryptix");

        alg.initEncrypt(aKey);
        compareIt(alg.crypt(input), output);

        out.println("\nDecrypting:");
        alg.initDecrypt(aKey);
        compareIt(alg.crypt(output), input);
    }

    private void test3() throws Exception {
        byte[] input = new byte[32];
        for (int i = 0; i < 16; i++)
            input[i] = input[i + 16] = (byte) i;

        byte[] output = {
            (byte)0x7C, (byte)0x34, (byte)0x91, (byte)0xD9,
            (byte)0x49, (byte)0x94, (byte)0xE7, (byte)0x0F,
            (byte)0x0E, (byte)0xC2, (byte)0xE7, (byte)0xA5,
            (byte)0xCC, (byte)0xB5, (byte)0xA1, (byte)0x4F,

            (byte)0x41, (byte)0xD2, (byte)0xF1, (byte)0x9D,
            (byte)0x7E, (byte)0x87, (byte)0x8D, (byte)0xB5,
            (byte)0x6C, (byte)0x74, (byte)0x46, (byte)0xD4,
            (byte)0x24, (byte)0xC3, (byte)0xAD, (byte)0xFC
        };
        out.println("\nTest vector (CBC):\nEncrypting:");
        Cipher alg = Cipher.getInstance("Square/CBC", "Cryptix");
        FeedbackCipher fbc = (FeedbackCipher) alg;
        fbc.setInitializationVector(new byte[fbc.getInitializationVectorLength()]);

        alg.initEncrypt(aKey);
        compareIt(alg.crypt(input), output);

        out.println("\nDecrypting:");
        alg.initDecrypt(aKey);
        compareIt(alg.crypt(output), input);
    }

    private void test4() throws Exception {
        byte[] input = new byte[32];
        for (int i = 0; i < 16; i++)
            input[i] = input[i + 16] = (byte) i;

        byte[] output = {
            (byte)0xff, (byte)0x58, (byte)0x6d, (byte)0xa5,
            (byte)0x6c, (byte)0xba, (byte)0xc5, (byte)0x06,
            (byte)0x4a, (byte)0x09, (byte)0xa4, (byte)0x0a,
            (byte)0xee, (byte)0xb6, (byte)0xae, (byte)0xaf,

            (byte)0xd5, (byte)0xcb, (byte)0x53, (byte)0x8e,
            (byte)0xea, (byte)0x28, (byte)0x97, (byte)0x4f,
            (byte)0x7c, (byte)0x75, (byte)0xe7, (byte)0x9b,
            (byte)0xcb, (byte)0x0d, (byte)0x4d, (byte)0x0e
        };
        out.println("\nTest vector (CFB):\nEncrypting:");
        Cipher alg = Cipher.getInstance("Square/CFB", "Cryptix");
        ((FeedbackCipher) alg).setInitializationVector(new byte[16]);

        alg.initEncrypt(aKey);
        compareIt(alg.crypt(input), output);

        out.println("\nDecrypting:");
        alg.initDecrypt(aKey);
        compareIt(alg.crypt(output), input);
    }

    private void test5() throws Exception {
        byte[] input = new byte[32];
        for (int i = 0; i < 16; i++)
            input[i] = input[i + 16] = (byte) i;

        byte[] output = {
            (byte)0xff, (byte)0x58, (byte)0x6d, (byte)0xa5,
            (byte)0x6c, (byte)0xba, (byte)0xc5, (byte)0x06,
            (byte)0x4a, (byte)0x09, (byte)0xa4, (byte)0x0a,
            (byte)0xee, (byte)0xb6, (byte)0xae, (byte)0xaf,

            (byte)0x35, (byte)0xc8, (byte)0x33, (byte)0xd3,
            (byte)0x5c, (byte)0x29, (byte)0x44, (byte)0x37,
            (byte)0x35, (byte)0xd2, (byte)0x25, (byte)0xbc,
            (byte)0x95, (byte)0x28, (byte)0xc3, (byte)0xc8
        };
        out.println("\nTest vector (OFB):\nEncrypting:");
        Cipher alg = Cipher.getInstance("Square/OFB", "Cryptix");
        FeedbackCipher fbc = (FeedbackCipher) alg;
        fbc.setInitializationVector(new byte[fbc.getInitializationVectorLength()]);

        alg.initEncrypt(aKey);
        compareIt(alg.crypt(input), output);

        out.println("\nDecrypting:");
        alg.initDecrypt(aKey);
        compareIt(alg.crypt(output), input);
    }

    private void compareIt (byte[] o1, byte[] o2) {
        out.print("  computed output:" + Hex.dumpString(o1));
        boolean ok = ArrayUtil.areEqual(o1, o2);
        if (!ok)
            out.print("\n certified output:" + Hex.dumpString(o2));
        passIf(ok, " *** Square OUTPUT");
    }
}
