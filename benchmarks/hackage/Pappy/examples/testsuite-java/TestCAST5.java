// $Id: TestCAST5.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestCAST5.java,v $
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
// Revision 1.7  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.6  1998/01/28 05:40:02  hopwood
// + Major update of test classes.
//
// Revision 1.5.1  1998/01/28  hopwood
// + Fixed conflicts with my version.
// + Cosmetics.
//
// Revision 1.5  1998/01/21 22:53:14  iang
// + (last was rubbish) now works with System.exit(1) and exceptions thrown.
//
// Revision 1.4  1998/01/21 22:17:42  iang
// + Added RFC-2144 refs, which tests are conformance.
// + Quickly added exit(1) and exception where data is tested.
//
// Revision 1.3  1997/11/22 07:05:40  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/22 05:59:02  iang
// + core.util ==> util.core
//
// Revision 1.1  1997/11/07 05:53:25  raif
// + Adapted to latest API.
//
// Revision 0.1.0.1  1997/08/15  R. Naffah
// + Use new cryptix.util classes.
// + More compact.
//
// Revision 0.1.0.0  1997/06/??  R. Naffah
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.provider.key.RawSecretKey;
import cryptix.util.core.ArrayUtil;
import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import xjava.security.Cipher;
import java.util.Date;

/**
 * Tests the output of the CAST5 (CAST-128) cipher algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestCAST5
extends BaseTest
{

// Variables and constants
//...........................................................................

    private Cipher alg;

    /** Input defined in RFC-2144 App B.1. */
    private static final byte[] input = Hex.fromString("0123456789ABCDEF");


// Test methods
//...........................................................................

    public static void main(String[] args) {
        new TestCAST5().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(7);

        alg = Cipher.getInstance("CAST5", "Cryptix");
        test1();
        test2();    // speed test
//        test3();    // maintenance (long) test...
    }
    
    /**
     * Conformance Test as defined in RFC-2144 Appendix B.1.
     */
    private void test1() throws Exception {
        String[][] data = {
	// Appendix B.1 test sets
        // key                               output
        {"0123456712345678234567893456789A", "238B4FE5847E44B2"}, // App B 128
        {"01234567123456782345",             "EB6A711A2C02271B"}, // App B 80
        {"0123456712",                       "7AC816D16E9B302E"}, // App B 40
	};

        for (int i = 0; i < data.length; i++) {
            RawSecretKey aKey = new RawSecretKey("CAST5", Hex.fromString(data[i][0]));
            byte[] output = Hex.fromString(data[i][1]);

            out.println("\nTest vector (" + (4 * data[i][0].length())+ "-bit key):");
            out.println("\nEncrypting:");

            alg.initEncrypt(aKey);
            compareIt(alg.crypt(input), output);

            out.println("\nDecrypting:");
            alg.initDecrypt(aKey);
            compareIt(alg.crypt(output), input);
        }
    }

    /**
     * Encryption and Decryption speed test. Uses same data as test3 below.
     */
    private void test2() throws Exception {
        RawSecretKey aKey = new RawSecretKey("CAST5", Hex.fromString("0123456712"));
        byte[] a = Hex.fromString("0123456789ABCDEF");    // same as input

        alg.initEncrypt(aKey);

        out.println("\nSpeed test (10,000 x 8-byte w/40-bit key):\n");
        out.println("...Encryption\n");
        out.println("      start date/time: " + new Date());
        for (int i = 0; i < 10; i++)
            for (int j = 0; j < 1000; j++)
                alg.crypt(a, 0, 8, a, 0);
        out.println("     finish date/time: " + new Date());
        
        alg.initDecrypt(aKey);
        out.println("\n...Decryption\n");
        out.println("      start date/time: " + new Date());
        for (int i = 0; i < 10; i++)
            for (int j = 0; j < 1000; j++)
                alg.crypt(a, 0, 8, a, 0);
        out.println("     finish date/time: " + new Date());

        out.println("\n result:");
        compareIt(a, input);
    }

    /**
     * Maintenance Test as defined in RFC-2144 Appendix B.2.
     * This test will take hours -- no kidding! On a P133 w/JDK 1.1.2 this is
     * what I get:
     * <pre>
     *   start date/time: Fri Jun 20 10:10:33 GMT+01:00 1997
     *  finish date/time: Fri Jun 20 12:57:51 GMT+01:00 1997
     * </pre>
     */
    private void test3() throws Exception {
        byte[]
            a = Hex.fromString("0123456712345678234567893456789A"),
            b = Hex.fromString("0123456712345678234567893456789A"),
            aOut = Hex.fromString("EEA9D0A249FD3BA6B3436FB89D6DCA92"),
            bOut = Hex.fromString("B2C95EB00C31AD7180AC05B8E83D696E");

        out.println("\nTest 1,000,000 encryptions with 128-bit key:\n");
        out.println("  start date/time: " + new Date());
        for (int i = 0; i < 1000; i++)
            for (int j = 0; j < 1000; j++) {
                alg.initEncrypt(new RawSecretKey("CAST5", b));
                a = alg.crypt(a);        // aL = encrypt(aL, b); aR = encrypt(aR, b);
                alg.initEncrypt(new RawSecretKey("CAST5", a));
                b = alg.crypt(b);        // bL = encrypt(bL, a); bR = encrypt(bR, a);
            }
        out.println(" finish date/time: " + new Date());
        out.println("\n result for 'a'\n");
        compareIt(a, aOut);
        out.println("\n result for 'b'\n");
        compareIt(b, bOut);
    }

    private void compareIt (byte[] o1, byte[] o2) {
        out.println("  computed: " + Hex.dumpString(o1));
        out.println(" certified: " + Hex.dumpString(o2));
        passIf(ArrayUtil.areEqual(o1, o2), " *** CAST5 OUTPUT");
    }
}
