// $Id: TestSHA1.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestSHA1.java,v $
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
// Revision 1.8  1998/01/28 05:40:02  hopwood
// + Major update of test classes.
//
// Revision 1.7.1  1998/01/28  hopwood
// + Cast to cryptix.provider.md.SHA1 when calling self_test.
//
// Revision 1.7  1998/01/20 23:28:20  iang
// + Fixed to test million a's, now that update method is fixed.
//
// Revision 1.6  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import java.security.MessageDigest;

/**
 * Tests the output of the SHA-1 message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * Note there are three sets of conformance data for SHA-1:
 * "abc" and "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
 * and the million a's, from FIPS 180-1, below.
 * The other data is added to flesh it out, and because it has
 * seen a bit of history, but is not conformance data.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> NIST FIPS PUB 180-1,
 *        "Secure Hash Standard",
 *        U.S. Department of Commerce, 17 April 1995.<br>
 *        <a href="http://www.itl.nist.gov/div897/pubs/fip180-1.htm">
 *        http://www.itl.nist.gov/div897/pubs/fip180-1.htm</a>
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 */
class TestSHA1
extends BaseTest
{

// Test data
//.............................................................................

    private static String[][] testData1 = {
    //    data string, md hex
         { "", "da39a3ee5e6b4b0d3255bfef95601890afd80709" },
         { "1", "356a192b7913b04c54574d18c28d46e6395428ab" },
         { "abc", "A9993E364706816ABA3E25717850C26C9CD0D89D" }, // 180-1 App A

         {                                                      // 180-1 App B
            "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
            "84983E441C3BD26EBAAE4AA1F95129E5E54670F1"
         },
         {                                                      // old Cj data
            "Anyone got any SHA-1 test data?",
            "09b9e9c04a84ce274942048acf3a6f2ff4a8a39c"
         },
         {                                                      // old Cj data
            "Of cabbages and kings",
            "5f093d74a9cb1f2f14537bcf3a8a1ffd59b038a2"
         },
         {                                                      // 180-1 App C
            "aaaaaaaaa...a (1 million times)",                  // special last
            "34AA973CD4C4DAA4F61EEB2BDBAD27316534016F"          // set is fixed
         },
    };


// Test methods
//.............................................................................
    
    public static void main (String[] args) {
        new TestSHA1().commandline(args);
    }

    protected void engineTest() throws Exception {
        byte[] x;
        String a;
        int good = 0;
        int fails = 0;
        int i;
        String[][] data = testData1;

        MessageDigest alg = MessageDigest.getInstance("SHA-1", "Cryptix");

        setExpectedPasses(data.length + 1);      // one for the self_test()
        for (i = 0; i < data.length; i++) {
            alg.reset();

            /*
             *  The last test set is the million a's test.
             */
            if (i == (data.length - 1)) {
                // a million times 'a' requires special handling
                // in the process test engineUpdate(byte) method
                for (int j = 0; j < 1000; j++)
                    for (int k = 0; k < 1000; k++)
                        alg.update((byte) 'a');
                x = alg.digest();
            } else                  // standard method
                x = alg.digest(data[i][0].getBytes());

            a = Hex.toString(x);
            out.println("         data: '" + data[i][0] + "'");
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][1]);
            boolean ok = a.equalsIgnoreCase(data[i][1]);
            passIf(ok, "Data Set #" + (i+1));
            if (ok) {
                out.println("   * Hash (#"+ ++good+"/"+(i+1)+") good" );

                // for self_test() generation
                // out.print("{");
                // for (int j = 0; j < x.length; j++)
                //     out.print(" " + x[j] + ",");
                // out.println("},");
            }
            else {
                out.println("===> Hash (#"+ ++fails+"/"+(i+1)+") FAILED  <===");
                out.println("     (no debugging available)");
            }
            out.println();
        }

        out.println("\nSHA-1 succeeded (" + i + " tests)");

        // this one last because exception stops it
        ((cryptix.provider.md.SHA1) alg).self_test();
        passIf(true, "Self Test (no diags)");   // passed if no exception
    }
}
