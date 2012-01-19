// $Id: TestMD5.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestMD5.java,v $
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
// Revision 1.7  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.6.1  1998/01/28  hopwood
// + Cast to cryptix.provider.md.MD5 when calling self_test.
//
// Revision 1.6  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.5  1998/01/05 03:43:29  iang
// + Updated references to show where the conformance data comes from.
//
// Revision 1.4  1998/01/05 01:38:28  iang
// + Added comment only: engineUpdate() method missing, test #6 fails.
//
// Revision 1.3  1997/12/16 21:43:57  iang
// + Added BaseTest to MD5, dropped byte-wise list from SHA0
//
// Revision 1.2  1997/12/16 21:37:00  iang
// + Tests for SHA{01} written, working.
// + MD5 written, but needs addition of BaseTest stuff.
//
// Revision 1.1  1997/12/16 10:39:01  iang
// + Written, from TestHAVAL, fails on all but first test, but that appears
//   to be a problem with the linkage code, not the test.  The data looks
//   good, compared against the old secret test kit.
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
 * Tests the output of the MD5 message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * Note there are seven sets of conformance data for MD5 listed
 * in Appendix A.5 of the RFC, as indicated.
 * Other data is sets picked up from other implementations.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Ronald L. Rivest,
 *        "<a href="http://www.roxen.com/rfc/rfc1321.html">
 *        The MD5 Message-Digest Algorithm</a>",
 *        IETF RFC-1321 (informational).
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
class TestMD5
extends BaseTest
{

// Test data
//.............................................................................

    private static String[][] testData1 = {
    //    data string, md hex
	 { "", "D41D8CD98F00B204E9800998ECF8427E" },       // A.5 1
	 { "a", "0CC175B9C0F1B6A831C399E269772661" },      // A.5 2
	 { "aa", "4124BC0A9335C27F086F24BA207A4912" },
	 { "abc", "900150983CD24FB0D6963F7D28E17F72" },    // A.5 3
	 { "aaa", "47BCE5C74F589F4867DBD57E9CA9F808" },
	 { "bbb", "08F8E0260C64418510CEFB2B06EEE5CD" },
	 { "ccc", "9DF62E693988EB4E1E1444ECE0578579" },
	 { "message digest", "F96B697D7CB7938D525A2F31AAF161D0" },  // A.5 4
	 { "abcdefg", "7AC66C0F148DE9519B8BD264312C4D64" },
	 { "abcdefghijk", "92B9CCCC0B98C3A0B8D0DF25A421C0E3" },

	 {       // A.5 5
	    "abcdefghijklmnopqrstuvwxyz",
	    "C3FCD3D76192E4007DFB496CCA67E13B"
	 },
	 {       // A.5 6
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
	    "D174AB98D277D9F5A5611C2C9F419D9F"
	 },
	 {       // A.5 7
	    "12345678901234567890123456789012345678901234567890123456789012345678901234567890",
	    "57EDF4A22BE3C955AC49DA2E2107B67A"
	 },
    };


// Test methods
//.............................................................................
    
    public static void main (String[] args) {
        new TestMD5().commandline(args);
    }

    protected void engineTest() throws Exception {
        byte[] x;
        String a;
        int good = 0;
        int fails = 0;
        int i;
        String[][] data = testData1;

        MessageDigest alg = MessageDigest.getInstance("MD5", "Cryptix");

        setExpectedPasses(data.length + 1);      // one for the self_test()
        for (i = 0; i < data.length; i++) {
            alg.reset();
            if (i != 6)
                x = alg.digest(data[i][0].getBytes());
            else {                // try the engineUpdate(byte) method
// BUG: update method missing
                for (int j = 0; j < data[i][0].length(); j++) {
                    alg.update((byte)(data[i][0].charAt(j)));
                }
                x = alg.digest();
            }
            a = Hex.toString(x);
            out.println("         data: '" + data[i][0] + "'");
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][1]);
            boolean ok = a.equalsIgnoreCase(data[i][1]);
            passIf(ok, "Data Set #" + (i+1));
            if (ok) {
                out.println("   * Hash (#"+ ++good+"/"+(i+1)+") good" );

//                // for self_test() generation
//                out.print("{");
//                for (int j = 0; j < x.length; j++)
//                    out.print(" " + x[j] + ",");
//                out.println("},");
            }
            else {
                out.println("===> Hash (#"+ ++fails+"/"+(i+1)+") FAILED  <===");
                out.println("     (no debugging available)");
            }
            out.println();
        }

        out.println("\nMD5 succeeded (" + i + " tests)");

        // this one last because exception stops it
        ((cryptix.provider.md.MD5) alg).self_test();
        passIf(true, "Self Test (no diags)");   // passed if no exception
    }
}
