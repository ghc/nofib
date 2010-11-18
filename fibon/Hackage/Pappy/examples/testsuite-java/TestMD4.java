// $Id: TestMD4.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestMD4.java,v $
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
// Revision 1.5  1998/01/28 05:40:00  hopwood
// + Major update of test classes.
//
// Revision 1.4  1998/01/05 03:43:29  iang
// + Updated references to show where the conformance data comes from.
//
// Revision 1.3  1997/11/22 07:05:41  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/22 05:59:03  iang
// core.util ==> util.core
//
// Revision 1.1  1997/11/07 05:53:26  raif
// *** empty log message ***
//
// Revision 1.2  1997/10/28 00:09:10  raif
// *** empty log message ***
//
// Revision 1.1  1997/10/27 22:50:17  raif
// + use new cryptix.util classes.
// + more compact.
//
// Revision 0.1.0.0  1997/06/??  R. Naffah
// + Original version.
//
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import java.security.MessageDigest;

/**
 * Tests the output of the MD4 message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * Note there are seven sets of conformance data for MD4 listed
 * in Appendix A.5 of the RFC, as indicated.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Ronald L. Rivest,
 *        "<a href="http://www.roxen.com/rfc/rfc1320.html">
 *        The MD4 Message-Digest Algorithm</a>",
 *        IETF RFC-1320 (informational).
 * </ol>
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestMD4
extends BaseTest
{

// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestMD4().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(7);

        String[][] data = {
            //    data, md
            //  ......................
            {"",               "31D6CFE0D16AE931B73C59D7E0C089C0"}, // A.5 1
            {"a",              "BDE52CB31DE33E46245E05FBDBD6FB24"}, // A.5 2
            {"abc",            "A448017AAF21D8525FC10AE87AA6729D"}, // A.5 3
            {"message digest", "D9130A8164549FE818874806E1C7014B"}, // A.5 4
            {"abcdefghijklmnopqrstuvwxyz",
                               "D79E1C308AA5BBCDEEA8ED63DF412DA9"}, // A.5 5
            {"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                               "043F8582F241DB351CE627E153E7F0E4"}, // A.5 6
            {"12345678901234567890123456789012345678901234567890123456789012345678901234567890",
                               "E33B4DDC9C38F2199C3E7B164FCC0536"}  // A.5 7
	};

        MessageDigest alg = MessageDigest.getInstance("MD4", "Cryptix");

        for (int i = 0; i < data.length; i++) {
            String a = Hex.toString(alg.digest(data[i][0].getBytes()));
            out.println("  test vector: " + data[i][0]);
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][1]);
            passIf(a.equals(data[i][1]), "MD4 #" + (i+1));
        }
    }
}
