// $Id: TestMD2.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestMD2.java,v $
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
 * Tests the output of the MD2 message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * Note there are seven sets of conformance data for MD2 listed
 * in Appendix A.5 of the RFC, as indicated.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Burton S. Kaliski Jr,
 *        "<a href="http://rfc.fh-koeln.de/rfc/html/rfc1319.html">
 *        The MD2 Message-Digest Algorithm</a>",
 *        IETF RFC-1319 (informational).
 * </ol>
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestMD2
extends BaseTest
{

// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestMD2().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(7);

        String[][] data = {
            //    data, md
            //  ......................
            {"",               "8350E5A3E24C153DF2275C9F80692773"}, // A.5 1
            {"a",              "32EC01EC4A6DAC72C0AB96FB34C0B5D1"}, // A.5 2
            {"abc",            "DA853B0D3F88D99B30283A69E6DED6BB"}, // A.5 3
            {"message digest", "AB4F496BFB2A530B219FF33031FE06B0"}, // A.5 4
            {"abcdefghijklmnopqrstuvwxyz",
                               "4E8DDFF3650292AB5A4108C3AA47940B"}, // A.5 5
            {"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                               "DA33DEF2A42DF13975352846C30338CD"}, // A.5 6
            {"12345678901234567890123456789012345678901234567890123456789012345678901234567890",
                               "D5976F79D83D3A0DC9806C3C66F3EFD8"}  // A.5 7
	};

        MessageDigest alg = MessageDigest.getInstance("MD2", "Cryptix");

        for (int i = 0; i < data.length; i++) {
            String a = Hex.toString(alg.digest(data[i][0].getBytes()));
            out.println("  test vector: " + data[i][0]);
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][1]);
            passIf(a.equals(data[i][1]), "MD2 #" + (i+1));
        }
    }
}
