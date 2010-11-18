// $Id: TestRIPEMD160.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestRIPEMD160.java,v $
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
// Revision 1.5  1998/01/28 05:40:01  hopwood
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
 * Tests the output of the RIPEMD-160 message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * Note there are nine sets of conformance data for RIPEMD* listed
 * in the Table of the Reference below.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Hans Dobbertin, Antoon Bosselaers, and Bart Preneel,
 *        "<a href="http://www.esat.kuleuven.ac.be/~bosselae/ripemd160.html">
 *        The hash function RIPEMD-160</a>",
 * </ol>
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestRIPEMD160
extends BaseTest
{

// Test methods
//................................................................................

    public static void main (String[] args) {
        new TestRIPEMD160().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(9);

        String[][] data = {
            //    data, md
            {"",
                "9C1185A5C5E9FC54612808977EE8F548B2258D31"},
            {"a",
                "0BDC9D2D256B3EE9DAAE347BE6F4DC835A467FFE"},
            {"abc",
                "8EB208F7E05D987A9B044A8E98C6B087F15A0BFC"},
            {"message digest",
                "5D0689EF49D2FAE572B881B123A85FFA21595F36"},
            {"abcdefghijklmnopqrstuvwxyz",
                "F71C27109C692C1B56BBDCEB5B9D2865B3708DBC"},
            {"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
                "12A053384A9C0C88E405A06C27DCF49ADA62EB2B"},
            {"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                "B0E20B6E3116640286ED3A87A5713079B21F5189"},
            {"12345678901234567890123456789012345678901234567890123456789012345678901234567890",
                "9B752E45573D4B39F4DBD3323CAB82BF63326BFB"},
            {"aaaaaaaaa...a (1 million times)",
                "52783243C1697BDBE16D37F97F68F08325DC1528"}};
        byte[] x;
        String a;
        MessageDigest alg = MessageDigest.getInstance("RIPEMD160", "Cryptix");

        for (int i = 0; i < data.length; i++) {
            if (i != 8)
                x = alg.digest(data[i][0].getBytes());
            else {
                // test for the million times 'a' requires special handling
                // in the process test engineUpdate(byte) method implementation
                for (int j = 0; j < 1000; j++)
                    for (int k = 0; k < 1000; k++)
                        alg.update((byte) 'a');
                x = alg.digest();
            }
            a = Hex.toString(x);
            out.println("Data: '" + data[i][0] + "'");
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][1]);
            passIf(a.equals(data[i][1]), " ***** RIPEMD-160");
        }
    }
}
