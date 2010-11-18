// $Id: TestRIPEMD128.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestRIPEMD128.java,v $
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
 * Tests the output of the RIPEMD-128 message digest algorithm implementation
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
 *        (this appears the best reference for RIPEMD128).
 * </ol>
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestRIPEMD128
extends BaseTest
{

// Test methods
//................................................................................

    public static void main (String[] args) {
        new TestRIPEMD128().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(9);

        String[][] data = {
            //    data, md
            {"",
                "CDF26213A150DC3ECB610F18F6B38B46"},
            {"a",
                "86BE7AFA339D0FC7CFC785E72F578D33"},
            {"abc",
                "C14A12199C66E4BA84636B0F69144C77"},
            {"message digest",
                "9E327B3D6E523062AFC1132D7DF9D1B8"},
            {"abcdefghijklmnopqrstuvwxyz",
                "FD2AA607F71DC8F510714922B371834E"},
            {"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
                "A1AA0689D0FAFA2DDC22E88B49133A06"},
            {"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                "D1E959EB179C911FAEA4624C60C5C702"},
            {"12345678901234567890123456789012345678901234567890123456789012345678901234567890",
                "3F45EF194732C2DBB2C4A2C769795FA3"},
            {"aaaaaaaaa...a (1 million times)",
                "4A7F5723F954EBA1216C9D8F6320431F"}};
        byte[] x;
        String a;
        MessageDigest alg = MessageDigest.getInstance("RIPEMD128", "Cryptix");

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
            passIf(a.equals(data[i][1]), " ***** RIPEMD-128");
        }
    }
}
