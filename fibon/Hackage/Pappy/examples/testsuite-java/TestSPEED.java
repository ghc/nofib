// $Id: TestSPEED.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestSPEED.java,v $
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
// Revision 1.6  2000/08/17 11:41:02  edwin
// java.* -> xjava.*
//
// Revision 1.5  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.4.1  1998/01/28  hopwood
// + Changed to use BaseTest API.
//
// Revision 1.4  1997/12/01 21:16:11  iang
// + Fixed.  Now uses standard (programming) order, not cryptography order in paper.
// + Changed diags a little.  Added exception on failure.
//
// Revision 1.3  1997/11/29 17:48:01  hopwood
// + Fixed parameter setting. Tests still fail.
//
// Revision 1.2  1997/11/28 05:08:13  iang
// + Typo.
//
// Revision 1.1  1997/11/28 04:20:15  iang
// + Written from TestBlowfish and old secret key kit data.
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
import xjava.security.FeedbackCipher;
import xjava.security.SecretKey;
import xjava.security.Parameterized;
import java.util.Date;

/**
 * Tests the output of the SPEED cipher algorithm implementation against
 * certified pre-computed output for a given set of reference input.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @see cryptix.provider.cipher.SPEED
 * @author  Systemics Ltd
 */
class TestSPEED
extends BaseTest
{

// Test data
//...........................................................................

    /**
     * Certification data taken from the paper.
     * Note that in the paper, the data is ordered according to
     * cryptography convention of RIGHT to LEFT, i.e., 7,6,5,4,3,2,1,0.
     * Here, it is ordered according to programming convention.
     */
    private static final String[][]
    testData1 = {
       //
       //  I tried to preserve the paper order in the code, but it
       //  was more trouble than it was worth.
       //
       { "64",                       // number of rounds // certification 1
         "0000000000000000",         // key
         "0000000000000000",         // plain text
      // "2E008019BC26856D",         // cipher text - paper-order
         "6D8526BC1980002E",         // cipher text - code-order
       },
       { "128",
         "00000000000000000000000000000000",
         "00000000000000000000000000000000",
      // "A44FBF29EDF6CBF8D7A2DFD57163B909", // paper-order
         "09B96371D5DFA2D7F8CBF6ED29BF4FA4", // code-order
       },
       { "128",                                          // certification 2
         "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
         "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      // "6C13E4B9C3171571AB54D816915BC4E8", // paper-order
         "E8C45B9116D854AB711517C3B9E4136C", // code-order
       },
       { "48",
      // "504F4E4D4C4B4A494847464544434241",
         "4142434445464748494A4B4C4D4E4F50", // code-order
      // "1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100",
         "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F",
      // "90C5981EF6A3D21BC178CACDAD6BF39B2E51CDB70A6EE875A73BF5ED883E3692",
         "92363E88EDF53BA775E86E0AB7CD512E9BF36BADCDCA78C11BD2A3F61E98C590",
       },
       { "256",
         "0000000000000000000000000000000000000000000000000000000000000000",
         "0000000000000000000000000000000000000000000000000000000000000000",
      // "6CD44D2B49BC6AA7E95FD1C4AF713A2C0AFA1701308D56298CDF27A02EB09BF5",
         "F59BB02EA027DF8C29568D300117FA0A2C3A71AFC4D15FE9A76ABC492B4DD46C",
       },
       { "256",
         "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
         "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      // "C8F3E864263FAF24222E38227BEBC022CF4A9A0ECE89FB81CA1B9BA3BA93D0C5",
         "C5D093BAA39B1BCA81FB89CE0E9A4ACF22C0EB7B22382E2224AF3F2664E8F3C8",
       },
       { "256",                                          // certification 3
      // "605F5E5D5C5B5A595857565554535251504F4E4D4C4B4A494847464544434241",
         "4142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F60",
      // "1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100",
         "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F",
      // "3DE16CFA9A626847434E1574693FEC1B3FAA558A296B61D708B131CCBA311068",
         "681031BACC31B108D7616B298A55AA3F1BEC3F6974154E434768629AFA6CE13D",

       },
   };


// Test methods
//...........................................................................

    public static void main(String[] args) {
        new TestSPEED().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(14);

        test1();
    }

    private void test1() throws Exception {
        byte[] ect, dct;
        String a, b;
	int i;

        for (i = 0; i < testData1.length; i++) {
            Cipher alg = Cipher.getInstance("SPEED", "Cryptix");
            ((Parameterized) alg).setParameter("rounds", new Integer(testData1[i][0]));
            ((Parameterized) alg).setParameter("blockSize",
                new Integer(testData1[i][2].length()/2));
            RawSecretKey key = new
                RawSecretKey("SPEED", Hex.fromString(testData1[i][1]));

            alg.initEncrypt(key);
            ect = alg.crypt(Hex.fromString(testData1[i][2]));
            a = Hex.toString(ect);
            alg.initDecrypt(key);
            dct = alg.crypt(ect);
            b = Hex.toString(dct);
            
            out.println("     plain:  " + testData1[i][2]);
            out.println("     cipher: " + a);
            out.println("     cert:   " + testData1[i][3]);

            passIf(a.equals(testData1[i][3]), " *** SPEED encrypt");

            out.println("     cipher: " + Hex.toString(ect));
            out.println("     plain:  " + b);
            out.println("     cert:   " + testData1[i][2]);
            
            passIf(b.equals(testData1[i][2]), " *** SPEED decrypt");

//          // print reversed data for replacing/comparing in the data above
//          out.println("\n  \"" +
//            Hex.toString(Hex.fromReversedString(testData1[i][1])) + "\"");
//          out.println("  \"" +
//            Hex.toString(Hex.fromReversedString(testData1[i][2])) + "\"");
//          out.println("  \"" +
//            Hex.toString(Hex.fromReversedString(testData1[i][3])) + "\"\n");
        }
    }
}
