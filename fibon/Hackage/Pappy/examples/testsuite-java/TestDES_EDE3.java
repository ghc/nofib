// $Id: TestDES_EDE3.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestDES_EDE3.java,v $
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
// Revision 1.10  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.9  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.8  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.7  1998/01/07 03:47:32  iang
// + Retested the 3-key data with SSLeay that has core 3-key code.
// + Upgraded the commentary to reflect.
//
// Revision 1.6  1997/12/21 20:24:03  iang
// + Ignore case so that it goes through without errors ...
//
// Revision 1.5  1997/12/21 16:03:48  iang
// + Dropped comment only.
//
// Revision 1.4  1997/12/21 15:39:29  iang
// + DES_EDE3 data was wrong, encrypt order was reversed.
// + Rewritten for BaseTest so exits work properly.
//
// Revision 1.3  1997/12/07 10:44:17  iang
// + Changed to DES-EDE3 as that is the naming convention?   check.
//
// Revision 1.2  1997/12/07 09:35:39  iang
// + Compile errors removed.
//
// Revision 1.1  1997/12/07 09:19:26  iang
// + Written.  data is from secret kit, generated from des.c.  needs confirming.
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

/**
 * Tests the output of the DES_EDE3 block cipher algorithm implementation
 * against pre-computed output for a given set of reference input.
 * <p>
 * To generate 3 key data, a hacked version of Eric Young's
 * des.c program was used.  There is no known certification data.
 * <i>Caveat Cryptographer</i> - the test program and the data
 * were generated locally.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 */
class TestDES_EDE3
extends BaseTest
{

// Variables and constants
//...........................................................................

    /**
     * To generate some 2-key test data using the crypto/des/des.c
     * program in SSLeay0-8-1, I did this (Pentium/FreeBSD):
     * <pre>
     *      echo -n "somedata" | dd conv=swab | hexdump -x
     * </pre>
     * Then, to test it with a hex-key of X, I did this:
     * <pre>
     *      cc -o des des.c ../../libcrypto.a
     *      echo -n "somedata" |
     *           des -b -3 -h -e  -k "X"  |
     *           dd conv=swab bs=8 count=1 |hexdump -x
     * </pre>
     * 
     * The 3 key data version of des.c is a local hack, that adds a
     * a -T flag to invoke the full 3 key mode.  email for details.
     * The core crypto des code of SSLeay0.8.1 is now 3 key, it is
     * just the des.c user program that lacks, so there is a fair
     * amount of confidence in the generated data.
     */
    private static final String[][]
    testData1 = {
          {     // same key ==> DES
                "010101010101010101010101010101010101010101010101",
                "95F8A5E5DD31D900", "8000000000000000" },
          {     // same key ==> DES
                "010101010101010101010101010101010101010101010101",
                "9D64555A9A10B852", "0000001000000000" },
          {     // same key ==> DES
                "3849674C2602319E3849674C2602319E3849674C2602319E",
                "51454B582DDF440A", "7178876E01F19B2A" },
          {     // same key ==> DES
                "04B915BA43FEB5B604B915BA43FEB5B604B915BA43FEB5B6",
                "42FD443059577FA2", "AF37FB421F8C4095" },


          {     // for checking first phase of below, defers to same
                "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF",
                "736F6D6564617461", "3D124FE2198BA318" },
          {     // note k1 == k3
                "0123456789ABCDEF55555555555555550123456789ABCDEF",
                "736F6D6564617461",
                "FBABA1FF9D05E9B1"
          },

          //
          //  To generate 3 key data, I hacked into it to add a -T mode
          //  ... phone for details.
          //  Caveat Cryptographer - I generated the test program and the data!
	  //  The core des code is now 3 key within SSLeay, so that gives
	  //  a fair amount of confidence.
          //
          {
                "0123456789ABCDEF5555555555555555FEDCBA9876543210",
                "736F6D6564617461", // "somedata"
                "18d748e563620572"  // 
          },
          {
                "0352020767208217860287665908219864056ABDFEA93457",
                "7371756967676C65", // "squiggle"
                "c07d2a0fa566fa30"
          },
          {     // some of the weak(?) keys found in the test data
                "010101010101010180010101010101010101010101010102",
                "0000000000000000",  // dd if=/dev/zero bs=8 count=1 2>&-
                "e6e6dd5b7e722974"
          },
          {     // some of the weak(?) keys found in the test data
                "10461034899880209107D0158919010119079210981A0101",
                "0000000000000000",
                "e1ef62c332fe825b"
          },
    };


// main/test methods
//.............................................................................
    
    public static void main (String[] args) {
        new TestDES_EDE3().commandline(args);
    }

    protected void engineTest() throws Exception {
        byte[] ect, dct;
        String a, b;
        int good = 0;
        int fails = 0;
        int i;
        String[][] data = testData1;

        Cipher alg = Cipher.getInstance("DES-EDE3", "Cryptix");

        setExpectedPasses(2 * data.length);

        // out.println("\nDES_EDE3 try self_test()");
        // alg.self_test();
        // passIf(true, "Self Test (no diags)");   // passed if no exception

        for (i = 0; i < data.length; i++) {
            out.println("     key:" + data[i][0]);
            RawSecretKey key = new RawSecretKey(
                    "DES_EDE3", Hex.fromString(data[i][0]));
            alg.initEncrypt(key);

            ect = alg.crypt(Hex.fromString(data[i][1]));
            a = Hex.toString(ect);
            alg.initDecrypt(key);
            dct = alg.crypt(ect);
            b = Hex.toString(dct);
            
            out.println("     p:" + data[i][1] +
                          " enc:" + a + " calc:" + data[i][2]);
            boolean ok = a.equalsIgnoreCase(data[i][2]);
            passIf(ok, "Data Set #" + (i+1));
            if (ok) {
                out.println( "   * Encrypt good" );
            } else {
                out.println("===> Encrypt (#"+ ++fails+") FAILED <===");
            }
        
            out.println("   enc:" + Hex.toString(ect) +
                          " dec:" + b + " calc:" + data[i][1]);
            ok = b.equalsIgnoreCase(data[i][1]);
            passIf(ok, "Data Set #" + (i+1));
            if (ok)
                out.println("   * Decrypt good");
            else
                out.println("===> Decrypt (#"+ ++fails+") FAILED  <===");
            out.println("");
        }

        out.println("\nDES_EDE3 succeeded (" + i + " data tests)");

    }

}
