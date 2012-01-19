// $Id: TestHMAC.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestHMAC.java,v $
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
// Revision 1.4  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.3  1998/02/01 05:08:44  hopwood
// + Committed changes below.
//
// Revision 1.2.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.2  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.1  1998/01/12 04:09:01  hopwood
// + Added to CVS.
//
// Revision 0.1.0  1998/01/12  hopwood
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1998 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.CryptixException;
import cryptix.util.core.ArrayUtil;
import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import java.security.MessageDigest;
import xjava.security.Parameterized;

/**
 * Tests the output of the HMAC message authentication code implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * <b>Copyright</b> &copy; 1998
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 */
public class TestHMAC
extends BaseTest
{

// Test data
//................................................................................

    /**
     * This data comes from RFC 2202.
     */
    private static final String[][] tests = {
      { "'Hi There",                                // text
        "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",         // HMAC-MD5 key
        "9294727a3638bb1c13f48ef8158bfc9d",         // HMAC-MD5 result
        "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b", // HMAC-SHA-1 key
        "b617318655057264e28bc0b6fb378c8ef146be00"  // HMAC-SHA-1 result
      },
      { "'what do ya want for nothing?",
        "4a656665", // ASCII "Jefe"
        "750c783e6ab0b503eaa86e310a5db738",
        "4a656665", // ASCII "Jefe"
        "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
      },
      { "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD" +
          "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "56be34521d144c88dbb8c733f0e8b3f6",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "125d7342b9ac11cd91a39af48aa17b4f63f175d3"
      },
      { "CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD" +
          "CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD",
        "0102030405060708090a0b0c0d0e0f10111213141516171819",
        "697eaf0aca3a3aea3a75164746ffaa79",
        "0102030405060708090a0b0c0d0e0f10111213141516171819",
        "4c9007f4026250c6bc8414f9bf50c86c2d7235da"
      },
      { "'Test With Truncation",
        // note: we don't support truncation, so these are the full-length outputs
        "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c",
        "56461ef2342edc00f9bab995690efd4c",
        "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c",
        "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"
      },
      { "'Test Using Larger Than Block-Size Key - Hash Key First",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" +
          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" +
          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "aa4ae5e15272d00e95705637ce8a3b55ed402112"
      },
      { "'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" +
          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "6f630fad67cda0ee1fb1f562db3aa53e",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" +
          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "e8e99d0f45237d786d6bbaa7965c7808bbff1a91"
      }
    };


// Test methods
//...........................................................................

    public static void main (String[] args) {
        new TestHMAC().commandline(args);
    }

    /**
     * Test using the data from RFC 2202. Note that this will fail if the
     * MD5 or SHA-1 MessageDigest classes are incorrect.
     */
    protected void engineTest() throws Exception {
        setExpectedPasses(14);

        MessageDigest hmac_md5 = MessageDigest.getInstance("HMAC-MD5", "Cryptix");
        MessageDigest hmac_sha1 = MessageDigest.getInstance("HMAC-SHA-1", "Cryptix");

        byte[] tmp, text, md5key, md5mac, sha1key, sha1mac;

        for (int i = 0; i < tests.length; i++) {
            String ts = tests[i][0];
            if (ts.startsWith("'")) {
                text = new byte[ts.length()-1];
                for (int j = 0; j < text.length; j++)
                    text[j] = (byte) ts.charAt(j+1);
            } else {
                text = Hex.fromString(ts);
            }
            md5key = Hex.fromString(tests[i][1]);
            md5mac = Hex.fromString(tests[i][2]);
            sha1key = Hex.fromString(tests[i][3]);
            sha1mac = Hex.fromString(tests[i][4]);

            ((Parameterized) hmac_md5).setParameter("key", md5key);
            tmp = hmac_md5.digest(text);
            passIf(ArrayUtil.areEqual(md5mac, tmp), "HMAC-MD5 #" + (i+1));

            ((Parameterized) hmac_sha1).setParameter("key", sha1key);
            tmp = hmac_sha1.digest(text);
            passIf(ArrayUtil.areEqual(sha1mac, tmp), "HMAC-SHA-1 #" + (i+1));
        }
    }
}
