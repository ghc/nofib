// $Id: TestIDEA.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestIDEA.java,v $
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
// Revision 1.5  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.4  1998/01/28 05:40:00  hopwood
// + Major update of test classes.
//
// Revision 1.3  1997/11/22 07:05:41  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/22 05:59:02  iang
// core.util ==> util.core
//
// Revision 1.1  1997/11/07 05:53:26  raif
// + adapted to latest API.
//
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
 * Tests the output of the IDEA block cipher algorithm implementation
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
class TestIDEA
extends BaseTest
{

// Variables and constants
//...........................................................................

    private static final String[][]
    testData1 = {
    //    key                                 plain text          cipher text (ECB)
    //    ................................    ................    ..................
        {"00010002000300040005000600070008", "0000000100020003", "11FBED2B01986DE5"},
        {"00010002000300040005000600070008", "0102030405060708", "540E5FEA18C2F8B1"},
        {"00010002000300040005000600070008", "0019324B647D96AF", "9F0A0AB6E10CED78"},
        {"00010002000300040005000600070008", "F5202D5B9C671B08", "CF18FD7355E2C5C5"},
        {"00010002000300040005000600070008", "FAE6D2BEAA96826E", "85DF52005608193D"},
        {"00010002000300040005000600070008", "0A141E28323C4650", "2F7DE750212FB734"},
        {"00010002000300040005000600070008", "050A0F14191E2328", "7B7314925DE59C09"},
        {"0005000A000F00140019001E00230028", "0102030405060708", "3EC04780BEFF6E20"},
        {"3A984E2000195DB32EE501C8C47CEA60", "0102030405060708", "97BCD8200780DA86"},
        {"006400C8012C019001F4025802BC0320", "05320A6414C819FA", "65BE87E7A2538AED"},
        {"9D4075C103BC322AFB03E7BE6AB30006", "0808080808080808", "F5DB1AC45E5EF9F9"}
    },

    testData2 = {
    //    key                                 plain text
    //    ................................    ................
        {"00010002000300040005000600070008", "0000000100020003"},
        {"00010002000300040005000600070008", "01020304050607084E"},
        {"00010002000300040005000600070008", "0019324B647D96AF4E2019"},
        {"00010002000300040005000600070008", "F5202D5B9C671B084E2009"},
        {"00010002000300040005000600070008", "FAE6D2BEAA96826E4E200019"},
        {"00010002000300040005000600070008", "0A141E28323C46504E200019"},
        {"00010002000300040005000600070008", "050A0F14191E23284E2019"},
        {"0005000A000F00140019001E00230028", "01020304050607080A000F"},
        {"3A984E2000195DB32EE501C8C47CEA60", "0102030405060708EA60"},
        {"006400C8012C019001F4025802BC0320", "05320A6414C819FA025802BC0320"},
        {"9D4075C103BC322AFB03E7BE6AB30006", "08080808080808086AB30006"}
    };



// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestIDEA().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(66);

        out.println("*** IDEA in ECB mode:\n");
        Cipher alg = Cipher.getInstance("IDEA", "Cryptix");
        test1(alg, testData1);

        out.println("\n*** IDEA in CFB mode:\n");
        alg = Cipher.getInstance("IDEA/CFB", "Cryptix");
        test2(alg, testData2);

        out.println("\n*** IDEA in OFB mode:\n");
        alg = Cipher.getInstance("IDEA/OFB", "Cryptix");
        test2(alg, testData2);
            
        out.println("\n*** IDEA in CFB-PGP mode:\n");
        alg = Cipher.getInstance("IDEA/CFB-PGP", "Cryptix");
        test2(alg, testData2);
            
        out.println("\n*** IDEA in CBC mode with PKCS#5 padding:\n");
        alg = Cipher.getInstance("IDEA/CBC/PKCS#5", "Cryptix");
        test2(alg, testData2);
    }

    private void test1 (Cipher alg, String[][] data) throws Exception {
        byte[] ect, dct;
        String a, b;

        for (int i = 0; i < data.length; i++) {
            RawSecretKey key = new RawSecretKey(
                "IDEA", Hex.fromString(data[i][0]));
        
            alg.initEncrypt(key);
            ect = alg.crypt(Hex.fromString(data[i][1]));
            a = Hex.toString(ect);
            alg.initDecrypt(key);
            dct = alg.crypt(ect);
            b = Hex.toString(dct);
            
            out.println("\nplain:" + data[i][1] + " enc:" + a + " calc:" + data[i][2]);
            passIf(a.equals(data[i][2]), "IDEA encrypt");

            out.println("  enc:" + Hex.toString(ect) + " dec:" + b + " calc:" +
                data[i][1]);
            passIf(b.equals(data[i][1]), "IDEA decrypt");
        }
    }
    
    private void test2 (Cipher alg, String[][] data) throws Exception {
        byte[] pt, ect, dct;
        byte[] iv = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08};
        ((FeedbackCipher) alg).setInitializationVector(iv);
        for (int i = 0; i < data.length; i++) {
            RawSecretKey key = new RawSecretKey("IDEA", Hex.fromString(data[i][0]));

            alg.initEncrypt(key);
            pt = Hex.fromString(data[i][1]);
            ect = alg.crypt(pt);
            alg.initDecrypt(key);
            dct = alg.crypt(ect);
            
            out.println("\nplain:" + Hex.toString(pt) +
                " enc:" + Hex.toString(ect) +
                " dec:" + Hex.toString(dct));
            passIf(ArrayUtil.areEqual(pt, dct), "IDEA feedback");
        }
    }
}
