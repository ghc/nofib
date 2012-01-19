// $Id: TestBlowfish.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestBlowfish.java,v $
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
// Revision 1.3.1  1998/01/13  hopwood
// + Changed to use BaseTest API.
//
// Revision 1.3  1997/11/22 07:05:40  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/22 05:59:02  iang
// core.util ==> util.core
//
// Revision 1.1  1997/11/07 05:53:25  raif
// + adapted to latest API.
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
import java.util.Date;

/**
 * Tests the output of the Blowfish cipher algorithm implementation against
 * certified pre-computed output for a given set of reference input.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestBlowfish
extends BaseTest
{

// Variables and constants
//...........................................................................

    private Cipher alg;

    private static final String[][]
    testData1 = {
    //   key                 plain text          cipher text (ECB)
    //   ..................  ..................  ..................
        {"0000000000000000", "0000000000000000", "4EF997456198DD78"},
        {"FFFFFFFFFFFFFFFF", "FFFFFFFFFFFFFFFF", "51866FD5B85ECB8A"},
        {"3000000000000000", "1000000000000001", "7D856F9A613063F2"},
        {"1111111111111111", "1111111111111111", "2466DD878B963C9D"},
        {"0123456789ABCDEF", "1111111111111111", "61F9C3802281B096"},
        {"1111111111111111", "0123456789ABCDEF", "7D0CC630AFDA1EC7"},
        {"FEDCBA9876543210", "0123456789ABCDEF", "0ACEAB0FC6A0A28D"},
        {"7CA110454A1A6E57", "01A1D6D039776742", "59C68245EB05282B"},
        {"0131D9619DC1376E", "5CD54CA83DEF57DA", "B1B8CC0B250F09A0"},
        {"07A1133E4A0B2686", "0248D43806F67172", "1730E5778BEA1DA4"},
        {"3849674C2602319E", "51454B582DDF440A", "A25E7856CF2651EB"},
        {"04B915BA43FEB5B6", "42FD443059577FA2", "353882B109CE8F1A"},
        {"0113B970FD34F2CE", "059B5E0851CF143A", "48F4D0884C379918"},
        {"0170F175468FB5E6", "0756D8E0774761D2", "432193B78951FC98"},
        {"43297FAD38E373FE", "762514B829BF486A", "13F04154D69D1AE5"},
        {"07A7137045DA2A16", "3BDD119049372802", "2EEDDA93FFD39C79"},
        {"04689104C2FD3B2F", "26955F6835AF609A", "D887E0393C2DA6E3"},
        {"37D06BB516CB7546", "164D5E404F275232", "5F99D04F5B163969"},
        {"1F08260D1AC2465E", "6B056E18759F5CCA", "4A057A3B24D3977B"},
        {"584023641ABA6176", "004BD6EF09176062", "452031C1E4FADA8E"},
        {"025816164629B007", "480D39006EE762F2", "7555AE39F59B87BD"},
        {"49793EBC79B3258F", "437540C8698F3CFA", "53C55F9CB49FC019"},
        {"4FB05E1515AB73A7", "072D43A077075292", "7A8E7BFA937E89A3"},
        {"49E95D6D4CA229BF", "02FE55778117F12A", "CF9C5D7A4986ADB5"},
        {"018310DC409B26D6", "1D9D5C5018F728C2", "D1ABB290658BC778"},
        {"1C587F1C13924FEF", "305532286D6F295A", "55CB3774D13EF201"},
        {"0101010101010101", "0123456789ABCDEF", "FA34EC4847B268B2"},
        {"1F1F1F1F0E0E0E0E", "0123456789ABCDEF", "A790795108EA3CAE"},
        {"E0FEE0FEF1FEF1FE", "0123456789ABCDEF", "C39E072D9FAC631D"},
        {"0000000000000000", "FFFFFFFFFFFFFFFF", "014933E0CDAFF6E4"},
        {"FFFFFFFFFFFFFFFF", "0000000000000000", "F21E9A77B71C49BC"},
        {"0123456789ABCDEF", "0000000000000000", "245946885754369A"},
        {"FEDCBA9876543210", "FFFFFFFFFFFFFFFF", "6B5C5A9C5D9E0A5A"}
    },
    testData2 = {
    //   cipher text (ECB)   key
    //   ..................  ......................................
/* BlowfishKeyGenerator now accepts 5 bytes as minimum key length -----------
        {"F9AD597C49DB005E", "F0"},
        {"E91D21C1D961A6D6", "F0E1"},
        {"E9C2B70A1BC65CF3", "F0E1D2"},
        {"BE1E639408640F05", "F0E1D2C3"},
-------------------------------------------------------------------------- */
        {"B39E44481BDB1E6E", "F0E1D2C3B4"},
        {"9457AA83B1928C0D", "F0E1D2C3B4A5"},
        {"8BB77032F960629D", "F0E1D2C3B4A596"},
        {"E87A244E2CC85E82", "F0E1D2C3B4A59687"},
        {"15750E7A4F4EC577", "F0E1D2C3B4A5968778"},
        {"122BA70B3AB64AE0", "F0E1D2C3B4A596877869"},
        {"3A833C9AFFC537F6", "F0E1D2C3B4A5968778695A"},
        {"9409DA87A90F6BF2", "F0E1D2C3B4A5968778695A4B"},
        {"884F80625060B8B4", "F0E1D2C3B4A5968778695A4B3C"},
        {"1F85031C19E11968", "F0E1D2C3B4A5968778695A4B3C2D"},
        {"79D9373A714CA34F", "F0E1D2C3B4A5968778695A4B3C2D1E"},
        {"93142887EE3BE15C", "F0E1D2C3B4A5968778695A4B3C2D1E0F"},
        {"03429E838CE2D14B", "F0E1D2C3B4A5968778695A4B3C2D1E0F00"},
        {"A4299E27469FF67B", "F0E1D2C3B4A5968778695A4B3C2D1E0F0011"},
        {"AFD5AED1C1BC96A8", "F0E1D2C3B4A5968778695A4B3C2D1E0F001122"},
        {"10851C0E3858DA9F", "F0E1D2C3B4A5968778695A4B3C2D1E0F00112233"},
        {"E6F51ED79B9DB21F", "F0E1D2C3B4A5968778695A4B3C2D1E0F0011223344"},
        {"64A6E14AFD36B46F", "F0E1D2C3B4A5968778695A4B3C2D1E0F001122334455"},
        {"80C7D7D45A5479AD", "F0E1D2C3B4A5968778695A4B3C2D1E0F00112233445566"},
        {"05044B62FA52D080", "F0E1D2C3B4A5968778695A4B3C2D1E0F0011223344556677"}
    };


// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestBlowfish().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(90);

        alg = Cipher.getInstance("Blowfish", "Cryptix");
        test1();
        test2();
        test3();
        test4();
    }

    private void test1() throws Exception {
        out.println("*** Blowfish (16-round) in ECB mode 1/2:\n");
        byte[] ect, dct;
        String a, b;

        for (int i = 0; i < testData1.length; i++) {
            RawSecretKey key = new
                RawSecretKey("Blowfish", Hex.fromString(testData1[i][0]));
        
            alg.initEncrypt(key);
            ect = alg.crypt(Hex.fromString(testData1[i][1]));
            a = Hex.toString(ect);
            alg.initDecrypt(key);
            dct = alg.crypt(ect);
            b = Hex.toString(dct);
            
            out.print("  plain: " + testData1[i][1] +
                ", cipher: " + a + ", cert: " + testData1[i][2]);

            passIf(a.equals(testData1[i][2]), "Test #1/Enc " + (i+1));
        
            out.print(" cipher: " + Hex.toString(ect) +
                ",  plain: " + b + ", cert: " + testData1[i][1]);

            passIf(b.equals(testData1[i][1]), "Test #1/Dec " + (i+1));
        }
    }
    
    private void test2() throws Exception {
        out.println("\n*** Blowfish (16-round) in ECB mode 2/2:\n");
        byte[] ect, input = Hex.fromString("FEDCBA9876543210");
        String a;

        for (int i = 0; i < testData2.length; i++) {
            byte[] k = Hex.fromString(testData2[i][1]);
            RawSecretKey key = new RawSecretKey("Blowfish", k);
        
            alg.initEncrypt(key);
            ect = alg.crypt(input);
            a = Hex.toString(ect);
            
            out.print("  plain: FEDCBA9876543210, cipher: " + a +
                " cert: " + testData2[i][0]);

            passIf(a.equals(testData2[i][0]), "Test #2/" + (i+1));
        }
    }
    
    private void test3() throws Exception {
        String
            a,
            in = "37363534333231204E6F77206973207468652074696D6520666F722000",
            o1 = "6B77B4D63006DEE605B156E27403979358DEB9E7154616D959F1652BD5FF92CC",
            o2 = "E73214A2822139CAF26ECF6D2EB9E76E3DA3DE04D1517200519D57A6C3",
            o3 = "E73214A2822139CA62B343CC5B65587310DD908D0C241B2263C2CF80DA";
        byte[]
            ect,
            input = Hex.fromString(in),
            iv = Hex.fromString("FEDCBA9876543210"),
            key = Hex.fromString("0123456789ABCDEFF0E1D2C3B4A59687");
        RawSecretKey k = new RawSecretKey("Blowfish", key);

        out.println("\n*** Blowfish (16-round) in CBC mode:\n");
        alg = Cipher.getInstance("Blowfish/CBC", "Cryptix");
        ((FeedbackCipher) alg).setInitializationVector(iv);
        alg.initEncrypt(k);
        // for CBC we need an input array whose length is x8
        // implicetly we'll pad with 0x00
        byte[] newIn = new byte[32];
        System.arraycopy(input, 0, newIn, 0, input.length);
        ect = alg.crypt(newIn);
        a = Hex.toString(ect);
        out.println("  plain: " + in);
        out.println(" cipher: " + a);
        out.println("   cert: " + o1);
        passIf(a.equals(o1), "Test #3/1");

        out.println("\n*** Blowfish (16-round) in CFB mode:\n");
        alg = Cipher.getInstance("Blowfish/CFB", "Cryptix");
        ((FeedbackCipher) alg).setInitializationVector(iv);
        alg.initEncrypt(k);
        ect = alg.crypt(input);
        a = Hex.toString(ect);
        out.println("  plain: " + in);
        out.println(" cipher: " + a);
        out.println("   cert: " + o2);
        passIf(a.equals(o2), "Test #3/2");

        out.println("\n*** Blowfish (16-round) in OFB mode:\n");
        alg = Cipher.getInstance("Blowfish/OFB", "Cryptix");
        ((FeedbackCipher) alg).setInitializationVector(iv);
        alg.initEncrypt(k);
        ect = alg.crypt(input);
        a = Hex.toString(ect);
        out.println("  plain: " + in);
        out.println(" cipher: " + a);
        out.println("   cert: " + o3);
        passIf(a.equals(o3), "Test #3/3");
    }

    /**
     * Encryption and Decryption speed test.
     * Uses last data triplet in test1 above.
     */
    private void test4 ()
    throws Exception {
        byte[]
            key = Hex.fromString("FEDCBA9876543210"),
            a = Hex.fromString("FFFFFFFFFFFFFFFF");
        RawSecretKey k = new RawSecretKey("Blowfish", key);

        alg.initEncrypt(k);

        out.println("\nSpeed test 100,000 x 8-byte:\n");
        out.println("...Encryption\n");
        out.println("      start date/time: " + new Date().toString());
        for (int i = 0; i < 100; i++)
            for (int j = 0; j < 1000; j++)
                alg.update(a, 0, 8, a, 0);
        out.println("     finish date/time: " + new Date().toString());
        
        alg.initDecrypt(k);
        out.println("\n...Decryption\n");
        out.println("      start date/time: " + new Date().toString());
        for (int i = 0; i < 100; i++)
            for (int j = 0; j < 1000; j++)
                alg.update(a, 0, 8, a, 0);
        out.println("     finish date/time: " + new Date().toString());

        out.println("\n result:");
        byte[] x = Hex.fromString("FFFFFFFFFFFFFFFF");
        out.println("  computed: " + Hex.dumpString(a));
        out.println(" certified: " + Hex.dumpString(x));
        passIf(ArrayUtil.areEqual(a, x), "Test #4");
    }
}
