// $Id: TestIJCE.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestIJCE.java,v $
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
// Revision 1.12  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.11  1998/02/01 05:08:45  hopwood
// + Committed changes below.
//
// Revision 1.10.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.10  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.9  1998/01/12 04:10:40  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.8  1997/12/03 03:38:45  hopwood
// + Committed changes below.
//
// Revision 1.7.1  1997/12/03  hopwood
// + Reversed changes in 1.7, because they mean that CipherIn/OutputStream
//   are not adequately tested with reads of different lengths.
//
// Revision 1.7  1997/12/02 08:50:12  raif
// + Added BufferedInput/OutputStream to CipherInput/OutputStream
//   in test2() to test and demonstrate use of buffering with new
//   CipherInputStream implementation.
//
// Revision 1.6  1997/12/02 03:20:51  hopwood
// + Committed changes below.
//
// Revision 1.5.1  1997/12/02  hopwood
// + Random numbers of bytes to be read in test2 were sometimes negative.
//
// Revision 1.5  1997/11/29 05:12:22  hopwood
// + Changes to use new test API (BaseTest).
//
// Revision 1.4  1997/11/22 07:05:41  raif
// *** empty log message ***
//
// Revision 1.3  1997/11/22 05:59:02  iang
// + core.util ==> util.core
//
// Revision 1.2  1997/11/10 07:31:33  raif
// + Incorporate support for Cipher.set/getParameter().
//
// Revision 1.1  1997/11/07 05:53:26  raif
// + Adapted to latest API.
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import xjava.security.Cipher;
import xjava.security.CipherInputStream;
import xjava.security.CipherOutputStream;
import xjava.security.FeedbackCipher;
import xjava.security.Mode;
import xjava.security.Parameterized;
import java.util.Random;

/**
 * Tests the current IJCE implementation using a SAFER cipher instance.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 * @author  David Hopwood
 */
public class TestIJCE
extends BaseTest
{

// Variables and constants
//...........................................................................

    private static byte[] input = new byte[16 * 18];
    private static final byte[]
        b = {
            (byte)0x42, (byte)0x43, (byte)0x1B, (byte)0xA4,
            (byte)0x0D, (byte)0x29, (byte)0x1F, (byte)0x81,
            (byte)0xD6, (byte)0x60, (byte)0x83, (byte)0xC6,
            (byte)0x05, (byte)0xD3, (byte)0xA4, (byte)0xD6},

        anIV = {
            (byte)0x74, (byte)0x53, (byte)0x6E, (byte)0xBD,
            (byte)0xC2, (byte)0x11, (byte)0x48, (byte)0x4A};

    private static final RawSecretKey aKey = new RawSecretKey("SAFER", b);


// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestIJCE().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(4);

        for (int i = 0; i < 2; i++) {
            for (int j = 1; j < 9; j++)
                input[16 * i + j + 7] = (byte)j;
        }
        for (int i = 0; i < 256; i++)
            input[32 + i] = (byte)i;

        out.println("input data\n" + Hex.dumpString(input));

        test1();
        test2();
    }

    /**
     * This is a special test really irrelevant to SAFER per se. It's
     * aim is to test the current implementation of the IJCE crypt and
     * update methods.
     * <p>
     * Basically it encrypts a bloc in one go with 'crypt' and again in
     * 2 chunks with 'update' + 'crypt' then compares both final outputs
     * for equality.
     */
    private void test1 ()
    throws Exception {
        out.println(
            "\nSAFER (K-128) in CBC mode (crypt vs. update):\n" +
            "Key = 42431BA40D291F81D66083C605D3A4D6, IV = 74536EBDC211484A\n");

        Cipher alg = Cipher.getInstance("SAFER/CBC/PKCS#7", "Cryptix");
        ((Parameterized) alg).setParameter("variant", "K-128");
        ((FeedbackCipher) alg).setInitializationVector(anIV);

        alg.initEncrypt(aKey);
        byte[] x = alg.crypt(input);
        int i = (input.length / 2) - 3;
        alg.initEncrypt(aKey);
        byte[] y = new byte[x.length];
        int j = alg.update(input, 0, i, y, 0);
        alg.crypt(input, i, input.length - i, y, j);
        compareIt(x, y);

        // now do the same in CFB mode
        out.println("\nSAFER (SK-128) in CFB mode (crypt vs. update):\n" +
            "Key = 42431BA40D291F81D66083C605D3A4D6; IV = 74536EBDC211484A\n");

        alg = Cipher.getInstance("SAFER/CFB");
        ((Parameterized) alg).setParameter("variant", "K-128");
        ((FeedbackCipher) alg).setInitializationVector(anIV);

        alg.initEncrypt(aKey);
        x = alg.crypt(input);
        i = (input.length / 2) - 3;
        alg.initEncrypt(aKey);
        y = new byte[x.length];
        j = alg.update(input, 0, i, y, 0);
        alg.crypt(input, i, input.length - i, y, j);
        compareIt(x, y);
    }

    /**
     * This one tests the CipherInputStream and CipherOutputStream
     * implementations.
     */
    private void test2 ()
    throws Exception {
        
        int[] data = {
  251, 221, 148,   3, 222,  99, 246, 105,   226,  42,  71,  84,  68,  15,  77, 253,
  159, 242, 171, 246, 112,  85,   1,  17,   201, 140,  36,  25, 241, 217, 207,  44,
   80, 203, 156, 123,   6, 117, 108,  33,   253, 118, 156,  50, 215,  83, 192, 211,
  192,  96, 132, 205, 118,  92, 206, 254,   202, 230, 171, 236,  51, 135, 233, 194,
   53,  57,  72,  89,   0, 227, 222,  39,   185,  94, 100, 159,  46, 224, 186, 134,
  232,  36,  54, 169,  81, 223, 150, 161,   105,  97, 100, 184,  37, 205, 111, 144,
   43, 156, 135, 129,  56,  57,   2, 115,    59, 120, 171,  26, 204,  97, 254,  53,
  164,  17, 135,  22,  23, 121,  49, 144,   173,  38, 181,  61, 227, 132, 145,  40,
   75,  69,  65, 164,  65,  64,  25,   9,   199,  38, 132, 165, 135,  97, 129, 144,
    4, 179, 112,  60, 182, 250, 222, 252,   248,  23,  59, 228, 191,   0,  56, 237,
  163,  20,  30, 104, 216,  45, 209, 254,   107, 188, 169,  97, 173,  41, 205, 249,
  164, 128, 120, 201, 215, 188,  35, 103,   108, 202, 174, 140, 138,  95, 241,  71,
   83, 199,  70, 162,  73, 226,  87,  84,    43,  17, 178, 203,  21, 250, 128, 159,
  137, 190, 174,  11, 215,  22, 155,  34,    34, 169,  13,  95, 208, 118, 231,  89,
   88, 179,   4,  29, 176,  17,  64, 235,    58,  88, 114,  65,  57,  96, 211, 163,
  188,  18, 100,   4,  91,  45, 233,  80,   217,  90,  45,  93, 144,  82,  60, 203,
  224,  81, 113, 231, 159, 185, 171, 128,   109, 224, 154, 141,  33,  66, 255,  44,
  112,  41, 125, 205, 154, 174, 226, 225,   173, 243, 107, 172, 214, 234,  22, 186
    };

        out.println("\nSAFER (K-128) in CBC mode:\n" +
            "Key = 42431BA40D291F81D66083C605D3A4D6, IV = 74536EBDC211484A\n");
        byte[] output = new byte[data.length];
        for (int i = 0; i < data.length; i++) output[i] = (byte)data[i];

        out.println("Checking an encryptor input stream...\n");
        Cipher alg = Cipher.getInstance("SAFER/CBC", "Cryptix");
        ((Parameterized) alg).setParameter("variant", "K-128");
        ((FeedbackCipher) alg).setInitializationVector(anIV);

        alg.initEncrypt(aKey);
        CipherInputStream cis =
            new CipherInputStream(new ByteArrayInputStream(input), alg);
        byte[] x = new byte[288];
        Random r = new Random();
        int len = 0, soFar = 0;
        while (len != -1) {
            soFar += len;
            len = cis.read(x, soFar, r.nextInt() & 0x0F);
        }
        cis.close();
        compareIt(x, output);

        out.println("\nChecking a decryptor output stream...\n");
        alg.initDecrypt(aKey);
        ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
        CipherOutputStream cos = new CipherOutputStream(baos2, alg);
        cos.write(x, 0, 100);
        cos.write(x, 100, 100);
        cos.write(x, 200, 88);
        cos.flush();
        cos.close();
        compareIt(baos2.toByteArray(), input);
    }

    private void compareIt(byte[] o1, byte[] o2) {
        out.println(" computed output: " + Hex.dumpString(o1));
        out.println("certified output: " + Hex.dumpString(o2));
        passIf(ArrayUtil.areEqual(o1, o2), "IJCE test");
    }
}
