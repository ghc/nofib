// $Id: SHA0.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: SHA0.java,v $
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
// Revision 1.7  1999/07/13 18:13:22  edwin
// Fixed cloning.
// This is a fix to: http://130.89.235.121/root/cryptix/old/cryptix/FAQ.html#bj_blockmessagedigest_clone
//
// Revision 1.6  1998/01/05 01:33:59  iang
// BUG: missing the engineUpdate() method.
//
// Revision 1.5  1997/12/16 21:58:25  iang
// MD5, SHA{01} debugged, got working, internal self_tests ok.
// BlockMessageDigest.bitcount() made long, was int, check calling
// where it is assumed by digest algorithms to be a long.
//
// Revision 1.4  1997/12/07 07:25:29  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/11/30  hopwood
// + Added references in documentation.
//
// Revision 1.3  1997/11/20 19:36:30  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.2  1997/11/10 07:31:32  raif
// + Removed reference to core.Cryptix.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.0.0  1997/08/27  David Hopwood
// + Split from cryptix.security.SHA0.
//
// Revision 0.2.5.2  1997/06/29  David Hopwood
// + Changes to support JCA.
// + Deprecated the static convenience methods.
//
// Revision 0.2.5.1  1997/03/15  Jill Baker
// + Moved this file here from old namespace.
//
// Revision 0.2.5.0  1997/02/24  Original author not stated
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1995-1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.md;

import cryptix.util.core.Debug;
import cryptix.util.core.Hex;

import java.io.PrintWriter;
import java.security.MessageDigest;

/**
 * This class implements the SHA-0 message digest algorithm
 * (not to be confused with the revised SHA-1 algorithm). SHA-1
 * is preferred for new applications.
 * <p>
 * <b>BUG</b>: The update method is missing.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> NIST FIPS PUB 180,
 *        "Secure Hash Standard",
 *        U.S. Department of Commerce, May 1993.
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1995-1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author Systemics Ltd
 * @author David Hopwood
 * @since  Cryptix 2.2.2
 */
public final class SHA0
extends BlockMessageDigest
implements Cloneable
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("SHA-0") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("SHA-0: " + s); }


// SHA-0 constants and variables
//...........................................................................

    /**
     * Length of the final hash (in bytes).
     */
    public static final int HASH_LENGTH = 20;

    /**
     * Length of a block (i.e. the number of bytes hashed in every transform).
     */
    public static final int DATA_LENGTH = 64;

    private int[] data;
    private int[] digest;
    private byte[] tmp;
    private int[] w;

    /**
     * Returns the length of the hash (in bytes).
     */
    protected int engineGetDigestLength() {
        return HASH_LENGTH;
    }

    /**
     * Returns the length of the data (in bytes) hashed in every transform.
     */
    protected int engineGetDataLength() {
        return DATA_LENGTH;
    }

    /**
     * The public constructor.
     */
    public SHA0()
    {
        super("SHA-0");
        java_init();
        reset();
    }

    private void java_init()
    {
        digest = new int[HASH_LENGTH/4];
        data = new int[DATA_LENGTH/4];
        tmp = new byte[DATA_LENGTH];
        w = new int[80];
    }

    /**
     *    This constructor is here to implement cloneability of this class.
     */
    private SHA0 (SHA0 md) {
        this();
        data = (int[])md.data.clone();
        digest = (int[])md.digest.clone();
        tmp = (byte[])md.tmp.clone();
        w = (int[])md.w.clone();
    }

    /**
     * Returns a copy of this MD object.
     */
    public Object clone() { return new SHA0(this); }

    /**
     * Initializes (resets) the message digest.
     */
    protected void engineReset()
    {
        super.engineReset();
        java_reset();
    }

    private void java_reset()
    {
        digest[0] = 0x67452301;
        digest[1] = 0xefcdab89;
        digest[2] = 0x98badcfe;
        digest[3] = 0x10325476;
        digest[4] = 0xc3d2e1f0;
    }

    /**
     * Adds data to the message digest.
     *
     * @param data    The data to be added.
     * @param offset  The start of the data in the array.
     * @param length  The amount of data to add.
     */
    protected void engineTransform(byte[] in)
    {
        java_transform(in);
    }

    private void java_transform(byte[] in)
    {
        byte2int(in, 0, data, 0, DATA_LENGTH/4);
        transform(data);
    }

    /**
     * Returns the digest of the data added and resets the digest.
     * @return    the digest of all the data added to the message digest as a byte array.
     */
    protected byte[] engineDigest(byte[] in, int length)
    {
        byte b[] = java_digest(in, length);
        engineReset();
        return b;
    }

    private byte[] java_digest(byte[] in, int pos)
    {
        if (pos != 0) System.arraycopy(in, 0, tmp, 0, pos);

        tmp[pos++] = (byte)0x80;

        if (pos > DATA_LENGTH - 8)
        {
            while (pos < DATA_LENGTH)
                tmp[pos++] = 0;

            byte2int(tmp, 0, data, 0, DATA_LENGTH/4);
            transform(data);
            pos = 0;
        }

        while (pos < DATA_LENGTH - 8)
            tmp[pos++] = 0;

        byte2int(tmp, 0, data, 0, (DATA_LENGTH/4)-2);

        // Big endian
        // WARNING: int>>>32 != 0 !!!
        // bitcount() used to return a long, now it's an int.
        long bc = bitcount();
        data[14] = (int) (bc>>>32); 
        data[15] = (int) bc;

        transform(data);

        byte buf[] = new byte[HASH_LENGTH];

        // Big endian
        int off = 0;
        for (int i = 0; i < HASH_LENGTH/4; ++i) {
            int d = digest[i];
            buf[off++] = (byte) (d>>>24);
            buf[off++] = (byte) (d>>>16);
            buf[off++] = (byte) (d>>>8);
            buf[off++] = (byte)  d;
        }
        return buf;
    }


// SHA-0 transform routines
//...........................................................................

    private static int f1(int a, int b, int c) { return (c^(a&(b^c))) + 0x5A827999; }
    private static int f2(int a, int b, int c) { return (a^b^c) + 0x6ED9EBA1; }
    private static int f3(int a, int b, int c) { return ((a&b)|(c&(a|b))) + 0x8F1BBCDC; }
    private static int f4(int a, int b, int c) { return (a^b^c) + 0xCA62C1D6; }

    private void transform (int[] X)
    {
        int A = digest[0];
        int B = digest[1];
        int C = digest[2];
        int D = digest[3];
        int E = digest[4];

        int W[] = w;
        for (int i=0; i<16; i++)
        {
            W[i] = X[i];
        }
        for (int i=16; i<80; i++)
        {
            int j = W[i-16] ^ W[i-14] ^ W[i-8] ^ W[i-3];
            W[i] = j;
//            if (revised)
//                W[i] = (j << 1) | (j >>> -1);
        }


        E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[0]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[1]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[2]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[3]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[4]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[5]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[6]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[7]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[8]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[9]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[10]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[11]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[12]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[13]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[14]; C =((C << 30)|(C >>> -30));
// System.out.println("A = "+Integer.toString(A, 16));
// System.out.println("B = "+Integer.toString(B, 16));
// System.out.println("C = "+Integer.toString(C, 16));
// System.out.println("D = "+Integer.toString(D, 16));
// System.out.println("E = "+Integer.toString(E, 16));

// System.out.println("W[15] = "+Integer.toString(W[15], 16));
// System.out.println("W[16] = "+Integer.toString(W[16], 16));
// System.out.println("W[17] = "+Integer.toString(W[17], 16));
// System.out.println("W[18] = "+Integer.toString(W[18], 16));
// System.out.println("W[19] = "+Integer.toString(W[19], 16));
        E += ((A << 5)|(A >>> -5)) + f1(B, C, D) + W[15]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f1(A, B, C) + W[16]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f1(E, A, B) + W[17]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f1(D, E, A) + W[18]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f1(C, D, E) + W[19]; C =((C << 30)|(C >>> -30));
// System.out.println("A = "+Integer.toString(A, 16));
// System.out.println("B = "+Integer.toString(B, 16));
// System.out.println("C = "+Integer.toString(C, 16));
// System.out.println("D = "+Integer.toString(D, 16));
// System.out.println("E = "+Integer.toString(E, 16));

        E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[20]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[21]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[22]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[23]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[24]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[25]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[26]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[27]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[28]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[29]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[30]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[31]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[32]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[33]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[34]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f2(B, C, D) + W[35]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f2(A, B, C) + W[36]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f2(E, A, B) + W[37]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f2(D, E, A) + W[38]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f2(C, D, E) + W[39]; C =((C << 30)|(C >>> -30));

        E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[40]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[41]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[42]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[43]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[44]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[45]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[46]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[47]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[48]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[49]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[50]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[51]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[52]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[53]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[54]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f3(B, C, D) + W[55]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f3(A, B, C) + W[56]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f3(E, A, B) + W[57]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f3(D, E, A) + W[58]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f3(C, D, E) + W[59]; C =((C << 30)|(C >>> -30));

        E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[60]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[61]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[62]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[63]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[64]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[65]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[66]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[67]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[68]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[69]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[70]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[71]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[72]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[73]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[74]; C =((C << 30)|(C >>> -30));
        E += ((A << 5)|(A >>> -5)) + f4(B, C, D) + W[75]; B =((B << 30)|(B >>> -30));
        D += ((E << 5)|(E >>> -5)) + f4(A, B, C) + W[76]; A =((A << 30)|(A >>> -30));
        C += ((D << 5)|(D >>> -5)) + f4(E, A, B) + W[77]; E =((E << 30)|(E >>> -30));
        B += ((C << 5)|(C >>> -5)) + f4(D, E, A) + W[78]; D =((D << 30)|(D >>> -30));
        A += ((B << 5)|(B >>> -5)) + f4(C, D, E) + W[79]; C =((C << 30)|(C >>> -30));

// System.out.println("A = "+Integer.toString(A, 16));
// System.out.println("B = "+Integer.toString(B, 16));
// System.out.println("C = "+Integer.toString(C, 16));
// System.out.println("D = "+Integer.toString(D, 16));
// System.out.println("E = "+Integer.toString(E, 16));
        digest[0] += A;
        digest[1] += B;
        digest[2] += C;
        digest[3] += D;
        digest[4] += E;

// for (int i=0; i<5; i++) { System.out.println("X["+i+"] = "+Integer.toString(digest[i], 16)); }
    }

    // why was this public?
    // Note: parameter order changed to be consistent with System.arraycopy.
    private static void byte2int(byte[] src, int srcOffset,
                                 int[] dst, int dstOffset, int length)
    {
        while (length-- > 0)
        {
            // Big endian
            dst[dstOffset++] = (src[srcOffset++]         << 24) |
                               ((src[srcOffset++] & 0xFF) << 16) |
                               ((src[srcOffset++] & 0xFF) <<  8) |
                                (src[srcOffset++] & 0xFF);
        }
    }
// // // // // // // // // // // //   T E S T   // // // // // // // //

/**
 * Entry point for <code>self_test</code>.
 */
public static final void
main(String argv[])
{
        try { self_test(); }
        catch(Throwable t) { t.printStackTrace(); System.exit(1);}
}

//
// Using Hex format 0xAB results in needing casts.
//
private static String[] texts =
{
  "",                                 // val data 1
  "abc",                              // val data 1
  "message digest",                   // val data 6
}; 
private static byte[][] hashs =
{
 {-7,108,-22,25,-118,-47,-35,86,23,-84,8,74,61,-110,-58,16,119,8,-64,-17,},
 {1,100,-72,-87,20,-51,42,94,116,-60,-9,-1,8,44,77,-105,-15,-19,-8,-128,},
 {-63,-80,-14,34,-47,80,-21,-71,-86,54,-92,12,-81,-36,-117,-53,-19,-125,11,20,},
}; 


/**
 * Do some basic tests.
 * Three of the validation data are included only, no output,
 * success or exception.
 * If you want more, write a test program!
 * @see cryptix.test.TestSHA0
 */     
public static final void
self_test()
throws Exception
{
    int length = hashs[0].length; 
    for (int i = 0; i < texts.length; i++)
    {
        SHA0 hash = new SHA0();
        byte[] text = texts[i].getBytes();

        for (int j = 0; j < texts[i].length(); j++)
            hash.engineUpdate(text[j]);

        if (notEquals(hash.engineDigest(), hashs[i]))
            throw new Exception("hash #"+ i +" failed");

    }       
}
            
private static final boolean
notEquals(byte[] a, byte[] b) 
{
    for (int i = 0; i < a.length; i++)
    {       
        if (a[i] != b[i])
            return true;
        // if (a[i] == -125) // force a trip up
        //     return true;
    }
    return false; 
}
}
