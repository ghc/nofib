// $Id: MD5.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: MD5.java,v $
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
// Revision 1.8  1999/07/13 18:13:22  edwin
// Fixed cloning.
// This is a fix to: http://130.89.235.121/root/cryptix/old/cryptix/FAQ.html#bj_blockmessagedigest_clone
//
// Revision 1.7  1998/01/05 03:41:19  iang
// Added references only.
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
// + Removed references to core.Cryptix.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.0.0  1997/08/27  David Hopwood
// + Split from cryptix.security.MD5.
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

import java.io.PrintWriter;
import java.security.MessageDigest;

import cryptix.util.core.Debug;
import cryptix.util.core.Hex;

/**
 * This class implements the MD5 message digest algorithm.
 * <p>
 * <b>BUG</b>: The update method is missing.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Ronald L. Rivest,
 *        "<a href="http://www.roxen.com/rfc/rfc1321.html">
 *        The MD5 Message-Digest Algorithm</a>",
 *        IETF RFC-1321 (informational).
 *   <li> Bruce Schneier,
 *        "Section 18.5 MD5,"
 *        <cite>Applied Cryptography, 2nd edition</cite>,
 *        John Wiley &amp; Sons, 1996
 *        <p>
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1995-1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 * @author  David Hopwood
 * @since   Cryptix 2.2
 */
public final class MD5 
extends BlockMessageDigest
implements Cloneable
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("MD5") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("MD5: " + s); }


// MD5 constants and variables
//...........................................................................

    /** Length of the final hash (in bytes). */
    private static final int HASH_LENGTH = 16;

    /** Length of a block (the number of bytes hashed in every transform). */
    private static final int DATA_LENGTH = 64;

    private int[] data;
    private int[] digest;
    private byte[] tmp;

    /** Returns the length of the hash (in bytes). */
    protected int engineGetDigestLength() { return HASH_LENGTH; }

    /** Returns the length of the data (in bytes) hashed in every transform. */
    protected int engineGetDataLength() { return DATA_LENGTH; }

    /**
     * The public constructor.
     */
    public MD5()
    {
        super("MD5");
        java_init();
        reset();
    }

    private void java_init() {
        digest = new int[HASH_LENGTH/4];
        data = new int[DATA_LENGTH/4];
        tmp = new byte[DATA_LENGTH];
    }

    /**
     *    This constructor is here to implement cloneability of this class.
     */
    private MD5 (MD5 md) {
        this();
        data = (int[])md.data.clone();
        digest = (int[])md.digest.clone();
        tmp = (byte[])md.tmp.clone();
    }

    /**
     * Returns a copy of this MD object.
     */
    public Object clone() { return new MD5(this); }

    /**
     * Initializes (resets) the message digest.
     */
    protected void engineReset()
    {
        super.engineReset();
        java_reset();
    }

    private void java_reset() {
        digest[0] = 0x67452301;
        digest[1] = 0xEFCDAB89;
        digest[2] = 0x98BADCFE;
        digest[3] = 0x10325476;
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

// if (DEBUG && debuglevel > 6) debug("pos=" + pos);
        tmp[pos++] = -128; // (byte)0x80;

        if (pos > DATA_LENGTH - 8)
        {
            while (pos < DATA_LENGTH)
                tmp[pos++] = 0;

            byte2int(tmp, 0, data, 0, DATA_LENGTH/4);
            transform(data);
            pos = 0;
        }
// if (DEBUG && debuglevel > 6) debug("pos2=" + pos);

        while (pos < DATA_LENGTH - 8)
            tmp[pos++] = 0;

        byte2int(tmp, 0, data, 0, (DATA_LENGTH/4)-2);

// if (DEBUG && debuglevel > 8) debug("bc=" + bitcount() + " d14=" + data[14] + " d15=" + data[15]);
        // Little endian
        // WARNING: int>>>32 != 0 !!!
        // bitcount() used to return a long, now it's an int.
        long bc = bitcount();
        data[14] = (int) bc;
        data[15] = (int) (bc>>>32);

// if (DEBUG && debuglevel > 8) debug("BC=" + bc + "BC>>>32=" + ((bc>>>32))  + " d14=" + data[14] + " d15=" + data[15]);
// if (DEBUG&&  debuglevel > 8) {
//   int bbb  = 8;
//   for (int jj = 0 ; jj < 35 ; jj++) {
//     System.out.println(" shift="  + jj  + "; result="  + (bbb>>>jj));
//   }
// }
    
//        data[14] = (int) bitcount();
//        data[15] = (int) (bitcount()>>>32);
// if (DEBUG && debuglevel > 8) debug("now d14=" + data[14] + " d15=" + data[15]);

        transform(data);

        byte buf[] = new byte[HASH_LENGTH];

        // Little endian
        int off = 0;
        for (int i = 0; i < HASH_LENGTH/4; ++i) {
            int d = digest[i];
            buf[off++] = (byte) d;
            buf[off++] = (byte) (d>>>8);
            buf[off++] = (byte) (d>>>16);
            buf[off++] = (byte) (d>>>24);
        }
        return buf;
    }


// MD5 transform routines
//...........................................................................

    static protected int F(int x,int y,int z) { return (z ^ (x & (y^z))); }
    static protected int G(int x,int y,int z) { return (y ^ (z & (x^y))); }
    static protected int H(int x,int y,int z) { return (x ^ y ^ z); }
    static protected int I(int x,int y,int z) { return (y  ^  (x | ~z)); }


    static protected int FF(int a,int b,int c,int d,int k,int s,int t)
    {
        a += k+t+F(b,c,d);
        a = (a << s | a >>> -s);
        return a+b;
    }

    static protected int GG(int a,int b,int c,int d,int k,int s,int t)
    {
        a += k+t+G(b,c,d);
        a = (a << s | a >>> -s);
        return a+b;
    }

    static protected int HH(int a,int b,int c,int d,int k,int s,int t)
    {
        a += k+t+H(b,c,d);
        a = (a << s | a >>> -s);
        return a+b;
    }

    static protected int II(int a,int b,int c,int d,int k,int s,int t)
    {
        a += k+t+I(b,c,d);
        a = (a << s | a >>> -s);
        return a+b;
    }


    protected void transform (int M[])
    {
        int a,b,c,d;

if (DEBUG && debuglevel > 5) debug("d1 " + digest[0] + " d1 " + digest[1] + " d2 " + digest[2] + " d3 " + digest[3]);
if (DEBUG && debuglevel > 8) {
  for (int xx = 0; xx < 16; xx++)
     debug("      " + xx + "= " + M[xx]);
}
        a = digest[0];
        b = digest[1];
        c = digest[2];
        d = digest[3];

        a = FF(a,b,c,d,M[ 0], 7,0xd76aa478);
        d = FF(d,a,b,c,M[ 1],12,0xe8c7b756);
        c = FF(c,d,a,b,M[ 2],17,0x242070db);
        b = FF(b,c,d,a,M[ 3],22,0xc1bdceee);
        a = FF(a,b,c,d,M[ 4], 7,0xf57c0faf);
        d = FF(d,a,b,c,M[ 5],12,0x4787c62a);
        c = FF(c,d,a,b,M[ 6],17,0xa8304613);
        b = FF(b,c,d,a,M[ 7],22,0xfd469501);
        a = FF(a,b,c,d,M[ 8], 7,0x698098d8);
        d = FF(d,a,b,c,M[ 9],12,0x8b44f7af);
        c = FF(c,d,a,b,M[10],17,0xffff5bb1);
        b = FF(b,c,d,a,M[11],22,0x895cd7be);
        a = FF(a,b,c,d,M[12], 7,0x6b901122);
        d = FF(d,a,b,c,M[13],12,0xfd987193);
        c = FF(c,d,a,b,M[14],17,0xa679438e);
        b = FF(b,c,d,a,M[15],22,0x49b40821);

        a = GG(a,b,c,d,M[ 1], 5,0xf61e2562);
        d = GG(d,a,b,c,M[ 6], 9,0xc040b340);
        c = GG(c,d,a,b,M[11],14,0x265e5a51);
        b = GG(b,c,d,a,M[ 0],20,0xe9b6c7aa);
        a = GG(a,b,c,d,M[ 5], 5,0xd62f105d);
        d = GG(d,a,b,c,M[10], 9,0x02441453);
        c = GG(c,d,a,b,M[15],14,0xd8a1e681);
        b = GG(b,c,d,a,M[ 4],20,0xe7d3fbc8);
        a = GG(a,b,c,d,M[ 9], 5,0x21e1cde6);
        d = GG(d,a,b,c,M[14], 9,0xc33707d6);
        c = GG(c,d,a,b,M[ 3],14,0xf4d50d87);
        b = GG(b,c,d,a,M[ 8],20,0x455a14ed);
        a = GG(a,b,c,d,M[13], 5,0xa9e3e905);
        d = GG(d,a,b,c,M[ 2], 9,0xfcefa3f8);
        c = GG(c,d,a,b,M[ 7],14,0x676f02d9);
        b = GG(b,c,d,a,M[12],20,0x8d2a4c8a);

        a = HH(a,b,c,d,M[ 5], 4,0xfffa3942);
        d = HH(d,a,b,c,M[ 8],11,0x8771f681);
        c = HH(c,d,a,b,M[11],16,0x6d9d6122);
        b = HH(b,c,d,a,M[14],23,0xfde5380c);
        a = HH(a,b,c,d,M[ 1], 4,0xa4beea44);
        d = HH(d,a,b,c,M[ 4],11,0x4bdecfa9);
        c = HH(c,d,a,b,M[ 7],16,0xf6bb4b60);
        b = HH(b,c,d,a,M[10],23,0xbebfbc70);
        a = HH(a,b,c,d,M[13], 4,0x289b7ec6);
        d = HH(d,a,b,c,M[ 0],11,0xeaa127fa);
        c = HH(c,d,a,b,M[ 3],16,0xd4ef3085);
        b = HH(b,c,d,a,M[ 6],23,0x04881d05);
        a = HH(a,b,c,d,M[ 9], 4,0xd9d4d039);
        d = HH(d,a,b,c,M[12],11,0xe6db99e5);
        c = HH(c,d,a,b,M[15],16,0x1fa27cf8);
        b = HH(b,c,d,a,M[ 2],23,0xc4ac5665);

        a = II(a,b,c,d,M[ 0], 6,0xf4292244);
        d = II(d,a,b,c,M[ 7],10,0x432aff97);
        c = II(c,d,a,b,M[14],15,0xab9423a7);
        b = II(b,c,d,a,M[ 5],21,0xfc93a039);
        a = II(a,b,c,d,M[12], 6,0x655b59c3);
        d = II(d,a,b,c,M[ 3],10,0x8f0ccc92);
        c = II(c,d,a,b,M[10],15,0xffeff47d);
        b = II(b,c,d,a,M[ 1],21,0x85845dd1);
        a = II(a,b,c,d,M[ 8], 6,0x6fa87e4f);
        d = II(d,a,b,c,M[15],10,0xfe2ce6e0);
        c = II(c,d,a,b,M[ 6],15,0xa3014314);
        b = II(b,c,d,a,M[13],21,0x4e0811a1);
        a = II(a,b,c,d,M[ 4], 6,0xf7537e82);
        d = II(d,a,b,c,M[11],10,0xbd3af235);
        c = II(c,d,a,b,M[ 2],15,0x2ad7d2bb);
        b = II(b,c,d,a,M[ 9],21,0xeb86d391);

        digest[0] += a;
        digest[1] += b;
        digest[2] += c;
        digest[3] += d;
if (DEBUG && debuglevel > 4) debug("d1 " + digest[0] + " d1 " + digest[1] + " d2 " + digest[2] + " d3 " + digest[3]);
    }

    // why was this public?
    // Note: parameter order changed to be consistent with System.arraycopy.
    private static void byte2int(byte[] src, int srcOffset,
                                 int[] dst, int dstOffset, int length)
    {
        while (length-- > 0)
        {
            // Little endian
            dst[dstOffset++] =  (src[srcOffset++] & 0xFF)        |
                               ((src[srcOffset++] & 0xFF) <<  8) |
                               ((src[srcOffset++] & 0xFF) << 16) |
                               ((src[srcOffset++] & 0xFF) << 24);
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
    catch(Throwable t) { t.printStackTrace(); System.exit(1); }
}

//
// Using Hex format 0xAB results in needing casts.
//
private static String[] texts =
{
  "",                                // v 1
  "message digest",                          // v 4
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",  // v 6
};
private static byte[][] hashs =
{
 {-44, 29, -116, -39, -113, 0, -78, 4, -23, -128, 9, -104, -20, -8, 66, 126,},
 {-7, 107, 105, 125, 124, -73, -109, -115, 82, 90, 47, 49, -86, -15, 97, -48,},
 {-47, 116, -85, -104, -46, 119, -39, -11, -91, 97, 28, 44, -97, 65, -99, -97,},
};

/**
 * Do some basic tests.
 * Three of the validation data are included only, no output,
 * success or exception.
 * If you want more, write a test program!
 * @see cryptix.examples.UnitMD5
 */
public static final void
self_test()
throws Exception
{
    int length = hashs[0].length;
    for (int i = 0; i < texts.length; i++)
    {
        MD5 hash = new MD5();
        byte[] text = texts[i].getBytes();

        for (int j = 0; j < texts[i].length(); j++)
            hash.engineUpdate(text[j]);

        if (notEquals(hash.engineDigest(), hashs[i]))
            throw new Exception("hash #"+ i +" failed");

        // for (int j = 0; j < hashs[i].length; j++)
        //      System.err.print(hashs[i][j] + ", ");
        // System.err.print("},\n{");

    }
}

private static final boolean
notEquals(byte[] a, byte[] b)
{
    for (int i = 0; i < a.length; i++)
    {
        if (a[i] != b[i])
            return true;
        // if (a[i] == 88) // force a trip up
        //      return true;
    }
    return false;
}

}
