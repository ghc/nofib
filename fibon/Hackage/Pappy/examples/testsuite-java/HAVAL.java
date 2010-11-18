// $Id: HAVAL.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: HAVAL.java,v $
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
// Revision 1.5  2000/08/17 11:40:57  edwin
// java.* -> xjava.*
//
// Revision 1.4  1997/11/20 19:36:30  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.3.2  1997/11/18  David Hopwood
// + Implement the java.security.Parameterized interface (a workaround for
//   Sun's version of MessageDigest not supporting set/getParameter).
//
// Revision 1.3.1  1997/11/15  David Hopwood
// + Renamed "Passes" and "BitLength" parameters to "passes" and "bitLength"
//   respectively.
// + Throw InvalidParameterException when the argument to setPasses or
//   setBitLength is out of range.
// + Added engineSet/GetParameter methods (at the moment these only work if the
//   IJCE 'src10' classes are first on the CLASSPATH).
//
// Revision 1.3  1997/11/08 14:40:22  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/07 05:53:25  raif
// + Changed native API to support variable number of passes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.2  1997/08/27  David Hopwood
// + Use new debugging API.
// + Misc. fixes.
//
// Revision 0.1.0.1  1997/08/14  David Hopwood
// + Renamed setPass to setPasses.
// + Renamed setLength to setBitLength.
// + Renamed mdLength variable to bitLength.
// + Added setDigestLength (corresponding to getDigestLength) which takes
//   a length in bytes.
// + Implemented engineGetDigestLength method.
// + Changed SPI methods from public to protected.
// + Changed name of first parameter to native_hash from ctx to context,
//   to be consistent with native code.
// + Check validity of arguments before calling native_hash.
// + Changed 'Object native_lock' to 'boolean native_ok'.
// + Removed 'synchronized (native_lock) { ... }' around native_hash call.
//   It isn't necessary to synchronize calls to native methods in this case,
//   because unlike the cipher classes, the native code for hash functions
//   is stateless.
// + Required native code version is 2.3.
// + Cosmetic changes.
//
// Revision 0.1.0.0  1997/07/14  R. Naffah
// + Original version.
//
// $Endlog$
/*
 * Ported to Java from Dr. Yuliang Zheng's 'C' code, last revised in
 * April 1997.
 *
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.provider.md;

import cryptix.util.core.Debug;

import java.io.PrintWriter;
import java.security.MessageDigest;
import xjava.security.Parameterized;
import xjava.security.VariableLengthDigest;
import java.security.Security;
import xjava.security.InvalidParameterTypeException;
import java.security.InvalidParameterException;
import xjava.security.NoSuchParameterException;

/**
 * A Java class to digest input according to the HAVAL algorithm.
 * <p>
 * HAVAL is a variable length MD with a variable number of passes. The values
 * for these two parameters are read from the provider '.properties' file. Here
 * is an example of the two property lines that do that:
 * <p>
 * <pre>
 *    Alg.passes.HAVAL = 3
 *    Alg.bitLength.HAVAL = 256
 * </pre>
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Y. Zheng, J. Pieprzyk and J. Seberry,
 *        "HAVAL --- a one-way hashing algorithm with variable length of output",
 *        <cite>Advances in Cryptology --- AUSCRYPT'92,
 *        Lecture Notes in Computer Science</cite>,
 *        Springer-Verlag, 1993.
 * <ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 * @author  David Hopwood
 * @since   Cryptix 2.2.2
 */
public class HAVAL
extends MessageDigest
implements Parameterized, VariableLengthDigest, Cloneable
{
// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final int debuglevel = DEBUG ? Debug.getLevel("HAVAL") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("HAVAL: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("HAVAL", 2, 3);
    public static cryptix.util.core.LinkStatus getLinkStatus() { return linkStatus; }

    /**
     * This flag is false if the native code is not being used (e.g. the
     * library did not load successfully, or the user disabled its use in
     * the properties file).
     */
    private boolean native_ok; // defaults to false

    private void link() {
        synchronized(linkStatus) {
            try {
                if (linkStatus.attemptLoad())
                    linkStatus.checkVersion(getLibMajorVersion(), getLibMinorVersion());
                if (linkStatus.useNative())
                    native_ok = true;
            } catch (UnsatisfiedLinkError e) {
                linkStatus.fail(e);
if (DEBUG && debuglevel > 2) debug(e.getMessage());
            }
if (DEBUG && debuglevel > 2) debug("Using native library? " + native_ok);
        }
    }


// Native implementation
//...........................................................................

    /** The functions that get the library version. */
    private native static int getLibMajorVersion();
    private native static int getLibMinorVersion();

    /**
     * Transforms context based on 1024 bits from input block starting from
     * the offset'th byte and given a specific number of passes.
     *
     * @param  ctx      the current context of this instance.
     * @param  block    input array containing data to hash.
     * @param  offset   index of where we should start reading from input.
     * @param  pass     number of passes to apply for this HAVAL instance.
     * @return an error string if one occurs or null otherwise.
     */
    private native static String
    native_hash (int[] ctx, byte[] block, int offset, int pass);


// HAVAL variables and constants
//...........................................................................

    private static final int VERSION = 1;
    private static final int DEFAULT_PASSES = 3;
    private static final int DEFAULT_BITLENGTH = 256;
    private static final int BLOCK_LENGTH = 128;
    private static final int CONTEXT_LENGTH = 8;
    
    /**
     * Defines the number of passes the algorithm has to go through
     * to produce a message digest value.
     * <p>
     * Allowed values are integers in the range {3, 4, 5}. The default
     * is DEFAULT_PASSES (3).
     */
    private int passes = DEFAULT_PASSES;

    /**
     * Defines the length of the message digest algorithm output
     * in bits.
     * <p>
     * Allowed values are chosen from the set: {128, 160, 192, 224, 256}.
     * The default is DEFAULT_BITLENGTH (256).
     */
    private int bitLength = DEFAULT_BITLENGTH;

    /** Number of bits in the current message being digested. */
    private long count;
    
    /** Current state of the algorithm transform main function. */
    private int[] context = new int[CONTEXT_LENGTH];

    /**
     * Temporary buffer containing unhashed bytes whose total length < 128.
     */
    private byte[] buffer = new byte[BLOCK_LENGTH];

    /**
     * Work buffer for the transformation functions. This object doesn't
     * get cloned!
     */
    private int[] X = new int[32];


// Constructors
//...........................................................................

    public HAVAL () {
        super("HAVAL");
        try {
            String ps = Security.getAlgorithmProperty("HAVAL", "passes");
            int p = Integer.parseInt(ps);
            if (p >= 3 && p <= 5) passes = p;
        } catch (Exception e) {}

        try {
            String ps = Security.getAlgorithmProperty("HAVAL", "bitLength");
            int len = Integer.parseInt(ps);
            if ((len % 32) == 0 && len >= 128 && len <= 256) bitLength = len;
        } catch (Exception e) {}

        engineReset();
        link();
    }

    /** This constructor is here to implement cloneability of this class. */
    private HAVAL (HAVAL md) {
        this();
        passes = md.passes;
        bitLength = md.bitLength;
        count = md.count;
        context = (int[]) (md.context.clone());
        buffer = (byte[]) (md.buffer.clone());
    }


// Cloneable method implementation
//...........................................................................

    /** Returns a copy of this MD object. */
    public Object clone() { return new HAVAL(this); }


// JCE methods
//...........................................................................

    /**
     * Resets this object disregarding any temporary data present at the
     * time of the invocation of this call.
     */
    protected void engineReset () {
        // algorithm context's initial values
        context[0] = 0x243F6A88;
        context[1] = 0x85A308D3;
        context[2] = 0x13198A2E;
        context[3] = 0x03707344;
        context[4] = 0xA4093822;
        context[5] = 0x299F31D0;
        context[6] = 0x082EFA98;
        context[7] = 0xEC4E6C89;
        count = 0L;
        for (int i = 0; i < BLOCK_LENGTH; i++)
            buffer[i] = 0;
    }

    /** Continues a HAVAL message digest using the input byte. */
    protected void engineUpdate (byte input) {
        // compute number of bytes still unhashed; i.e. present in buffer
        int i = (int)(count % BLOCK_LENGTH);
        count++;                                    // update number of bytes
        buffer[i] = input;
        if (i == BLOCK_LENGTH - 1)  
            transform(buffer, 0);
    }

    /**
     * Hashes a byte array from a given offset for a specified length.
     * to be used in conjunction with engineReset() and finish().
     *
     * @param  input    byte array from which data is to be hashed.
     * @param  offset   start index of bytes to hash in input.
     * @param  len      number of bytes to hash.
     */
    protected void engineUpdate (byte[] input, int offset, int len) {
        // make sure we don't exceed input's allocated size/length.
        if (offset < 0 || len < 0 || (long)offset + len > input.length)
            throw new ArrayIndexOutOfBoundsException();

        // compute number of bytes still unhashed; ie. present in buffer
        int bufferNdx = (int)(count % BLOCK_LENGTH);
        count += len;                               // update number of bytes
        int partLen = BLOCK_LENGTH - bufferNdx;
        int i = 0;

        // hash as many 128-byte blocks as possible
        if (len >= partLen) {
            System.arraycopy(input, offset, buffer, bufferNdx, partLen);    
            transform(buffer, 0);
            for (i = partLen; i + BLOCK_LENGTH - 1 < len; i+= BLOCK_LENGTH)
                transform(input, offset + i);
            bufferNdx = 0;
        }
        // buffer remaining input
        if (i < len)
            System.arraycopy(input, offset + i, buffer, bufferNdx, len - i);
    }

    /**
     * Completes the hash computation by performing final operations such
     * as padding. At the return of this engineDigest, the MD engine is
     * reset.
     *
     * @return the array of bytes for the resulting hash value.
     */
    protected byte[] engineDigest() {
        // pad out to 118 mod 128. Other 10 bytes have special use.
        int bufferNdx = (int)(count % BLOCK_LENGTH);
        int padLen = bufferNdx < 118 ? 118 - bufferNdx : 246 - bufferNdx;
        
        byte[] tail = new byte[padLen + 10];
        tail[0] = 0x01;

        // save the version number (LSB 3), the number of passes (3 bits in the
        // middle), the fingerprint length (MSB 2 bits and next byte) and the
        // number of bits in the unpadded message.
        tail[padLen] = (byte)(((bitLength & 0x3) << 6) | ((passes & 0x7) << 3) |
                                (VERSION & 0x7));
        tail[padLen + 1] = (byte)(bitLength >>> 2);

        // convert count (long) into an array of 8 bytes
        for (int i = 0; i < 8; i++)
            tail[padLen + 2 + i] = (byte)((count * 8) >>> (8 * i));
            
        engineUpdate(tail, 0, tail.length);

        // tailor the context for the designated bitLength output
        tailorDigestBits();

        // cast enough top HAVAL context (array of 8 ints) values into
        // an array of (bitLength / 8) bytes.
        byte[] result = new byte[bitLength / 8];
        for (int i = 0; i < bitLength / 32; i++)
            for (int j = 0; j < 4; j++)
                result[i * 4 + j] = (byte)(context[i] >>> (8 * j));

        engineReset();
        return result;
    }

    /** <b>SPI</b>: Returns the digest length in bytes. */
    protected int engineGetDigestLength() { return bitLength / 8; }

    public void setParameter(String param, Object value)
    throws NoSuchParameterException, InvalidParameterException,
           InvalidParameterTypeException {
        engineSetParameter(param, value);
    }

    public Object getParameter(String param)
    throws NoSuchParameterException, InvalidParameterException {
        return engineGetParameter(param);
    }

    protected void engineSetParameter(String param, Object value)
    throws NoSuchParameterException, InvalidParameterException,
           InvalidParameterTypeException {
        if (param.equalsIgnoreCase("passes")) {
            if (value instanceof Integer)
                setPasses(((Integer) value).intValue());
            else
                throw new InvalidParameterTypeException("passes.HAVAL");
        } else if (param.equalsIgnoreCase("bitLength")) {
            if (value instanceof Integer)
                setBitLength(((Integer) value).intValue());
            else
                throw new InvalidParameterTypeException("bitLength.HAVAL");
        } else
            throw new NoSuchParameterException(param + ".HAVAL");
    }

    protected Object engineGetParameter(String param)
    throws NoSuchParameterException, InvalidParameterException {
        if (param.equalsIgnoreCase("passes"))
            return new Integer(passes);
        else if (param.equalsIgnoreCase("bitLength"))
            return new Integer(bitLength);
        else
            throw new NoSuchParameterException(param + ".HAVAL");
    }


// Own methods
//...........................................................................

    /**
     * Sets the number of passes for this HAVAL object, resetting all
     * internal variables.
     */
    public void setPasses(int p) {
        if (p >= 3 && p <= 5) {
            passes = p;
            engineReset();
        } else
            throw new InvalidParameterException();
    }

    /**
     * Sets the output length of this HAVAL object in bits, resetting all
     * internal variables.
     */
    public void setBitLength(int len) {
        if ((len % 32) == 0 && len >= 128 && len <= 256) {
            bitLength = len;
            engineReset();
        } else
            throw new InvalidParameterException();
    }

    /**
     * Sets the output length of this HAVAL object in bytes, resetting all
     * internal variables.
     */
    public void setDigestLength(int len) {
        setBitLength(len * 8);
    }

    /** Hashes a 128-byte block starting at offset in b. */
    private void transform (byte[] block, int offset) {
        // if allowed to use native library then now's the time
        if (native_ok) {
            // should never happen, but this would be a security bug so check it anyway:
            if (context.length != CONTEXT_LENGTH || offset < 0 ||
                (long)offset + BLOCK_LENGTH > block.length) {
                throw new InternalError(getAlgorithm() +
                    ": context.length != " + CONTEXT_LENGTH + " || offset < 0 || " +
                    "(long)offset + " + BLOCK_LENGTH + " > block.length");
            }
            linkStatus.check(native_hash(context, block, offset, passes));
            return;
        }
        // encodes 128 bytes from input block into an array of 32 integers.
        for (int i = 0; i < 32; i++)
            X[i] = (block[offset++] & 0xFF)       |
                   (block[offset++] & 0xFF) <<  8 |
                   (block[offset++] & 0xFF) << 16 |
                   (block[offset++] & 0xFF) << 24;

        int t0 = context[0], t1 = context[1], t2 = context[2], t3 = context[3],
            t4 = context[4], t5 = context[5], t6 = context[6], t7 = context[7];

        // Pass 1
        t7 = FF_1(t7, t6, t5, t4, t3, t2, t1, t0, X[ 0]);
        t6 = FF_1(t6, t5, t4, t3, t2, t1, t0, t7, X[ 1]);
        t5 = FF_1(t5, t4, t3, t2, t1, t0, t7, t6, X[ 2]);
        t4 = FF_1(t4, t3, t2, t1, t0, t7, t6, t5, X[ 3]);
        t3 = FF_1(t3, t2, t1, t0, t7, t6, t5, t4, X[ 4]);
        t2 = FF_1(t2, t1, t0, t7, t6, t5, t4, t3, X[ 5]);
        t1 = FF_1(t1, t0, t7, t6, t5, t4, t3, t2, X[ 6]);
        t0 = FF_1(t0, t7, t6, t5, t4, t3, t2, t1, X[ 7]);

        t7 = FF_1(t7, t6, t5, t4, t3, t2, t1, t0, X[ 8]);
        t6 = FF_1(t6, t5, t4, t3, t2, t1, t0, t7, X[ 9]);
        t5 = FF_1(t5, t4, t3, t2, t1, t0, t7, t6, X[10]);
        t4 = FF_1(t4, t3, t2, t1, t0, t7, t6, t5, X[11]);
        t3 = FF_1(t3, t2, t1, t0, t7, t6, t5, t4, X[12]);
        t2 = FF_1(t2, t1, t0, t7, t6, t5, t4, t3, X[13]);
        t1 = FF_1(t1, t0, t7, t6, t5, t4, t3, t2, X[14]);
        t0 = FF_1(t0, t7, t6, t5, t4, t3, t2, t1, X[15]);

        t7 = FF_1(t7, t6, t5, t4, t3, t2, t1, t0, X[16]);
        t6 = FF_1(t6, t5, t4, t3, t2, t1, t0, t7, X[17]);
        t5 = FF_1(t5, t4, t3, t2, t1, t0, t7, t6, X[18]);
        t4 = FF_1(t4, t3, t2, t1, t0, t7, t6, t5, X[19]);
        t3 = FF_1(t3, t2, t1, t0, t7, t6, t5, t4, X[20]);
        t2 = FF_1(t2, t1, t0, t7, t6, t5, t4, t3, X[21]);
        t1 = FF_1(t1, t0, t7, t6, t5, t4, t3, t2, X[22]);
        t0 = FF_1(t0, t7, t6, t5, t4, t3, t2, t1, X[23]);

        t7 = FF_1(t7, t6, t5, t4, t3, t2, t1, t0, X[24]);
        t6 = FF_1(t6, t5, t4, t3, t2, t1, t0, t7, X[25]);
        t5 = FF_1(t5, t4, t3, t2, t1, t0, t7, t6, X[26]);
        t4 = FF_1(t4, t3, t2, t1, t0, t7, t6, t5, X[27]);
        t3 = FF_1(t3, t2, t1, t0, t7, t6, t5, t4, X[28]);
        t2 = FF_1(t2, t1, t0, t7, t6, t5, t4, t3, X[29]);
        t1 = FF_1(t1, t0, t7, t6, t5, t4, t3, t2, X[30]);
        t0 = FF_1(t0, t7, t6, t5, t4, t3, t2, t1, X[31]);

        // Pass 2
        t7 = FF_2(t7, t6, t5, t4, t3, t2, t1, t0, X[ 5], 0x452821E6);
        t6 = FF_2(t6, t5, t4, t3, t2, t1, t0, t7, X[14], 0x38D01377);
        t5 = FF_2(t5, t4, t3, t2, t1, t0, t7, t6, X[26], 0xBE5466CF);
        t4 = FF_2(t4, t3, t2, t1, t0, t7, t6, t5, X[18], 0x34E90C6C);
        t3 = FF_2(t3, t2, t1, t0, t7, t6, t5, t4, X[11], 0xC0AC29B7);
        t2 = FF_2(t2, t1, t0, t7, t6, t5, t4, t3, X[28], 0xC97C50DD);
        t1 = FF_2(t1, t0, t7, t6, t5, t4, t3, t2, X[ 7], 0x3F84D5B5);
        t0 = FF_2(t0, t7, t6, t5, t4, t3, t2, t1, X[16], 0xB5470917);

        t7 = FF_2(t7, t6, t5, t4, t3, t2, t1, t0, X[ 0], 0x9216D5D9);
        t6 = FF_2(t6, t5, t4, t3, t2, t1, t0, t7, X[23], 0x8979FB1B);
        t5 = FF_2(t5, t4, t3, t2, t1, t0, t7, t6, X[20], 0xD1310BA6);
        t4 = FF_2(t4, t3, t2, t1, t0, t7, t6, t5, X[22], 0x98DFB5AC);
        t3 = FF_2(t3, t2, t1, t0, t7, t6, t5, t4, X[ 1], 0x2FFD72DB);
        t2 = FF_2(t2, t1, t0, t7, t6, t5, t4, t3, X[10], 0xD01ADFB7);
        t1 = FF_2(t1, t0, t7, t6, t5, t4, t3, t2, X[ 4], 0xB8E1AFED);
        t0 = FF_2(t0, t7, t6, t5, t4, t3, t2, t1, X[ 8], 0x6A267E96);

        t7 = FF_2(t7, t6, t5, t4, t3, t2, t1, t0, X[30], 0xBA7C9045);
        t6 = FF_2(t6, t5, t4, t3, t2, t1, t0, t7, X[ 3], 0xF12C7F99);
        t5 = FF_2(t5, t4, t3, t2, t1, t0, t7, t6, X[21], 0x24A19947);
        t4 = FF_2(t4, t3, t2, t1, t0, t7, t6, t5, X[ 9], 0xB3916CF7);
        t3 = FF_2(t3, t2, t1, t0, t7, t6, t5, t4, X[17], 0x0801F2E2);
        t2 = FF_2(t2, t1, t0, t7, t6, t5, t4, t3, X[24], 0x858EFC16);
        t1 = FF_2(t1, t0, t7, t6, t5, t4, t3, t2, X[29], 0x636920D8);
        t0 = FF_2(t0, t7, t6, t5, t4, t3, t2, t1, X[ 6], 0x71574E69);

        t7 = FF_2(t7, t6, t5, t4, t3, t2, t1, t0, X[19], 0xA458FEA3);
        t6 = FF_2(t6, t5, t4, t3, t2, t1, t0, t7, X[12], 0xF4933D7E);
        t5 = FF_2(t5, t4, t3, t2, t1, t0, t7, t6, X[15], 0x0D95748F);
        t4 = FF_2(t4, t3, t2, t1, t0, t7, t6, t5, X[13], 0x728EB658);
        t3 = FF_2(t3, t2, t1, t0, t7, t6, t5, t4, X[ 2], 0x718BCD58);
        t2 = FF_2(t2, t1, t0, t7, t6, t5, t4, t3, X[25], 0x82154AEE);
        t1 = FF_2(t1, t0, t7, t6, t5, t4, t3, t2, X[31], 0x7B54A41D);
        t0 = FF_2(t0, t7, t6, t5, t4, t3, t2, t1, X[27], 0xC25A59B5);

        // Pass 3
        t7 = FF_3(t7, t6, t5, t4, t3, t2, t1, t0, X[19], 0x9C30D539);
        t6 = FF_3(t6, t5, t4, t3, t2, t1, t0, t7, X[ 9], 0x2AF26013);
        t5 = FF_3(t5, t4, t3, t2, t1, t0, t7, t6, X[ 4], 0xC5D1B023);
        t4 = FF_3(t4, t3, t2, t1, t0, t7, t6, t5, X[20], 0x286085F0);
        t3 = FF_3(t3, t2, t1, t0, t7, t6, t5, t4, X[28], 0xCA417918);
        t2 = FF_3(t2, t1, t0, t7, t6, t5, t4, t3, X[17], 0xB8DB38EF);
        t1 = FF_3(t1, t0, t7, t6, t5, t4, t3, t2, X[ 8], 0x8E79DCB0);
        t0 = FF_3(t0, t7, t6, t5, t4, t3, t2, t1, X[22], 0x603A180E);

        t7 = FF_3(t7, t6, t5, t4, t3, t2, t1, t0, X[29], 0x6C9E0E8B);
        t6 = FF_3(t6, t5, t4, t3, t2, t1, t0, t7, X[14], 0xB01E8A3E);
        t5 = FF_3(t5, t4, t3, t2, t1, t0, t7, t6, X[25], 0xD71577C1);
        t4 = FF_3(t4, t3, t2, t1, t0, t7, t6, t5, X[12], 0xBD314B27);
        t3 = FF_3(t3, t2, t1, t0, t7, t6, t5, t4, X[24], 0x78AF2FDA);
        t2 = FF_3(t2, t1, t0, t7, t6, t5, t4, t3, X[30], 0x55605C60);
        t1 = FF_3(t1, t0, t7, t6, t5, t4, t3, t2, X[16], 0xE65525F3);
        t0 = FF_3(t0, t7, t6, t5, t4, t3, t2, t1, X[26], 0xAA55AB94);

        t7 = FF_3(t7, t6, t5, t4, t3, t2, t1, t0, X[31], 0x57489862);
        t6 = FF_3(t6, t5, t4, t3, t2, t1, t0, t7, X[15], 0x63E81440);
        t5 = FF_3(t5, t4, t3, t2, t1, t0, t7, t6, X[ 7], 0x55CA396A);
        t4 = FF_3(t4, t3, t2, t1, t0, t7, t6, t5, X[ 3], 0x2AAB10B6);
        t3 = FF_3(t3, t2, t1, t0, t7, t6, t5, t4, X[ 1], 0xB4CC5C34);
        t2 = FF_3(t2, t1, t0, t7, t6, t5, t4, t3, X[ 0], 0x1141E8CE);
        t1 = FF_3(t1, t0, t7, t6, t5, t4, t3, t2, X[18], 0xA15486AF);
        t0 = FF_3(t0, t7, t6, t5, t4, t3, t2, t1, X[27], 0x7C72E993);

        t7 = FF_3(t7, t6, t5, t4, t3, t2, t1, t0, X[13], 0xB3EE1411);
        t6 = FF_3(t6, t5, t4, t3, t2, t1, t0, t7, X[ 6], 0x636FBC2A);
        t5 = FF_3(t5, t4, t3, t2, t1, t0, t7, t6, X[21], 0x2BA9C55D);
        t4 = FF_3(t4, t3, t2, t1, t0, t7, t6, t5, X[10], 0x741831F6);
        t3 = FF_3(t3, t2, t1, t0, t7, t6, t5, t4, X[23], 0xCE5C3E16);
        t2 = FF_3(t2, t1, t0, t7, t6, t5, t4, t3, X[11], 0x9B87931E);
        t1 = FF_3(t1, t0, t7, t6, t5, t4, t3, t2, X[ 5], 0xAFD6BA33);
        t0 = FF_3(t0, t7, t6, t5, t4, t3, t2, t1, X[ 2], 0x6C24CF5C);

        if (passes >= 4) {
            // Pass 4. executed only when passes = 4 or 5
            t7 = FF_4(t7, t6, t5, t4, t3, t2, t1, t0, X[24], 0x7A325381);
            t6 = FF_4(t6, t5, t4, t3, t2, t1, t0, t7, X[ 4], 0x28958677);
            t5 = FF_4(t5, t4, t3, t2, t1, t0, t7, t6, X[ 0], 0x3B8F4898);
            t4 = FF_4(t4, t3, t2, t1, t0, t7, t6, t5, X[14], 0x6B4BB9AF);
            t3 = FF_4(t3, t2, t1, t0, t7, t6, t5, t4, X[ 2], 0xC4BFE81B);
            t2 = FF_4(t2, t1, t0, t7, t6, t5, t4, t3, X[ 7], 0x66282193);
            t1 = FF_4(t1, t0, t7, t6, t5, t4, t3, t2, X[28], 0x61D809CC);
            t0 = FF_4(t0, t7, t6, t5, t4, t3, t2, t1, X[23], 0xFB21A991);
            t7 = FF_4(t7, t6, t5, t4, t3, t2, t1, t0, X[26], 0x487CAC60);
            t6 = FF_4(t6, t5, t4, t3, t2, t1, t0, t7, X[ 6], 0x5DEC8032);
            t5 = FF_4(t5, t4, t3, t2, t1, t0, t7, t6, X[30], 0xEF845D5D);
            t4 = FF_4(t4, t3, t2, t1, t0, t7, t6, t5, X[20], 0xE98575B1);
            t3 = FF_4(t3, t2, t1, t0, t7, t6, t5, t4, X[18], 0xDC262302);
            t2 = FF_4(t2, t1, t0, t7, t6, t5, t4, t3, X[25], 0xEB651B88);
            t1 = FF_4(t1, t0, t7, t6, t5, t4, t3, t2, X[19], 0x23893E81);
            t0 = FF_4(t0, t7, t6, t5, t4, t3, t2, t1, X[ 3], 0xD396ACC5);

            t7 = FF_4(t7, t6, t5, t4, t3, t2, t1, t0, X[22], 0x0F6D6FF3);
            t6 = FF_4(t6, t5, t4, t3, t2, t1, t0, t7, X[11], 0x83F44239);
            t5 = FF_4(t5, t4, t3, t2, t1, t0, t7, t6, X[31], 0x2E0B4482);
            t4 = FF_4(t4, t3, t2, t1, t0, t7, t6, t5, X[21], 0xA4842004);
            t3 = FF_4(t3, t2, t1, t0, t7, t6, t5, t4, X[ 8], 0x69C8F04A);
            t2 = FF_4(t2, t1, t0, t7, t6, t5, t4, t3, X[27], 0x9E1F9B5E);
            t1 = FF_4(t1, t0, t7, t6, t5, t4, t3, t2, X[12], 0x21C66842);
            t0 = FF_4(t0, t7, t6, t5, t4, t3, t2, t1, X[ 9], 0xF6E96C9A);
            t7 = FF_4(t7, t6, t5, t4, t3, t2, t1, t0, X[ 1], 0x670C9C61);
            t6 = FF_4(t6, t5, t4, t3, t2, t1, t0, t7, X[29], 0xABD388F0);
            t5 = FF_4(t5, t4, t3, t2, t1, t0, t7, t6, X[ 5], 0x6A51A0D2);
            t4 = FF_4(t4, t3, t2, t1, t0, t7, t6, t5, X[15], 0xD8542F68);
            t3 = FF_4(t3, t2, t1, t0, t7, t6, t5, t4, X[17], 0x960FA728);
            t2 = FF_4(t2, t1, t0, t7, t6, t5, t4, t3, X[10], 0xAB5133A3);
            t1 = FF_4(t1, t0, t7, t6, t5, t4, t3, t2, X[16], 0x6EEF0B6C);
            t0 = FF_4(t0, t7, t6, t5, t4, t3, t2, t1, X[13], 0x137A3BE4);
        }
        if (passes == 5) {
            t7 = FF_5(t7, t6, t5, t4, t3, t2, t1, t0, X[27], 0xBA3BF050);
            t6 = FF_5(t6, t5, t4, t3, t2, t1, t0, t7, X[ 3], 0x7EFB2A98);
            t5 = FF_5(t5, t4, t3, t2, t1, t0, t7, t6, X[21], 0xA1F1651D);
            t4 = FF_5(t4, t3, t2, t1, t0, t7, t6, t5, X[26], 0x39AF0176);
            t3 = FF_5(t3, t2, t1, t0, t7, t6, t5, t4, X[17], 0x66CA593E);
            t2 = FF_5(t2, t1, t0, t7, t6, t5, t4, t3, X[11], 0x82430E88);
            t1 = FF_5(t1, t0, t7, t6, t5, t4, t3, t2, X[20], 0x8CEE8619);
            t0 = FF_5(t0, t7, t6, t5, t4, t3, t2, t1, X[29], 0x456F9FB4);

            t7 = FF_5(t7, t6, t5, t4, t3, t2, t1, t0, X[19], 0x7D84A5C3);
            t6 = FF_5(t6, t5, t4, t3, t2, t1, t0, t7, X[ 0], 0x3B8B5EBE);
            t5 = FF_5(t5, t4, t3, t2, t1, t0, t7, t6, X[12], 0xE06F75D8);
            t4 = FF_5(t4, t3, t2, t1, t0, t7, t6, t5, X[ 7], 0x85C12073);
            t3 = FF_5(t3, t2, t1, t0, t7, t6, t5, t4, X[13], 0x401A449F);
            t2 = FF_5(t2, t1, t0, t7, t6, t5, t4, t3, X[ 8], 0x56C16AA6);
            t1 = FF_5(t1, t0, t7, t6, t5, t4, t3, t2, X[31], 0x4ED3AA62);
            t0 = FF_5(t0, t7, t6, t5, t4, t3, t2, t1, X[10], 0x363F7706);

            t7 = FF_5(t7, t6, t5, t4, t3, t2, t1, t0, X[ 5], 0x1BFEDF72);
            t6 = FF_5(t6, t5, t4, t3, t2, t1, t0, t7, X[ 9], 0x429B023D);
            t5 = FF_5(t5, t4, t3, t2, t1, t0, t7, t6, X[14], 0x37D0D724);
            t4 = FF_5(t4, t3, t2, t1, t0, t7, t6, t5, X[30], 0xD00A1248);
            t3 = FF_5(t3, t2, t1, t0, t7, t6, t5, t4, X[18], 0xDB0FEAD3);
            t2 = FF_5(t2, t1, t0, t7, t6, t5, t4, t3, X[ 6], 0x49F1C09B);
            t1 = FF_5(t1, t0, t7, t6, t5, t4, t3, t2, X[28], 0x075372C9);
            t0 = FF_5(t0, t7, t6, t5, t4, t3, t2, t1, X[24], 0x80991B7B);

            t7 = FF_5(t7, t6, t5, t4, t3, t2, t1, t0, X[ 2], 0x25D479D8);
            t6 = FF_5(t6, t5, t4, t3, t2, t1, t0, t7, X[23], 0xF6E8DEF7);
            t5 = FF_5(t5, t4, t3, t2, t1, t0, t7, t6, X[16], 0xE3FE501A);
            t4 = FF_5(t4, t3, t2, t1, t0, t7, t6, t5, X[22], 0xB6794C3B);
            t3 = FF_5(t3, t2, t1, t0, t7, t6, t5, t4, X[ 4], 0x976CE0BD);
            t2 = FF_5(t2, t1, t0, t7, t6, t5, t4, t3, X[ 1], 0x04C006BA);
            t1 = FF_5(t1, t0, t7, t6, t5, t4, t3, t2, X[25], 0xC1A94FB6);
            t0 = FF_5(t0, t7, t6, t5, t4, t3, t2, t1, X[15], 0x409F60C4);
        }
        context[0] += t0;
        context[1] += t1;
        context[2] += t2;
        context[3] += t3;
        context[4] += t4;
        context[5] += t5;
        context[6] += t6;
        context[7] += t7;
    }

    /** Tailors the last output. */
    private void tailorDigestBits () {
        int t;
        if (bitLength == 128) {
            t = (context[7] & 0x000000FF) | (context[6] & 0xFF000000) | 
                (context[5] & 0x00FF0000) | (context[4] & 0x0000FF00);
            context[0] += t >>> 8 | t << 24;

            t = (context[7] & 0x0000FF00) | (context[6] & 0x000000FF) | 
                (context[5] & 0xFF000000) | (context[4] & 0x00FF0000);
            context[1] += t >>> 16 | t << 16;

            t = (context[7] & 0x00FF0000) | (context[6] & 0x0000FF00) |
                (context[5] & 0x000000FF) | (context[4] & 0xFF000000);
            context[2] += t >>> 24 | t << 8;

            t = (context[7] & 0xFF000000) | (context[6] & 0x00FF0000) |
                (context[5] & 0x0000FF00) | (context[4] & 0x000000FF);
            context[3] += t;
        } else if (bitLength == 160) {
            t = (context[7] & 0x3F) | (context[6] & (0x7F << 25)) |
                (context[5] & (0x3F << 19));
            context[0] += t >>> 19 | t << 13;

            t = (context[7] & (0x3F << 6)) | (context[6] & 0x3F) |
                (context[5] & (0x7F << 25));
            context[1] += t >>> 25 | t << 7;

            t = (context[7] & (0x7F << 12)) | (context[6] & (0x3F << 6)) |
                (context[5] & 0x3F);
            context[2] += t;

            t = (context[7] & (0x3F << 19)) | (context[6] & (0x7F << 12)) |
                (context[5] & (0x3F << 6));
            context[3] += (t >>> 6);

            t = (context[7] & (0x7F << 25)) | (context[6] & (0x3F << 19)) |
                (context[5] & (0x7F << 12));
            context[4] += (t >>> 12);
        } else if (bitLength == 192) {
            t = (context[7] & 0x1F) | (context[6] & (0x3F << 26));
            context[0] += t >>> 26 | t << 6;

            t = (context[7] & (0x1F << 5)) | (context[6] & 0x1F);
            context[1] += t;

            t = (context[7] & (0x3F << 10)) | (context[6] & (0x1F << 5));
            context[2] += (t >>> 5);

            t = (context[7] & (0x1F << 16)) | (context[6] & (0x3F << 10));
            context[3] += (t >>> 10);

            t = (context[7] & (0x1F << 21)) | (context[6] & (0x1F << 16));
            context[4] += (t >>> 16);

            t = (context[7] & (0x3F << 26)) | (context[6] & (0x1F << 21));
            context[5] += (t >>> 21);
        } else if (bitLength == 224) {
            context[0] += ((context[7] >>> 27) & 0x1F);
            context[1] += ((context[7] >>> 22) & 0x1F);
            context[2] += ((context[7] >>> 18) & 0x0F);
            context[3] += ((context[7] >>> 13) & 0x1F);
            context[4] += ((context[7] >>>  9) & 0x0F);
            context[5] += ((context[7] >>>  4) & 0x1F);
            context[6] += (context[7]         & 0x0F);
        }
    }

    /*
     * Permutations phi_{i,j}, i=3,4,5, j=1,...,i.
     *
     * passes = 3:
     *            6 5 4 3 2 1 0
     *            | | | | | | | (replaced by)
     *  phi_{3,1}:   1 0 3 5 6 2 4
     *  phi_{3,2}:   4 2 1 0 5 3 6
     *  phi_{3,3}:   6 1 2 3 4 5 0
     *
     * passes = 4:
     *            6 5 4 3 2 1 0
     *            | | | | | | | (replaced by)
     *  phi_{4,1}:   2 6 1 4 5 3 0
     *  phi_{4,2}:   3 5 2 0 1 6 4
     *  phi_{4,3}:   1 4 3 6 0 2 5
     *  phi_{4,4}:   6 4 0 5 2 1 3
     *
     * passes = 5:
     *            6 5 4 3 2 1 0
     *            | | | | | | | (replaced by)
     *  phi_{5,1}:   3 4 1 0 5 2 6
     *  phi_{5,2}:   6 2 1 0 3 4 5
     *  phi_{5,3}:   2 6 0 4 3 1 5
     *  phi_{5,4}:   1 5 3 2 0 4 6
     *  phi_{5,5}:   2 5 0 6 4 3 1
     */
    private int FF_1 (int x7, int x6, int x5, int x4, int x3, int x2, int x1, int x0,
                        int w) {
        int t;
        switch (passes) {
            case 3:     t = f_1(x1, x0, x3, x5, x6, x2, x4);    break;
            case 4:     t = f_1(x2, x6, x1, x4, x5, x3, x0);    break;
            default:    t = f_1(x3, x4, x1, x0, x5, x2, x6);
        }
        return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w;
    }
    private int FF_2 (int x7, int x6, int x5, int x4, int x3, int x2, int x1, int x0,
                        int w, int c) {
        int t;
        switch (passes) {
            case 3:     t = f_2(x4, x2, x1, x0, x5, x3, x6);    break;
            case 4:     t = f_2(x3, x5, x2, x0, x1, x6, x4);    break;
            default:    t = f_2(x6, x2, x1, x0, x3, x4, x5);
        }
        return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
    }
    private int FF_3 (int x7, int x6, int x5, int x4, int x3, int x2, int x1, int x0,
                        int w, int c) {
        int t;
        switch (passes) {
            case 3:     t = f_3(x6, x1, x2, x3, x4, x5, x0);    break;
            case 4:     t = f_3(x1, x4, x3, x6, x0, x2, x5);    break;
            default:    t = f_3(x2, x6, x0, x4, x3, x1, x5);
        }
        return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
    }
    private int FF_4 (int x7, int x6, int x5, int x4, int x3, int x2, int x1, int x0,
                        int w, int c) {
        int t;
        switch (passes) {
            case 4:     t = f_4(x6, x4, x0, x5, x2, x1, x3);    break;
            default:    t = f_4(x1, x5, x3, x2, x0, x4, x6);
        }
        return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
    }
    private int FF_5 (int x7, int x6, int x5, int x4, int x3, int x2, int x1, int x0,
                        int w, int c) {
        int t = f_5(x2, x5, x0, x6, x4, x3, x1);
        return (t >>> 7 | t << 25) + (x7 >>> 11 | x7 << 21) + w + c;
    }

    private int f_1 (int x6, int x5, int x4, int x3, int x2, int x1, int x0) {
        return x1 & (x0 ^ x4) ^ x2 & x5 ^ x3 & x6 ^ x0;
    }
    private int f_2 (int x6, int x5, int x4, int x3, int x2, int x1, int x0) {
        return x2 & (x1 & ~x3 ^ x4 & x5 ^ x6 ^ x0) ^ x4 & (x1 ^ x5) ^ x3 & x5 ^ x0;
    }
    private int f_3 (int x6, int x5, int x4, int x3, int x2, int x1, int x0) {
        return x3 & (x1 & x2 ^ x6 ^ x0) ^ x1 & x4 ^ x2 & x5 ^ x0;
    }
    private int f_4 (int x6, int x5, int x4, int x3, int x2, int x1, int x0) {
        return x4 & (x5 & ~x2 ^ x3 & ~x6 ^ x1 ^ x6 ^ x0) ^ x3 & (x1 & x2 ^ x5 ^ x6) ^ x2 & x6 ^ x0;
    }
    private int f_5 (int x6, int x5, int x4, int x3, int x2, int x1, int x0) {
        return x0 & (x1 & x2 & x3 ^ ~x5) ^ x1 & x4 ^ x2 & x5 ^ x3 & x6;
    }
}
