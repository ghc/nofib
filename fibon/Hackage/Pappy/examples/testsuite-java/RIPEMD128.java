// $Id: RIPEMD128.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: RIPEMD128.java,v $
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
// Revision 1.4  1997/12/07 07:25:29  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/12/07  hopwood
// + Removed '-' from RIPEMD-128 in comments and debug output, since RIPEMD128 is
//   the standard name.
//
// Revision 1.3  1997/12/05 19:09:02  raif
// + removed '-' from string in Debug.getLevel() invocation.
// + cosmetics.
//
// Revision 1.2  1997/11/20 19:36:30  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.3  1997/08/14  David Hopwood
// + Added CONTEXT_LENGTH constant.
// + Implemented engineGetDigestLength method.
// + Required native code version is 2.3.
//
// Revision 0.1.0.2  1997/07/21  David Hopwood
// + Changed name of first parameter to native_hash from ctx to context,
//   to be consistent with native code.
// + Check validity of arguments before calling native_hash.
//
// Revision 0.1.0.1  1997/07/15  David Hopwood
// + Changed 'Object native_lock' to 'boolean native_ok'.
// + Removed 'synchronized (native_lock) { ... }' around native_hash call.
//   It isn't necessary to synchronize calls to native methods in this case,
//   because unlike the cipher classes, the native code for hash functions
//   is stateless.
//
// Revision 0.1.0.0  1997/07/14  R. Naffah
// + Original version
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.md;

import cryptix.util.core.Debug;

import java.io.PrintWriter;
import java.security.MessageDigest;

/**
 * Implements the RIPEMD128 message digest algorithm in Java as per the
 * reference below.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Hans Dobbertin, Antoon Bosselaers and Bart Preneel,
 *        "RIPEMD-160: A Strengthened Version of RIPEMD," 18 April 1996.
 *        A joint publication by the German Information Security Agency
 *        (POB 20 03 63, D-53133 Bonn, Germany)
 *        and the Katholieke Universiteit Leuven, ESAT-COSIC
 *        (K. Mercierlaan 94, B-3001 Heverlee, Belgium).
 * </ol>
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
public class RIPEMD128
extends MessageDigest
implements Cloneable
{
// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static int debuglevel = DEBUG ? Debug.getLevel("RIPEMD128") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("RIPEMD128: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("RIPEMD", 2, 3);
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


// Native implementation of RIPEMD128/160 hash
//...........................................................................

    /** The methods that get the library version. */
    private native static int getLibMajorVersion();
    private native static int getLibMinorVersion();

    /**
     * Transforms context based on 512 bits from input block starting from
     * the offset'th byte.
     *
     * @param context   the current context of this instance.
     * @param block     input array containing data to hash.
     * @param offset    index of where we should start reading from input.
     * @return an error string if one occurs, or null otherwise.
     */
    private native static String
    native_hash (int[] context, byte[] block, int offset);


// RIPEMD128 specific object variables
//...........................................................................

    private static final int BLOCK_LENGTH = 64;
    private static final int CONTEXT_LENGTH = 4;

    /** 128-bit h0, h1, h2, h3 (interim result) */
    private int[] context = new int[CONTEXT_LENGTH];

    /** Number of bytes processed so far. */
    private long count;
    
    /** 512 bits input buffer = 16 x 32-bit words = 8 x 64 bytes */
    private byte[] buffer = new byte[BLOCK_LENGTH];
    
    /** 512 bits work buffer = 16 x 32-bit words */
    private int[] X = new int[16];

    /**
     * Constants for the transform method. They're defined as static because
     * they're common to all RIPEMD128 instantiated objects; and final since
     * they're non-modifiable.
     */
    private static final int[]
        // selection of message word
        R  = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                7,  4, 13,  1, 10,  6, 15,  3, 12,  0,  9,  5,  2, 14, 11,  8,
                3, 10, 14,  4,  9, 15,  8,  1,  2,  7,  0,  6, 13, 11,  5, 12,
                1,  9, 11, 10,  0,  8, 12,  4, 13,  3,  7, 15, 14,  5,  6,  2},
        Rp = {  5, 14,  7,  0,  9,  2, 11,  4, 13,  6, 15,  8,  1, 10,  3, 12,
                6, 11,  3,  7,  0, 13,  5, 10, 14, 15,  8, 12,  4,  9,  1,  2,
               15,  5,  1,  3,  7, 14,  6,  9, 11,  8, 12,  2, 10,  0,  4, 13,
                8,  6,  4,  1,  3, 11, 15,  0,  5, 12,  2, 13,  9,  7, 10, 14},
    
        // amount for rotate left (rol)    
        S  = { 11, 14, 15, 12,  5,  8,  7,  9, 11, 13, 14, 15,  6,  7,  9,  8,
                7,  6,  8, 13, 11,  9,  7, 15,  7, 12, 15,  9, 11,  7, 13, 12,
               11, 13,  6,  7, 14,  9, 13, 15, 14,  8, 13,  6,  5, 12,  7,  5,
               11, 12, 14, 15, 14, 15,  9,  8,  9, 14,  5,  6,  8,  6,  5, 12},
        Sp = {  8,  9,  9, 11, 13, 15, 15,  5,  7,  7,  8, 11, 14, 14, 12,  6,
                9, 13, 15,  7, 12,  8,  9, 11,  7,  7, 12,  7,  6, 15, 13, 11,
                9,  7, 15, 11,  8,  6,  6, 14, 12, 13,  5, 14, 13, 13,  7,  5,
               15,  5,  8, 11, 14, 14,  6, 14,  6,  9, 12,  9, 12,  5, 15,  8};


// Constructors
//...........................................................................
    
    public RIPEMD128 () {
        super("RIPEMD128");
        engineReset();
        link();
    }

    /** This constructor is here to implement cloneability of this class. */
    private RIPEMD128 (RIPEMD128 md) {
        this();
        context = (int[]) (md.context.clone());
        buffer = (byte[]) (md.buffer.clone());
        count = md.count;
    }


// Cloneable method implementation
//...........................................................................

    /** Return a copy of this MD object. */
    public Object clone() { return new RIPEMD128(this); }


// JCE methods
//...........................................................................

    /**
     * Resets this object disregarding any temporary data present at the
     * time of the invocation of this call.
     */
    protected void engineReset () {
        // magic RIPEMD128 initialisation constants
        context[0] = 0x67452301;
        context[1] = 0xEFCDAB89;
        context[2] = 0x98BADCFE;
        context[3] = 0x10325476;
        count = 0L;
        for (int i = 0; i < BLOCK_LENGTH; i++) buffer[i] = 0;
    }

    /** Continue a RIPEMD128 message digest using the input byte. */
    protected void engineUpdate (byte input) {
        // compute number of bytes still unhashed; ie. present in buffer
        int i = (int)(count % BLOCK_LENGTH);
        count++;                                    // update number of bytes
        buffer[i] = input;
        if (i == BLOCK_LENGTH - 1)
            transform(buffer, 0);
    }
    
    /**
     * RIPEMD128 block update operation.
     * <p>
     * Continues a RIPEMD128 message digest operation, by filling the buffer,
     * transform(ing) data in 512-bit message block(s), updating the variables
     * context and count, and leaving (buffering) the remaining bytes in buffer
     * for the next update or finish.
     *
     * @param input     input block
     * @param offset    start of meaningful bytes in input
     * @param len       count of bytes in input block to consider
     */
    public void engineUpdate (byte[] input, int offset, int len) {
        // make sure we don't exceed input's allocated size/length
        if (offset < 0 || len < 0 || (long)offset + len > input.length)
            throw new ArrayIndexOutOfBoundsException();

        // compute number of bytes still unhashed; ie. present in buffer
        int bufferNdx = (int)(count % BLOCK_LENGTH);
        count += len;                               // update number of bytes
        int partLen = BLOCK_LENGTH - bufferNdx;
        int    i = 0;

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
     * Complete the hash computation by performing final operations such
     * as padding. At the return of this engineDigest, the MD engine is
     * reset.
     *
     * @return the array of bytes for the resulting hash value.
     */
    protected byte[] engineDigest () {
        // pad output (in bytes) to 56 mod 64
        int    bufferNdx = (int)(count % BLOCK_LENGTH);
        int    padLen = (bufferNdx < 56) ? (56 - bufferNdx) : (120 - bufferNdx);

        // padding is always binary 1 followed by binary 0s
        byte[] tail = new byte[padLen + 8];
        tail[0] = (byte)0x80;

        // save number of bits, casting the long to an array of 8 bytes
        for (int i = 0; i < 8; i++)
            tail[padLen + i] = (byte)((count * 8) >>> (8 * i));
        // append length before final transform
        engineUpdate(tail, 0, tail.length);

        byte[] result = new byte[16];
        // cast this RIPEMD128's context (array of 4 ints) into an array
        // of 16 bytes.
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                result[i * 4 + j] = (byte)((context[i] >>> (8 * j)) & 0xFF);

        engineReset();
        return result;
    }

    /** <b>SPI</b>: Return the digest length in bytes. */
    protected int engineGetDigestLength() { return 16; }


// Own methods
//...........................................................................
    
    /**
     * RIPEMD128 basic transformation.
     * <p>
     * Transforms context based on 512 bits from input block starting from
     * the offset'th byte.
     */
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
            linkStatus.check(native_hash(context, block, offset));
            return;
        }
        int A, B, C, D, Ap, Bp, Cp, Dp, T, s, i;
        
        // encode 64 bytes from input block into an array of 16 unsigned
        // integers.
        for (i = 0; i < 16; i++)
            X[i] = (block[offset++] & 0xFF)       |
                   (block[offset++] & 0xFF) <<  8 |
                   (block[offset++] & 0xFF) << 16 |
                   (block[offset++] & 0xFF) << 24;
        A = Ap = context[0];
        B = Bp = context[1];
        C = Cp = context[2];
        D = Dp = context[3];

        // rounds 0...15
        for (i = 0; i < 16; i++) {
            s = S[i];
            T = A + (B ^ C ^ D) + X[i];
            A = D; D = C; C = B; B = T << s | T >>> (32 - s);

            s = Sp[i];
            T = Ap + ((Bp & Dp) | (Cp & ~Dp)) + X[Rp[i]] + 0x50A28BE6;
            Ap = Dp; Dp = Cp; Cp = Bp; Bp = T << s | T >>> (32 - s);
        }
        // rounds 16...31
        for (i = 16; i < 32; i++) {
            s = S[i];
            T = A + ((B & C) | (~B & D)) + X[R[i]] + 0x5A827999;
            A = D; D = C; C = B; B = T << s | T >>> (32 - s);
            
            s = Sp[i];
            T = Ap + ((Bp | ~Cp) ^ Dp) + X[Rp[i]] + 0x5C4DD124;
            Ap = Dp; Dp = Cp; Cp = Bp; Bp = T << s | T >>> (32 - s);
        }
        // rounds 32...47
        for (i = 32; i < 48; i++) {
            s = S[i];
            T = A + ((B | ~C) ^ D) + X[R[i]] + 0x6ED9EBA1;
            A = D; D = C; C = B; B = T << s | T >>> (32 - s);

            s = Sp[i];
            T = Ap + ((Bp & Cp) | (~Bp & Dp)) + X[Rp[i]] + 0x6D703EF3;
            Ap = Dp; Dp = Cp; Cp = Bp; Bp = T << s | T >>> (32 - s);
        }
        // rounds 48...63
        for (i = 48; i < 64; i++) {
            s = S[i];
            T = A + ((B & D) | (C & ~D)) + X[R[i]] + 0x8F1BBCDC;
            A = D; D = C; C = B; B = T << s | T >>> (32 - s);

            s = Sp[i];
            T = Ap + (Bp ^ Cp ^ Dp) + X[Rp[i]];
            Ap = Dp; Dp = Cp; Cp = Bp; Bp = T << s | T >>> (32 - s);
        }
        T = context[1] + C + Dp;
        context[1] = context[2] + D + Ap;
        context[2] = context[3] + A + Bp;
        context[3] = context[0] + B + Cp;
        context[0] = T;
    }
}
