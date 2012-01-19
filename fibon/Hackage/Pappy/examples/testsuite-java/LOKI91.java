// $Id: LOKI91.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: LOKI91.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 17:36:02  baford
// Checked in my "test suite":
// cryptix32-20001002-r3.2.0/src/cryptix/provider/cipher/*.java
//
// Revision 1.3  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.2  1997/12/09 04:43:45  hopwood
// + Various.
//
// Revision 1.1  1997/12/05 19:07:29  raif
// *** empty log message ***
//
// Revision 1.4  1997/12/05 19:02:28  raif
// + changed spelling to LOKI91.
//
// Revision 1.3  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// + Removed weak key info from documentation; included reference to
//   LOKI91KeyGenerator instead.
//
// Revision 1.2  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.1.1  1997/10/29  David Hopwood
// + Renamed class to Loki91 (from LOKI91). Loki is not an acronym.
//
// Revision 0.1.1.0  1997/08/17  David Hopwood
// + Tightened some of the access specifiers (e.g. SPI methods were public,
//   and are now protected).
// + Ensured that this class is final, and added a comment that this is for
//   security reasons.
//   If it were not final, some VMs have a bug that would allow a subclass
//   that implements Cloneable to call Object's or Cipher's clone() method
//   using invokenonvirtual, which would duplicate the pointer to the native
//   state. Then calling finalize() on the original and using the clone (for
//   example) would result in freed memory being written to :-(.
// + Required native code version is 2.3.
//
// Revision 0.1.0.5  1997/08/01  David Hopwood
// + Moved isWeak() to LOKI91KeyGenerator, to be consistent with
//   DES/TripleDES.
// + Removed checks for weak keys from this class.
//
// Revision 0.1.0.4  1997/07/22  R. Naffah
// + Made the isWeak() method static for use from LOKI91KeyGenerator
//   instances.
//
// Revision 0.1.0.3  1997/07/16  R. Naffah
// + Added native support. Tested OK with/without LOKI91.DLL.
// + Removed the instance initializers for the S and P arrays and
//   replaced them with static code.
// + Modified return type of exp31() to byte rather than int.
// + Minor changes for a slight performance improvement. Thanks
//   Lawrie :))
// + Changed the way we store and check weak and semi-weak keys.
// + Treat weak and semi-weak keys in the same manner.
// + Use new features of JDK 1.1 to reduce the size of the .class
//   file by pre-computing the S-Box and Permutation arrays.
//
// Revision 0.1.0.2  1997/??/??  R. Naffah
// + Uses pre-computed values for the S-Box based on code obtained
//   under license from the UNSW, as well as pre-computation of a
//   Permutation array and a recursion formula.
//
// Revision 0.1.0.1  1997/??/??  R. Naffah
// + Original version. Ported to Java from the C-code implementation
//   (v3.0) by:
//   Matthew Kwan <mkwan@crypto.cs.adfa.oz.au> and
//   Lawrence Brown <lpb@cs.adfa.oz.au>
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.cipher;

import cryptix.util.core.Debug;
import cryptix.CryptixException;

import java.io.PrintWriter;
import xjava.security.Cipher;
import java.security.Key;
import java.security.KeyException;
import java.security.Security;
import xjava.security.SymmetricCipher;

/**
 * LOKI is a proposed Australian alternative cipher to DES. It was first
 * designed by L. Brown, J. Pieprzyk and J. Seberry in 1990 and later re-
 * designed (and renamed LOKI91), with improved resistance to differential
 * cryptanalysis, by L. Brown, M. Kwan, J. Pieprzyk and J. Seberry.
 * <p>
 * LOKI91 is a 64-bit symmetric block cipher with a 64-bit user key.
 * See <samp><a href=cryptix.provider.key.LOKI91KeyGenerator.html">
 * LOKI91KeyGenerator</a></samp> for information about weak keys.
 * <p>
 * This current version is based on special C-code obtained, under license,
 * from The School of Computer Science, UC, UNSW. The speed gain is achieved
 * by pre-computing the 4,096 values of the substitutions (S-Box) used by the
 * algorithm as well as the permutations for all possible 256 input values for
 * each of the four 8-bit blocks.
 * <p>
 * <b>References:<b>
 * <ol>
 *   <li> Bruce Schneier,
 *        <cite>Applied Cryptography, 2nd edition</cite>,
 *        John Wiley &amp; Sons 1996, 314-316.
 *        <p>
 *   <li> Personal correspondance with Dr. Lawrence Brown.
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 * @since   Cryptix 2.2.2
 */
public final class LOKI91 // must be final for security reasons
extends Cipher
implements SymmetricCipher
{
// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("LOKI91") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("LOKI91: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("LOKI91", 2, 3);

    /**
     * Gets an object representing the native linking status of this class.
     */
    public static cryptix.util.core.LinkStatus getLinkStatus() { return linkStatus; }

    /**
     * The native reference to the current native key schedule
     * structure. Defaults to 0 but is set by native code after a
     * successful call to native_init().
     * <p>
     * IMPORTANT: Do not change the name of this variable without
     * duplicating the same in the native code.
     */
    private long native_cookie;

    /**
     * This object must be synchronized on while calling any native instance
     * method. It is null if the native code is not being used (e.g. the
     * library did not load successfully, or the user disabled its use in
     * the properties file).
     */
    private Object native_lock; // defaults to null

    private void link() {
        synchronized(linkStatus) {
            try {
                if (linkStatus.attemptLoad()) {
                    linkStatus.checkVersion(getLibMajorVersion(), getLibMinorVersion());
                    linkStatus.check(native_clinit());
                }
                if (linkStatus.useNative()) {
                    linkStatus.check(native_init());
                    native_lock = new Object();
                }
            } catch (UnsatisfiedLinkError e) {
                linkStatus.fail(e);
if (DEBUG && debuglevel > 2) debug(e.getMessage());
            }
if (DEBUG && debuglevel > 2) debug("Using native library? " + (native_lock != null));
        }
    }


// Native support API
//...........................................................................

    // The methods that get the library version.
    private native static int getLibMajorVersion();
    private native static int getLibMinorVersion();

    /**
     * Static initialization and self-test method for the native code.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_clinit();

    /**
     * Initializes the native state for this cipher object and allocates
     * needed native storage for the internal key schedule. A reference
     * to the newly allocated space, if successful, is stored in the
     * instance variable <code>native_cookie</code>.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_init();

    /**
     * This function expands the user key to an internal form.
     *
     * @param cookie    a valid reference to the native key structure. This
     *                  value is set by the native library upon return from
     *                  native_init() (see link() method at the top).
     * @param userKey   a byte array representing the user key
     * @return an error String, or null if there was no error
     */
    private native String native_ks(long cookie, byte[] userKey);

    /**
     * Encrypts/decrypts a data block.
     * <p>
     * FUTURE: possibly change this to be able to process more than one block,
     * to reduce native method call overhead.
     * <p>
     * SECURITY: the caller <strong>must</strong> ensure that:
     * <ul>
     *   <li> <code>in != null</code>
     *   <li> <code>out != null</code>
     *   <li> <code>inOffset >= 0</code>
     *   <li> <code>(long)inOffset + BLOCK_SIZE <= in.length</code>
     *   <li> <code>outOffset >= 0</code>
     *   <li> <code>(long)outOffset + BLOCK_SIZE <= out.length</code>
     * </ul>
     *
     * @param  cookie       a valid reference to the native key structure. This
     *                      value is set by the native library upon return from
     *                      native_init() (see link() method at the top).
     * @param  in           input array containing data to encrypt or decrypt
     *                      depending on the value of the encrypt boolean parameter.
     * @param  inOffset     index of where we should start reading from input.
     * @param  out          output array containing data decrypted or encrypted
     *                      depending on the value of the encrypt boolean parameter.
     * @param  outOffset    index of where we should start writing to output.
     * @param  encrypt      if true then encrypt, otherwise decrypt.
     * @return the number of bytes crypted (always BLOCK_SIZE) or 0 if an error
     *                      occurred.
     */
    private native int native_crypt(long cookie, byte[] in, int inOffset,
                                    byte[] out, int outOffset, boolean encrypt);

    /**
     * Finalizes the native state for this object.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_finalize();


// Loki Variables and constants
//............................................................................

    /**
     * Number of bytes in a Loki data-block.
     */
    private static final int BLOCK_SIZE = 8;

    /**
     * Number of rounds for the Loki algorithm.
     */
    private static final int ROUNDS = 16;

    /**
     * Table specifying the pre-computed S-box values. We use the new
     * Blank Final feature of JDK 1.1 to generate it.
     */
    private static final byte[] S = new byte[0x1000];

    /**
     * Pre-computed permutation arrays generated by applying the Loki
     * original permutation array <code>Po</code> on four 8-bit blocks.
     * <p>
     * When you generate these values, you'll notice that the permutation
     * operation (a) for the first group of 8-bit, represents a value
     * between 0 and 256 as an expanded version of its binary representation,
     * with 3 extra 0 bits between each bit; and (b) for the other three 8-bit
     * groups, applies the following recursion formula: Pi+1[j] = Pi[j] * 2
     * (or left-shift by one bit).
     * <p>
     * As a consequence, we don't need to pre-compute all four groups of
     * permutations. Only the first is enough! This translates into a run-
     * time space savings of at least 3 * 256 * ints or 3KB.
     * <p>
     * The generation code per-se, like that of the S-Box relies on the JDK
     * 1.1 new feature: Blank Finals.
     */
    private static final int[] P = new int[256];

    /**
     * Subkeys for the algorithm 16 rounds.
     */
    private int[] sKey = new int[ROUNDS];


// Static code
//............................................................................

    static {
        // initialising the S array
        int[] sGen = {          // LOKI91 S-box generator polynomials (bases)
            375, 379, 391, 395, 397, 415, 419, 425,
            433, 445, 451, 463, 471, 477, 487, 499};
        int r;                  // row value --top 2 & bottom 2
        int c;                  // column value --middle 8 bits
        int t;                  // base value for S functions

        for (int i = 0; i < 0x1000; i++) {
            r = ((i >>> 8) & 0x0C) | (i & 0x3);
            c =  (i >>> 2) & 0xFF;
            t = (c + ((r * 17) ^ 0xFF)) & 0xFF;
            S[i] = exp31(t, sGen[r]);
        }

        // initialising the P array
        int[] Po = {
            31, 23, 15, 7, 30, 22, 14,  6, 29, 21, 13, 5, 28, 20, 12,  4,
            27, 19, 11, 3, 26, 18, 10,  2, 25, 17,  9, 1, 24, 16,  8,  0};
        int s;                  // the newly permuted 32-bit block

        for (int i = 0; i < 0x100; i++) {
            s = 0;
            for (int j = 0; j < 32; j++)
                s |= ((i >>> Po[j]) & 0x1) << (31 - j);
            P[i] = s;
        }
    }

    /**
     * Returns the result of exponentiation given the base and generator
     * of GF.
     *
     * @param b  base of exponentiation, the exponent being always 31.
     * @param g  irreducible polynomial generating Galois Field (GF).
     * @return the result of (b ** 31) mod g.
     */
    private static final byte exp31 (int b, int g) {
        if (b == 0) return 0;

        int r = b;                    // r = b **  1
        b = mult(b, b, g);            // b = b **  2
        r = mult(r, b, g);            // r = b **  3
        b = mult(b, b, g);            // b = b **  4
        r = mult(r, b, g);            // r = b **  7
        b = mult(b, b, g);            // b = b **  8
        r = mult(r, b, g);            // r = b ** 15
        b = mult(b, b, g);            // b = b ** 16
        return (byte)mult(r, b, g);    // r = b ** 31
    }

    /**
     * Returns the product of two binary numbers a and b, using the generator
     * g as the modulus: p = (a * b) mod g. g Generates a suitable Galois
     * Field in GF(2 ** 8).
     *
     * @param a  operand for multiply.
     * @param b  operand for multiply.
     * @param g  irreducible polynomial generating Galois Field.
     * @return the result of (a * b) % g.
     */
    private static final int mult (int a, int b, int g) {
        int p = 0;
        while (b != 0) {
            if ((b & 0x01) != 0) p ^= a;
            a <<= 1;
            if (a > 0xFF) a ^= g;
            b >>>= 1;
        }
        return p;
    }


// Constructor, finalizer, and clone()
//............................................................................

    /**
     * Constructs a LOKI91 cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public LOKI91 () {
        super(false, false, "Cryptix");
        link();
    }

    /**
     * Cleans up resources used by this instance, if necessary.
     */
    protected final void finalize() {
        if (native_lock != null) {
            synchronized(native_lock) {
                String error = native_finalize(); // may be called more than once
                if (error != null)
                    debug(error + " in native_finalize");
            }
        }
    }

    /**
     * Always throws a CloneNotSupportedException (cloning of ciphers is not
     * supported for security reasons).
     */
    public final Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }


// Implementation of JCE methods
//............................................................................
    
    /**
     * <b>SPI</b>: Returns the length of an input block, in bytes.
     *
     * @return the length in bytes of an input block for this cipher.
     */
    protected int engineBlockSize () { return BLOCK_SIZE; }

    /**
     * <b>SPI</b>: Initializes this cipher for encryption, using the
     * specified key.
     *
     * @param key   the key to use for encryption.
     * @exception   KeyException if the key is invalid.
     */
    protected void engineInitEncrypt (Key key)
    throws KeyException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param key   the key to use for decryption.
     * @exception   KeyException if the key is invalid.
     */
    protected void engineInitDecrypt (Key key)
    throws KeyException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: This is the main engine method for updating data.
     * <p>
     * <i>in</i> and <i>out</i> may be the same array, and the input and output
     * regions may overlap.
     *
     * @param  in           the input data.
     * @param  inOffset     the offset into in specifying where the data starts.
     * @param  inLen        the length of the subarray.
     * @param  out          the output array.
     * @param  outOffset    the offset indicating where to start writing into
     *                      the out array.
     * @return the number of bytes written.
     * @exception CryptixException if the native library is being used, and it
     *                      reports an error.
     */
    protected int
    engineUpdate(byte[] in, int inOffset, int inLen, byte[] out, int outOffset) {
        if (inLen < 0) throw new IllegalArgumentException("inLen < 0");
        int blockCount = inLen / BLOCK_SIZE;
        inLen = blockCount * BLOCK_SIZE;

        boolean doEncrypt = (getState() == ENCRYPT);

        // Avoid overlapping input and output regions.
        if (in == out && (outOffset >= inOffset && outOffset < (long)inOffset+inLen ||
                          inOffset >= outOffset && inOffset < (long)outOffset+inLen)) {
            byte[] newin = new byte[inLen];
            System.arraycopy(in, inOffset, newin, 0, inLen);
            in = newin;
            inOffset = 0;
        }
        if (native_lock != null) {
            synchronized(native_lock) {
                // If in == null || out == 0, evaluating their lengths will throw a
                // NullPointerException.

                if (inOffset < 0 || (long)inOffset + inLen > in.length ||
                    outOffset < 0 || (long)outOffset + inLen > out.length)
                    throw new ArrayIndexOutOfBoundsException(getAlgorithm() +
                        ": Arguments to native_crypt would cause a buffer overflow");

                // In future, we may pass more than one block to native_crypt to reduce
                // native method overhead.

                for (int i = 0; i < blockCount; i++) {
                    if (0 == native_crypt(native_cookie, in, inOffset, out, outOffset,
                                          doEncrypt))
                        throw new CryptixException(getAlgorithm() + ": Error in native code");

                    inOffset += BLOCK_SIZE;
                    outOffset += BLOCK_SIZE;
                }
            }
        } else if (doEncrypt) { // state == ENCRYPT
            for (int i = 0; i < blockCount; i++) {
                blockEncrypt(in, inOffset, out, outOffset);
                inOffset += BLOCK_SIZE;
                outOffset += BLOCK_SIZE;
            }
        } else {                // state == DECRYPT
            for (int i = 0; i < blockCount; i++) {
                blockDecrypt(in, inOffset, out, outOffset);
                inOffset += BLOCK_SIZE;
                outOffset += BLOCK_SIZE;
            }
        }
        return inLen;
    }


// Own methods
//............................................................................

    /**
     * Expands a userKey to a working Loki key (sKey).
     *
     * @param key expected 64-bit user-key from which we'll
     *            build a session key.
     */
    private synchronized void makeKey (Key key)
    throws KeyException {

        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new KeyException("Null LOKI91 key");

        // consider only 8 bytes if user data is longer than that
        if (userkey.length < 8)
            throw new KeyException("Invalid LOKI91 user key length");

        // If native library available then use it. If not or if
        // native method returned error then revert to 100% Java.

        if (native_lock != null) {
            synchronized(native_lock) {
                try {
                    linkStatus.check(native_ks(native_cookie, userkey));
                    return;
                } catch (Error error) {
                    native_finalize();
                    native_lock = null;
if (DEBUG && debuglevel > 0) debug(error + ". Will use 100% Java.");
                }
            }
        }

        sKey[0] = (userkey[0] & 0xFF) << 24 |
                  (userkey[1] & 0xFF) << 16 |
                  (userkey[2] & 0xFF) <<  8 |
                  (userkey[3] & 0xFF);
        sKey[1] = sKey[0] << 12 | sKey[0] >>> 20;
        sKey[2] = (userkey[4] & 0xFF) << 24 |
                  (userkey[5] & 0xFF) << 16 |
                  (userkey[6] & 0xFF) <<  8 |
                  (userkey[7] & 0xFF);
        sKey[3] = sKey[2] << 12 | sKey[2] >>> 20;

        for (int i = 4; i < ROUNDS; i += 4) {
            sKey[i    ] = sKey[i - 3] << 13 | sKey[i - 3] >>> 19;
            sKey[i + 1] = sKey[i    ] << 12 | sKey[i    ] >>> 20;
            sKey[i + 2] = sKey[i - 1] << 13 | sKey[i - 1] >>> 19;
            sKey[i + 3] = sKey[i + 2] << 12 | sKey[i + 2] >>> 20;
        }
    }

    /**
     * Main LOKI91 encryption routine.
     *
     * @param in     the input plain text 64-bit vlock.
     * @param off    the offset into <i>in</i> specifying where
     *               data starts.
     * @param out    will contain the cipher-text block.
     * @param outOff index in out where cipher-text starts.
     */
    private void blockEncrypt (byte[] in, int off, byte[] out, int outOff) {
        int L = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                 in[off++] & 0xFF;
        int R = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                 in[off  ] & 0xFF;
        int a;

        for (int i = 0; i < ROUNDS;) {            // encrypt with the 16 subkeys
            a = R ^ sKey[i++];
            L ^= P[S[ a                  & 0xFFF] & 0xFF]      |
                 P[S[(a >>>  8)          & 0xFFF] & 0xFF] << 1 |
                 P[S[(a >>> 16)          & 0xFFF] & 0xFF] << 2 |
                 P[S[(a >>> 24 | a << 8) & 0xFFF] & 0xFF] << 3;

            a = L ^ sKey[i++];
            R ^= P[S[ a                  & 0xFFF] & 0xFF]      |
                 P[S[(a >>>  8)          & 0xFFF] & 0xFF] << 1 |
                 P[S[(a >>> 16)          & 0xFFF] & 0xFF] << 2 |
                 P[S[(a >>> 24 | a << 8) & 0xFFF] & 0xFF] << 3;
        }
        out[outOff++] = (byte)(R >>> 24);
        out[outOff++] = (byte)(R >>> 16);
        out[outOff++] = (byte)(R >>>  8);
        out[outOff++] = (byte) R;
        out[outOff++] = (byte)(L >>> 24);
        out[outOff++] = (byte)(L >>> 16);
        out[outOff++] = (byte)(L >>>  8);
        out[outOff  ] = (byte) L;
    }

    /**
     * Main LOKI91 decryption routine.
     *
     * @param in     the 64-bit cipher text buffer.
     * @param off    the offset into <i>in</i> specifying where
     *               data starts.
     * @param out    will contain the plain-text block.
     * @param outOff index in out where plain-text starts.
     */
    private void blockDecrypt (byte[] in, int off, byte[] out, int outOff) {
        int L = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                 in[off++] & 0xFF;
        int R = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                 in[off  ] & 0xFF;
        int a;

        for (int i = ROUNDS; i > 0;) {            // subkeys in reverse order
            a = R ^ sKey[--i];
            L ^= P[S[ a                  & 0xFFF] & 0xFF]      |
                 P[S[(a >>>  8)          & 0xFFF] & 0xFF] << 1 |
                 P[S[(a >>> 16)          & 0xFFF] & 0xFF] << 2 |
                 P[S[(a >>> 24 | a << 8) & 0xFFF] & 0xFF] << 3;

            a = L ^ sKey[--i];
            R ^= P[S[ a                  & 0xFFF] & 0xFF]      |
                 P[S[(a >>>  8)          & 0xFFF] & 0xFF] << 1 |
                 P[S[(a >>> 16)          & 0xFFF] & 0xFF] << 2 |
                 P[S[(a >>> 24 | a << 8) & 0xFFF] & 0xFF] << 3;
        }
        out[outOff++] = (byte)(R >>> 24);
        out[outOff++] = (byte)(R >>> 16);
        out[outOff++] = (byte)(R >>>  8);
        out[outOff++] = (byte) R;
        out[outOff++] = (byte)(L >>> 24);
        out[outOff++] = (byte)(L >>> 16);
        out[outOff++] = (byte)(L >>>  8);
        out[outOff  ] = (byte) L;
    }
}
