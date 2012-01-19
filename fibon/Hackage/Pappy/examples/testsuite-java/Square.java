// $Id: Square.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: Square.java,v $
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
// Revision 1.6  2000/08/17 11:40:52  edwin
// java.* -> xjava.*
//
// Revision 1.5  1998/02/21 19:15:10  iang
// Updated javadoc comment to refer to test data.  Not used yet.
//
// Revision 1.4  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.3  1997/11/20 19:31:41  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.2  1997/11/08 14:39:20  raif
// *** empty log message ***
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.1  1997/10/26  David Hopwood
// + Implementation is now in the same style as Blowfish 0.3.2.x.
// + Formatting changes.
//
// Revision 0.3.2.0  1997/09/18  David Hopwood
// + Ensured that this class is final, and added a comment that this is for
//   security reasons.
//   If it were not final, some VMs have a bug that would allow a subclass
//   that implements Cloneable to call Object's or Cipher's clone() method
//   using invokenonvirtual, which would duplicate the pointer to the native
//   state. Then calling finalize() on the original and using the clone (for
//   example) would result in freed memory being written to :-(.
// + Native linking and debugging brought up to date with Blowfish 0.3.2.0.
//
// Revision 0.3.1.0  1997/07/14  David Hopwood
// + Jump in version number (from 0.1.1.1) to be the same as Blowfish,
//   DES and IDEA.
// + Blowfish, DES, IDEA, and Square 0.3.1.0 are now at the same API
//   level and in the same style.
// + Fixed security bug (out-of-bounds read) introduced in 0.1.1.1 -
//   same bug as for Blowfish 0.3.0.5.
// + Renamed outs variable in engineUpdate to temp, to avoid similarity
//   with out.
//
// Revision 0.1.1.1  1997/07/12  P. Barreto & R. Naffah
// + Amended native implementation as per Blowfish 0.3.0.5.
// + Minor changes for better readability and improved performance.
//
// Revision 0.1.1.0  1997/07/08  R. Naffah & P. Barreto
// + Tested OK with and without Square.DLL;
// + Optimised the sub-keys generation sub-function code by in-lining
//   the values of the diffusion matrix G and hence removing its need
//   (in the code) altogether;
// + Added support for native library;
// + Incorporated D. Hopwood framework for native library linking
//   and debugging;
// + Use initialisation code to generate the SE, SD, TE and TD arrays.
//   Size down from 15K to 6K.
//
// Revision 0.1.0.0  1997/06/30  R. Naffah
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.provider.cipher;

import cryptix.util.core.Debug;
import cryptix.CryptixException;
import cryptix.util.core.ArrayUtil;
import cryptix.util.core.Hex;
import cryptix.provider.key.RawSecretKey;

import java.io.PrintWriter;
import xjava.security.Cipher;
import java.security.Key;
import java.security.InvalidKeyException;
import java.security.Security;
import xjava.security.SymmetricCipher;

/** 
 * A subclass of Cipher to implement a Java class of the Square algorithm.
 * <p>
 * Square is a cipher algorithm developed by Joan Daemen <Daemen.J@banksys.com>
 * and Vincent Rijmen <vincent.rijmen@esat.kuleuven.ac.be>
 * <p>
 * <b>References:</b>
 * <p>
 * <blockquote>
 * <UL>
 *    <li>The
 *        <a href="http://www.esat.kuleuven.ac.be/%7Erijmen/square/">
 *        Square home page</a>
 *        has up-to-date comments, implementations, and certification data.
 *    <li>J. Daemen, L.R. Knudsen, V. Rijmen,
 *        "<a href="http://www.esat.kuleuven.ac.be/%7Erijmen/downloadable/square/fse.ps.gz">
 *        The block cipher Square</a>,"
 *        <cite>Fast Software Encryption</cite>,
 *        LNCS 1267, E. Biham, Ed., Springer-Verlag, 1997, pp. 149-165. 
 * </UL>
 * </blockquote>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 * @author  Paulo S.L.M. Barreto
 * @author  David Hopwood
 * @since   Cryptix 2.2
 */
public final class Square // must be final for security reasons
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("Square") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("Square: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("Square", 2, 3);

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
     * @param  cookie   a valid reference to the native key structure. This
     *                  value is set by the native library upon return from
     *                  native_init() (see link() method at the top).
     * @param  userKey  a byte array representing the user key
     * @return an error String, or null if there was no error
     */
//    private native String native_ks(long cookie, byte[] userKey, boolean encrypt);
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


// Square variables and constants
//............................................................................

    /**
     * Encryption and decryption Square S-Box values
     */
    private static final byte[] SE = new byte[256];              // blank finals
    private static final byte[] SD = new byte[256];

    /**
     * Transposition boxes for encryption and decryption
     */
    private static final int[] TE = new int[256];
    private static final int[] TD = new int[256];

    /**
     * Square block size in bytes
     */
    private static final int BLOCK_SIZE = 16;
    
    /*
     * Square's number of rounds.
     */
    private static final int R = 8;

    /*
     * This instance's Square key schedule.
     */
    private int[][] sKey = new int[R + 1][4];

    private static final int ROOT = 0x1F5;          // for generating GF(2**8)

    private static final int[] OFFSET = new int[R];


// Static code to build some tables
//............................................................................

    static 
    {
        int i, j;

        //
        // Generate exp and log tables used in multiplication over GF(2 ** m)
        //
        byte[] exp = new byte[256];
        byte[] log = new byte[256];

        exp[0] = 1;
        for (i = 1; i < 256; i++) {
            j = exp[i - 1] << 1;
            if ((j & 0x100) != 0)
                j ^= ROOT;      // reduce j (mod ROOT)

            exp[i] = (byte)j;
            log[j & 0xFF] = (byte)i;
        }

        //
        // Compute the substitution box SE[] and its inverse SD[]
        // based on F(x) = x**{-1} plus affine transform of the output.
        //
        SE[0] = 0;
        SE[1] = 1;
        for (i = 2; i < 256; i++)
            SE[i] = exp[(255 - log[i]) & 0xFF];

        //
        // Let SE[i] be represented as an 8-row vector V over GF(2);
        // the affine transformation is A * V + T, where the rows of
        // the 8 x 8 matrix A are contained in trans[0]...trans[7]
        // and the 8-row vector T is contained in 0xB1.
        //
        int[] trans = {0x01, 0x03, 0x05, 0x0F, 0x1F, 0x3D, 0x7B, 0xD6};
        int u, v;
        for (i = 0; i < 256; i++) {
            v = 0xB1;                           // the affine part of the transform
            for (j = 0; j < 8; j++) {
                u = SE[i] & trans[j] & 0xFF;    // column-wise multiplication over GF(2)
                u ^= u >>> 4;                   // sum of all bits of u over GF(2)
                u ^= u >>> 2;
                u ^= u >>> 1;
                u &= 1;
                v ^= u << j;                    // row alignment of the result
            }
            SE[i] = (byte) v;
            SD[v] = (byte) i;                   // inverse substitution box
        }

        //
        // Generate the OFFSET values.
        //
        OFFSET[0] = 1;
        for (i = 1; i < R; i++) {
            OFFSET[i] = mul(OFFSET[i - 1], 2);
            OFFSET[i - 1] <<= 24;
        }
        OFFSET[R - 1] <<= 24;

        //
        // Generate the TE and TD transposition boxes.
        // Notes:
        // (1) The function mul below computes the product of two
        //     elements of GF(2 ** 8) with ROOT as reduction polynomial
        //     (see implementation below in Square's Own Methods section)
        // (2) the values used in computing the TE and TD values are
        //     the GF(2 ** 8) coefficients of the diffusion polynomial c(x)
        //     and its inverse (modulo x ** 4 + 1) d(x), defined in sections
        //     2.1 and 4 of the algorithm designers' paper (see References
        //     above).
        //
        int se, sd;
        for (i = 0; i < 256; i++) {
            se = SE[i] & 0xFF;
            sd = SD[i] & 0xFF;
            TE[i] =  SE[i & 3] == 0 ? 0 :
            mul(se, 2) << 24 | se << 16 | se << 8 | mul(se, 3);
            TD[i] =  SD[i & 3] == 0 ? 0 :
            mul(sd, 14) << 24 | mul(sd, 9) << 16 | mul(sd, 13) << 8 | mul(sd, 11);
        }
    }


// Constructor, finalizer, and clone()
//............................................................................

    /**
     * Constructs a Square cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public Square() {
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
    public int engineBlockSize() { return BLOCK_SIZE; }

    /**
     * <b>SPI</b>: Initializes this cipher for encryption, using the
     * specified key.
     *
     * @param  key  the key to use for encryption.
     * @exception InvalidKeyException when one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The encoded byte array form of the key is zero-length;
     *                <li> The length of the user key data array is out of the
     *                permissible limits.
     *              </ul>
     */
    protected void engineInitEncrypt(Key key)
    throws InvalidKeyException {
        makeKey(key, true);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for decryption.
     * @exception InvalidKeyException when one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The encoded byte array form of the key is zero-length;
     *                <li> The length of the user key data array is out of the
     *                permissible limits.
     *              </ul>
     */
    protected void engineInitDecrypt(Key key)
    throws InvalidKeyException {
        makeKey(key, false);
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
                square(in, inOffset, out, outOffset, TE, SE);
                inOffset += BLOCK_SIZE;
                outOffset += BLOCK_SIZE;
            }
        } else {                // state == DECRYPT
            for (int i = 0; i < blockCount; i++) {
                square(in, inOffset, out, outOffset, TD, SD);
                inOffset += BLOCK_SIZE;
                outOffset += BLOCK_SIZE;
            }
        }
        return inLen;
    }


// Own methods
//............................................................................

    /**
     * Expands a user-key to a working key schedule.
     *
     * @param  key          the user-key object to use.
     * @param  doEncrypt    true for encryption, false for decryption.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not KEY_LENGTH.
     *              </ul>
     */
    private void makeKey(Key key, boolean doEncrypt)
    throws InvalidKeyException {

        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        if (userkey.length != BLOCK_SIZE)
            throw new InvalidKeyException(getAlgorithm() + ": Invalid user key length");

        // If native library available then use it. If not or if
        // native method returned error then revert to 100% Java.

        if (native_lock != null) {
            synchronized(native_lock) {
                try {
//                    linkStatus.check(native_ks(native_cookie, userkey, doEncrypt));
                    linkStatus.check(native_ks(native_cookie, userkey));
                    return;
                } catch (Error error) {
                    native_finalize();
                    native_lock = null;
if (DEBUG && debuglevel > 0) debug(error + ". Will use 100% Java.");
                }
            }
        }

        int i, j = 0;
        if (doEncrypt) {
            for (i = 0; i < 4; i++)
                sKey[0][i] = (userkey[j++] & 0xFF) << 24 | (userkey[j++] & 0xFF) << 16 |
                             (userkey[j++] & 0xFF) <<  8 | (userkey[j++] & 0xFF);

            for (i = 1; i < R + 1; i++) {
                j = i - 1;
                sKey[i][0] = sKey[j][0] ^ rot32L(sKey[j][3], 8) ^ OFFSET[j];
                sKey[i][1] = sKey[j][1] ^ sKey[i][0];
                sKey[i][2] = sKey[j][2] ^ sKey[i][1];
                sKey[i][3] = sKey[j][3] ^ sKey[i][2];
         
                transform(sKey[j], sKey[j]);
            }
        } else {
            int[][] tKey = new int[R + 1][4];

            // apply the key evolution function
            for (i = 0; i < 4; i++)
                tKey[0][i] = (userkey[j++] & 0xFF) << 24 | (userkey[j++] & 0xFF) << 16 |
                             (userkey[j++] & 0xFF) <<  8 | (userkey[j++] & 0xFF);

            for (i = 1; i < R + 1; i++) {
                j = i - 1;
                tKey[i][0] = tKey[j][0] ^ rot32L(tKey[j][3], 8) ^ OFFSET[j];
                tKey[i][1] = tKey[j][1] ^ tKey[i][0];
                tKey[i][2] = tKey[j][2] ^ tKey[i][1];
                tKey[i][3] = tKey[j][3] ^ tKey[i][2];
            }
            for (i = 0; i < R; i++)
                System.arraycopy(tKey[R - i], 0, sKey[i], 0, 4);

            transform(tKey[0], sKey[R]);
        }
    }

    /**
     * Applies the Theta function to an input <i>in</i> in order to
     * produce in <i>out</i> an internal session sub-key.
     * <p>
     * Both <i>in</i> and <i>out</i> are arrays of four ints.
     * <p>
     * Pseudo-code is:
     * <pre>
     *    for (i = 0; i < 4; i++) {
     *        out[i] = 0;
     *        for (j = 0, n = 24; j < 4; j++, n -= 8) {
     *            k = mul(in[i] >>> 24, G[0][j]) ^
     *                mul(in[i] >>> 16, G[1][j]) ^
     *                mul(in[i] >>>  8, G[2][j]) ^
     *                mul(in[i]       , G[3][j]);
     *            out[i] ^= k << n;
     *        }
     *    }
     * </pre>
     */
    private static void transform (int[] in, int[] out) {
        int l3, l2, l1, l0, m;
        for (int i = 0; i < 4; i++) {
            l3 = in[i];
            l2 = l3 >>>  8;
            l1 = l3 >>> 16;
            l0 = l3 >>> 24;
            m  = ((mul(l0, 2) ^ mul(l1, 3) ^ l2 ^ l3) & 0xFF) << 24;
            m ^= ((l0 ^ mul(l1, 2) ^ mul(l2, 3) ^ l3) & 0xFF) << 16;
            m ^= ((l0 ^ l1 ^ mul(l2, 2) ^ mul(l3, 3)) & 0xFF) <<  8;
            m ^= (mul(l0, 3) ^l1 ^ l2 ^ mul(l3, 2)  ) & 0xFF;
            out[i] = m;
        }
    }

    /**
     * Left rotate a 32-bit chunk.
     *
     * @param  x    the 32-bit data to rotate
     * @param  s    number of places to left-rotate by
     * @return the newly permutated value.
     */
    private static int rot32L (int x, int s) { return x << s | x >>> (32 - s); }

    /**
     * Right rotate a 32-bit chunk.
     *
     * @param  x    the 32-bit data to rotate
     * @param  s    number of places to right-rotate by
     * @return the newly permutated value.
     */
    private static int rot32R (int x, int s) { return x >>> s | x << (32 - s); }

    /**
     * Returns the product of two binary numbers a and b, using
     * the generator ROOT as the modulus: p = (a * b) mod ROOT.
     * ROOT Generates a suitable Galois Field in GF(2 ** 8).
     * <p>
     * For best performance call it with abs(b) < abs(a).
     *
     * @param  a    operand for multiply.
     * @param  b    operand for multiply.
     * @return the result of (a * b) % ROOT.
     */
    private static final int mul (int a, int b) {
        if (a == 0)
            return 0;

        a &= 0xFF;
        b &= 0xFF;
        int p = 0;
        while (b != 0) {
            if ((b & 0x01) != 0)
                p ^= a;
            a <<= 1;
            if (a > 0xFF)
                a ^= ROOT;
            b >>>= 1;
        }
        return p & 0xFF;
    }

    /**
     * Applies the Square algorithm (for both encryption and decryption since
     * it is the same) on a 128-bit plain/cipher text into a same length cipher/
     * plain text using the Square formulae, relevant sub-keys, transposition
     * and S-Box values.
     *
     * @param  in       contains the plain-text 128-bit block.
     * @param  off      start index within input where data is considered.
     * @param  out      will contain the cipher-text block.
     * @param  outOff   index in out where cipher-text starts.
     * @param  T        reference to either the encryption (TE) or decryption
     *                  (TD) transposition vector.
     * @param  S        reference to either the encryption (SE) or decryption
     *                  (SD) S-Box values.
     */
    private void
    square (byte[] in, int off, byte[] out, int outOff, int[] T, byte[] S) {

        int a = (in[off++] & 0xFF) << 24 | (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 | (in[off++] & 0xFF);
        int b = (in[off++] & 0xFF) << 24 | (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 | (in[off++] & 0xFF);
        int c = (in[off++] & 0xFF) << 24 | (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 | (in[off++] & 0xFF);
        int d = (in[off++] & 0xFF) << 24 | (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 | (in[off++] & 0xFF);

        int aa, bb, cc, dd;
        int i, j, k;

        a ^= sKey[0][0];
        b ^= sKey[0][1];
        c ^= sKey[0][2];
        d ^= sKey[0][3];

        // R - 1 full rounds
        for (i = 1; i < R; i++) {
            aa =       T[(a >>> 24) & 0xFF]      ^
                rot32R(T[(b >>> 24) & 0xFF],  8) ^
                rot32R(T[(c >>> 24) & 0xFF], 16) ^
                rot32R(T[(d >>> 24) & 0xFF], 24) ^ sKey[i][0];

            bb =       T[(a >>> 16) & 0xFF]      ^
                rot32R(T[(b >>> 16) & 0xFF],  8) ^
                rot32R(T[(c >>> 16) & 0xFF], 16) ^
                rot32R(T[(d >>> 16) & 0xFF], 24) ^ sKey[i][1];

            cc =       T[(a >>>  8) & 0xFF]      ^
                rot32R(T[(b >>>  8) & 0xFF],  8) ^
                rot32R(T[(c >>>  8) & 0xFF], 16) ^
                rot32R(T[(d >>>  8) & 0xFF], 24) ^ sKey[i][2];

            dd =       T[ a         & 0xFF]      ^
                rot32R(T[ b         & 0xFF],  8) ^
                rot32R(T[ c         & 0xFF], 16) ^
                rot32R(T[ d         & 0xFF], 24) ^ sKey[i][3];

            a = aa;
            b = bb;
            c = cc;
            d = dd;
        }
        // last round (diffusion becomes only transposition)
        for (i = 0, j = 24; i < 4; i++, j -= 8) {
            k = (S[(a >>> j) & 0xFF] & 0xFF) << 24 |
                (S[(b >>> j) & 0xFF] & 0xFF) << 16 |
                (S[(c >>> j) & 0xFF] & 0xFF) <<  8 |
                (S[(d >>> j) & 0xFF] & 0xFF);
            k ^= sKey[R][i];

            out[outOff++] = (byte)((k >>> 24) & 0xFF);
            out[outOff++] = (byte)((k >>> 16) & 0xFF);
            out[outOff++] = (byte)((k >>>  8) & 0xFF);
            out[outOff++] = (byte) (k         & 0xFF);
        }
    }


// Test methods
//...........................................................................

    static final private String[][] tests = {
        { "000102030405060708090a0b0c0d0e0f",   // KEY
          "000102030405060708090a0b0c0d0e0f",   // PLAINTEXT
          "7C3491D94994E70F0EC2E7A5CCB5A14F"},  // CIPHERTEXT
         
        { "000102030405060708090a0b0c0d0e0f",
          "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f",
          "7C3491D94994E70F0EC2E7A5CCB5A14F7C3491D94994E70F0EC2E7A5CCB5A14F"}
    };


    public static final void main (String[] args) {
        try { self_test(); }
        catch (Exception e) { e.printStackTrace(); }
    }
    
    private static void self_test()
    throws Exception {
        Cipher cryptor = Cipher.getInstance("Square", "Cryptix");
        RawSecretKey userKey;
        byte[] tmp, pt, ct;

        for (int i = 0; i < tests.length; i++) {
            userKey = new RawSecretKey("Square", Hex.fromString(tests[i][0]));
            pt = Hex.fromString(tests[i][1]);
            ct = Hex.fromString(tests[i][2]);

            cryptor.initEncrypt(userKey);
            tmp = cryptor.crypt(pt);
            if (!ArrayUtil.areEqual(ct, tmp)) {
                System.out.println("     input: " + Hex.toString(pt));
                System.out.println("  computed: " + Hex.toString(tmp));
                System.out.println(" certified: " + Hex.toString(ct));
                throw new CryptixException("encrypt #"+ i +" failed");
            }

            cryptor.initDecrypt(userKey);
            tmp = cryptor.crypt(ct);
            if (!ArrayUtil.areEqual(pt, tmp))
                throw new CryptixException("decrypt #"+ i +" failed");
        }
if (DEBUG && debuglevel > 0) debug("Self-test OK");
    }
}
