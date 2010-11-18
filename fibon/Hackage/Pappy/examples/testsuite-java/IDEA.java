// $Id: IDEA.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: IDEA.java,v $
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
// Revision 1.4  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.3  1997/11/29 04:42:55  hopwood
// + Changes to engineUpdate method.
//
// + Fixed invertKey to do the right thing (i.e. nothing) when the native
//   library is being used.
// + Formatting changes.
//
// Revision 1.2  1997/11/20 19:31:39  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.0  1997/08/21  David Hopwood
// + Removed all deprecated methods and fields. Cryptix 2.2 compatibility
//   is now handled by the separate cryptix.security.IDEA class.
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
// Revision 0.3.1.0  1997/07/14  David Hopwood
// + Blowfish, DES, IDEA, and Square 3.1.0 are now at the same API
//   level and in the same style.
// + Fixed security bug (out-of-bounds read) introduced in 3.0.1 -
//   same bug as for Blowfish 3.0.5.
// + Renamed outs variable in engineUpdate to temp, to avoid similarity
//   with out.
//
// Revision 0.3.0.1  1997/07/09  R. Naffah
// + Fully IJCE-compliant!
// + Tested OK with and without IDEA.DLL.
// + Re-wrote the JNI code to conform with modifs;
// + Changed method signatures to follow same style/params as the other
//   ciphers in the library;
// + Changed methods mul and inv to return shorts;
// + Now use one short[] as the session key;
// + Brought it to the same API level as Blowfish 3.0.5 and DES 3.0.2;
//
// Revision 0.3.0.0  1997/07/0?  David Hopwood
// + JCE, native linking, debugging, etc...
//
// Revision 0.2.5.2  1997/04/03  Systemics
// + Documentation.  Deprecated public variables and made methods private.
//
// Revision 0.2.5.1  1997/03/15  Jill Baker
// + Moved this file here from old namespace.
//
// Revision 0.2.5.0  1997/02/24  Systemics
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1995-97 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
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
import xjava.security.SymmetricCipher;

/**
 * IDEA is a block cipher with a key length of 16 bytes and a block length of
 * 8 bytes. It is highly popular, being the original cipher in PGP, and has
 * received a lot of cryptanalytic attention.
 * <p>
 * IDEA was written by Dr. X. Lai and Prof. J. Massey.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> See the <a href="http://www.ascom.ch/Web/systec/security/idea.htm">IDEA page</a>
 *        for more details.
 *        <p>
 *   <li> The algorithm is subject to patent claims by
 *        <a href="http://www.ascom.ch/systec/">Ascom Systec Ltd</a>
 *        (applied for May 1991), and is
 *        <a href="http://www.ascom.ch/Web/systec/policy/normal/policy.html">licensable</a>.
 * </ol>
 * <p>
 *
 * <b>Copyright</b> &copy; 1995-1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 *
 * <p><b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 * @author  David Hopwood
 * @author  Raif S. Naffah
 * @since   Cryptix 2.2.2
 */
public final class IDEA // must be final for security reasons
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("IDEA") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("IDEA: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("IDEA", 2, 3);

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


// IDEA constants and variables
//............................................................................

    private static final int
        ROUNDS = 8,
        BLOCK_SIZE = 8,
        KEY_LENGTH = 16;

    /**
     * The number of array elements in the expanded key schedule, i.e. both
     * encryption and decryption keys.
     * <p>
     * For IDEA this is <code>6 * ROUNDS + 4 = 52</code>.
     */
    private static final int INTERNAL_KEY_LENGTH = 52;

    /**
     * The key schedule.
     */
    private short[] ks = new short[INTERNAL_KEY_LENGTH];


// Constructor, finalizer, and clone()
//............................................................................

    /**
     * Constructs an IDEA cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public IDEA() {
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
     * @param  key  the key to use for encryption.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not KEY_LENGTH.
     *              </ul>
     */
    protected void engineInitEncrypt (Key key)
    throws InvalidKeyException, CryptixException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for encryption.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not KEY_LENGTH.
     *              </ul>
     */
    protected void engineInitDecrypt (Key key)
    throws InvalidKeyException, CryptixException {
        makeKey(key);
        invertKey();
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
     * Expands a user-key to a working key schedule.
     * <p>
     * IDEA has separate key schedules for encryption and decryption. This
     * generates the encryption schedule; calling <code>invertKey</code>
     * afterward will generate the decryption schedule.
     *
     * @param  key  the user-key object to use.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is outside the
     *                     permissible limits.
     *              </ul>
     * @exception CryptixException if a self-test fails.
     */
    private void makeKey(Key key)
    throws InvalidKeyException, CryptixException {

        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        if (userkey.length != KEY_LENGTH)
            throw new InvalidKeyException(getAlgorithm() + ": Invalid user key length");

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

        /*
         * Expand user key of 128 bits to full 832 bits of encryption key.
         */
        ks[0] = (short)((userkey[ 0] & 0xFF) << 8 | (userkey[ 1] & 0xFF));
        ks[1] = (short)((userkey[ 2] & 0xFF) << 8 | (userkey[ 3] & 0xFF));
        ks[2] = (short)((userkey[ 4] & 0xFF) << 8 | (userkey[ 5] & 0xFF));
        ks[3] = (short)((userkey[ 6] & 0xFF) << 8 | (userkey[ 7] & 0xFF));
        ks[4] = (short)((userkey[ 8] & 0xFF) << 8 | (userkey[ 9] & 0xFF));
        ks[5] = (short)((userkey[10] & 0xFF) << 8 | (userkey[11] & 0xFF));
        ks[6] = (short)((userkey[12] & 0xFF) << 8 | (userkey[13] & 0xFF));
        ks[7] = (short)((userkey[14] & 0xFF) << 8 | (userkey[15] & 0xFF));

        for (int i = 0, zoff = 0, j = 8; j < INTERNAL_KEY_LENGTH; i &= 7, j++) {
            i++;
            ks[i + 7 + zoff] = (short)((ks[(i & 7) + zoff] << 9) |
                ((ks[((i + 1) & 7) + zoff] >>> 7) & 0x1FF));
            zoff += i & 8;
        }
    }

    /**
     * IDEA, being a symmetric cipher, uses the same algorithm for both
     * encryption and decryption. What changes is the key. For the inverse
     * operation (of either encryption or decryption) the following method
     * inverts the key to make it ready for application of the cipher method.
     */
    private void invertKey () {
        if (native_lock == null) {
            int i, j = 4, k = INTERNAL_KEY_LENGTH - 1;
            short[] temp = new short[INTERNAL_KEY_LENGTH];
            temp[k--] = inv(ks[3]);
            temp[k--] = (short) -ks[2];
            temp[k--] = (short) -ks[1];
            temp[k--] = inv(ks[0]);
            for (i = 1; i < ROUNDS; i++, j += 6) {
                temp[k--] = ks[j + 1];
                temp[k--] = ks[j];
                temp[k--] = inv(ks[j + 5]);
                temp[k--] = (short) -ks[j + 3];
                temp[k--] = (short) -ks[j + 4];
                temp[k--] = inv(ks[j + 2]);
            }
            temp[k--] = ks[j + 1];
            temp[k--] = ks[j];
            temp[k--] = inv(ks[j + 5]);
            temp[k--] = (short) -ks[j + 4];
            temp[k--] = (short) -ks[j + 3];
            temp[k--] = inv(ks[j + 2]);
            System.arraycopy(temp, 0, ks, 0, INTERNAL_KEY_LENGTH);
        }
    }

    /**
     * IDEA encryption/decryption algorithm using the current key schedule.
     *
     * @param  in       an array containing the plaintext block
     * @param  inOffset the starting offset of the plaintext block
     * @param  out      an array containing the ciphertext block
     * @param  inOffset the starting offset of the ciphertext block
     */
    private void blockEncrypt (byte[] in, int inOffset, byte[] out, int outOffset) {
        short
            x1 = (short)(((in[inOffset++] & 0xFF) << 8) | (in[inOffset++] & 0xFF)),
            x2 = (short)(((in[inOffset++] & 0xFF) << 8) | (in[inOffset++] & 0xFF)),
            x3 = (short)(((in[inOffset++] & 0xFF) << 8) | (in[inOffset++] & 0xFF)),
            x4 = (short)(((in[inOffset++] & 0xFF) << 8) | (in[inOffset  ] & 0xFF)),
            s2, s3;
        int i = 0,
            round = ROUNDS;

        while (round-- > 0) {
            x1 = mul(x1, ks[i++]);
            x2 += ks[i++];
            x3 += ks[i++];
            x4 = mul(x4, ks[i++]);

            s3 = x3;
            x3 = mul(x1 ^ x3, ks[i++]);
            s2 = x2;
            x2 = mul(x3 + (x2 ^ x4), ks[i++]);
            x3 += x2;

            x1 ^= x2;
            x4 ^= x3;
            x2 ^= s3;
            x3 ^= s2;
        }
        s2 = mul(x1, ks[i++]);
        out[outOffset++] = (byte)(s2 >>> 8);
        out[outOffset++] = (byte) s2;
        s2 = (short)(x3 + ks[i++]);
        out[outOffset++] = (byte)(s2 >>> 8);
        out[outOffset++] = (byte) s2;
        s2 = (short)(x2 + ks[i++]);
        out[outOffset++] = (byte)(s2 >>> 8);
        out[outOffset++] = (byte) s2;
        s2 = mul(x4, ks[i]);
        out[outOffset++] = (byte)(s2 >>> 8);
        out[outOffset  ] = (byte) s2;
    }

    /**
     * IDEA uses the same algorithm for both encryption and decryption. Only the
     * key schedule changes.
     */
    private void blockDecrypt (byte[] in, int inOffset, byte[] out, int outOffset) {
        blockEncrypt(in, inOffset, out, outOffset);
    }

    /**
     * Multiplication modulo (2**16)+1.
     */
    private static short mul (int a, int b) {
        a &= 0xFFFF;
        b &= 0xFFFF;
        int p;
        if (a != 0) {
            if (b != 0) {
                p = a * b;
                b = p & 0xFFFF;
                a = p >>> 16;
                return (short)(b - a + (b < a ? 1 : 0));
            } else
                return (short)(1 - a);
        } else
            return (short)(1 - b);
    }

    /**
     * Compute inverse of x, modulo (2**16)+1, using Euclidean gcd algorithm.
     *
     * The Euclidean part of this algorithm could live in a
     * general purpose math library, but then it would probably
     * end up too slow.
     */
    private static short inv (short xx) {
        int x = xx & 0xFFFF;       // only lower 16 bits
        if (x <= 1)
            return (short)x;        // 0 and 1 are self-inverse

        int t1 = 0x10001 / x;        // Since x >= 2, this fits into 16 bits
        int y = 0x10001 % x;
        if (y == 1)
            return (short)(1 - t1);

        int t0 = 1;
        int q;
        do {
            q = x / y;
            x = x % y;
            t0 += q * t1;
            if (x == 1)
                return (short)t0;
            q = y / x;
            y %= x;
            t1 += q * t0;
        } while (y != 1);
        return (short)(1 - t1);
    }


// Test methods
//................................................................................
//
// Don't expand this code please without thinking about it,
// much better to write a separate program.

    /**
     * Entry point for very basic <code>self_test</code>.
     */
    public static void
    main(String argv[]) 
    {
        try { self_test(); }
        catch(Throwable t) { t.printStackTrace(); }
    }

    //
    // This is (apparently) the official certification data.
    // Use ints as Java grumbles about negative hex values.
    //
    static final private byte[][][] tests =
    {
      { // cert 1
        { 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8},  // key
        { 0, 0, 0, 1, 0, 2, 0, 3},                          // plain
        { 17, -5, -19, 43, 1, -104, 109, -27},              // cipher
      },
      { // cert 8
        { 58, -104, 78, 32, 0, 25, 93, -77, 46, -27, 1, -56, -60, 124, -22, 96},
        { 1, 2, 3, 4, 5, 6, 7, 8},
        { -105, -68, -40, 32, 7, -128, -38, -122},
      },
      { // cert 9
        { 0, 100, 0, -56, 1, 44, 1, -112, 1, -12, 2, 88, 2, -68, 3, 32},  // key
        { 5, 50, 10, 100, 20, -56, 25, -6},                               // plain
        { 101, -66, -121, -25, -94, 83, -118, -19},                       // cipher
      },
    };

    /**
     * Do some basic tests.
     * Three of the certification data are included only, no output,
     * success or exception.
     * If you want more, write a test program!
     * @see cryptix.examples.IDEA
     */
    public static void self_test()
    throws Throwable {

        Cipher cryptor = Cipher.getInstance("IDEA", "Cryptix");
        RawSecretKey userKey;
        byte[] tmp;

        for (int i = 0; i < tests.length; i++) {
            userKey = new RawSecretKey("IDEA", tests[i][0]);

            cryptor.initEncrypt(userKey);
            tmp = cryptor.crypt(tests[i][1]);
            if (!ArrayUtil.areEqual(tests[i][2], tmp))
                throw new CryptixException("encrypt #"+ i +" failed");

            cryptor.initDecrypt(userKey);
            tmp = cryptor.crypt(tests[i][2]);
            if (!ArrayUtil.areEqual(tests[i][1], tmp))
                throw new CryptixException("decrypt #"+ i +" failed");
        }
if (DEBUG && debuglevel > 0) debug("Self-test OK");
    }
}
