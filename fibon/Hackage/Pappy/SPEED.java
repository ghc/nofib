// $Id: SPEED.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: SPEED.java,v $
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
// Revision 1.6  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.5  1997/12/09 04:43:45  hopwood
// + Various.
//
// Revision 1.4  1997/11/29 17:49:03  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/11/29  hopwood
// + Changed parameter objects to be Integers, rather than decimal Strings.
//
// Revision 1.3  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.2  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.1  1997/09/18  David Hopwood
// + Fixed missing import of Cipher.
// + Replaced the constructors that take block size and number of rounds
//   with properties.
// + Changed native_crypt signature to pass the block size and number of
//   rounds.
//
// Revision 0.3.2.0  1997/09/06  David Hopwood
// + Removed all deprecated methods and fields. Cryptix 2.2 compatibility
//   is now handled by the separate cryptix.security.SPEED class.
// + Ensured that this class is final, and added a comment that this is for
//   security reasons.
//   If it were not final, some VMs have a bug that would allow a subclass
//   that implements Cloneable to call Object's or Cipher's clone() method
//   using invokenonvirtual, which would duplicate the pointer to the native
//   state. Then calling finalize() on the original and using the clone (for
//   example) would result in freed memory being written to :-(.
// + Native linking and debugging brought up to date with Blowfish 3.2.0
//   (hence the jump in version number).
// + Documentation changes.
//
// Revision 0.1.0.1  1997/04/20  Systemics Ltd
// + Minimal self_test().
//
// Revision 0.1.0.0  1997/04/01  Systemics Ltd
// + Original version, as released.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix development team.  All rights reserved.
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
import java.security.InvalidParameterException;
import xjava.security.InvalidParameterTypeException;
import xjava.security.NoSuchParameterException;
import java.security.Security;
import xjava.security.SymmetricCipher;

/**
 * SPEED is a block cipher with variable key size, data block size and number
 * of rounds (in the style of RC5).
 * <P>
 * These parameters are set as follows:
 * <ul>
 *   <li> The key size is taken from the encoded form of the key passed to
 *        <code>initEncrypt</code> or <code>initDecrypt</code>. It can be
 *        any even number of bytes from 6 to 32 (6, 8, 10, ... 32).
 *        <p>
 *   <li> The length of the data block defaults to the value of the parameter
 *        "Alg.blockSize.SPEED", or 8 bytes (c.f. IDEA and DES) if that parameter
 *        is not found.
 *        It can be set by calling setBlockSize(), and can be 8, 16, or 32 bytes.
 *        <p>
 *   <li> The number of rounds defaults to the value of the parameter
 *        "Alg.rounds.SPEED", or 64 (which gives 'adequate' security according
 *        to the paper below) if that parameter is not found.
 *        It can be set by calling setRounds(), and can be any number from 32
 *        upwards, divisible by 4 (32, 36, ...).
 * </ul>
 * <p>
 * These are recommended settings for 'adequate' security:
 * <pre>
 *    +--------------------------------------------------+
 *    |   block size   |   key length   |     rounds     |
 *    |==================================================|
 *    |       8        |      >= 8      |     >= 64      |
 *    |--------------------------------------------------|
 *    |      16        |      >= 8      |     >= 48      |
 *    |--------------------------------------------------|
 *    |      32        |      >= 8      |     >= 48      |
 *    +--------------------------------------------------+
 * </pre>
 * <p>
 * SPEED was designed by <a href="mailto:yzheng@fcit.monash.edu.au">Yuliang Zheng</a>,
 * and is in the public domain.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> <a href="http://pscit-www.fcit.monash.edu.au/~yuliang/">Y. Zheng</a>
 *        "The SPEED Cipher,"
 *        <cite>Proceedings of Financial Cryptography 97</cite>,
 *        Springer-Verlag (forthcoming).
 *        FC97 held at Anguilla, BWI, 24-28 February 1997.
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 * @author  David Hopwood
 * @since   Cryptix 2.2.2
 */
public final class SPEED // must be final for security reasons
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("SPEED") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("SPEED: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("SPEED", 2, 3);

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

    // if schedule is done in Java:
    //   (long cookie, int key_bits, int data_bits,
    //    int s0, int s1, int s2, int f_wd_len, int h_wd_len,
    //    int f_wd_mask, int h_wd_mask, int v_shift, int kb_bits,
    //    int[] round_key, int key_len_dbyte);

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
     *   <li> <code>(long)inOffset + block_size <= in.length</code>
     *   <li> <code>outOffset >= 0</code>
     *   <li> <code>(long)outOffset + block_size <= out.length</code>
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
     * @param  rounds       the number of rounds.
     * @param  blocksize    the block size in bytes.
     * @return the number of bytes crypted (always blocksize) or 0 if an error
     *                      occurred.
     */
    private native int native_crypt(long cookie, byte[] in, int inOffset,
                                    byte[] out, int outOffset, boolean encrypt,
                                    int rounds, int blocksize);

    /**
     * Finalizes the native state for this object.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_finalize();


// SPEED constants and variables
//...........................................................................

    private static final int
        MIN_NOF_ROUNDS = 32,            // minimum number of rounds for SPEED
        MIN_USER_KEY_LENGTH =  48 / 8,  // given in bytes from a value in bits
        MAX_USER_KEY_LENGTH = 256 / 8;  // given in bytes from a value in bits

    //
    // Defaults are set to the same as IDEA, e.g. 16 key and 8 data bytes.
    // However, the real key length is taken from the key provided.
    //
    private int
        key_length = 16,    // in bytes
        rounds = 64,        // 'adequate' security, c.f. paper
        block_size = 8;     // in bytes

    //
    // Names ending in _bits refer to number of bits; those ending in _len
    // refer to number of bytes. Other names are taken from the C reference code.
    //
    // Note that as ints, some of these might give trouble with 256 bit
    // data blocks, as the full 32 bits are used.
    //
    private int key_bits;       // is key_length * 8
    private int data_bits;      // is block_size * 8
    private int s0, s1, s2;     // taken from sqrt(15) and key_length
    private int f_wd_len;
    private int h_wd_len;
    private int f_wd_mask;      // long ?? if data_bits == 256
    private int h_wd_mask;
    private int v_shift;
    private int kb_bits;
    private int[] round_key;    // the internal key buffer
    private int key_len_dbyte;


// Constructor, finalizer, and clone()
//...........................................................................

    /**
     * Constructs a SPEED cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public SPEED() {
        super(false, false, "Cryptix");
        link();

        try {
            String ps = Security.getAlgorithmProperty("SPEED", "rounds");
            if (ps != null) setRounds(Integer.parseInt(ps));
        } catch (Exception e) {
if (DEBUG && debuglevel > 0) debug("Could not set number of rounds");
        }
        try {
            String ps = Security.getAlgorithmProperty("SPEED", "blockSize");
            if (ps != null) setBlockSize(Integer.parseInt(ps));
        } catch (Exception e) {
if (DEBUG && debuglevel > 0) debug("Could not set block size");
        }
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
//...........................................................................

    /**
     * <b>SPI</b>: Return the data block length of this cipher.
     * Default (8 bytes) is returned before instantiation,
     * actual length used by object returned after instantiation.
     *
     * @return the block length in bytes.
     */
    protected int engineBlockSize() { return block_size; }

    /**
     * <b>SPI</b>: Initializes this cipher for encryption, using the
     * specified key.
     *
     * @param  key  the key to use for encryption.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not ...
     *              </ul>
     */
    public void engineInitEncrypt (Key key)
    throws InvalidKeyException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for decryption.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not ...
     *              </ul>
     */
    public void engineInitDecrypt (Key key)
    throws InvalidKeyException, CryptixException {
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
        int BLOCK_SIZE = block_size; // use local variable to ensure that
                                     // BLOCK_SIZE does not change during this call.

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
                                          doEncrypt, rounds, BLOCK_SIZE))
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

    /**
     * <b>SPI</b>: Sets the specified algorithm parameter to the specified
     * value.
     * <p>
     * SPEED has two parameters:
     * <ul>
     *   <li> "rounds", which specifies the number of rounds for this instance
     *        as a decimal String.
     *   <li> "blockSize", which specifies the block size for this instance,
     *        also as a decimal String.
     * </ul>
     *
     * @param  param    the string name of the parameter. 
     * @param  value    the parameter value.
     * @exception InvalidParameterException if param is an invalid
     *                  parameter for this cipher implementation, the
     *                  parameter is already set and cannot be set again, a
     *                  security exception occurs, and so on.
     * @exception InvalidParameterTypeException if value is of the wrong
     *                  type.
     */
    protected void engineSetParameter(String param, Object value)
    throws NoSuchParameterException, InvalidParameterException,
           InvalidParameterTypeException {
        if (param.equalsIgnoreCase("rounds")) {
            if (value instanceof Integer)
                setRounds(((Integer) value).intValue());
            else
                throw new InvalidParameterTypeException("rounds.SPEED");
        } else if (param.equalsIgnoreCase("blockSize")) {
            if (value instanceof Integer)
                setBlockSize(((Integer) value).intValue());
            else
                throw new InvalidParameterTypeException("blockSize.SPEED");
        } else
            throw new NoSuchParameterException(param + ".SPEED");
    }

    /**
     * <b>SPI</b>: Gets the value of the specified algorithm parameter.
     * <p>
     * SPEED has two parameters:
     * <ul>
     *   <li> "rounds", which specifies the number of rounds for this instance
     *        as a decimal String.
     *   <li> "blockSize", which specifies the block size for this instance,
     *        also as a decimal String.
     * </ul>
     *
     * @param  param    the string name of the parameter. 
     * @return the object that represents the parameter value, or null if there
     *                  is none.
     */
    protected Object engineGetParameter(String param)
    throws NoSuchParameterException, InvalidParameterException {
        if (param.equalsIgnoreCase("rounds"))
            return new Integer(rounds);
        else if (param.equalsIgnoreCase("blockSize"))
            return new Integer(block_size);
        else
            throw new NoSuchParameterException(param + ".SPEED");
    }


// Own methods
//............................................................................

    /**
     * Sets the number of rounds for this cipher. Allowed only when this
     * cipher is in the UNINITIALIZED state; otherwise an exception is
     * thrown.
     * <p>
     * If the specified number is invalid, an IllegalArgumentException is
     * thrown.
     *
     * @param  rounds   the desired number of rounds: >= 32, multiple of 4
     * @exception IllegalStateException if this cipher is not uninitialised.
     * @exception InvalidParameterException if the given number of rounds is
     *                  not supported.
     */
    public void setRounds(int rounds) {
        if (getState() != UNINITIALIZED)
            throw new IllegalStateException(getAlgorithm() +
                ": Cipher not in UNINITIALIZED state");

        if (rounds < MIN_NOF_ROUNDS || rounds % 4 != 0)
            throw new IllegalArgumentException(getAlgorithm() +
                ": Invalid number of rounds");

        this.rounds = rounds;
    }

    /**
     * Returns the currently set number of rounds for this instance.
     *
     * @return the number of rounds.
     */
    public int getRounds() { return rounds; }

    /**
     * Sets the block size in bytes for this cipher. Allowed only when this
     * cipher is in the UNINITIALIZED state; otherwise an exception is
     * thrown.
     * <p>
     * If the specified number is invalid, an IllegalArgumentException is
     * thrown.
     *
     * @param  blocksize    the desired block size in bytes: 8, 16 or 32
     * @exception IllegalStateException if this cipher is not uninitialised.
     * @exception IllegalArgumentException if the given number of rounds is
     *                      not supported.
     */
    public void setBlockSize(int blocksize) {
        if (getState() != UNINITIALIZED)
            throw new IllegalStateException(getAlgorithm() +
                ": Cipher not in UNINITIALIZED state");

        if (blocksize != 8 && blocksize != 16 && blocksize != 32)
            throw new IllegalArgumentException(getAlgorithm() +
                ": Invalid block size");

        block_size = blocksize;
    }

    /**
     * Expands a user-key to a working key schedule.
     *
     * @param  key  the user-key object to use.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not ...
     *              </ul>
     */
    private void makeKey (Key key)
    throws InvalidKeyException {

        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        int len = userkey.length;
        if (len < MIN_USER_KEY_LENGTH || len > MAX_USER_KEY_LENGTH)
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

        set_constants(userkey.length);

        kb = new int[kb_bits];          // scheduling temp buffer
        round_key = new int[rounds];    // real internal key

        //
        // Copy the key into the double-byte temporary buffer
        //
        for (int i = 0; i < key_len_dbyte; i++)
            kb[i] = userkey[2*i] | userkey[2*i + 1] << 8;

        //
        // Fill out the buffer from earlier parts of the key
        //
        for (int i = key_len_dbyte; i < kb_bits; i++) {
            int t = (s2 & s1) ^ (s1 & s0) ^ (s0 & s2);
            t = (t << 5) | (t >>> 11);
            t += s2 + kb[i % key_len_dbyte];
            t &= 0xFFFF;
            s2 = s1;
            s1 = s0;
            s0 = kb[i] = t;
        }

if (DEBUG && debuglevel >= 5) debug("kb_bits=" + kb_bits + ", kb.length=" + kb.length + ", round_key.length=" + round_key.length);

        //
        // Transfer the double-byte temporary key into the real key
        //
        switch (data_bits)
        {
        case 256:
            for (int i = 0; i < kb_bits / 2; i++)
                round_key[i] = kb[2*i] | (kb[2*i + 1] << 16);
            break;

        case 128:
            for (int i = 0; i < kb_bits; i++)
                round_key[i] = kb[i];
            break;

        case 64:
            for (int i = 0; i < kb_bits; i++)
            {
                round_key[2*i]   = kb[i] & 0xFF;
                round_key[2*i+1] = (kb[i] >>> 8) & 0xFF;
            }
            break;

        default:
            throw new CryptixException("SPEED: " + data_bits + " illegal in key_schedule?");
        }
    }

    /**
     * Work out all the various constant choices.
     */
    private void set_constants(int key_length)
    {
        this.key_length = key_length;
        key_bits = key_length * 8;
        key_len_dbyte = key_length / 2;
        set_sqrt_15(key_bits);

        data_bits = block_size * 8;
        f_wd_len = data_bits / 8;    // ???  SPEED word in bits
        h_wd_len = f_wd_len / 2;    // half a SPEED word in bits

        switch (data_bits)
        {
        case 256:
            f_wd_mask = 0xFFFFFFFF;
            h_wd_mask = 0xFFFF;
            v_shift = 11;
            kb_bits = 2 * rounds;
            break ;

        case 128:
            f_wd_mask = 0xFFFF;
            h_wd_mask = 0xFF;
            v_shift = 4;
            kb_bits = rounds;
            break ;

        case 64:
            f_wd_mask = 0xFF;
            h_wd_mask = 0xF;
            v_shift = 1;
            kb_bits = rounds / 2;
            break ;

        default:
            throw new CryptixException("SPEED: " + data_bits + " is bad data size (not 64/128/256)");
        }
    }

    /**
     * Three constants are taken from the square root of 15, with 3 different
     * ones for each possible key length.
     * Also checks validity of key length here.
     */
    private void set_sqrt_15(int size)
    {
        /* fractional part of sqrt(15), used in key scheduling */
        switch (size)
        {
          case  48: s0 = 0xDF7B; s1 = 0xD629; s2 = 0xE9DB; return;
          case  64: s0 = 0x362F; s1 = 0x5D00; s2 = 0xF20F; return;
          case  80: s0 = 0xC3D1; s1 = 0x1FD2; s2 = 0x589B; return;
          case  96: s0 = 0x4312; s1 = 0x91EB; s2 = 0x718E; return;
          case 112: s0 = 0xBF2A; s1 = 0x1E7D; s2 = 0xB257; return;
          case 128: s0 = 0x77A6; s1 = 0x1654; s2 = 0x6B2A; return;
          case 144: s0 = 0x0D9B; s1 = 0xA9D3; s2 = 0x668F; return;
          case 160: s0 = 0x19BE; s1 = 0xF855; s2 = 0x6D98; return;
          case 176: s0 = 0x022D; s1 = 0xE4E2; s2 = 0xD017; return;
          case 192: s0 = 0xEA2F; s1 = 0x7572; s2 = 0xC3B5; return;
          case 208: s0 = 0x1086; s1 = 0x480C; s2 = 0x3AA6; return;
          case 224: s0 = 0x9CA0; s1 = 0x98F7; s2 = 0xD0E4; return;
          case 240: s0 = 0x253C; s1 = 0xC901; s2 = 0x55F3; return;
          case 256: s0 = 0x9BF4; s1 = 0xF659; s2 = 0xD76C; return;

          default:
            throw new CryptixException("SPEED: " + size + " is bad key length (not 48 .. 256 % 16)");
        }
    }

    private int[] kb;    // scheduling temp buffer, for dump of data

    void dump()
    {
        if (0 == data_bits)
        {
            err.println("no data set yet");
            return ;
        }

        err.println("KEY SCHEDULE");
        err.println(" data_bits " + data_bits);
        err.println(" kb_bits " + kb_bits);
        err.println(" kb.length " + kb.length);
        err.println(" f_wd_mask " + Hex.intToString(f_wd_mask));
        err.println(" h_wd_mask " + Hex.intToString(h_wd_mask));
        err.println(" v_shift " + v_shift);

        err.println(" double byte buffer");
        for(int i = 0; i < key_len_dbyte; i++)
        {
            err.print(" " + Hex.intToString(kb[i]));
        }
        err.println();

        switch (data_bits)
        {
        case 256:
            for (int i = 0; i < kb_bits / 2; i++)
                err.print(" " + Hex.intToString(round_key[i]));
            break;

        case 128:
            for (int i = 0; i < kb_bits; i++)
                err.print(" " + Hex.shortToString(round_key[i]));
            break;

        case 64:
            for (int i = 0; i < kb_bits * 2; i++)
            {
                err.print(" " + Hex.byteToString(round_key[i]));
            }
            break;

        default:
            throw new CryptixException("SPEED: data_bits=" + data_bits + " illegal in key_schedule?");
        }
        err.println();
    }

    private void to_internal(byte in[], int offset, int buf[])
    {
        //
        // Translate bytes into SPEED internal words.
        //
        switch (data_bits)
        {
        case 256:
            for (int i = 0; i < 8; i++)
            {
                buf[i] =
                      ((in[offset + 4*i  ]      ) & 0xFF)
                    | ((in[offset + 4*i+1] <<  8) & 0xFF00)
                    | ((in[offset + 4*i+2] << 16) & 0xFF0000)
                    | ((in[offset + 4*i+3] << 24) & 0xFF000000);
            }
            break;

        case 128:
            // err.println("TO 128");
            for (int i = 0; i < 8; i++)
            {
                buf[i] =
                      ((in[offset + 2*i  ]      ) & 0xFF)
                    | ((in[offset + 2*i+1] <<  8) & 0xFF00);
                //err.print(" " + Hex.intToString(buf[i]));
            }
            // err.println();
            // err.println("-----");
            break;

        case 64:
            for (int i = 0; i < 8; i++)
            {
                buf[i] = (in[offset + i] & 0xFF);
            }
            break;

        default:
            throw new CryptixException("SPEED: " + data_bits + " illegal in key_schedule?");
        }
    }

    /**
     * Encrypts a block. The in and out buffers can be the same.
     *
     * @param in The data to be encrypted.
     * @param in_offset   The start of data within the in buffer.
     * @param out The encrypted data.
     * @param out_offset  The start of data within the out buffer.
     */
    protected void 
    blockEncrypt(byte in[], int in_offset, byte out[], int out_offset)
    {
        int[] big_in  = new int[8];     // long ??
        int[] big_out = new int[8];     // long ??

        to_internal(in, in_offset, big_in);
        encrypt(big_in, big_out);
        from_internal(big_out, out, out_offset);
    }

    private void from_internal(int buf[], byte out[], int offset)
    {
        //
        // Translate internal words back into bytes.
        //
        switch (data_bits)
        {
        case 256:
            for (int i = 0; i < 8; i++)
            {
                out[4*i   + offset] = (byte)( buf[i]         & 0xFF);
                out[4*i+1 + offset] = (byte)((buf[i] >>>  8) & 0xFF);
                out[4*i+2 + offset] = (byte)((buf[i] >>> 16) & 0xFF);
                out[4*i+3 + offset] = (byte)((buf[i] >>> 24) & 0xFF);
            }
            break;

        case 128:
            for (int i = 0; i < 8; i++)
            {
                out[2*i   + offset] = (byte)( buf[i]         & 0xFF);
                out[2*i+1 + offset] = (byte)((buf[i] >>>  8) & 0xFF);
            }
            break;

        case 64:
            for (int i = 0; i < 8; i++)
            {
                out[i + offset] = (byte)(buf[i] & 0xFF);
            }
            break;

        default:
            throw new CryptixException("SPEED: data_bits=" + data_bits + " illegal in key_schedule?");
        }
    }

    /**
     * Decrypts a block. The in and out buffers can be the same.
     *
     * @param in The data to be decrypted.
     * @param in_offset   The start of data within the in buffer.
     * @param out The decrypted data.
     * @param out_offset  The start of data within the out buffer.
     */
    protected void
    blockDecrypt(byte in[], int in_offset, byte out[], int out_offset)
    {
        //
        // Translate bytes into SPEED internal words.
        //
        int[] big_in  = new int[8];     // long ??
        int[] big_out = new int[8];     // long ??

        to_internal(in, in_offset, big_in);
        decrypt(big_in, big_out);
        from_internal(big_out, out, out_offset);
    }

    /**
     * Encrypts a block.
     */
    private void encrypt(int in[], int out[])
    {
        int t0 = in[0], 
            t1 = in[1],
            t2 = in[2],
            t3 = in[3],
            t4 = in[4],
            t5 = in[5],
            t6 = in[6],
            t7 = in[7];

        int k = 0;   /* points to the first round key */
        int quarter_rounds = rounds / 4;

        //
        // In the following 4 passes, only the first assignment
        // changes, which is the nonlinear function.
        //
        /* Pass 1 uses FF1 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int temp = ((t6) & (t3)) ^ ((t5) & (t1)) ^
                       ((t4) & (t2)) ^ ((t1) & (t0)) ^ (t0);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;
            //
            //    rotate_data_right(t7, h_wd_len - 1)
            //    + rotate_data_right(temp, vv)
            //    + round_key[k++];
            //
            t7 &= f_wd_mask;        // not re-used
            int rot1 = (t7 >>> (h_wd_len - 1))
                       | (t7 << (f_wd_len - (h_wd_len - 1)));

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)
                       | (temp << (f_wd_len - vv));

            temp = rot1 + rot2 + round_key[k++];

            t7 = t6; t6 = t5; t5 = t4; t4 = t3;
            t3 = t2; t2 = t1; t1 = t0; t0 = temp & f_wd_mask;
        }

if (DEBUG_SLOW && debuglevel >= 5) {
    debug("PASS 1: " + Hex.intToString(t7) +
                 " " + Hex.intToString(t6) +
                 " " + Hex.intToString(t5) +
                 " " + Hex.intToString(t4) +
                 " " + Hex.intToString(t3) +
                 " " + Hex.intToString(t2) +
                 " " + Hex.intToString(t1) +
                 " " + Hex.intToString(t0) +
                 " ");
}

        /* Pass 2 uses FF2 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int temp = ((t6) & (t4) & (t0)) ^ ((t4) & (t3) & (t0))
                ^ ((t5) & (t2)) ^ ((t4) & (t3)) ^ ((t4) & (t1))
                ^ ((t3) & (t0)) ^ (t1);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;
            //
            //    rotate_data_right(t7, h_wd_len - 1)
            //    + rotate_data_right(temp, vv)
            //    + round_key[k++];
            //
            t7 &= f_wd_mask;        // not re-used
            int rot1 = (t7 >>> (h_wd_len - 1))
                       | (t7 << (f_wd_len - (h_wd_len - 1)));

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)
                       | (temp << (f_wd_len - vv));

            temp = rot1 + rot2 + round_key[k++];

            t7 = t6; t6 = t5; t5 = t4; t4 = t3;
            t3 = t2; t2 = t1; t1 = t0; t0 = temp & f_wd_mask;
        }

if (DEBUG_SLOW && debuglevel >= 5) {
    debug("PASS 2: " + Hex.intToString(t7) +
                 " " + Hex.intToString(t6) +
                 " " + Hex.intToString(t5) +
                 " " + Hex.intToString(t4) +
                 " " + Hex.intToString(t3) +
                 " " + Hex.intToString(t2) +
                 " " + Hex.intToString(t1) +
                 " " + Hex.intToString(t0) +
                 " ");
}

        /* Pass 3 uses FF3 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int temp = ((t5) & (t4) & (t0)) ^ ((t6) & (t4))
                ^ ((t5) & (t2)) ^ ((t3) & (t0))
                ^ ((t1) & (t0)) ^ (t3);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;
            //
            //    rotate_data_right(t7, h_wd_len - 1)
            //    + rotate_data_right(temp, vv)
            //    + round_key[k++];
            //
            t7 &= f_wd_mask;        // not re-used
            int rot1 = (t7 >>> (h_wd_len - 1))
                       | (t7 << (f_wd_len - (h_wd_len - 1)));

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)
                       | (temp << (f_wd_len - vv));

            temp = rot1 + rot2 + round_key[k++];

            t7 = t6; t6 = t5; t5 = t4; t4 = t3;
            t3 = t2; t2 = t1; t1 = t0; t0 = temp & f_wd_mask;
        }

if (DEBUG_SLOW && debuglevel >= 5) {
    debug("PASS 3: " + Hex.intToString(t7) +
                 " " + Hex.intToString(t6) +
                 " " + Hex.intToString(t5) +
                 " " + Hex.intToString(t4) +
                 " " + Hex.intToString(t3) +
                 " " + Hex.intToString(t2) +
                 " " + Hex.intToString(t1) +
                 " " + Hex.intToString(t0) +
                 " ");
}

        /* Pass 4 uses FF4  */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int temp = ((t6) & (t4) & (t2) & (t0))
                ^ ((t6) & (t5)) ^ ((t4) & (t3))
                ^ ((t3) & (t2)) ^ ((t1) & (t0)) ^ (t2);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;
            //
            //    rotate_data_right(t7, h_wd_len - 1)
            //    + rotate_data_right(temp, vv)
            //    + round_key[k++];
            //
            t7 &= f_wd_mask;        // not re-used
            int rot1 = (t7 >>> (h_wd_len - 1))
                       | (t7 << (f_wd_len - (h_wd_len - 1)));

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)
                       | (temp << (f_wd_len - vv));

            temp = rot1 + rot2 + round_key[k++];

            t7 = t6; t6 = t5; t5 = t4; t4 = t3;
            t3 = t2; t2 = t1; t1 = t0; t0 = temp & f_wd_mask;
        }

if (DEBUG_SLOW && debuglevel >= 5) {
    debug("PASS 4: " + Hex.intToString(t7) +
                 " " + Hex.intToString(t6) +
                 " " + Hex.intToString(t5) +
                 " " + Hex.intToString(t4) +
                 " " + Hex.intToString(t3) +
                 " " + Hex.intToString(t2) +
                 " " + Hex.intToString(t1) +
                 " " + Hex.intToString(t0) +
                 " ");
}

        out[0] = t0; out[1] = t1; out[2] = t2; out[3] = t3;
        out[4] = t4; out[5] = t5; out[6] = t6; out[7] = t7;
    }

    /**
     * Decrypts a block.
     */
    private void decrypt(int in[], int out[])
    {
        int t0 = in[0] & 0xFFFFFFFF, 
            t1 = in[1] & 0xFFFFFFFF,
            t2 = in[2] & 0xFFFFFFFF,
            t3 = in[3] & 0xFFFFFFFF,
            t4 = in[4] & 0xFFFFFFFF,
            t5 = in[5] & 0xFFFFFFFF,
            t6 = in[6] & 0xFFFFFFFF,
            t7 = in[7] & 0xFFFFFFFF;

        int k = rounds - 1;   /* points to the first round key */
        int quarter_rounds = rounds / 4;
//        err.println("START " + k + " " + quarter_rounds);
//        err.println(
//            " " + Integer.toString((0xFFFFFFFF & t7), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t6), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t5), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t4), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t3), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t2), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t1), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t0), 16) +
//            " ");


        /* Inverse of Pass 4 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int new7 = t0; t0 = t1; t1 = t2; t2 = t3;
            t3       = t4; t4 = t5; t5 = t6; t6 = t7;

//        err.print("PASS 4." + i);
            int temp = ((t6) & (t4) & (t2) & (t0))
                ^ ((t6) & (t5)) ^ ((t4) & (t3))
                ^ ((t3) & (t2)) ^ ((t1) & (t0)) ^ (t2);

//        err.print(" "+Integer.toString((0xFFFFFFFF & temp), 16));
            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;

//        err.print(" "+Integer.toString((0xFFFFFFFF & vv), 16));
            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)    // rotate right
                       | (temp << (f_wd_len - vv));

            new7 -= rot2 + round_key[k--];    // check --

            new7 &= f_wd_mask;
//        err.print(" "+Integer.toString((0xFFFFFFFF & new7), 16));
            t7 = (new7 << (h_wd_len - 1))    // rotate left
                       | (new7 >>> (f_wd_len - (h_wd_len - 1)));
//        err.println();
//        err.println(
//            " " + Integer.toString((0xFFFFFFFF & t7), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t6), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t5), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t4), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t3), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t2), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t1), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t0), 16) +
//            " ");
        }
//        err.println("PASS 4");
//        err.println(
//            " " + Integer.toString((0xFFFFFFFF & t7), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t6), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t5), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t4), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t3), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t2), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t1), 16) +
//            " " + Integer.toString((0xFFFFFFFF & t0), 16) +
//            " ");

        /* Inverse of Pass 3 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int new7 = t0; t0 = t1; t1 = t2; t2 = t3;
            t3       = t4; t4 = t5; t5 = t6; t6 = t7;

            int temp = ((t5) & (t4) & (t0)) ^ ((t6) & (t4))
                ^ ((t5) & (t2)) ^ ((t3) & (t0))
                ^ ((t1) & (t0)) ^ (t3);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)    // rotate right
                       | (temp << (f_wd_len - vv));

            new7 -= rot2 + round_key[k--];    // check --

            new7 &= f_wd_mask;
            t7 = (new7 << (h_wd_len - 1))    // rotate left
                       | (new7 >>> (f_wd_len - (h_wd_len - 1)));
        }

        /* Inverse of Pass 2 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int new7 = t0; t0 = t1; t1 = t2; t2 = t3;
            t3       = t4; t4 = t5; t5 = t6; t6 = t7;

            int temp = ((t6) & (t4) & (t0)) ^ ((t4) & (t3) & (t0))
                ^ ((t5) & (t2)) ^ ((t4) & (t3)) ^ ((t4) & (t1))
                ^ ((t3) & (t0)) ^ (t1);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)    // rotate right
                       | (temp << (f_wd_len - vv));

            new7 -= rot2 + round_key[k--];    // check --

            new7 &= f_wd_mask;
            t7 = (new7 << (h_wd_len - 1))    // rotate left
                       | (new7 >>> (f_wd_len - (h_wd_len - 1)));
        }

        /* Inverse of Pass 1 */
        for (int i = 0; i < quarter_rounds; i++)
        {
            int new7 = t0; t0 = t1; t1 = t2; t2 = t3;
            t3       = t4; t4 = t5; t5 = t6; t6 = t7;

            int temp = ((t6) & (t3)) ^ ((t5) & (t1)) ^
                       ((t4) & (t2)) ^ ((t1) & (t0)) ^ (t0);

            int vv   = (((temp >>> h_wd_len) + temp) & h_wd_mask)
                   >>> v_shift;

            temp &= f_wd_mask;        // not re-used
            int rot2 = (temp >>> vv)    // rotate right
                       | (temp << (f_wd_len - vv));

            new7 -= rot2 + round_key[k--];    // check --

            new7 &= f_wd_mask;
            t7 = (new7 << (h_wd_len - 1))    // rotate left
                       | (new7 >>> (f_wd_len - (h_wd_len - 1)));
        }

        out[0] = t0; out[1] = t1; out[2] = t2; out[3] = t3;
        out[4] = t4; out[5] = t5; out[6] = t6; out[7] = t7;

    }

///////////////////////////////// T E S T /////////////////////////

    /**
     * Entry point for self_test.
     */
    public static final void main(String argv[]) 
    {
        try {
            self_test(new PrintWriter(System.err), argv);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Runs algorithm through test data, including certification data
     * provided in paper.
     */
    public static void
    self_test(PrintWriter out, String argv[])
    throws Exception
    {
        out.println("Note: hex strings are printed in conventional order, not the order");
        out.println("      in the SPEED paper.");
        out.println();

        //
        // Note that the paper uses certification data that is indexed
        // from RIGHT to LEFT, i.e., 7, 6, 5, 4, 3, 2, 1, 0.
        //
        test(out,  64,            // certification 1
            "0000000000000000",
            "0000000000000000",
            "2E008019BC26856D");
        test(out, 128,
            "00000000000000000000000000000000",
            "00000000000000000000000000000000",
            "A44FBF29EDF6CBF8D7A2DFD57163B909");
        test(out, 128,            // certification 2
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            "6C13E4B9C3171571AB54D816915BC4E8");
        test(out,  48,
            "504F4E4D4C4B4A494847464544434241",
            "1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100",
            "90C5981EF6A3D21BC178CACDAD6BF39B2E51CDB70A6EE875A73BF5ED883E3692");
        test(out, 256,
            "0000000000000000000000000000000000000000000000000000000000000000",
            "0000000000000000000000000000000000000000000000000000000000000000",
            "6CD44D2B49BC6AA7E95FD1C4AF713A2C0AFA1701308D56298CDF27A02EB09BF5");
        test(out, 256,
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            "C8F3E864263FAF24222E38227BEBC022CF4A9A0ECE89FB81CA1B9BA3BA93D0C5");
        test(out, 256,            // certification 3
            "605F5E5D5C5B5A595857565554535251504F4E4D4C4B4A494847464544434241",
            "1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100",
            "3DE16CFA9A626847434E1574693FEC1B3FAA558A296B61D708B131CCBA311068");
    }

    private static void
    test(PrintWriter out, int rounds, String keyStr, String plainStr, String cipherStr)
    throws Exception
    {
        // Note that the paper uses certification data that is indexed
        // from RIGHT to LEFT, i.e., 7, 6, 5, 4, 3, 2, 1, 0.
        byte keyBytes[] = Hex.fromReversedString(keyStr);
        byte plain[] = Hex.fromReversedString(plainStr);
        byte cipher[] = Hex.fromReversedString(cipherStr);

        SPEED speed = new SPEED();
        speed.setBlockSize(plain.length);
        speed.setRounds(rounds);
        Key key = new RawSecretKey("SPEED", keyBytes);

        speed.initEncrypt(key);
        byte encP[] = speed.crypt(plain);
        String a, b;
        out.println("    key:" + Hex.toString(keyBytes));
        out.println("  plain:" + Hex.toString(plain));
        out.println("    enc:" + (a = Hex.toString(encP)));
        b = Hex.toString(cipher);
        if (a.equals(b))
            out.print("encryption good; ");
        else
        {
            out.println("   calc:" + b);
            out.println(" ********* SPEED ENCRYPTION FAILED ********* ");
            speed.dump();
        }

        speed.initDecrypt(key);
        byte[] decC = speed.crypt(encP);
        a = Hex.toString(decC);
        b = Hex.toString(plain);
        if (a.equals(b))
            out.println("decryption good");
        else
        {
            out.println();
            out.println("    enc:" + Hex.toString(encP));
            out.println("    dec:" + a);
            out.println("   calc:" + b);
            out.println(" ********* SPEED DECRYPTION FAILED ********* ");
            speed.dump();
        }
    }
}
