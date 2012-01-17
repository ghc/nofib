// $Id: SAFER.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: SAFER.java,v $
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
// Revision 1.8  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.7  1998/01/28 00:17:43  hopwood
// + Committed changes below.
//
// Revision 1.6.1  1998/01/28  hopwood
// + Minor HTML comment fixes.
//
// Revision 1.6  1998/01/22 04:42:17  iang
// + Added URL to V1.2 test kit, which includes C and tests.
//
// Revision 1.5  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.4  1997/11/20 19:31:41  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.3.1  1997/11/15  David Hopwood
// + Renamed "Rounds" parameter (in constructor) to "rounds".
// + Fixed off-by-one error when checking argument to setRounds.
// + Throw InvalidParameterException when the argument to setRounds is
//   out of range.
// + Constructor now calls setVariant and setRounds, instead of duplicating
//   code.
// + Added getRounds and getVariant methods.
// + Simplified engineGetParameter (by calling getVariant when needed).
// + Fixed documentation for the default number of rounds, when there is no
//   entry in the properties file. The original comment said MAX_NOF_ROUNDS
//   (13); should be SK128_DEFAULT_NOF_ROUNDS (10).
//
// Revision 1.3  1997/11/10 07:31:32  raif
// + Added support for engineSet/GetParameter();
// + Changed the signature of setVariant(int) to setVariant(String);
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.1  1997/10/26  David Hopwood
// + Renamed "Variant" parameter to "variant".
//
// Revision 0.3.2.0  1997/09/06  David Hopwood
// + Replaced "mode" with "variant" everywhere, to prevent confusion with
//   encryption modes.
// + Native linking and debugging brought up to date with Blowfish 0.3.2.0
//   (hence the jump in version number).
//
// Revision 0.1.2.0  1997/07/14  R. Naffah
// + Tested OK with and without SAFER.DLL;
// + Added support for native library;
// + Incorporated D. Hopwood framework for native library linking
//   and debugging;
// + Minor syntax improvements.
//
// Revision 0.1.1.1  1997/06/30  R. Naffah
// + Minor changes for a slight performance improvement.
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
import java.security.InvalidParameterException;
import xjava.security.InvalidParameterTypeException;
import xjava.security.NoSuchParameterException;
import java.security.Security;
import xjava.security.SymmetricCipher;

/** 
 * A subclass of Cipher to implement the SAFER algorithm in Java.
 * <p>
 * SAFER (Secure And Fast Encryption Routine) is a block-cipher algorithm
 * developed by Prof. J.L. Massey at the Swiss Federal Institute of Technology.
 * SAFER is usable in four versions (referred to in this implementation as
 * VARIANTS): SAFER K-64, SAFER K-128, SAFER SK-64 and SAFER SK-128. The numerals
 * 64 and 128 stand for the length of the user-selected key, 'K' stands for the
 * original key schedule and 'SK' stands for the strengthened key schedule.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Massey, J.L.,
 *        "SAFER K-64: A Byte-Oriented Block Ciphering Algorithm", pp. 1-17 in
 *        Fast Software Encryption (Ed. R. Anderson),
 *        Proceedings of the Cambridge Security Workshop, Cambridge, U.K.,
 *        December 9-11, 1993,<br>
 *        <cite>Lecture Notes in Computer Science No. 809</cite>.
 *        Heidelberg and New York: Springer, 1994.
 *        <p>
 *   <li> Massey, J.L.,
 *        "SAFER K-64: One Year Later",
 *        preliminary manuscript of a paper presented at the K. U. Leuven
 *        Workshop on Cryptographic Algorithms, December 14-16, 1994.<br>
 *        To be published in the Proceedings of this workshop by Springer.
 *        <p>
 *   <li> Massey, J.L.,
 *        "Announcement of a Strengthened Key Schedule for the Cipher SAFER",
 *        Sept. 9, 1995, (see file 'SAFER_SK.TXT' included in the toolkit).
 *        <p>
 *   <li> Richard De Moliner &lt;demoliner@isi.ee.ethz.ch&gt;
 *        <a href="ftp://ftp.isi.ee.ethz.ch/pub/simpl/safer.V1.2.tar.Z">
 *        SAFER toolkit V1.2</a>
 *        includes C implementation, additional notes, test data, test program.
 * </ol>
 * <p>
 * Ported to Java from public domain 'C' code latest revised on September
 * 9, 1995 by:
 * <blockquote>
 *     Richard De Moliner (demoliner@isi.ee.ethz.ch)<br>
 *     Signal and Information Processing Laboratory<br>
 *     Swiss Federal Institute of Technology<br>
 *     CH-8092 Z&uuml;rich, Switzerland.
 * </blockquote>
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
public final class SAFER // must be final for security reasons
extends Cipher
implements SymmetricCipher
{
// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("SAFER") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("SAFER: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("SAFER", 2, 3);

    /**
     * Gets an object representing the native linking status of this class.
     */
    public static cryptix.util.core.LinkStatus getLinkStatus() { return linkStatus; }

    /**
     * The native reference to the current native session key(s)
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

    // The functions that get the library version.
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
     * <em>What is the restriction on rounds?</em>
     *
     * @param  cookie   a valid reference to the native key structure. This
     *                  value is set by the native library upon return from
     *                  native_init() (see link() method at the top).
     * @param  userKey1 a byte array representing the top 64 bits of the user
     *                  key.
     * @param  userKey2 a byte array representing the bottom 64 bits of the
     *                  user key.
     * @param  rounds   the number of rounds used.
     * @param  strong   whether the strengthened key schedule is to be used.
     * @return an error String, or null if there was no error
     */
    private native String native_ks(long cookie, byte[] userKey1, byte[] userKey2,
                                    int rounds, boolean strong);

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


// SAFER variables and constants
//...........................................................................

    public static final int
        SK128_VARIANT = 0,            // use as default
        SK64_VARIANT = 1,
        K128_VARIANT = 2,
        K64_VARIANT = 3;

    private static final int
        K64_DEFAULT_NOF_ROUNDS = 6,
        K128_DEFAULT_NOF_ROUNDS = 10,
        SK64_DEFAULT_NOF_ROUNDS = 8,
        SK128_DEFAULT_NOF_ROUNDS = 10,
        MAX_NOF_ROUNDS = 13,
        BLOCK_SIZE = 8,           // SAFER block size in bytes
        KEY_LENGTH = (1 + BLOCK_SIZE * (1 + 2 * MAX_NOF_ROUNDS)),

        TAB_LEN = 256;
                        
    private int[] sKey;           // only the lower byte in each int is used
    private int rounds = SK128_DEFAULT_NOF_ROUNDS;
    private int variant = SK128_VARIANT;

    private static final int[] EXP = new int[TAB_LEN];        // blank finals
    private static final int[] LOG = new int[TAB_LEN];


// Static code
//...........................................................................

    static {
        int exp = 1;
        for (int i = 0; i < TAB_LEN; i++) {
            EXP[i] = exp & 0xFF;
            LOG[EXP[i]] = i;
            exp = exp * 45 % 257;
        }
    }


// Constructor, finalizer, and clone()
//...........................................................................

    /**
     * Calls the Cipher constructor with <code>implBuffering</code> false,
     * <code>implPadding</code> false and the provider set to "Cryptix".
     * <p>
     * Sets the variant of this cipher based on the currently set "variant"
     * property in the provider properties file. The current JCE syntax
     * for the SAFER algorithm variant property is:
     * <pre>
     *    Alg.variant.SAFER = ...
     * </pre>
     * <p>
     * Valid alternatives for variant are:
     * <ul>
     *   <li> SK-128, SK128, sk-128 and sk128: Strengthened key schedule of
     *        length 128 bits.
     *   <li> SK-64, SK64, sk-64 and sk64: Strengthened key schedule of
     *        length 64 bits.
     *   <li> K-128, K128, k-128 and k128: Non-strengthened key schedule of
     *        length 128 bits.
     *   <li> K-64, K64, k-64 and k64: Non-strengthened key schedule of length
     *        64 bits.
     * </ul>
     * <p>
     * Once the variant is set, a default value for the number of rounds
     * to use is also set as follows:
     * <pre>
     *    Variant   Number of rounds = current value
     *    -------   --------------------------------
     *    SK-128    SK128_DEFAULT_NOF_ROUNDS = 10
     *    SK-64     SK64_DEFAULT_NOF_ROUNDS  =  8
     *    K-128     K128_DEFAULT_NOF_ROUNDS  = 10
     *    K-64      K64_DEFAULT_NOF_ROUNDS   =  6
     * </pre>
     * <p>
     * If no variant property is found in the provider's properties file
     * a strengthened key schedule of 128 bits is used with 10 rounds.
     * <p>
     * This constructor also attempts to set the desired number of rounds
     * for this cipher object from a "rounds" property in the provider's
     * properties file. Acceptable values are non-zero integers between 1
     * and the MAX_NOF_ROUNDS constant; i.e. 13. If no such property is
     * found, or is found but deemed invalid, then the already set value
     * (depending on the variant property as determined above) remains
     * unaltered.
     */
    public SAFER() {
        super(false, false, "Cryptix");

        sKey = new int[KEY_LENGTH];

        // at this point variant and rounds are set to their default values
        // see 'Variables and constants' section in the code above.
        try {
            String ps = Security.getAlgorithmProperty("SAFER", "variant");
            if (ps != null) setVariant(ps);
        } catch (Exception e) {}

        switch (variant) {
            case SK128_VARIANT: rounds = SK128_DEFAULT_NOF_ROUNDS; break;
            case SK64_VARIANT:  rounds = SK64_DEFAULT_NOF_ROUNDS; break;
            case K128_VARIANT:  rounds = K128_DEFAULT_NOF_ROUNDS; break;
            case K64_VARIANT:   rounds = K64_DEFAULT_NOF_ROUNDS; break;
        }

        try {
            String ps = Security.getAlgorithmProperty("SAFER", "rounds");
            if (ps != null) setRounds(Integer.parseInt(ps));
        } catch (Exception e) {}

        link();
    }

    /** Cleans up resources used by this instance, if necessary. */
    protected final void finalize() {
        if (native_lock != null) {
            synchronized(native_lock) {
                String error = native_finalize(); // may be called more than once
                if (error != null) debug(error + " in native_finalize");
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
     * <b>SPI</b>: Returns the length of an input block, in bytes.
     *
     * @return the length in bytes of an input block for this cipher.
     */
    public int engineBlockSize () { return BLOCK_SIZE; }

    /**
     * <b>SPI</b>: Initializes this cipher for encryption, using the
     * specified key.
     *
     * @param  key  the key to use for encryption.
     * @exception KeyException if the key is invalid.
     */
    public void engineInitEncrypt (Key key)
    throws KeyException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for decryption.
     * @exception KeyException if the key is invalid.
     */
    public void engineInitDecrypt (Key key)
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

    protected void engineSetParameter(String param, Object value)
    throws NoSuchParameterException, InvalidParameterException,
           InvalidParameterTypeException {
        if (param.equalsIgnoreCase("rounds")) {
            if (value instanceof Integer)
                setRounds(((Integer) value).intValue());
            else
                throw new InvalidParameterTypeException("rounds.SAFER");
        } else if (param.equalsIgnoreCase("variant")) {
            if (value instanceof String)
                setVariant((String) value);
            else
                throw new InvalidParameterTypeException("variant.SAFER");
        } else
            throw new NoSuchParameterException(param + ".SAFER");
    }

    protected Object engineGetParameter(String param)
    throws NoSuchParameterException, InvalidParameterException {
        if (param.equalsIgnoreCase("rounds"))
            return new Integer(rounds);
        else if (param.equalsIgnoreCase("variant"))
            return getVariant();
        else
            throw new NoSuchParameterException(param + ".SAFER");
    }


// Own methods
//...........................................................................

    /**
     * Sets the number of rounds for this cipher. Allowed only when this
     * cipher is in the UNINITIALIZED state; otherwise an exception is
     * thrown.
     * <p>
     * If the specified number is invalid, the current one remains unchanged.
     *
     * @param  rounds   the desired number of rounds for this cipher.
     * @throw  IllegalStateException if this cipher is not uninitialised.
     */
    public void setRounds(int rounds) {
        if (getState() != UNINITIALIZED)
            throw new IllegalStateException("Cipher not in UNINITIALIZED state");
            
        if (rounds > 0 && rounds <= MAX_NOF_ROUNDS)
            this.rounds = rounds;
        else
            throw new InvalidParameterException();
    }

    /**
     * Gets the number of rounds for this cipher.
     */
    public int getRounds() { return rounds; }

    /**
     * Sets the variant for this cipher. Allowed only when this cipher is in
     * the UNINITIALIZED state; otherwise an exception is thrown.
     *
     * @param  ps  The desired new variant identifier for this cipher.
     * @throw  IllegalStateException If this cipher is not uninitialised.
     * @throw  InvalidParameterException If the variant identifier is invalid.
     */
    public void setVariant (String ps) {
        if (getState() != UNINITIALIZED)
            throw new IllegalStateException("Cipher not in UNINITIALIZED state");

        if (ps.equalsIgnoreCase("SK128") || ps.equalsIgnoreCase("SK-128"))
            variant = SK128_VARIANT;
        else if (ps.equalsIgnoreCase("SK64") || ps.equalsIgnoreCase("SK-64"))
            variant = SK64_VARIANT;
        else if (ps.equalsIgnoreCase("K128") || ps.equalsIgnoreCase("K-128"))
            variant = K128_VARIANT;
        else if (ps.equalsIgnoreCase("K64") || ps.equalsIgnoreCase("K-64"))
            variant = K64_VARIANT;
        else
            throw new InvalidParameterException();
    }

    /**
     * Gets the variant for this cipher ("SK-128", "SK-64", "K-128", or "K-64").
     */
    public String getVariant() {
        switch (variant) {
            case SK128_VARIANT: return "SK-128";
            case SK64_VARIANT:  return "SK-64";
            case K128_VARIANT:  return "K-128";
            case K64_VARIANT:   return "K-64";
            default:            throw new InternalError("variant = " + variant);
        }
    }

    /**
     * Encryption algorithm.
     *
     * @param  in       contains the plaintext block.
     * @param  inOff    start index within input where data should be considered.
     * @param  out      will contain the ciphertext block.
     * @param  outOff   index in out where ciphertext starts.
     */
    private void blockEncrypt (byte[] in, int inOff, byte[] out, int outOff) {
        int k = 0,
            round = sKey[k++];
            
        if (MAX_NOF_ROUNDS < round)
            round = MAX_NOF_ROUNDS;
            
        int t,
            a = in[inOff++],
            b = in[inOff++],
            c = in[inOff++],
            d = in[inOff++],
            e = in[inOff++],
            f = in[inOff++],
            g = in[inOff++],
            h = in[inOff++];
            
        for (int i = 0; i < round; i++) {
            a ^= sKey[k++];
            b += sKey[k++];
            c += sKey[k++];
            d ^= sKey[k++];
            e ^= sKey[k++];
            f += sKey[k++];
            g += sKey[k++];
            h ^= sKey[k++];
            
            a = EXP[a & 0xFF] + sKey[k++];
            b = LOG[b & 0xFF] ^ sKey[k++];
            c = LOG[c & 0xFF] ^ sKey[k++];
            d = EXP[d & 0xFF] + sKey[k++];
            e = EXP[e & 0xFF] + sKey[k++];
            f = LOG[f & 0xFF] ^ sKey[k++];
            g = LOG[g & 0xFF] ^ sKey[k++];
            h = EXP[h & 0xFF] + sKey[k++];
            
            b += a;        a += b;            //    PHT(a, b);
            d += c;        c += d;            //    PHT(c, d);
            f += e;        e += f;            //    PHT(e, f);
            h += g;        g += h;            //    PHT(g, h);
            c += a;        a += c;            //    PHT(a, c);
            g += e;        e += g;            //    PHT(e, g);
            d += b;        b += d;            //    PHT(b, d);
            h += f;        f += h;            //    PHT(f, h);
            e += a;        a += e;            //    PHT(a, e);
            f += b;        b += f;            //    PHT(b, f);
            g += c;        c += g;            //    PHT(c, g);
            h += d;        d += h;            //    PHT(d, h);
            
            t = b; b = e; e = c; c = t; t = d; d = f; f = g; g = t;
        }
        out[outOff++] = (byte)(a ^ sKey[k++]);
        out[outOff++] = (byte)(b + sKey[k++]);
        out[outOff++] = (byte)(c + sKey[k++]);
        out[outOff++] = (byte)(d ^ sKey[k++]);
        out[outOff++] = (byte)(e ^ sKey[k++]);
        out[outOff++] = (byte)(f + sKey[k++]);
        out[outOff++] = (byte)(g + sKey[k++]);
        out[outOff++] = (byte)(h ^ sKey[k++]);
    }

    /**
     * Decryption algorithm.
     *
     * @param  in       contains the ciphertext block.
     * @param  inOff    index within input where cipher data should be considered
     * @param  out      will contain the plaintext block.
     * @param  outOff   index in out where plaintext starts.
     */
    private void blockDecrypt (byte[] in, int inOff, byte[] out, int outOff) {
        int round = sKey[0];
        if (MAX_NOF_ROUNDS < round)
            round = MAX_NOF_ROUNDS;
            
        int t,
            a = in[inOff++],
            b = in[inOff++],
            c = in[inOff++],
            d = in[inOff++],
            e = in[inOff++],
            f = in[inOff++],
            g = in[inOff++],
            h = in[inOff++];

        int k = BLOCK_SIZE * (1 + 2 * round);
        
        h ^= sKey[k];
        g -= sKey[--k];
        f -= sKey[--k];
        e ^= sKey[--k];
        d ^= sKey[--k];
        c -= sKey[--k];
        b -= sKey[--k];
        a ^= sKey[--k];
        
        for (int i = 0; i < round; i++) {
            t = e; e = b; b = c; c = t; t = f; f = d; d = g; g = t;
            
            a -= e;        e -= a;            //    IPHT(a, e);
            b -= f;        f -= b;            //    IPHT(b, f);
            c -= g;        g -= c;            //    IPHT(c, g);
            d -= h;        h -= d;            //    IPHT(d, h);
            a -= c;        c -= a;            //    IPHT(a, c);
            e -= g;        g -= e;            //    IPHT(e, g);
            b -= d;        d -= b;            //    IPHT(b, d);
            f -= h;        h -= f;            //    IPHT(f, h);
            a -= b;        b -= a;            //    IPHT(a, b);
            c -= d;        d -= c;            //    IPHT(c, d);
            e -= f;        f -= e;            //    IPHT(e, f);
            g -= h;        h -= g;            //    IPHT(g, h);

            h -= sKey[--k];
            g ^= sKey[--k];
            f ^= sKey[--k];
            e -= sKey[--k];
            d -= sKey[--k];
            c ^= sKey[--k];
            b ^= sKey[--k];
            a -= sKey[--k];
            
            h = LOG[h & 0xFF] ^ sKey[--k];
            g = EXP[g & 0xFF] - sKey[--k];
            f = EXP[f & 0xFF] - sKey[--k];
            e = LOG[e & 0xFF] ^ sKey[--k];
            d = LOG[d & 0xFF] ^ sKey[--k];
            c = EXP[c & 0xFF] - sKey[--k];
            b = EXP[b & 0xFF] - sKey[--k];
            a = LOG[a & 0xFF] ^ sKey[--k];
        }    
        out[outOff++] = (byte)a;
        out[outOff++] = (byte)b;
        out[outOff++] = (byte)c;
        out[outOff++] = (byte)d;
        out[outOff++] = (byte)e;
        out[outOff++] = (byte)f;
        out[outOff++] = (byte)g;
        out[outOff++] = (byte)h;
    }
    
    /**
     * Expands a userKey to a working SAFER key (sKey).
     * <p>
     * The key bytes are fist extracted from the user-key and formatted
     * into a 16-byte array (128 bits) which is then passed to the
     * Safer_Expand_Userkey() method. The length of the array is known
     * by the currently set variant of this object. If there isn't enough
     * bytes in the user key to make a valid SAFER user-key (64 or 128
     * bits), the user-key is either trunctated or copied appropriately
     * to obtain enough bytes for the Safer_Expand_Userkey() method. An
     * exception is thrown only if the user-key is null;
     */
    private synchronized void makeKey (Key key)
    throws KeyException {
        byte[] keyBytes = key.getEncoded();
        if (keyBytes == null) throw new KeyException("Invalid SAFER key");

        byte[] userKey = new byte[2 * BLOCK_SIZE];
        int keyLen = keyBytes.length,
            len = 2 * BLOCK_SIZE,
            userKeyLenSoFar = 0;

        while (len >= keyLen) {
            System.arraycopy(keyBytes, 0, userKey, userKeyLenSoFar, keyLen);
            len -= keyLen;
            userKeyLenSoFar += keyLen;
        }
        System.arraycopy(keyBytes, 0, userKey, userKeyLenSoFar, len);
        
        byte[]
            key1 = new byte[BLOCK_SIZE],
            key2 = new byte[BLOCK_SIZE];

        System.arraycopy(userKey, 0, key1, 0, BLOCK_SIZE);
        System.arraycopy(userKey, BLOCK_SIZE, key2, 0, BLOCK_SIZE);
        Safer_Expand_Userkey(key1, key2);
    }

    /**
     * Expands a user-selected key of length 64 bits or 128 bits to the
     * encryption / decryption sKey.
     *
     * Note: SAFER K-64 and SAFER SK-64 with a user-selected key 'z' of
     * length 64 bits are identical to SAFER K-128 and SAFER SK-128 with
     * a user-selected key 'z z' of length 128 bits, respectively.
     *
     * @param    userkey_1    contains the first 64 bits of user key.
     * @param    userkey_2    contains the second 64 bits of user key.
     */
    private void Safer_Expand_Userkey (byte[] userkey_1, byte[] userkey_2) {
        // If native library available then use it. If not or if
        // native method returned error then revert to 100% Java.

        if (native_lock != null) {
            synchronized(native_lock) {
                try {
                    linkStatus.check(
                        native_ks(
                            native_cookie, userkey_1, userkey_2, rounds, isStrong()));
                    return;
                } catch (Error error) {
                    native_finalize();
                    native_lock = null;
if (DEBUG && debuglevel > 0) debug(error + ". Will use 100% Java.");
                }
            }
        }

        byte[]
            ka = new byte[BLOCK_SIZE + 1],
            kb = new byte[BLOCK_SIZE + 1];

        int k = 0;
        sKey[k++] = (byte)rounds;
        for (int j = 0; j < BLOCK_SIZE; j++) {
            ka[j] = (byte)(userkey_1[j] << 5 |(userkey_1[j] & 0xFF) >>> 3);
            ka[BLOCK_SIZE] ^= ka[j];
            sKey[k++] = userkey_2[j];
            kb[j] = userkey_2[j];
            kb[BLOCK_SIZE] ^= kb[j];
        }
        for (int i = 1; i <= rounds; i++) {
            for (int j = 0; j < BLOCK_SIZE + 1; j++) {
                ka[j] = (byte)(ka[j] << 6 |(ka[j] & 0xFF) >>> 2);
                kb[j] = (byte)(kb[j] << 6 |(kb[j] & 0xFF) >>> 2);
            }
            for (int j = 0; j < BLOCK_SIZE; j++)
                if (isStrong())
                    sKey[k++] = (ka[(j + 2 * i - 1) % (BLOCK_SIZE + 1)]
                        + EXP[EXP[18 * i + j + 1]]) & 0xFF;
                else
                    sKey[k++] = (ka[j] + EXP[EXP[18 * i + j + 1]]) & 0xFF;

            for (int j = 0; j < BLOCK_SIZE; j++)
                if (isStrong())
                    sKey[k++] = (kb[(j + 2 * i) % (BLOCK_SIZE + 1)]
                        + EXP[EXP[18 * i + j + 10]]) & 0xFF;
                else
                    sKey[k++] = (kb[j] + EXP[EXP[18 * i + j + 10]]) & 0xFF;
        }
    }

    /** Returns true if this cipher should use a strengthened key schedule. */
    private boolean isStrong () { return (variant < K128_VARIANT); }
}
