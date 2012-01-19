// $Id: RC4.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: RC4.java,v $
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
// Revision 1.5  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.4  1998/04/03 07:23:55  kettler
// Made sBox and x, y non-static to allow more than one
// instance to be used at the same time.
//
// Revision 1.3  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// + Added inLen parameter to native_crypt.
//
// Revision 1.2  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.0  1997/09/18  David Hopwood
// + Rewrote engineUpdate, and moved the RC4 algorithm into a separate rc4
//   method. A temporary array is no longer needed except when the input and
//   output regions overlap.
// + Native linking and debugging are up to date with Blowfish 3.2.0
//   (hence the jump in version number).
//
// Revision 0.1.1.1  1997/08/28  David Hopwood
// + Documentation changes.
//
// Revision 0.1.1.0  1997/08/19  David Hopwood
// + Added native support API.
// + Made BLOCK_SIZE private.
// + Ensured that this class is final, and added a comment that this is for
//   security reasons.
//   If it were not final, some VMs have a bug that would allow a subclass
//   that implements Cloneable to call Object's or Cipher's clone() method
//   using invokenonvirtual, which would duplicate the pointer to the native
//   state. Then calling finalize() on the original and using the clone (for
//   example) would result in freed memory being written to :-(.
//
// Revision 0.1.0.0  1997/07/15  R. Naffah
// + Original version.
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
import java.security.InvalidKeyException;
import xjava.security.SymmetricCipher;

/**    
 * This class implements the RC4 (TM) stream cipher.
 * <p>
 * The source code (C version) from which this port was done, is the one
 * posted to the sci.crypt, alt.security, comp.security.misc, and
 * alt.privacy newsgroups on Wed, 14 Sep 1994 06:35:31 GMT by
 * "David Sterndark" &lt;sterndark@netcom.com&gt;
 * (Message-ID: &lt;sternCvKL4B.Hyy@netcom.com&gt;)
 * <p>
 * RC4 (TM) was designed by Ron Rivest, and was previously a trade secret of
 * RSA Data Security, Inc. The algorithm is now in the public domain. The name
 * "RC4" is a trademark of RSA Data Security, Inc.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Bruce Schneier,
 *        "Section 17.1 RC4,"
 *        <cite>Applied Cryptography, 2nd edition</cite>,
 *        John Wiley &amp; Sons, 1996.
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
public final class RC4 // must be final for security reasons
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("RC4") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("RC4: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("RC4", 2, 3);
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
     * Encrypts/decrypts an array. For stream ciphers, encryption is the same
     * process as decryption.
     * <p>
     * SECURITY: the caller <strong>must</strong> ensure that:
     * <ul>
     *   <li> <code>in != null</code>
     *   <li> <code>out != null</code>
     *   <li> <code>inOffset >= 0</code>
     *   <li> <code>(long)inOffset + inLen <= in.length</code>
     *   <li> <code>outOffset >= 0</code>
     *   <li> <code>(long)outOffset + inLen <= out.length</code>
     * </ul>
     *
     * @param  cookie       a valid reference to the native key structure. This
     *                      value is set by the native library upon return from
     *                      native_init() (see link() method at the top).
     * @param  in           input array containing data to encrypt/decrypt.
     * @param  inOffset     index of where we should start reading from input.
     * @param  inLen        number of bytes to encrypt/decrypt.
     * @param  out          output array containing data encrypted/decrypted.
     * @param  outOffset    index of where we should start writing to output.
     * @return the number of bytes crypted (always inLen) or 0 if an error
     *                      occurred.
     */
    private native int native_crypt(long cookie, byte[] in, int inOffset, int inLen,
                                    byte[] out, int outOffset);

    /**
     * Finalizes the native state for this object.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_finalize();


// RC4 constants and variables
//............................................................................

    /**
     * Will hold the contents of the current set S-box.
     */
    private int[] sBox = new int[256];

    /**
     * The two indices for the S-box computation referred to as i and j
     * in Schneier.
     */
    private int x, y;

    /**
     * The block size of this cipher. Being a stream cipher this value
     * is 1!
     */
    private static final int BLOCK_SIZE = 1;


// Constructor, finalizer, and clone()
//............................................................................

    /**
     * Constructs an RC4 cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public RC4() {
        super(false, false, "Cryptix");
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
    public int engineBlockSize () { return BLOCK_SIZE; }

    /**
     * <b>SPI</b>: Initializes this cipher for encryption, using the
     * specified key.
     *
     * @param  key  the key to use for encryption.
     * @exception InvalidKeyException if the key is invalid.
     */
    public void engineInitEncrypt(Key key)
    throws InvalidKeyException {
        makeKey(key);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for decryption.
     * @exception InvalidKeyException if the key is invalid.
     */
    public void engineInitDecrypt(Key key)
    throws InvalidKeyException {
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

                if (0 == native_crypt(native_cookie, in, inOffset, inLen, out, outOffset))
                    throw new CryptixException(getAlgorithm() + ": Error in native code");
            }
        } else
            rc4(in, inOffset, inLen, out, outOffset);

        return inLen;
    }


// Own methods
//............................................................................

    /**
     * RC4 encryption/decryption. The input and output regions are assumed not to
     * overlap.
     *
     * @param  in           the input data.
     * @param  inOffset     the offset into in specifying where the data starts.
     * @param  inLen        the length of the subarray.
     * @param  out          the output array.
     * @param  outOffset    the offset indicating where to start writing into
     *                      the out array.
     */
    private void rc4(byte[] in, int inOffset, int inLen, byte[] out, int outOffset) {
        int xorIndex, t;

        for (int i = 0; i < inLen; i++) {
            x = (x + 1) & 0xFF;
            y = (sBox[x] + y) & 0xFF;

            t = sBox[x];
            sBox[x] = sBox[y];
            sBox[y] = t;

            xorIndex = (sBox[x] + sBox[y]) & 0xFF;
            out[outOffset++] = (byte)(in[inOffset++] ^ sBox[xorIndex]);
        }
    }

    /**
     * Expands a user-key to a working key schedule.
     * <p>
     * The key bytes are first extracted from the user-key and then
     * used to build the contents of this key schedule.
     * <p>
     * The method's only exceptions are when the user-key's contents
     * are null, or a byte array of zero length.
     *
     * @param  key  the user-key object to use.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The encoded byte array form of the key is zero-length;
     *              </ul>
     */
    private void makeKey(Key key)
    throws InvalidKeyException {

        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        int len = userkey.length;
        if (len == 0)
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

        x =  y = 0;
        for (int i = 0; i < 256; i++)
            sBox[i] = i;

        int i1 = 0, i2 = 0, t;

        for (int i = 0; i < 256; i++) {
            i2 = ((userkey[i1] & 0xFF) + sBox[i] + i2) & 0xFF;

            t = sBox[i];
            sBox[i] = sBox[i2];
            sBox[i2] = t;

            i1 = (i1 + 1) % len;
        }
    }
}
