// $Id: DES.java,v 1.1 2002/09/05 19:27:05 baford Exp $
//
// $Log: DES.java,v $
// Revision 1.1  2002/09/05 19:27:05  baford
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
// Revision 1.5  1997/12/19 06:03:31  hopwood
// + Committed changes below.
//
// Revision 1.4.1  1997/12/19  hopwood
// + Added reference and URL for FIPS 46-2.
//
// Revision 1.4  1997/11/29 04:42:56  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.3  1997/11/22 07:05:39  raif
// + Removed most of the test data to cryptix.util.test.Des.mtest
//   for use with Maker.
// + Cosmetics.
//
// Revision 1.2  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1.1  1997/11/20  David Hopwood
// + Added native_crypt3 method (the C code will need to be written or
//   copied from Eric Young's implementation).
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.0  1997/08/17  David Hopwood
// + Removed all deprecated methods and fields. Cryptix 2.2 compatibility
//   is now handled by the separate cryptix.security.DES class.
// + Tightened some of the access specifiers (e.g. SPI methods were public,
//   and are now protected).
// + Ensured that this class is final, and added a comment that this is for
//   security reasons.
//   If it were not final, some VMs have a bug that would allow a subclass
//   that implements Cloneable to call Object's or Cipher's clone() method
//   using invokenonvirtual, which would duplicate the pointer to the native
//   state. Then calling finalize() on the original and using the clone (for
//   example) would result in freed memory being written to :-(.
//
// Revision 0.3.1.1  1997/08/06  David Hopwood
// + Changed BLOCK_LENGTH to BLOCK_SIZE in most places, leaving
//   BLOCK_LENGTH deprecated.
// + Added missing finalize() method.
// + Now consistent with Blowfish 0.3.1.3.
//
// Revision 0.3.1.0  1997/08/02  David Hopwood
// + Merged my version, Raif's version, and the version from Systemics'
//   server.
// + Now consistent with Blowfish 0.3.1.2 (including security fixes to
//   engineUpdate).
// + Added reference to DES-EDE3 in comments.
// + Required native code version is 2.3.
//
// Revision 0.3.0.2  1997/07/14  R. Naffah
// + Java code tests OK.
// + Consistent with Blowfish 0.3.0.5.
// + Included code (crypt3() method) from cryptix.tools.Crypt
//   ported by John F. Dumas (jdumas@zgs.com) to generate Unix-like
//   crypt(3) passwords.
//
// Revision 0.3.0.1  1997/07/05  David Hopwood
// + Many changes (JCE, native linking, debugging, ...), to be
//   consistent with Blowfish.
//
// Revision 0.3.0.0  1997/04/05  Systemics
// + Added Geoffrey Keating's Java code to DLL stubs, wrote doco, etc.
// + Moved copyright notices to end.
//
// Revision 0.2.5.2  1996/?/0?  Geoffrey Keating
// + Ported Eric Young's C version to Java.
//
// Revision 0.2.5.1  1997/03/15  Jill Baker
// + Moved this file here from old namespace.
//
// Revision 0.2.5.0  1997/02/24  Systemics
// + Original version - DLL links only.
//
// $Endlog$
/*
 * Copyright (c) 1995-1997 Systemics Ltd
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
 * DES is a block cipher with an 8 byte block size. The key length
 * is 8 bytes, but only 56 bits are used as the parity bit in each
 * byte is ignored.
 * <P>
 * This algorithm has been seriously analysed over the last 30 years,
 * and no significant weaknesses have been reported. Its only known
 * flaw is that the key length of 56 bits makes it relatively easy to
 * brute-force it.
 * <p>
 * To overcome this near-fatal flaw, it is recommended that DES be
 * used in Triple DES mode. The JCA algorithm name for the recommended
 * form of Triple DES is "DES-EDE3/CBC", which is implemented by the
 * <samp><a href="cryptix.provider.cipher.DES_EDE3.html">DES_EDE3</a></samp>
 * and <samp><a href="cryptix.provider.mode.CBC.html">CBC</a></samp classes.
 * <p>
 * DES was written by IBM and first released in 1976. The algorithm is
 * freely usable for both single and triple encryption.
 * <p>
 * <b>References:</b>
 * <ol>
 *    <li> <a href="mailto:schneier@counterpane.com">Bruce Schneier</a>,
 *         "Chapter 12 Data Encryption Standard,"
 *         <cite>Applied Cryptography, 2nd edition</cite>,
 *         John Wiley &amp; Sons, 1996.
 *         <p>
 *    <li> NIST FIPS PUB 46-2 (supercedes FIPS PUB 46-1),
 *         "Data Encryption Standard",
 *         U.S. Department of Commerce, December 1993.<br>
 *         <a href="http://www.itl.nist.gov/div897/pubs/fip46-2.htm">
 *         http://www.itl.nist.gov/div897/pubs/fip46-2.htm</a>
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Systemics Ltd
 * @author  Geoffrey Keating (this Java implementation)
 * @author  Eric Young
 * @author  David Hopwood
 * @author  Raif S. Naffah
 * @author  John F. Dumas (jdumas@zgs.com)
 * @since   Cryptix 2.2.2
 */
public final class DES // must be final for security reasons
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("DES") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("DES: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("DES", 2, 3);

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
     * Implements the Unix crypt(3) algorithm in native code.
     *
     * @param  cookie       a valid reference to the native key structure. This
     *                      value is set by the native library upon return from
     *                      native_init() (see link() method at the top).
     * @param  E0           first 32 bits of input.
     * @param  E1           second 32 bits of input.
     */
    private native int[] native_crypt3(long cookie, int E0, int E1);

    /**
     * Finalizes the native state for this object.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_finalize();


// DES constants and variables
//...........................................................................

    private static final int
        ROUNDS = 16,                        // number of encryption/decryption rounds
        BLOCK_SIZE = 8,                     // DES block size in bytes
        KEY_LENGTH = 8,                     // DES key length in bytes
        INTERNAL_KEY_LENGTH = ROUNDS * 2;   // number of elements in key schedule

    /**
     * Table for PC2 permutations in key schedule computation.
     */
    private static final int[] SKB = new int[8 * 64];       // blank final

    /**
     * The internal key schedule.
     */
    private int[] sKey = new int[INTERNAL_KEY_LENGTH];

    /**
     * Table for S-boxes and permutations, used in encrypt_base.
     */
    private static final int SP_TRANS[] = new int[8 * 64];  // blank final


// Static code
//...........................................................................

    static {
        //
        // build the SKB table
        //
        
        // represent the bit number that each permutated bit is derived from
        // according to FIPS-46
        String cd =
            "D]PKESYM`UBJ\\@RXA`I[T`HC`LZQ"+"\\PB]TL`[C`JQ@Y`HSXDUIZRAM`EK";
        int j, s, bit;
        int count = 0;
        int offset = 0;
        for (int i = 0; i < cd.length(); i++) {
            s = cd.charAt(i) - '@';
            if (s != 32) {
                bit = 1 << count++;
                for (j = 0; j < 64; j++)
                    if ((bit & j) != 0) SKB[offset + j] |= 1 << s;
                if (count == 6) {
                    offset += 64;
                    count = 0;
                }
            }
        }

        //
        // build the SP_TRANS table
        //
        
        // I'd _really_ like to just say 'SP_TRANS = { ... }', but
        // that would be terribly inefficient (code size + time). 
        // Instead we use a compressed representation --GK
        String spt =
            "g3H821:80:H03BA0@N1290BAA88::3112aIH8:8282@0@AH0:1W3A8P810@22;22"+
            "A18^@9H9@129:<8@822`?:@0@8PH2H81A19:G1@03403A0B1;:0@1g192:@919AA"+
            "0A109:W21492H@0051919811:215011139883942N8::3112A2:31981jM118::A"+
            "101@I88:1aN0<@030128:X;811`920:;H0310D1033@W980:8A4@804A3803o1A2"+
            "021B2:@1AH023GA:8:@81@@12092B:098042P@:0:A0HA9>1;289:@1804:40Ph="+
            "1:H0I0HP0408024bC9P8@I808A;@0@0PnH0::8:19J@818:@iF0398:8A9H0<13@"+
            "001@11<8;@82B01P0a2989B:0AY0912889bD0A1@B1A0A0AB033O91182440A9P8"+
            "@I80n@1I03@1J828212A`A8:12B1@19A9@9@8^B:0@H00<82AB030bB840821Q:8"+
            "310A302102::A1::20A1;8"; // OK, try to type _that_!
            // [526 chars, 3156 bits]
        // The theory is that each bit position in each int of SP_TRANS is
        // set in exactly 32 entries. We keep track of set bits.
        offset = 0;
        int k, c, param;
        for (int i = 0; i < 32; i++) { // each bit position
            k = -1; // pretend the -1th bit was set
            bit = 1 << i;
            for (j = 0; j < 32; j++) { // each set bit
                // Each character consists of two three-bit values:
                c = spt.charAt(offset >> 1) - '0' >> (offset & 1) * 3 & 7;
                offset++;
                if (c < 5) {
                    // values 0...4 indicate a set bit 1...5 positions
                    // from the previous set bit
                    k += c + 1;
                    SP_TRANS[k] |= bit;
                    continue;
                }
                // other values take at least an additional parameter:
                // the next value in the sequence.
                param = spt.charAt(offset >> 1) - '0' >> (offset & 1) * 3 & 7;
                offset++;
                if (c == 5) {
                    // indicates a bit set param+6 positions from
                    // the previous set bit
                    k += param + 6;
                    SP_TRANS[k] |= bit;
                } else if (c == 6) {
                    // indicates a bit set (param * 64) + 1 positions
                    // from the previous set bit
                    k += (param << 6) + 1;
                    SP_TRANS[k] |= bit;
                } else {
                    // indicates that we should skip (param * 64) positions,
                    // then process the next value which will be in the range
                    // 0...4.
                    k += param << 6;
                    j--;
                }
            }
        }
    }


// Constructor, finalizer, and clone()
//...........................................................................

    /**
     * Constructs a DES cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public DES() {
        super(false, false, "Cryptix");
        link();
    }

    /** Cleans up resources used by this instance, if necessary. */
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
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not KEY_LENGTH.
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
     *                <li> The length of the user key array is not KEY_LENGTH.
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
        } else {
            for (int i = 0; i < blockCount; i++) {
                des(in, inOffset, out, outOffset, doEncrypt);
                inOffset += BLOCK_SIZE;
                outOffset += BLOCK_SIZE;
            }
        }
        return inLen;
    }


// Own methods
//...........................................................................

    /**
     * Expands a user-key to a working key schedule.
     *
     * @param  key  the user-key object to use.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is not KEY_LENGTH.
     *              </ul>
     */
    private void makeKey (Key key)
    throws InvalidKeyException {

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
        int i = 0;
        int c = (userkey[i++] & 0xFF)       |
                (userkey[i++] & 0xFF) <<  8 |
                (userkey[i++] & 0xFF) << 16 |
                (userkey[i++] & 0xFF) << 24;
        int d = (userkey[i++] & 0xFF)       |
                (userkey[i++] & 0xFF) <<  8 |
                (userkey[i++] & 0xFF) << 16 |
                (userkey[i++] & 0xFF) << 24;

        int t = ((d >>> 4) ^ c) & 0x0F0F0F0F;
        c ^= t;
        d ^= t << 4;
        t = ((c << 18) ^ c) & 0xCCCC0000;
        c ^= t ^ t >>> 18;        
        t = ((d << 18) ^ d) & 0xCCCC0000;
        d ^= t ^ t >>> 18;        
        t = ((d >>> 1) ^ c) & 0x55555555;
        c ^= t;
        d ^= t << 1;
        t = ((c >>> 8) ^ d) & 0x00FF00FF;
        d ^= t;
        c ^= t << 8;
        t = ((d >>> 1) ^ c) & 0x55555555;
        c ^= t;
        d ^= t << 1;

        d = (d & 0x000000FF) <<  16 |
            (d & 0x0000FF00)        |
            (d & 0x00FF0000) >>> 16 |
            (c & 0xF0000000) >>>  4;
        c &= 0x0FFFFFFF;

        int s;
        int j = 0;

        for (i = 0; i < ROUNDS; i++) {
            if ((0x7EFC >> i & 1) == 1) {
                c = (c >>> 2 | c << 26) & 0x0FFFFFFF;
                d = (d >>> 2 | d << 26) & 0x0FFFFFFF;
            } else {
                c = (c >>> 1 | c << 27) & 0x0FFFFFFF;
                d = (d >>> 1 | d << 27) & 0x0FFFFFFF;
            }
            s = SKB[           c         & 0x3F                        ] |
                SKB[0x040 | (((c >>>  6) & 0x03) | ((c >>>  7) & 0x3C))] |
                SKB[0x080 | (((c >>> 13) & 0x0F) | ((c >>> 14) & 0x30))] |
                SKB[0x0C0 | (((c >>> 20) & 0x01) | ((c >>> 21) & 0x06)
                                                 | ((c >>> 22) & 0x38))];
            t = SKB[0x100 | ( d         & 0x3F                      )] |
                SKB[0x140 | (((d >>>  7) & 0x03) | ((d >>>  8) & 0x3c))] |
                SKB[0x180 | ((d >>> 15) & 0x3F                      )] |
                SKB[0x1C0 | (((d >>> 21) & 0x0F) | ((d >>> 22) & 0x30))];

            sKey[j++] = t <<  16 | (s & 0x0000FFFF);
            s         = s >>> 16 | (t & 0xFFFF0000);
            sKey[j++] = s <<   4 |  s >>> 28;
        }
    }
    
    /**
     * Encrypts/decrypts a block, of length BLOCK_SIZE.
     *
     * @param  in       an array containing the input block
     * @param  inOffset the starting offset of the input block
     * @param  out      an array containing the output block
     * @param  inOffset the starting offset of the output block
     * @param  encrypt  true to encrypt, false to decrypt
     */
    protected void
    des (byte[] in, int inOffset, byte[] out, int outOffset, boolean encrypt) {
        int[] lr = {
            (in[inOffset++] & 0xFF)       |
            (in[inOffset++] & 0xFF) <<  8 |
            (in[inOffset++] & 0xFF) << 16 |
            (in[inOffset++] & 0xFF) << 24,
            (in[inOffset++] & 0xFF)       |
            (in[inOffset++] & 0xFF) <<  8 |
            (in[inOffset++] & 0xFF) << 16 |
            (in[inOffset  ] & 0xFF) << 24};

        initialPermutation(lr);

        // do it!
        if (encrypt)
            encrypt_base(lr);
        else
            decrypt_base(lr);

        finalPermutation(lr);

        int R = lr[0];
        int L = lr[1];
        
        out[outOffset++] = (byte) L;
        out[outOffset++] = (byte)(L >>  8);
        out[outOffset++] = (byte)(L >> 16);
        out[outOffset++] = (byte)(L >> 24);
        out[outOffset++] = (byte) R;
        out[outOffset++] = (byte)(R >>  8);
        out[outOffset++] = (byte)(R >> 16);
        out[outOffset  ] = (byte)(R >> 24);
    }

    /** Implements DES encryption without IP and FP. */
    private void encrypt_base (int[] io) {
        int L = io[0];
        int R = io[1];
        
        // look! we fit all four variables (plus the class itself)
        // into short byte-codes!
        int u = R << 1 | R >>> 31;
        R = L << 1 | L >>> 31;
        L = u;
        int t;

        for (int i = 0; i < INTERNAL_KEY_LENGTH;) {
            u = R ^ sKey[i++];
            t = R ^ sKey[i++];
            t = t >>> 4 | t << 28;
            L ^= (SP_TRANS[0x040 | (t         & 0x3F)] |
                  SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                  SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                  SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                  SP_TRANS[          u         & 0x3F ] |
                  SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                  SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                  SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);

            u = L ^ sKey[i++];
            t = L ^ sKey[i++];
            t = t >>> 4 | t << 28;
            R ^= (SP_TRANS[0x040 | (t         & 0x3F)] |
                  SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                  SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                  SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                  SP_TRANS[          u         & 0x3F ] |
                  SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                  SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                  SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);
        }
        io[0] = R >>> 1 | R << 31;
        io[1] = L >>> 1 | L << 31;
    }

    /** Implements DES decryption without IP and FP. */
    private void decrypt_base (int[] io) {
        int L = io[0];
        int R = io[1];
        
        // look! we fit all four variables (plus the class itself)
        // into short byte-codes!
        int u = R << 1 | R >>> 31;
        R = L << 1 | L >>> 31;
        L = u;
        int t;

        for (int i = INTERNAL_KEY_LENGTH - 1; i > 0;) {
            t = R ^ sKey[i--];
            u = R ^ sKey[i--];
            t = t >>> 4 | t << 28;
            L ^= (SP_TRANS[0x040 | (t         & 0x3F)] |
                  SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                  SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                  SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                  SP_TRANS[          u         & 0x3F ] |
                  SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                  SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                  SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);

            t = L ^ sKey[i--];
            u = L ^ sKey[i--];
            t = t >>> 4 | t << 28;
            R ^= (SP_TRANS[0x040 | (t         & 0x3F)] |
                  SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                  SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                  SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                  SP_TRANS[          u         & 0x3F ] |
                  SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                  SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                  SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);
        }
        io[0] = R >>> 1 | R << 31;
        io[1] = L >>> 1 | L << 31;
    }
    
    private static void initialPermutation (int[] io) {
        int L = io[0];
        int R = io[1];
        int t = ((R >>> 4) ^ L) & 0x0F0F0F0F;
        L ^= t;
        R ^= t << 4;
        t = ((L >>> 16) ^ R) & 0x0000FFFF;
        R ^= t;
        L ^= t << 16;
        t = ((R >>> 2) ^ L) & 0x33333333;
        L ^= t;
        R ^= t << 2;
        t = ((L >>> 8) ^ R) & 0x00FF00FF;
        R ^= t;
        L ^= t << 8;
        t = ((R >>> 1) ^ L) & 0x55555555;
        io[0] = L ^ t;
        io[1] = R ^ (t << 1);
    }
    
    private static void finalPermutation (int[] io) {
        int L = io[1];
        int R = io[0];
        int t = (R >>> 1 ^ L) & 0x55555555;
        L ^= t;
        R ^= t << 1;
        t = (L >>> 8 ^ R) & 0x00FF00FF;
        R ^= t;
        L ^= t << 8;
        t = (R >>> 2 ^ L) & 0x33333333;
        L ^= t;
        R ^= t << 2;
        t = (L >>> 16 ^ R) & 0x0000FFFF;
        R ^= t;
        L ^= t << 16;
        t = (R >>> 4 ^ L) & 0x0F0F0F0F;
        io[1] = L ^ t;
        io[0] = R ^ (t << 4);
    }

    /**
     * Implements the Unix crypt(3) algorithm.
     * <p>
     * This method is intended only for use by the class
     * <a href=cryptix.tools.UnixCrypt.html><samp>cryptix.tools.UnixCrypt</samp></a>,
     * and should not be used directly by applications.
     *
     * @param  E0    first 32 bits of input.
     * @param  E1    second 32 bits of input.
     */
    public int[] crypt3(int E0, int E1) {
        if (native_lock != null) {
            synchronized(native_lock) {
                int[] result = native_crypt3(native_cookie, E0, E1);
                if (result == null)
                    throw new CryptixException(getAlgorithm() + ": Error in native code");
                return result;
            }
        }
        int L = 0;
        int R = 0;
        int t, u, v;
        for (int i = 0; i < 25; i++) {
            for (int j = 0; j < ROUNDS * 2;) {
                v = R ^ (R >>> 16);
                u = v & E0;
                v &= E1;
                u ^= (u << 16) ^ R ^ sKey[j++];
                t = v ^ (v << 16) ^ R ^ sKey[j++];
                t = t >>> 4 | t << 28;
                L ^= (SP_TRANS[0x040 | ( t         & 0x3F)] |
                      SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                      SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                      SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                      SP_TRANS[          u         & 0x3F ] |
                      SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                      SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                      SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);
                    
                v = L ^ (L >>> 16);
                u = v & E0;
                v &= E1;
                u ^= (u << 16) ^ L ^ sKey[j++];
                t = v ^ (v << 16) ^ L ^ sKey[j++];
                t = t >>> 4 | t << 28;
                R ^= (SP_TRANS[0x040 | (t         & 0x3F)] |
                      SP_TRANS[0x0C0 | ((t >>>  8) & 0x3F)] |
                      SP_TRANS[0x140 | ((t >>> 16) & 0x3F)] |
                      SP_TRANS[0x1C0 | ((t >>> 24) & 0x3F)] |
                      SP_TRANS[          u         & 0x3F ] |
                      SP_TRANS[0x080 | ((u >>>  8) & 0x3F)] |
                      SP_TRANS[0x100 | ((u >>> 16) & 0x3F)] |
                      SP_TRANS[0x180 | ((u >>> 24) & 0x3F)]);
            }
            t = L; L = R; R = t;
        }
        t = L;
        L = R >>> 1 | R << 31;
        R = t >>> 1 | t << 31;

        t = (R >>> 1 ^ L) & 0x55555555;
        L ^= t;
        R ^= t << 1;
        t = (L >>> 8 ^ R) & 0x00FF00FF;
        R ^= t;
        L ^= t << 8;
        t = (R >>> 2 ^ L) & 0x33333333;
        L ^= t;
        R ^= t << 2;
        t = (L >>> 16 ^ R) & 0x0000FFFF;
        R ^= t;
        L ^= t << 16;
        t = (R >>> 4 ^ L) & 0x0F0F0F0F;

        int[] result = {L ^ t, R ^ (t << 4)};
        return result;
    }


// Test methods
//...........................................................................

    /** Entry point for <code>self_test</code>. */
    public static void main(String[] argv) {
        try {
            self_test();
            // time_test(System.out, "1c587f1c13924fef", "305532286d6f295a", "63fac0d034d9f793");
        } catch (Exception e) { e.printStackTrace(); }
    }
    
    private static final String[][] tests =
    {//   KEY                 PLAINTEXT           CIPHERTEXT
        {"0101010101010101", "95f8a5e5dd31d900", "8000000000000000"},
        {"0101010101010101", "dd7f121ca5015619", "4000000000000000"},
        {"0101010101010101", "2e8653104f3834ea", "2000000000000000"},
/*
        {"0101010101010101", "4bd388ff6cd81d4f", "1000000000000000"},
        {"0101010101010101", "20b9e767b2fb1456", "0800000000000000"},
        {"0101010101010101", "55579380d77138ef", "0400000000000000"},
        {"0101010101010101", "6cc5defaaf04512f", "0200000000000000"},
        {"0101010101010101", "0d9f279ba5d87260", "0100000000000000"},
        {"0101010101010101", "d9031b0271bd5a0a", "0080000000000000"},
        {"0101010101010101", "424250b37c3dd951", "0040000000000000"},
        {"0101010101010101", "b8061b7ecd9a21e5", "0020000000000000"},
        {"0101010101010101", "f15d0f286b65bd28", "0010000000000000"},
        {"0101010101010101", "add0cc8d6e5deba1", "0008000000000000"},
        {"0101010101010101", "e6d5f82752ad63d1", "0004000000000000"},
        {"0101010101010101", "ecbfe3bd3f591a5e", "0002000000000000"},
        {"0101010101010101", "f356834379d165cd", "0001000000000000"},
        {"0101010101010101", "2b9f982f20037fa9", "0000800000000000"},
        {"0101010101010101", "889de068a16f0be6", "0000400000000000"},
        {"0101010101010101", "e19e275d846a1298", "0000200000000000"},
        {"0101010101010101", "329a8ed523d71aec", "0000100000000000"},
        {"0101010101010101", "e7fce22557d23c97", "0000080000000000"},
        {"0101010101010101", "12a9f5817ff2d65d", "0000040000000000"},
        {"0101010101010101", "a484c3ad38dc9c19", "0000020000000000"},
        {"0101010101010101", "fbe00a8a1ef8ad72", "0000010000000000"},
        {"0101010101010101", "750d079407521363", "0000008000000000"},
        {"0101010101010101", "64feed9c724c2faf", "0000004000000000"},
        {"0101010101010101", "f02b263b328e2b60", "0000002000000000"},
        {"0101010101010101", "9d64555a9a10b852", "0000001000000000"},
        {"0101010101010101", "d106ff0bed5255d7", "0000000800000000"},
        {"0101010101010101", "e1652c6b138c64a5", "0000000400000000"},
        {"0101010101010101", "e428581186ec8f46", "0000000200000000"},
        {"0101010101010101", "aeb5f5ede22d1a36", "0000000100000000"},
        {"0101010101010101", "e943d7568aec0c5c", "0000000080000000"},
        {"0101010101010101", "df98c8276f54b04b", "0000000040000000"},
        {"0101010101010101", "b160e4680f6c696f", "0000000020000000"},
        {"0101010101010101", "fa0752b07d9c4ab8", "0000000010000000"},
        {"0101010101010101", "ca3a2b036dbc8502", "0000000008000000"},
        {"0101010101010101", "5e0905517bb59bcf", "0000000004000000"},
        {"0101010101010101", "814eeb3b91d90726", "0000000002000000"},
        {"0101010101010101", "4d49db1532919c9f", "0000000001000000"},
        {"0101010101010101", "25eb5fc3f8cf0621", "0000000000800000"},
        {"0101010101010101", "ab6a20c0620d1c6f", "0000000000400000"},
        {"0101010101010101", "79e90dbc98f92cca", "0000000000200000"},
        {"0101010101010101", "866ecedd8072bb0e", "0000000000100000"},
        {"0101010101010101", "8b54536f2f3e64a8", "0000000000080000"},
        {"0101010101010101", "ea51d3975595b86b", "0000000000040000"},
        {"0101010101010101", "caffc6ac4542de31", "0000000000020000"},
        {"0101010101010101", "8dd45a2ddf90796c", "0000000000010000"},
        {"0101010101010101", "1029d55e880ec2d0", "0000000000008000"},
        {"0101010101010101", "5d86cb23639dbea9", "0000000000004000"},
        {"0101010101010101", "1d1ca853ae7c0c5f", "0000000000002000"},
        {"0101010101010101", "ce332329248f3228", "0000000000001000"},
        {"0101010101010101", "8405d1abe24fb942", "0000000000000800"},
        {"0101010101010101", "e643d78090ca4207", "0000000000000400"},
        {"0101010101010101", "48221b9937748a23", "0000000000000200"},
        {"0101010101010101", "dd7c0bbd61fafd54", "0000000000000100"},
        {"0101010101010101", "2fbc291a570db5c4", "0000000000000080"},
        {"0101010101010101", "e07c30d7e4e26e12", "0000000000000040"},
        {"0101010101010101", "0953e2258e8e90a1", "0000000000000020"},
        {"0101010101010101", "5b711bc4ceebf2ee", "0000000000000010"},
        {"0101010101010101", "cc083f1e6d9e85f6", "0000000000000008"},
        {"0101010101010101", "d2fd8867d50d2dfe", "0000000000000004"},
        {"0101010101010101", "06e7ea22ce92708f", "0000000000000002"},
        {"0101010101010101", "166b40b44aba4bd6", "0000000000000001"},
        {"8001010101010101", "0000000000000000", "95a8d72813daa94d"},
        {"4001010101010101", "0000000000000000", "0eec1487dd8c26d5"},
        {"2001010101010101", "0000000000000000", "7ad16ffb79c45926"},
        {"1001010101010101", "0000000000000000", "d3746294ca6a6cf3"},
        {"0801010101010101", "0000000000000000", "809f5f873c1fd761"},
        {"0401010101010101", "0000000000000000", "c02faffec989d1fc"},
        {"0201010101010101", "0000000000000000", "4615aa1d33e72f10"},
        {"0180010101010101", "0000000000000000", "2055123350c00858"},
        {"0140010101010101", "0000000000000000", "df3b99d6577397c8"},
        {"0120010101010101", "0000000000000000", "31fe17369b5288c9"},
        {"0110010101010101", "0000000000000000", "dfdd3cc64dae1642"},
        {"0108010101010101", "0000000000000000", "178c83ce2b399d94"},
        {"0104010101010101", "0000000000000000", "50f636324a9b7f80"},
        {"0102010101010101", "0000000000000000", "a8468ee3bc18f06d"},
        {"0101800101010101", "0000000000000000", "a2dc9e92fd3cde92"},
        {"0101400101010101", "0000000000000000", "cac09f797d031287"},
        {"0101200101010101", "0000000000000000", "90ba680b22aeb525"},
        {"0101100101010101", "0000000000000000", "ce7a24f350e280b6"},
        {"0101080101010101", "0000000000000000", "882bff0aa01a0b87"},
        {"0101040101010101", "0000000000000000", "25610288924511c2"},
        {"0101020101010101", "0000000000000000", "c71516c29c75d170"},
        {"0101018001010101", "0000000000000000", "5199c29a52c9f059"},
        {"0101014001010101", "0000000000000000", "c22f0a294a71f29f"},
        {"0101012001010101", "0000000000000000", "ee371483714c02ea"},
        {"0101011001010101", "0000000000000000", "a81fbd448f9e522f"},
        {"0101010801010101", "0000000000000000", "4f644c92e192dfed"},
        {"0101010401010101", "0000000000000000", "1afa9a66a6df92ae"},
        {"0101010201010101", "0000000000000000", "b3c1cc715cb879d8"},
        {"0101010180010101", "0000000000000000", "19d032e64ab0bd8b"},
        {"0101010140010101", "0000000000000000", "3cfaa7a7dc8720dc"},
        {"0101010120010101", "0000000000000000", "b7265f7f447ac6f3"},
        {"0101010110010101", "0000000000000000", "9db73b3c0d163f54"},
        {"0101010108010101", "0000000000000000", "8181b65babf4a975"},
        {"0101010104010101", "0000000000000000", "93c9b64042eaa240"},
        {"0101010102010101", "0000000000000000", "5570530829705592"},
        {"0101010101800101", "0000000000000000", "8638809e878787a0"},
        {"0101010101400101", "0000000000000000", "41b9a79af79ac208"},
        {"0101010101200101", "0000000000000000", "7a9be42f2009a892"},
        {"0101010101100101", "0000000000000000", "29038d56ba6d2745"},
        {"0101010101080101", "0000000000000000", "5495c6abf1e5df51"},
        {"0101010101040101", "0000000000000000", "ae13dbd561488933"},
        {"0101010101020101", "0000000000000000", "024d1ffa8904e389"},
        {"0101010101018001", "0000000000000000", "d1399712f99bf02e"},
        {"0101010101014001", "0000000000000000", "14c1d7c1cffec79e"},
        {"0101010101012001", "0000000000000000", "1de5279dae3bed6f"},
        {"0101010101011001", "0000000000000000", "e941a33f85501303"},
        {"0101010101010801", "0000000000000000", "da99dbbc9a03f379"},
        {"0101010101010401", "0000000000000000", "b7fc92f91d8e92e9"},
        {"0101010101010201", "0000000000000000", "ae8e5caa3ca04e85"},
        {"0101010101010180", "0000000000000000", "9cc62df43b6eed74"},
        {"0101010101010140", "0000000000000000", "d863dbb5c59a91a0"},
        {"0101010101010120", "0000000000000000", "a1ab2190545b91d7"},
        {"0101010101010110", "0000000000000000", "0875041e64c570f7"},
        {"0101010101010108", "0000000000000000", "5a594528bebef1cc"},
        {"0101010101010104", "0000000000000000", "fcdb3291de21f0c0"},
        {"0101010101010102", "0000000000000000", "869efd7f9f265a09"},
        {"1046913489980131", "0000000000000000", "88d55e54f54c97b4"},
        {"1007103489988020", "0000000000000000", "0c0cc00c83ea48fd"},
        {"10071034c8980120", "0000000000000000", "83bc8ef3a6570183"},
        {"1046103489988020", "0000000000000000", "df725dcad94ea2e9"},
        {"1086911519190101", "0000000000000000", "e652b53b550be8b0"},
        {"1086911519580101", "0000000000000000", "af527120c485cbb0"},
        {"5107b01519580101", "0000000000000000", "0f04ce393db926d5"},
        {"1007b01519190101", "0000000000000000", "c9f00ffc74079067"},
        {"3107915498080101", "0000000000000000", "7cfd82a593252b4e"},
        {"3107919498080101", "0000000000000000", "cb49a2f9e91363e3"},
        {"10079115b9080140", "0000000000000000", "00b588be70d23f56"},
        {"3107911598090140", "0000000000000000", "406a9a6ab43399ae"},
        {"1007d01589980101", "0000000000000000", "6cb773611dca9ada"},
        {"9107911589980101", "0000000000000000", "67fd21c17dbb5d70"},
        {"9107d01589190101", "0000000000000000", "9592cb4110430787"},
        {"1007d01598980120", "0000000000000000", "a6b7ff68a318ddd3"},
        {"1007940498190101", "0000000000000000", "4d102196c914ca16"},
        {"0107910491190401", "0000000000000000", "2dfa9f4573594965"},
        {"0107910491190101", "0000000000000000", "b46604816c0e0774"},
        {"0107940491190401", "0000000000000000", "6e7e6221a4f34e87"},
        {"19079210981a0101", "0000000000000000", "aa85e74643233199"},
        {"1007911998190801", "0000000000000000", "2e5a19db4d1962d6"},
        {"10079119981a0801", "0000000000000000", "23a866a809d30894"},
        {"1007921098190101", "0000000000000000", "d812d961f017d320"},
        {"100791159819010b", "0000000000000000", "055605816e58608f"},
        {"1004801598190101", "0000000000000000", "abd88e8b1b7716f1"},
        {"1004801598190102", "0000000000000000", "537ac95be69da1e1"},
        {"1004801598190108", "0000000000000000", "aed0f6ae3c25cdd8"},
        {"1002911598100104", "0000000000000000", "b3e35a5ee53e7b8d"},
        {"1002911598190104", "0000000000000000", "61c79c71921a2ef8"},
        {"1002911598100201", "0000000000000000", "e2f5728f0995013c"},
        {"1002911698100101", "0000000000000000", "1aeac39a61f0a464"},
        {"7ca110454a1a6e57", "01a1d6d039776742", "690f5b0d9a26939b"},
        {"0131d9619dc1376e", "5cd54ca83def57da", "7a389d10354bd271"},
        {"07a1133e4a0b2686", "0248d43806f67172", "868ebb51cab4599a"},
        {"3849674c2602319e", "51454b582ddf440a", "7178876e01f19b2a"},
        {"04b915ba43feb5b6", "42fd443059577fa2", "af37fb421f8c4095"},
        {"0113b970fd34f2ce", "059b5e0851cf143a", "86a560f10ec6d85b"},
        {"0170f175468fb5e6", "0756d8e0774761d2", "0cd3da020021dc09"},
        {"43297fad38e373fe", "762514b829bf486a", "ea676b2cb7db2b7a"},
        {"07a7137045da2a16", "3bdd119049372802", "dfd64a815caf1a0f"},
        {"04689104c2fd3b2f", "26955f6835af609a", "5c513c9c4886c088"},
        {"37d06bb516cb7546", "164d5e404f275232", "0a2aeeae3ff4ab77"},
        {"1f08260d1ac2465e", "6b056e18759f5cca", "ef1bf03e5dfa575a"},
        {"584023641aba6176", "004bd6ef09176062", "88bf0db6d70dee56"},
        {"025816164629b007", "480d39006ee762f2", "a1f9915541020b56"},
        {"49793ebc79b3258f", "437540c8698f3cfa", "6fbf1cafcffd0556"},
        {"4fb05e1515ab73a7", "072d43a077075292", "2f22e49bab7ca1ac"},
        {"49e95d6d4ca229bf", "02fe55778117f12a", "5a6b612cc26cce4a"},
        {"018310dc409b26d6", "1d9d5c5018f728c2", "5f4c038ed12b2e41"},
        {"1c587f1c13924fef", "305532286d6f295a", "63fac0d034d9f793"}
*/
    };
    
    private static void self_test()
    throws Exception {
        Cipher cryptor = Cipher.getInstance("DES", "Cryptix");
        RawSecretKey userKey;
        byte[] tmp, pt, ct;

        for (int i = 0; i < tests.length; i++) {
            userKey = new RawSecretKey("DES", Hex.fromString(tests[i][0]));
            pt = Hex.fromString(tests[i][1]);
            ct = Hex.fromString(tests[i][2]);

            cryptor.initEncrypt(userKey);
            tmp = cryptor.crypt(pt);
            if (!ArrayUtil.areEqual(ct, tmp))
                   throw new CryptixException("encrypt #"+ i +" failed");

            cryptor.initDecrypt(userKey);
            tmp = cryptor.crypt(ct);
               if (!ArrayUtil.areEqual(pt, tmp))
                throw new CryptixException("decrypt #"+ i +" failed");
        }
if (DEBUG && debuglevel > 0) debug("Self-test OK");
    }

/*
    private static void
    time_test(PrintWriter out, String keyStr, String plainStr, String cipherStr)
    throws InvalidKeyException {

        byte key[] = Hex.fromString(keyStr);
        byte plain[] = Hex.fromString(plainStr);
        byte cipher[] = Hex.fromString(cipherStr);
        DES des;
        out.println();
        out.println();
        out.println("Starting time tests ...");
        out.println();
        out.println("Calculating the approximate speed ...");
        out.println();

        int count=10;
        long begin;
        double d;
        do {
            des = new DES(key);
            long i;
            count *= 2;
            begin = System.currentTimeMillis();
            for (i=count; i!=0; i--)
                des.des(plain, 0, cipher, 0, true);
            d = System.currentTimeMillis() - begin;
        } while (d < 3000);

        count *= 2;
        out.println("Doing "+count+" encryptions");

        out.println("Calculating the real speed ...");
        out.println();

        begin = System.currentTimeMillis();
        for (int i = count; i != 0; i--) {
            des = new DES(key);
            des.des(plain, 0, cipher, 0, true);
        }
        d = (System.currentTimeMillis() - begin)/1000;
        out.println(count + " encryptions in " + d + " seconds");

        d = (double) count / d;
        out.println("DES encryptions per sec = " + d);
    }
*/
}

/* The DES cipher (in ECB mode).
 *
 * Parts of this code Copyright (C) 1996 Geoffrey Keating. All rights reserved.
 *
 * Its use is FREE FOR COMMERCIAL AND NON-COMMERCIAL USE
 * as long as the following conditions are adhered to.
 * 
 * Copyright remains Geoffrey Keating's, and as such any Copyright notices in
 * the code are not to be removed.  If this code is used in a product,
 * Geoffrey Keating should be given attribution as the author of the parts used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    This product includes software developed by Geoffrey Keating
 *    (geoffk@discus.anu.edu.au)
 * 
 * THIS SOFTWARE IS PROVIDED BY GEOFFREY KEATING ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 */
/* Parts of this code (in particular, the string representing SP_TRANS)
 * are Copyright (C) 1995 Eric Young (eay@mincom.oz.au). All rights reserved.
 *
 * Its use is FREE FOR COMMERCIAL AND NON-COMMERCIAL USE
 * as long as the following conditions are adhered to.
 * 
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.  If this code is used in a product,
 * Eric Young should be given attribution as the author of the parts used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    This product includes software developed by Eric Young (eay@mincom.oz.au)
 * 
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
