// $Id: Blowfish.java,v 1.1 2002/09/05 19:27:05 baford Exp $
//
// $Log: Blowfish.java,v $
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
// Revision 1.8  2000/08/17 11:40:50  edwin
// java.* -> xjava.*
//
// Revision 1.7  1997/12/30 11:09:49  raif
// *** empty log message ***
//
// Revision 1.6.1  1997/12/30  raif
// + further performance optimisation based on Peter Hjelt (MXV)
//   (mxv@iterate.com) tip of unfolding the session key array
//   into individual native java type objects. The previous code
//   is still used when the number of rounds is different than
//   the default value (16).
// + added a DEFAULT_NOF_ROUNDS constant.
//
// Revision 1.6  1997/12/27 10:52:40  raif
// *** empty log message ***
//
// Revision 1.5.1  1997/12/27  raif
// + minor optimisations. TestBlowfish and Maker blowfish.mtest
//   run OK.
//
// Revision 1.5  1997/12/09 04:43:45  hopwood
// + Various.
//
// Revision 1.4  1997/11/29 04:42:55  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.3  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.2  1997/11/07 05:53:24  raif
// *** empty log message ***
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.3.2.1  1997/09/18  David Hopwood
// + Renamed "Rounds" parameter to "rounds".
//
// Revision 0.3.2.0  1997/08/15  David Hopwood
// + Removed all deprecated methods and fields. Cryptix 2.2 compatibility
//   is now handled by the separate cryptix.security.Blowfish class.
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
// Revision 0.3.1.3  1997/08/06  David Hopwood
// + Changed BLOCK_LENGTH back to BLOCK_SIZE in most places, leaving
//   BLOCK_LENGTH deprecated.
//
// Revision 0.3.1.2  1997/08/02  David Hopwood
// + Renamed BLOCK_SIZE to BLOCK_LENGTH, for consistency with DES.
// + Restored constant fields from Cryptix 2.2, but made them deprecated.
// + Changed minimum key length from 64 to 40 bits.
// + Make sure the value of rounds is stored in a local variable in the
//   encryption/decryption methods, for efficiency.
//
// Revision 0.3.1.1  1997/07/31  David Hopwood
// + Changed to make it easier to allow a variable number of rounds
//   in the native implementation (between 16 and 20 inclusive).
//   This is still disabled, by defining MAX_NOF_ROUNDS as 16,
//   until we have enough P0 data. BF_ROUNDS in blowfish.h and
//   the constants in bf_pi.h will also need to be changed for it
//   to work.
// + Fixed the size of the P array to depend on the maximum number
//   of rounds.
// + Removed all uses of state variable (it is no longer needed).
// + Made keyLength() public, since it is public in BlockCipher.
// + Required native code version is 2.3.
//
// Revision 0.3.1.0  1997/07/14  David Hopwood
// + Blowfish, DES, IDEA, and Square 3.1.0 are now at the same API
//   level and in the same style.
// + Fixed security bug (out-of-bounds read) introduced in 3.0.5 when
//   native buffer overflow check was moved.
// + Renamed outs variable in engineUpdate to temp, to avoid similarity
//   with out.
//
// Revision 0.3.0.5  1997/07/09  R. Naffah
// + This is now fully compliant w/ IJCE!
// + Tested OK w/ and w/o Blowfish.DLL.
// + Modified the self_test method to use IJCE constructs.
// + Removed local var. state since Blowfish's superclass, BlockCipher, now
//   extends java.security.Cipher.
// + Use renamed cryptix.Cryptix.
// + Moved native buffer overflow check in engineUpdate outside the
//   per-block loop for efficiency.
//
// Revision 0.3.0.4  1997/07/05  David Hopwood
// + Changed native_finalize to return a String, so that any errors can be
//   reported.
// + Made engineUpdate protected, not public.
// + Added check for buffer overflow when calling native code in engineUpdate.
// + Made setRounds throw an IllegalArgumentException if that number of rounds
//   is not supported.
//
// Revision 0.3.0.3  1997/07/04  David Hopwood
// + Removed redundant override of blockLength(), since it is defined in
//   CryptixCipher.
// + Added native_lock object to synchronize on. This fixes a
//   potential race condition where finalize() could be called by a
//   subclass during the execution of a native method, causing the memory
//   for the native key schedule to be freed while it is being used.
//
//   Note that a more straightforward attempted solution of making all
//   the native methods synchronized would not work -- there would
//   be a small window between getting the non-null cookie value in order
//   to pass it to a native method, and actually calling that method.
//   In this window, the native key schedule might be freed.
//
// Revision 0.3.0.2  1997/07/04  R. Naffah
// + Tested OK with and without blowfish.dll.
// + Modified the signatures of the native_crypt() and native_ks methods
//   to (a) improve performance, (b) parallel the native c-code (Eric Young's
//   Reference implementation) and (c) work around a probable JNI bug
//   that causes Unhandled Exception (in JAVAI.DLL) at run-time when making
//   a NewGlobalRef to the cookie on some platforms.
// + Modified debug() definition to always prepend the class name;
// + Moved the initial P and S values checking to static{} so it can
//   be still performed but only once. Saves time when generating
//   new/multiple keys;
// + Merged all current versions of the implementations into this paving
//   the way for Blowfish to be fully operable within the IJCE framework.
//
// Revision 0.3.0.1  1997/06/26  David Hopwood
// + Many changes (JCE, native linking, debugging, ...)
//
// Revision 0.3.0.0  1997/04/15  Systemics
// + Added Java code.  Links not considered/tested.
//
// Revision 0.2.5.1  1997/03/15  Jill Baker
// + Moved this file here from old namespace.
//
// Revision 0.2.5.0  1997/02/24  Original Author not stated
// + Original version.
//
// $Endlog$
/*
 * Ported to Java(tm) from the C-code reference implementation --part of
 * the SSL implementation 0.6.6-- written by Eric Young (eay@mincom.oz.au).
 *
 * Copyright (c) 1997 Systemics Ltd
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
import java.security.InvalidParameterException;
import xjava.security.InvalidParameterTypeException;
import java.security.Security;
import xjava.security.SymmetricCipher;

/**
 * This class implements the Blowfish block cipher.
 * <p>
 * Blowfish was designed by <a href="mailto:schneier@counterpane.com">Bruce
 * Schneier</a>. The algorithm is in the public domain.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Bruce Schneier,
 *        "Section 14.3 Blowfish,"
 *        <cite>Applied Cryptography, 2nd edition</cite>,
 *        John Wiley &amp; Sons, 1996
 *        <p>
 *   <li> Bruce Schneier,
 *        "Description of a New Variable-Length Key, 64-Bit Cipher (Blowfish),"
 *        <cite>Fast Software Encryption Cambridge Security Workshop Proceedings</cite>,
 *        Springer-Verlag, 1004, pp 191-204.
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
 * @author  Raif S. Naffah
 * @since   Cryptix 2.2
 */
public final class Blowfish // must be final for security reasons
extends Cipher
implements SymmetricCipher
{
// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("Blowfish") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("Blowfish: " + s); }


// Native library linking methods and vars.
//...........................................................................

    private static NativeLink linkStatus = new NativeLink("Blowfish", 2, 3);

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
     * @param  rounds   the number of rounds used.
     * @return an error String, or null if there was no error
     */
    private native String native_ks(long cookie, byte[] userKey, int rounds);

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
     *   <li> <code>16 <= rounds && rounds <= 20</code>
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
     * @param  rounds       the number of rounds used.
     * @return the number of bytes crypted (always BLOCK_SIZE) or 0 if an error
     *                      occurred.
     */
    private native int native_crypt(long cookie, byte[] in, int inOffset,
                                    byte[] out, int outOffset, boolean encrypt,
                                    int rounds);

    /**
     * Finalizes the native state for this object.
     *
     * @return a string if an error occurred or null otherwise.
     */
    private native String native_finalize();


// Blowfish constants and variables
//...........................................................................

    //
    // Using array initializers is inefficient in code size: each 32-bit entry
    // takes 8 bytes of code plus 5 bytes of constant pool. Consider encoding
    // it in a String, as DES does.
    //

    /** Cipher initialization data. */
    private static final int[]
        P0 = {
            0x243F6A88, 0x85A308D3, 0x13198A2E, 0x03707344,
            0xA4093822, 0x299F31D0, 0x082EFA98, 0xEC4E6C89,
            0x452821E6, 0x38D01377, 0xBE5466CF, 0x34E90C6C,
            0xC0AC29B7, 0xC97C50DD, 0x3F84D5B5, 0xB5470917,
            0x9216D5D9, 0x8979FB1B},
        S0 = {
            0xD1310BA6, 0x98DFB5AC, 0x2FFD72DB, 0xD01ADFB7,
            0xB8E1AFED, 0x6A267E96, 0xBA7C9045, 0xF12C7F99,
            0x24A19947, 0xB3916CF7, 0x0801F2E2, 0x858EFC16,
            0x636920D8, 0x71574E69, 0xA458FEA3, 0xF4933D7E,
            0x0D95748F, 0x728EB658, 0x718BCD58, 0x82154AEE,
            0x7B54A41D, 0xC25A59B5, 0x9C30D539, 0x2AF26013,
            0xC5D1B023, 0x286085F0, 0xCA417918, 0xB8DB38EF,
            0x8E79DCB0, 0x603A180E, 0x6C9E0E8B, 0xB01E8A3E,
            0xD71577C1, 0xBD314B27, 0x78AF2FDA, 0x55605C60,
            0xE65525F3, 0xAA55AB94, 0x57489862, 0x63E81440,
            0x55CA396A, 0x2AAB10B6, 0xB4CC5C34, 0x1141E8CE,
            0xA15486AF, 0x7C72E993, 0xB3EE1411, 0x636FBC2A,
            0x2BA9C55D, 0x741831F6, 0xCE5C3E16, 0x9B87931E,
            0xAFD6BA33, 0x6C24CF5C, 0x7A325381, 0x28958677,
            0x3B8F4898, 0x6B4BB9AF, 0xC4BFE81B, 0x66282193,
            0x61D809CC, 0xFB21A991, 0x487CAC60, 0x5DEC8032,
            0xEF845D5D, 0xE98575B1, 0xDC262302, 0xEB651B88,
            0x23893E81, 0xD396ACC5, 0x0F6D6FF3, 0x83F44239,
            0x2E0B4482, 0xA4842004, 0x69C8F04A, 0x9E1F9B5E,
            0x21C66842, 0xF6E96C9A, 0x670C9C61, 0xABD388F0,
            0x6A51A0D2, 0xD8542F68, 0x960FA728, 0xAB5133A3,
            0x6EEF0B6C, 0x137A3BE4, 0xBA3BF050, 0x7EFB2A98,
            0xA1F1651D, 0x39AF0176, 0x66CA593E, 0x82430E88,
            0x8CEE8619, 0x456F9FB4, 0x7D84A5C3, 0x3B8B5EBE,
            0xE06F75D8, 0x85C12073, 0x401A449F, 0x56C16AA6,
            0x4ED3AA62, 0x363F7706, 0x1BFEDF72, 0x429B023D,
            0x37D0D724, 0xD00A1248, 0xDB0FEAD3, 0x49F1C09B,
            0x075372C9, 0x80991B7B, 0x25D479D8, 0xF6E8DEF7,
            0xE3FE501A, 0xB6794C3B, 0x976CE0BD, 0x04C006BA,
            0xC1A94FB6, 0x409F60C4, 0x5E5C9EC2, 0x196A2463,
            0x68FB6FAF, 0x3E6C53B5, 0x1339B2EB, 0x3B52EC6F,
            0x6DFC511F, 0x9B30952C, 0xCC814544, 0xAF5EBD09,
            0xBEE3D004, 0xDE334AFD, 0x660F2807, 0x192E4BB3,
            0xC0CBA857, 0x45C8740F, 0xD20B5F39, 0xB9D3FBDB,
            0x5579C0BD, 0x1A60320A, 0xD6A100C6, 0x402C7279,
            0x679F25FE, 0xFB1FA3CC, 0x8EA5E9F8, 0xDB3222F8,
            0x3C7516DF, 0xFD616B15, 0x2F501EC8, 0xAD0552AB,
            0x323DB5FA, 0xFD238760, 0x53317B48, 0x3E00DF82,
            0x9E5C57BB, 0xCA6F8CA0, 0x1A87562E, 0xDF1769DB,
            0xD542A8F6, 0x287EFFC3, 0xAC6732C6, 0x8C4F5573,
            0x695B27B0, 0xBBCA58C8, 0xE1FFA35D, 0xB8F011A0,
            0x10FA3D98, 0xFD2183B8, 0x4AFCB56C, 0x2DD1D35B,
            0x9A53E479, 0xB6F84565, 0xD28E49BC, 0x4BFB9790,
            0xE1DDF2DA, 0xA4CB7E33, 0x62FB1341, 0xCEE4C6E8,
            0xEF20CADA, 0x36774C01, 0xD07E9EFE, 0x2BF11FB4,
            0x95DBDA4D, 0xAE909198, 0xEAAD8E71, 0x6B93D5A0,
            0xD08ED1D0, 0xAFC725E0, 0x8E3C5B2F, 0x8E7594B7,
            0x8FF6E2FB, 0xF2122B64, 0x8888B812, 0x900DF01C,
            0x4FAD5EA0, 0x688FC31C, 0xD1CFF191, 0xB3A8C1AD,
            0x2F2F2218, 0xBE0E1777, 0xEA752DFE, 0x8B021FA1,
            0xE5A0CC0F, 0xB56F74E8, 0x18ACF3D6, 0xCE89E299,
            0xB4A84FE0, 0xFD13E0B7, 0x7CC43B81, 0xD2ADA8D9,
            0x165FA266, 0x80957705, 0x93CC7314, 0x211A1477,
            0xE6AD2065, 0x77B5FA86, 0xC75442F5, 0xFB9D35CF,
            0xEBCDAF0C, 0x7B3E89A0, 0xD6411BD3, 0xAE1E7E49,
            0x00250E2D, 0x2071B35E, 0x226800BB, 0x57B8E0AF,
            0x2464369B, 0xF009B91E, 0x5563911D, 0x59DFA6AA,
            0x78C14389, 0xD95A537F, 0x207D5BA2, 0x02E5B9C5,
            0x83260376, 0x6295CFA9, 0x11C81968, 0x4E734A41,
            0xB3472DCA, 0x7B14A94A, 0x1B510052, 0x9A532915,
            0xD60F573F, 0xBC9BC6E4, 0x2B60A476, 0x81E67400,
            0x08BA6FB5, 0x571BE91F, 0xF296EC6B, 0x2A0DD915,
            0xB6636521, 0xE7B9F9B6, 0xFF34052E, 0xC5855664,
            0x53B02D5D, 0xA99F8FA1, 0x08BA4799, 0x6E85076A},
        S1 = {
            0x4B7A70E9, 0xB5B32944, 0xDB75092E, 0xC4192623,
            0xAD6EA6B0, 0x49A7DF7D, 0x9CEE60B8, 0x8FEDB266,
            0xECAA8C71, 0x699A17FF, 0x5664526C, 0xC2B19EE1,
            0x193602A5, 0x75094C29, 0xA0591340, 0xE4183A3E,
            0x3F54989A, 0x5B429D65, 0x6B8FE4D6, 0x99F73FD6,
            0xA1D29C07, 0xEFE830F5, 0x4D2D38E6, 0xF0255DC1,
            0x4CDD2086, 0x8470EB26, 0x6382E9C6, 0x021ECC5E,
            0x09686B3F, 0x3EBAEFC9, 0x3C971814, 0x6B6A70A1,
            0x687F3584, 0x52A0E286, 0xB79C5305, 0xAA500737,
            0x3E07841C, 0x7FDEAE5C, 0x8E7D44EC, 0x5716F2B8,
            0xB03ADA37, 0xF0500C0D, 0xF01C1F04, 0x0200B3FF,
            0xAE0CF51A, 0x3CB574B2, 0x25837A58, 0xDC0921BD,
            0xD19113F9, 0x7CA92FF6, 0x94324773, 0x22F54701,
            0x3AE5E581, 0x37C2DADC, 0xC8B57634, 0x9AF3DDA7,
            0xA9446146, 0x0FD0030E, 0xECC8C73E, 0xA4751E41,
            0xE238CD99, 0x3BEA0E2F, 0x3280BBA1, 0x183EB331,
            0x4E548B38, 0x4F6DB908, 0x6F420D03, 0xF60A04BF,
            0x2CB81290, 0x24977C79, 0x5679B072, 0xBCAF89AF,
            0xDE9A771F, 0xD9930810, 0xB38BAE12, 0xDCCF3F2E,
            0x5512721F, 0x2E6B7124, 0x501ADDE6, 0x9F84CD87,
            0x7A584718, 0x7408DA17, 0xBC9F9ABC, 0xE94B7D8C,
            0xEC7AEC3A, 0xDB851DFA, 0x63094366, 0xC464C3D2,
            0xEF1C1847, 0x3215D908, 0xDD433B37, 0x24C2BA16,
            0x12A14D43, 0x2A65C451, 0x50940002, 0x133AE4DD,
            0x71DFF89E, 0x10314E55, 0x81AC77D6, 0x5F11199B,
            0x043556F1, 0xD7A3C76B, 0x3C11183B, 0x5924A509,
            0xF28FE6ED, 0x97F1FBFA, 0x9EBABF2C, 0x1E153C6E,
            0x86E34570, 0xEAE96FB1, 0x860E5E0A, 0x5A3E2AB3,
            0x771FE71C, 0x4E3D06FA, 0x2965DCB9, 0x99E71D0F,
            0x803E89D6, 0x5266C825, 0x2E4CC978, 0x9C10B36A,
            0xC6150EBA, 0x94E2EA78, 0xA5FC3C53, 0x1E0A2DF4,
            0xF2F74EA7, 0x361D2B3D, 0x1939260F, 0x19C27960,
            0x5223A708, 0xF71312B6, 0xEBADFE6E, 0xEAC31F66,
            0xE3BC4595, 0xA67BC883, 0xB17F37D1, 0x018CFF28,
            0xC332DDEF, 0xBE6C5AA5, 0x65582185, 0x68AB9802,
            0xEECEA50F, 0xDB2F953B, 0x2AEF7DAD, 0x5B6E2F84,
            0x1521B628, 0x29076170, 0xECDD4775, 0x619F1510,
            0x13CCA830, 0xEB61BD96, 0x0334FE1E, 0xAA0363CF,
            0xB5735C90, 0x4C70A239, 0xD59E9E0B, 0xCBAADE14,
            0xEECC86BC, 0x60622CA7, 0x9CAB5CAB, 0xB2F3846E,
            0x648B1EAF, 0x19BDF0CA, 0xA02369B9, 0x655ABB50,
            0x40685A32, 0x3C2AB4B3, 0x319EE9D5, 0xC021B8F7,
            0x9B540B19, 0x875FA099, 0x95F7997E, 0x623D7DA8,
            0xF837889A, 0x97E32D77, 0x11ED935F, 0x16681281,
            0x0E358829, 0xC7E61FD6, 0x96DEDFA1, 0x7858BA99,
            0x57F584A5, 0x1B227263, 0x9B83C3FF, 0x1AC24696,
            0xCDB30AEB, 0x532E3054, 0x8FD948E4, 0x6DBC3128,
            0x58EBF2EF, 0x34C6FFEA, 0xFE28ED61, 0xEE7C3C73,
            0x5D4A14D9, 0xE864B7E3, 0x42105D14, 0x203E13E0,
            0x45EEE2B6, 0xA3AAABEA, 0xDB6C4F15, 0xFACB4FD0,
            0xC742F442, 0xEF6ABBB5, 0x654F3B1D, 0x41CD2105,
            0xD81E799E, 0x86854DC7, 0xE44B476A, 0x3D816250,
            0xCF62A1F2, 0x5B8D2646, 0xFC8883A0, 0xC1C7B6A3,
            0x7F1524C3, 0x69CB7492, 0x47848A0B, 0x5692B285,
            0x095BBF00, 0xAD19489D, 0x1462B174, 0x23820E00,
            0x58428D2A, 0x0C55F5EA, 0x1DADF43E, 0x233F7061,
            0x3372F092, 0x8D937E41, 0xD65FECF1, 0x6C223BDB,
            0x7CDE3759, 0xCBEE7460, 0x4085F2A7, 0xCE77326E,
            0xA6078084, 0x19F8509E, 0xE8EFD855, 0x61D99735,
            0xA969A7AA, 0xC50C06C2, 0x5A04ABFC, 0x800BCADC,
            0x9E447A2E, 0xC3453484, 0xFDD56705, 0x0E1E9EC9,
            0xDB73DBD3, 0x105588CD, 0x675FDA79, 0xE3674340,
            0xC5C43465, 0x713E38D8, 0x3D28F89E, 0xF16DFF20,
            0x153E21E7, 0x8FB03D4A, 0xE6E39F2B, 0xDB83ADF7},
        S2 = {
            0xE93D5A68, 0x948140F7, 0xF64C261C, 0x94692934,
            0x411520F7, 0x7602D4F7, 0xBCF46B2E, 0xD4A20068,
            0xD4082471, 0x3320F46A, 0x43B7D4B7, 0x500061AF,
            0x1E39F62E, 0x97244546, 0x14214F74, 0xBF8B8840,
            0x4D95FC1D, 0x96B591AF, 0x70F4DDD3, 0x66A02F45,
            0xBFBC09EC, 0x03BD9785, 0x7FAC6DD0, 0x31CB8504,
            0x96EB27B3, 0x55FD3941, 0xDA2547E6, 0xABCA0A9A,
            0x28507825, 0x530429F4, 0x0A2C86DA, 0xE9B66DFB,
            0x68DC1462, 0xD7486900, 0x680EC0A4, 0x27A18DEE,
            0x4F3FFEA2, 0xE887AD8C, 0xB58CE006, 0x7AF4D6B6,
            0xAACE1E7C, 0xD3375FEC, 0xCE78A399, 0x406B2A42,
            0x20FE9E35, 0xD9F385B9, 0xEE39D7AB, 0x3B124E8B,
            0x1DC9FAF7, 0x4B6D1856, 0x26A36631, 0xEAE397B2,
            0x3A6EFA74, 0xDD5B4332, 0x6841E7F7, 0xCA7820FB,
            0xFB0AF54E, 0xD8FEB397, 0x454056AC, 0xBA489527,
            0x55533A3A, 0x20838D87, 0xFE6BA9B7, 0xD096954B,
            0x55A867BC, 0xA1159A58, 0xCCA92963, 0x99E1DB33,
            0xA62A4A56, 0x3F3125F9, 0x5EF47E1C, 0x9029317C,
            0xFDF8E802, 0x04272F70, 0x80BB155C, 0x05282CE3,
            0x95C11548, 0xE4C66D22, 0x48C1133F, 0xC70F86DC,
            0x07F9C9EE, 0x41041F0F, 0x404779A4, 0x5D886E17,
            0x325F51EB, 0xD59BC0D1, 0xF2BCC18F, 0x41113564,
            0x257B7834, 0x602A9C60, 0xDFF8E8A3, 0x1F636C1B,
            0x0E12B4C2, 0x02E1329E, 0xAF664FD1, 0xCAD18115,
            0x6B2395E0, 0x333E92E1, 0x3B240B62, 0xEEBEB922,
            0x85B2A20E, 0xE6BA0D99, 0xDE720C8C, 0x2DA2F728,
            0xD0127845, 0x95B794FD, 0x647D0862, 0xE7CCF5F0,
            0x5449A36F, 0x877D48FA, 0xC39DFD27, 0xF33E8D1E,
            0x0A476341, 0x992EFF74, 0x3A6F6EAB, 0xF4F8FD37,
            0xA812DC60, 0xA1EBDDF8, 0x991BE14C, 0xDB6E6B0D,
            0xC67B5510, 0x6D672C37, 0x2765D43B, 0xDCD0E804,
            0xF1290DC7, 0xCC00FFA3, 0xB5390F92, 0x690FED0B,
            0x667B9FFB, 0xCEDB7D9C, 0xA091CF0B, 0xD9155EA3,
            0xBB132F88, 0x515BAD24, 0x7B9479BF, 0x763BD6EB,
            0x37392EB3, 0xCC115979, 0x8026E297, 0xF42E312D,
            0x6842ADA7, 0xC66A2B3B, 0x12754CCC, 0x782EF11C,
            0x6A124237, 0xB79251E7, 0x06A1BBE6, 0x4BFB6350,
            0x1A6B1018, 0x11CAEDFA, 0x3D25BDD8, 0xE2E1C3C9,
            0x44421659, 0x0A121386, 0xD90CEC6E, 0xD5ABEA2A,
            0x64AF674E, 0xDA86A85F, 0xBEBFE988, 0x64E4C3FE,
            0x9DBC8057, 0xF0F7C086, 0x60787BF8, 0x6003604D,
            0xD1FD8346, 0xF6381FB0, 0x7745AE04, 0xD736FCCC,
            0x83426B33, 0xF01EAB71, 0xB0804187, 0x3C005E5F,
            0x77A057BE, 0xBDE8AE24, 0x55464299, 0xBF582E61,
            0x4E58F48F, 0xF2DDFDA2, 0xF474EF38, 0x8789BDC2,
            0x5366F9C3, 0xC8B38E74, 0xB475F255, 0x46FCD9B9,
            0x7AEB2661, 0x8B1DDF84, 0x846A0E79, 0x915F95E2,
            0x466E598E, 0x20B45770, 0x8CD55591, 0xC902DE4C,
            0xB90BACE1, 0xBB8205D0, 0x11A86248, 0x7574A99E,
            0xB77F19B6, 0xE0A9DC09, 0x662D09A1, 0xC4324633,
            0xE85A1F02, 0x09F0BE8C, 0x4A99A025, 0x1D6EFE10,
            0x1AB93D1D, 0x0BA5A4DF, 0xA186F20F, 0x2868F169,
            0xDCB7DA83, 0x573906FE, 0xA1E2CE9B, 0x4FCD7F52,
            0x50115E01, 0xA70683FA, 0xA002B5C4, 0x0DE6D027,
            0x9AF88C27, 0x773F8641, 0xC3604C06, 0x61A806B5,
            0xF0177A28, 0xC0F586E0, 0x006058AA, 0x30DC7D62,
            0x11E69ED7, 0x2338EA63, 0x53C2DD94, 0xC2C21634,
            0xBBCBEE56, 0x90BCB6DE, 0xEBFC7DA1, 0xCE591D76,
            0x6F05E409, 0x4B7C0188, 0x39720A3D, 0x7C927C24,
            0x86E3725F, 0x724D9DB9, 0x1AC15BB4, 0xD39EB8FC,
            0xED545578, 0x08FCA5B5, 0xD83D7CD3, 0x4DAD0FC4,
            0x1E50EF5E, 0xB161E6F8, 0xA28514D9, 0x6C51133C,
            0x6FD5C7E7, 0x56E14EC4, 0x362ABFCE, 0xDDC6C837,
            0xD79A3234, 0x92638212, 0x670EFA8E, 0x406000E0},
        S3 = {
            0x3A39CE37, 0xD3FAF5CF, 0xABC27737, 0x5AC52D1B,
            0x5CB0679E, 0x4FA33742, 0xD3822740, 0x99BC9BBE,
            0xD5118E9D, 0xBF0F7315, 0xD62D1C7E, 0xC700C47B,
            0xB78C1B6B, 0x21A19045, 0xB26EB1BE, 0x6A366EB4,
            0x5748AB2F, 0xBC946E79, 0xC6A376D2, 0x6549C2C8,
            0x530FF8EE, 0x468DDE7D, 0xD5730A1D, 0x4CD04DC6,
            0x2939BBDB, 0xA9BA4650, 0xAC9526E8, 0xBE5EE304,
            0xA1FAD5F0, 0x6A2D519A, 0x63EF8CE2, 0x9A86EE22,
            0xC089C2B8, 0x43242EF6, 0xA51E03AA, 0x9CF2D0A4,
            0x83C061BA, 0x9BE96A4D, 0x8FE51550, 0xBA645BD6,
            0x2826A2F9, 0xA73A3AE1, 0x4BA99586, 0xEF5562E9,
            0xC72FEFD3, 0xF752F7DA, 0x3F046F69, 0x77FA0A59,
            0x80E4A915, 0x87B08601, 0x9B09E6AD, 0x3B3EE593,
            0xE990FD5A, 0x9E34D797, 0x2CF0B7D9, 0x022B8B51,
            0x96D5AC3A, 0x017DA67D, 0xD1CF3ED6, 0x7C7D2D28,
            0x1F9F25CF, 0xADF2B89B, 0x5AD6B472, 0x5A88F54C,
            0xE029AC71, 0xE019A5E6, 0x47B0ACFD, 0xED93FA9B,
            0xE8D3C48D, 0x283B57CC, 0xF8D56629, 0x79132E28,
            0x785F0191, 0xED756055, 0xF7960E44, 0xE3D35E8C,
            0x15056DD4, 0x88F46DBA, 0x03A16125, 0x0564F0BD,
            0xC3EB9E15, 0x3C9057A2, 0x97271AEC, 0xA93A072A,
            0x1B3F6D9B, 0x1E6321F5, 0xF59C66FB, 0x26DCF319,
            0x7533D928, 0xB155FDF5, 0x03563482, 0x8ABA3CBB,
            0x28517711, 0xC20AD9F8, 0xABCC5167, 0xCCAD925F,
            0x4DE81751, 0x3830DC8E, 0x379D5862, 0x9320F991,
            0xEA7A90C2, 0xFB3E7BCE, 0x5121CE64, 0x774FBE32,
            0xA8B6E37E, 0xC3293D46, 0x48DE5369, 0x6413E680,
            0xA2AE0810, 0xDD6DB224, 0x69852DFD, 0x09072166,
            0xB39A460A, 0x6445C0DD, 0x586CDECF, 0x1C20C8AE,
            0x5BBEF7DD, 0x1B588D40, 0xCCD2017F, 0x6BB4E3BB,
            0xDDA26A7E, 0x3A59FF45, 0x3E350A44, 0xBCB4CDD5,
            0x72EACEA8, 0xFA6484BB, 0x8D6612AE, 0xBF3C6F47,
            0xD29BE463, 0x542F5D9E, 0xAEC2771B, 0xF64E6370,
            0x740E0D8D, 0xE75B1357, 0xF8721671, 0xAF537D5D,
            0x4040CB08, 0x4EB4E2CC, 0x34D2466A, 0x0115AF84,
            0xE1B00428, 0x95983A1D, 0x06B89FB4, 0xCE6EA048,
            0x6F3F3B82, 0x3520AB82, 0x011A1D4B, 0x277227F8,
            0x611560B1, 0xE7933FDC, 0xBB3A792B, 0x344525BD,
            0xA08839E1, 0x51CE794B, 0x2F32C9B7, 0xA01FBAC9,
            0xE01CC87E, 0xBCC7D1F6, 0xCF0111C3, 0xA1E8AAC7,
            0x1A908749, 0xD44FBD9A, 0xD0DADECB, 0xD50ADA38,
            0x0339C32A, 0xC6913667, 0x8DF9317C, 0xE0B12B4F,
            0xF79E59B7, 0x43F5BB3A, 0xF2D519FF, 0x27D9459C,
            0xBF97222C, 0x15E6FC2A, 0x0F91FC71, 0x9B941525,
            0xFAE59361, 0xCEB69CEB, 0xC2A86459, 0x12BAA8D1,
            0xB6C1075E, 0xE3056A0C, 0x10D25065, 0xCB03A442,
            0xE0EC6E0E, 0x1698DB3B, 0x4C98A0BE, 0x3278E964,
            0x9F1F9532, 0xE0D392DF, 0xD3A0342B, 0x8971F21E,
            0x1B0A7441, 0x4BA3348C, 0xC5BE7120, 0xC37632D8,
            0xDF359F8D, 0x9B992F2E, 0xE60B6F47, 0x0FE3F11D,
            0xE54CDA54, 0x1EDAD891, 0xCE6279CF, 0xCD3E7E6F,
            0x1618B166, 0xFD2C1D05, 0x848FD2C5, 0xF6FB2299,
            0xF523F357, 0xA6327623, 0x93A83531, 0x56CCCD02,
            0xACF08162, 0x5A75EBB5, 0x6E163697, 0x88D273CC,
            0xDE966292, 0x81B949D0, 0x4C50901B, 0x71C65614,
            0xE6C6C7BD, 0x327A140A, 0x45E1D006, 0xC3F27B9A,
            0xC9AA53FD, 0x62A80F00, 0xBB25BFE2, 0x35BDD2F6,
            0x71126905, 0xB2040222, 0xB6CBCF7C, 0xCD769C2B,
            0x53113EC0, 0x1640E3D3, 0x38ABBD60, 0x2547ADF0,
            0xBA38209C, 0xF746CE76, 0x77AFA1C5, 0x20756060,
            0x85CBFE4E, 0x8AE88DD8, 0x7AAAF9B0, 0x4CF9AA7E,
            0x1948C25C, 0x02FB8A8C, 0x01C36AE4, 0xD6EBE1F9,
            0x90D4F869, 0xA65CDEA0, 0x3F09252D, 0xC208E69F,
            0xB74E6132, 0xCE77E25B, 0x578FDFE3, 0x3AC372E6};

    private static final int
        DEFAULT_NOF_ROUNDS = 16,        // default value
        MIN_NOF_ROUNDS = 16,            // this is also the default value
        MAX_NOF_ROUNDS = 16,            // should be 20, but we don't have enough P0 data
        BLOCK_SIZE = 8,                 // Blowfish block size in bytes
        MIN_USER_KEY_LENGTH =  40 / 8,  // given in bytes from a value in bits
        MAX_USER_KEY_LENGTH = 448 / 8;  // given in bytes from a value in bits

    /**
     * The current number of rounds. May get changed if a 'rounds'
     * property is set in the security provider properties file.
     * Defaults to DEFAULT_NOF_ROUNDS; ie. 16.
     */
    private int rounds = DEFAULT_NOF_ROUNDS;

    /**
     * This Blowfish instance's P-array placeholder. Actual size depends
     * on the maximum number of rounds.
     */
    private int[] P = new int[MAX_NOF_ROUNDS + 2];

    /**
     * Individual ints representing the session key. Based on 16 rounds
     * Blowfish; ie. the current supported number of rounds which is also
     * the default value.
     */
    private int
        K0, K1,  K2,  K3,  K4,  K5,  K6,  K7,  K8,
        K9, K10, K11, K12, K13, K14, K15, K16, K17;
    
    /** This instance's S-boxes data. */
    private int[] S = new int[1024];


// Static code
//...........................................................................

    /** Set to true iff CRC passes for initial S and P0 values. */
    private static final boolean OK;    // blank final

    static {
        // compute OK

        int crc = 0;
        int i;

        //
        // Step 1.
        // Walk through initial values P0.
        //
        for (i = 0; i < MAX_NOF_ROUNDS + 2; i++)
            crc = (crc << 1 | crc >>> 31) + P0[i];

        //
        // ...and the Si (s-boxes).
        //
        for (i = 0; i < 256; i++) {
            crc *= 13;
            crc = (crc << 11 | crc >>> 21) + S0[i];
        }
        for (i = 0; i < 256; i++) {
            crc *= 13;
            crc = (crc << 11 | crc >>> 21) + S1[i];
        }
        for (i = 0; i < 256; i++) {
            crc *= 13;
            crc = (crc << 11 | crc >>> 21) + S2[i];
        }
        for (i = 0; i < 256; i++) {
            crc *= 13;
            crc = (crc << 11 | crc >>> 21) + S3[i];
        }

if (DEBUG && debuglevel >= 4) debug("crc = " + Hex.intToString(crc));

        //
        // This number is a check for the S and P boxes.
        //
        OK = (crc == 0x55861A61);
if (DEBUG && debuglevel >= 3) debug("Good CRC on initial P and S boxes? " + OK);
    }


// Constructor, finalizer, and clone()
//...........................................................................

    /**
     * Constructs a Blowfish cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     * <p>
     * We define a "rounds" property to allow running the algorithm in a number
     * of rounds different than the default value; i.e. 16.
     * Note however that there is not enough defined initial data for any
     * number of rounds other than 16 for the time being.
     *
     * @exception CryptixException if a CRC check fails on the initial
     *            values of the S and P boxes.
     */
    public Blowfish() {
        super(false, false, "Cryptix");
        
        if (! OK) throw new CryptixException(
            getAlgorithm() + ": CRC failed on initial data");

        link();

        try {
            String ps = Security.getAlgorithmProperty("Blowfish", "rounds");
            if (ps != null) setRounds(Integer.parseInt(ps));
        } catch (Exception e) {
if (DEBUG && debuglevel > 0) debug("Could not set number of rounds");
        }
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
    protected int engineBlockSize() { return BLOCK_SIZE; }

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
     * @exception CryptixException if any one of the two self-tests fail.
     *              The two self-tests are as follows: <ol>
     *                <li> encrypt 0 ten times using the initial S and P boxes
     *                values. Check the result against a known value. If equal
     *                decrypt the result 10 times and compare it to 0;
     *                <li> complete the key expansion process and use the newly
     *                formed key to encrypt 10 times 0; decrypt the result 10
     *                times and compare.
     *              </ol>
     */
    protected void engineInitEncrypt(Key key)
    throws InvalidKeyException, CryptixException {
        makeKey(key);
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
     * @exception CryptixException if any one of the two self-tests fail.
     *              The two self-tests are as follows: <ol>
     *                <li> encrypt 0 ten times using the initial S and P boxes
     *                values. Check the result against a known value. If equal
     *                decrypt the result 10 times and compare it to 0;
     *                <li> complete the key expansion process and use the newly
     *                formed key to encrypt 10 times 0; decrypt the result 10
     *                times and compare.
     *              </ol>
     */
    protected void engineInitDecrypt (Key key)
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
                                          doEncrypt, rounds))
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
     * Blowfish has a single parameter, "rounds", which specifies the
     * number of rounds for this instance as a decimal String.
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
    throws InvalidParameterException, InvalidParameterTypeException {
        if (!(value instanceof String))
            throw new InvalidParameterTypeException(getAlgorithm() +
                ": value is not a String");
        try {
            if (param.equals("rounds"))
                setRounds(Integer.parseInt((String) value));
        }
        catch (Exception e) {
            throw new InvalidParameterException(e.toString());
        }
        throw new InvalidParameterException(getAlgorithm() + ": " + param);
    }

    /**
     * <b>SPI</b>: Gets the value of the specified algorithm parameter.
     * <p>
     * Blowfish has a single parameter, "rounds", which specifies the
     * number of rounds for this instance as a decimal String.
     *
     * @param  param    the string name of the parameter. 
     * @return the object that represents the parameter value, or null if there
     *                  is none.
     */
    protected Object engineGetParameter(String param) {
        if (param.equals("rounds")) return Integer.toString(rounds);
        return null;
    }


// Own methods
//...........................................................................

    /**
     * Sets the number of rounds for this cipher. Allowed only when this
     * cipher is in the UNINITIALIZED state; otherwise an exception is
     * thrown.
     * <p>
     * If the specified number is invalid, an IllegalArgumentException is
     * thrown.
     * <p>
     * Note that there is not enough defined initial data for any number
     * of rounds other than 16 for the time being.
     *
     * @param  r    the desired number of rounds for this cipher.
     * @throw IllegalStateException if this cipher is not uninitialised.
     * @exception IllegalArgumentException if the given number of rounds is
     *              not supported.
     */
    public void setRounds(int r) {
        if (getState() != UNINITIALIZED)
            throw new IllegalStateException(getAlgorithm() +
                ": Cipher not in UNINITIALIZED state");

        if (r < MIN_NOF_ROUNDS || r > MAX_NOF_ROUNDS)
            throw new IllegalArgumentException(getAlgorithm() +
                ": Invalid number of rounds");

        rounds = r;
    }

    /**
     * Returns the currently set number of rounds for this instance.
     *
     * @return the number of rounds.
     */
    public int getRounds() { return rounds; }

    /**
     * The normal entry to the encryption process. It is guaranteed
     * to be called with enough bytes in the input to carry on an
     * encryption of one full block.
     * <p>
     * The code of the Blowfish encryption engine, found here, is also
     * replicated in the BF_encrypt method found later. The reason for
     * this duplication is performance. This method outputs the result
     * in a byte array form, suitable for the user data encryption
     * operations, while BF_encrypt outputs its result as an int array
     * suitable for, and used during, the expansion of the user-key into
     * a Blowfish key schedule.
     *
     * @param  in       contains the input plaintext.
     * @param  off      index of in from which to start considering data.
     * @param  out      will contain the ciphertext block.
     * @param  outOff   index of out where the ciphertext will start.
     */
    private void blockEncrypt (byte[] in, int off, byte[] out, int outOff)
    {
        int L = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                (in[off++] & 0xFF);
        int R = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                (in[off  ] & 0xFF);

        if (rounds != DEFAULT_NOF_ROUNDS) {
            L ^= P[0];
            for (int i = 0; i < rounds; ) {
                R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ P[++i];
                L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ P[++i];
            }
            R ^= P[rounds + 1];
        } else {
            L ^= K0;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K1;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K2;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K3;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K4;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K5;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K6;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K7;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K8;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K9;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K10;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K11;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K12;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K13;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K14;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K15;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K16;
            R ^= K17;
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
     * The normal entry to the decryption process. It is guaranteed
     * to be called with enough bytes in the input to carry on a
     * decryption of one full block.
     * <p>
     * Because the Blowfish cipher engine is designed to handle two
     * 32-bit blocks, this method's purpose is to transform on entry
     * and exit the data to/from 32-bit blocks; i.e. Java ints.
     * <p>
     * The input becomes two 32-bit blocks as Left and Right halves
     * onto which the Blowfish cipher function is applied ROUNDS times
     * in reverse order to that of the encryption.
     *
     * @param  in       contains the input plaintext.
     * @param  off      index of in from which to start considering data.
     * @param  out      will contain the ciphertext block.
     * @param  outOff   index of out where the ciphertext will start.
     */
    private void blockDecrypt (byte[] in, int off, byte[] out, int outOff)
    {
        int L = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                (in[off++] & 0xFF);
        int R = (in[off++] & 0xFF) << 24 |
                (in[off++] & 0xFF) << 16 |
                (in[off++] & 0xFF) <<  8 |
                (in[off  ] & 0xFF);

        if (rounds != DEFAULT_NOF_ROUNDS) {
            L ^= P[rounds + 1];
            for (int i = rounds; i > 0; ) {
                R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ P[i--];
                L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ P[i--];
            }
            R ^= P[0];
        } else {
            L ^= K17;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K16;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K15;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K14;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K13;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K12;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K11;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K10;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K9;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K8;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K7;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K6;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K5;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K4;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K3;
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ K2;
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ K1;
            R ^= K0;
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
     * Expands a user-key to a working Blowfish P array (P) and S-box
     * data (S).
     * <p>
     * The key bytes are first extracted from the user-key and then
     * used, repetitively if need be, to build the contents of this
     * key schedule and S-box values.
     * <p>
     * The Blowfish algorithm uses a single key schedule for both encryption
     * and decryption. The process (key byte values and algorithm formulae)
     * are used in one direction during encryption and simply reversed
     * during decryption.
     *
     * @param  key  the user-key object to use.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> key.getEncoded().length is invalid.
     *              </ul>
     * @exception CryptixException if any one of the two self-tests fail.
     *              The two self-tests are as follows: <ol>
     *                <li> encrypt 0 ten times using the initial S and P boxes
     *                     values. Check the result against a known value. If equal
     *                     decrypt the result 10 times and compare it to 0;
     *                <li> complete the key expansion process and use the newly
     *                     formed key to encrypt 10 times 0; decrypt the result 10
     *                     times and compare.
     *              </ol>
     */
    private void makeKey (Key key)
    throws InvalidKeyException, CryptixException
    {
        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        int len = userkey.length;
        if (len < MIN_USER_KEY_LENGTH || len > MAX_USER_KEY_LENGTH)
            throw new InvalidKeyException(getAlgorithm() +
                ": Invalid user key length");

        // If native library available then use it. If not or if
        // native method returned error then revert to 100% Java.

        if (native_lock != null) {
            synchronized(native_lock) {
                try {
                    linkStatus.check(native_ks(native_cookie, userkey, rounds));
                    return;
                } catch (Error error) {
                    native_finalize();
                    native_lock = null;
if (DEBUG && debuglevel > 0) debug(error + ". Will use 100% Java.");
                }
            }
        }
        System.arraycopy(S0, 0, S,   0, 256);
        System.arraycopy(S1, 0, S, 256, 256);
        System.arraycopy(S2, 0, S, 512, 256);
        System.arraycopy(S3, 0, S, 768, 256);

        System.arraycopy(P0, 0, P, 0, rounds + 2);

        //
        // Self test #1
        // 1. Encrypt 0 ten times using initial S-Boxes and a zero-value key.
        //    Check result against pre-computed value.
        // 2. Decrypt it 10 times and check if == 0.
        //
        int[] block = {0, 0};
        int st = 0;

        for (st = 0; st < 10; st++)
            BF_encrypt(block[0], block[1], block, 0);

        if (block[0] != 0xAAFE4EBD || block[1] != 0x26D725FC)
            throw new CryptixException(getAlgorithm() +
                ": Self Test 1 failed: encrypt^10(0) = " + Hex.toString(block));

if (DEBUG && debuglevel >= 3) debug("Self Test 1 Good");

        while (--st >= 0)
            BF_decrypt(block[0], block[1], block, 0);

        if (block[0] != 0 || block[1] != 0)
            throw new CryptixException(getAlgorithm() +
                ": Self Test 1 failed: decrypt^10(encrypt^10(0)) = " +
                Hex.toString(block));

//        if (len > MAX_USER_KEY_LENGTH) len = MAX_USER_KEY_LENGTH;

        int ri;
        for (int i = 0, j = 0; i < rounds + 2; i++) {
            ri = 0;
            for (int k = 0; k < 4; k++) {
                ri = (ri << 8) | (userkey[j++] & 0xFF);
                j %= len;
            }
            P[i] ^= ri;
        }

        // use the former to effectively generate the P-array for this key
        BF_encrypt(0, 0, P, 0);
        for (int i = 2; i < rounds + 2; i += 2)
            BF_encrypt(P[i - 2], P[i - 1], P, i);

        // and the S-boxes
        BF_encrypt(P[rounds], P[rounds + 1], S, 0);
        for (int i = 2; i < 1024; i += 2)
            BF_encrypt(S[i - 2], S[i - 1], S, i);

        //
        // Self test #2
        // Encrypt 0 ten times.  Decrypt it ten times.  Should be the same.
        //
        for (st = 0; st < 10; st++)
            BF_encrypt(block[0], block[1], block, 0);

        while (--st >= 0)
            BF_decrypt(block[0], block[1], block, 0);

        if (block[0] != 0 || block[1] != 0)
            throw new CryptixException(getAlgorithm() +
                ": Self Test 2 failed: decrypt^10(encrypt^10(0)) = " +
                Hex.toString(block));

if (DEBUG && debuglevel >= 3) debug("Self Test 2 Good");

        if (rounds == DEFAULT_NOF_ROUNDS) {
            // unfold the P array into individual Ki's.
            K0 =  P[0];  K1 =  P[1];  K2 =  P[2];  K3 =  P[3];  K4 =  P[4];
            K5 =  P[5];  K6 =  P[6];  K7 =  P[7];  K8 =  P[8];  K9 =  P[9];
            K10 = P[10]; K11 = P[11]; K12 = P[12]; K13 = P[13]; K14 = P[14];
            K15 = P[15]; K16 = P[16]; K17 = P[17];
        }
    }

    /**
     * See description in <code>blockEncrypt</code> above.
     * <p>
     * This method is only called by the <code>makeKey</code> method to
     * generate the key schedule from user data. It outputs the result to an
     * int array.
     *
     * @param L         left half (32-bit) of the plain text block.
     * @param R         right half (32-bit) of the plain text block.
     * @param out       the int array where the result will be saved.
     * @param outOff    where the data starts in the byte array.
     * @see #blockEncrypt
     */
    private void BF_encrypt (int L, int R, int[] out, int outOff)
    {
        L ^= P[0];
        for (int i = 0; i < rounds; ) {
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ P[++i];
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ P[++i];
        }
        out[outOff++] = R ^ P[rounds + 1];
        out[outOff  ] = L;
    }

    /**
     * See description in <code>blockDecrypt</code> above.
     * <p>
     * This method is only called by the <code>makeKey</code> method during
     * self-test operation.
     *
     * @param L         left half (32-bit) of the ciphertext block,
     * @param R         right half (32-bit) of the ciphertext block.
     * @param out       the int array where the result will be saved.
     * @param outOff    where the data starts in the byte array.
     * @see #blockDecrypt
     */
    private void BF_decrypt (int L, int R, int[] out, int outOff)
    {
        L ^= P[rounds + 1];
        for (int i = rounds; i > 0; ) {
            R ^= (((S[(L >>> 24) & 0xFF] + S[256 + ((L >>> 16) & 0xFF)]) ^ S[512 + ((L >>>  8) & 0xFF)]) + S[768 + (L & 0xFF)]) ^ P[i--];
            L ^= (((S[(R >>> 24) & 0xFF] + S[256 + ((R >>> 16) & 0xFF)]) ^ S[512 + ((R >>>  8) & 0xFF)]) + S[768 + (R & 0xFF)]) ^ P[i--];
        }
        out[outOff++] = R ^ P[0];
        out[outOff  ] = L;
    }


// Test methods
//...........................................................................
//
// Don't expand this code please without thinking about it,
// much better to write a separate program.
//

    /** Entry point for very basic <code>self_test</code>. */
    public static void main(String[] args) {
        try { self_test(); }
        catch (Exception e) { e.printStackTrace(); }
    }

    /**
     * This is (apparently) the official certification data.
     * Use decimal as Java grumbles about hex values > 0x7F.
     */
    private static final byte[][][] tests = {
      { // cert 1
        { // key
         97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
         111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122
        },
        { 66, 76, 79, 87, 70, 73, 83, 72},  // plain
        { 50, 78, -48, -2, -12, 19, -94, 3} // cipher
      },
      { // cert 2
        {
          87, 104, 111, 32, 105, 115, 32, 74, 111, 104, 110, 32, 71, 97, 108,
          116, 63
        },
        { -2, -36, -70, -104, 118, 84, 50, 16},
        { -52, -111, 115, 43, -128, 34, -10, -124}
      },
      { // cert 3 - Ayn Rand with 4th () and 8th (d) chars hi bit set.
        { 65, 121, 110, -96, 82, 97, 110, -28}, // key
        { -2, -36, -70, -104, 118, 84, 50, 16}, // plain
        { -31, 19, -12, 16, 44, -4, -50, 67}    // cipher
      }
    };

    /**
     * Do some basic tests.
     * Three of the certification data are included only, no output,
     * success or exception.
     * If you want more, write a test program!
     *
     * @see cryptix.examples.TestBlowfish
     */
    private static void self_test()
    throws Exception {
        Cipher cryptor = Cipher.getInstance("Blowfish", "Cryptix");
        RawSecretKey userKey;
        byte[] tmp;

        for (int i = 0; i < tests.length; i++) {
            userKey = new RawSecretKey("Blowfish", tests[i][0]);

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
