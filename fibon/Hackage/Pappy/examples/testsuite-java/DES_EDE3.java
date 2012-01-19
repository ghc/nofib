// $Id: DES_EDE3.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: DES_EDE3.java,v $
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
// Revision 1.5  1997/12/22 03:57:38  hopwood
// + Cosmetics.
//
// Revision 1.4  1997/12/21 15:40:31  iang
// + Added an extra reference.
//
// Revision 1.3  1997/11/29 04:42:55  hopwood
// + Changes to engineUpdate method.
//
// Revision 1.2  1997/11/20 19:31:40  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.0  1997/08/21  David Hopwood
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
import cryptix.provider.key.RawSecretKey;

import java.io.PrintWriter;
import xjava.security.Cipher;
import java.security.Key;
import java.security.KeyException;
import java.security.InvalidKeyException;
import xjava.security.SymmetricCipher;

/**
 * This class implements Triple DES EDE encryption with three independent
 * keys. DES-EDE3 acts as a block cipher with an 8 byte block size.
 * <p>
 * The encoded form of the Triple DES key should be a 24-byte array,
 * consisting of three 8-byte single DES keys in order - K1, K2 and K3.
 * Encryption and decryption are done as follows:
 * <ul>
 *   <li> C = E<sub>K3</sub>(D<sub>K2</sub>(E<sub>K1</sub>(P)))
 *   <li> P = D<sub>K1</sub>(E<sub>K2</sub>(D<sub>K3</sub>(C)))
 * </ul>
 * <p>
 * The alternating encryption and decryption was designed by IBM to
 * enable compatibility with single DES, when all three keys are equal
 * (although it is now rare for Triple DES to be used in that way).
 * <p>
 * When DES-EDE3 is used with the CBC mode class (algorithm name
 * "DES-EDE3/CBC"), the result is Outer-CBC, and only one IV is used.
 * <p>
 * DES was written by IBM and first released in 1976. The algorithm is
 * freely usable for both single and triple encryption.
 * <p>
 * <b>References:</b>
 * <ol>
 *    <li> <a href="mailto:schneier@counterpane.com">Bruce Schneier</a>,
 *         "Chapter 12 Data Encryption Standard," and
 *         "Section 15.2 Triple Encryption,"
 *         <cite>Applied Cryptography, 2nd edition</cite>,
 *         John Wiley &amp; Sons, 1996
 *         <p>
 *    <li> R.C. Merkle and M. Hellman,
 *         "On the Security of Multiple Encryption,"
 *         <cite>Communications of the ACM</cite>,
 *         vol. 24 no. 7, 1981, pages 465-467.
 *         <p>
 *    <li> P Karn, P Metzger, W A Simpson
 *         "The ESP Triple DES Transform,"
 *         <cite>Internet Draft</cite>,
 *         draft-simpson-esp-des3-x-01.txt
 * </ol>
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 * @since   Cryptix 2.2.2
 */
public class DES_EDE3
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("DES-EDE3") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("DES-EDE3: " + s); }


// DES-EDE3 constants and variables
//............................................................................

    private static final int
        BLOCK_SIZE = 8,
        KEY_LENGTH = 24,
        DES_KEY_LENGTH = 8;

    private Cipher des1, des2, des3;


// Constructor
//............................................................................

    /**
     * Constructs a DES-EDE3 cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public DES_EDE3() {
        super(false, false, "Cryptix");
        des1 = new DES();
        des2 = new DES();
        des3 = new DES();
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
     * @exception KeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is invalid.
     *              </ul>
     */
    public void engineInitEncrypt (Key key)
    throws KeyException {
        Key[] keys = splitKey(key);

        des1.initEncrypt(keys[0]);
        des2.initDecrypt(keys[1]);
        des3.initEncrypt(keys[2]);
    }

    /**
     * <b>SPI</b>: Initializes this cipher for decryption, using the
     * specified key.
     *
     * @param  key  the key to use for decryption.
     * @exception KeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is invalid.
     *              </ul>
     */
    protected void engineInitDecrypt (Key key)
    throws KeyException {
        Key[] keys = splitKey(key);

        des1.initDecrypt(keys[2]);
        des2.initEncrypt(keys[1]);
        des3.initDecrypt(keys[0]);
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
    engineUpdate (byte[] in, int inOffset, int inLen, byte[] out, int outOffset) {
        byte[] temp = des1.update(in, inOffset, inLen);
        des2.update(temp, 0, temp.length, temp, 0);
        return des3.update(temp, 0, temp.length, out, outOffset);
    }


// Own methods
//............................................................................

    /**
     * Splits a Triple DES key into three single DES keys.
     *
     * @param  key  the key to be split.
     * @return a 3-element array containing the single DES keys, in the order
     *              in which they were encoded.
     * @exception InvalidKeyException if one of the following occurs: <ul>
     *                <li> key.getEncoded() == null;
     *                <li> The length of the user key array is invalid.
     *              </ul>
     */
    private Key[] splitKey(Key key) throws InvalidKeyException {
        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        if (userkey.length != KEY_LENGTH)
            throw new InvalidKeyException(getAlgorithm() + ": Invalid user key length");

        Key[] keys = new RawSecretKey[3];
        for (int i = 0; i < 3; i++)
            keys[i] = new RawSecretKey("DES", userkey, i*DES_KEY_LENGTH,
                                       DES_KEY_LENGTH);

        return keys;
    }
}
