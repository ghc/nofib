// $Id: DES2X.java,v 1.1 2002/09/05 19:27:05 baford Exp $
//
// $Log: DES2X.java,v $
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
// Revision 1.3  2000/08/17 11:40:51  edwin
// java.* -> xjava.*
//
// Revision 1.2  1998/05/31 14:03:17  kettler
// + Reordered the XOR keys so the getState() call is not used anymore.
//
// Revision 1.1  1998/05/27 22:05:02  kettler
// + Added DES2X another variant of using DES with 56+3*64 bit keys
//
//
// $Endlog$
/*
 * Copyright (c) 1998 Systemics Ltd
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
 * This class implements DES2X encryption with four independent
 * keys. DES2X acts as a block cipher with an 8 byte block size.
 * <p>
 * The encoded form of the DESX key should be a 32-byte array,
 * consisting of one 8-byte DES key K and three XOR keys K1, K2 and K3
 * in order - K, K1, K2 and K3.
 * Encryption and decryption are done as follows:
 * <ul>
 *   <li> C = E(E(P XOR K1) XOR K2) XOR K3
 *   <li> P = D(D(C XOR K3) XOR K2) XOR K1
 * </ul>
 * <p>
 * When DES2X is used with the CBC mode class (algorithm name
 * "DES2X/CBC"), the result is Outer-CBC, and only one IV is used.
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
 * <b>Copyright</b> &copy; 1998
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Sascha Kettler
 * @since   Cryptix 3.0.4
 */
public class DES2X
extends Cipher
implements SymmetricCipher
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final boolean DEBUG_SLOW = Debug.GLOBAL_DEBUG_SLOW;
    private static final int debuglevel = DEBUG ? Debug.getLevel("DESX") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("DESX: " + s); }


// DESX constants and variables
//............................................................................

    private static final int
        BLOCK_SIZE = 8,
        KEY_LENGTH = 32,
        SINGLE_KEY_LENGTH = 8;

    private Cipher des;

    private byte[] XORkey1;
    private byte[] XORkey2;
    private byte[] XORkey3;


// Constructor
//............................................................................

    /**
     * Constructs a DES2X cipher object, in the UNINITIALIZED state.
     * This calls the Cipher constructor with <i>implBuffering</i> false,
     * <i>implPadding</i> false and the provider set to "Cryptix".
     */
    public DES2X() {
        super(false, false, "Cryptix");
        des = new DES();
	XORkey1 = null;
	XORkey2 = null;
	XORkey3 = null;
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
        byte[][] keys = splitKey(key);
	RawSecretKey DESkey = new RawSecretKey("DES", keys[0]);

        des.initEncrypt(DESkey);

        XORkey1 = keys[1];
        XORkey2 = keys[2];
        XORkey3 = keys[3];
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
        byte[][] keys = splitKey(key);
	RawSecretKey DESkey = new RawSecretKey("DES", keys[0]);

        des.initDecrypt(DESkey);

        XORkey1 = keys[3];
        XORkey2 = keys[2];
        XORkey3 = keys[1];
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
        int i;

        byte[] temp = new byte[inLen];

        for (i=0; i<inLen; i++) {
          temp[i] = (byte) 
            (in[i+inOffset] ^ XORkey1[i % SINGLE_KEY_LENGTH]);
        }
	  
        des.update(temp, 0, inLen, temp, 0);
	  
        for (i=0; i<inLen; i++) {
          temp[i] ^= XORkey2[i % SINGLE_KEY_LENGTH];
        }
	  
        des.update(temp, 0, inLen, temp, 0);
	  
        for (i=0; i<inLen; i++) {
          out[i+outOffset] = (byte) 
            (temp[i] ^ XORkey3[i % SINGLE_KEY_LENGTH]);
        }

        return inLen;
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
    private byte[][] splitKey(Key key) throws InvalidKeyException {
        byte[] userkey = key.getEncoded();
        if (userkey == null)
            throw new InvalidKeyException(getAlgorithm() + ": Null user key");

        if (userkey.length != KEY_LENGTH)
            throw new InvalidKeyException(getAlgorithm() + ": Invalid user key length");

        byte[][] keys = new byte[4][SINGLE_KEY_LENGTH];
        for (int i = 0; i < 4; i++)
            System.arraycopy (userkey, i*SINGLE_KEY_LENGTH,
			      keys[i], 0, SINGLE_KEY_LENGTH);

        return keys;
    }
}
