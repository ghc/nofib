// $Id: UnixCrypt.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: UnixCrypt.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:04  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.5  2000/08/17 11:41:04  edwin
// java.* -> xjava.*
//
// Revision 1.4  1998/02/04 01:49:07  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1998/02/03  hopwood
// + Moved self-test to cryptix.test.TestUnixCrypt.
// + Made crypt synchronized.
//
// Revision 1.3  1998/01/10 04:42:45  hopwood
// + Committed changes below.
//
// Revision 1.2.1  1998/01/01  hopwood
// + Renamed selftest to self_test and made it private (to follow conventions
//   in other classes).
// + Fixed broken HTML comment.
//
// Revision 1.2  1998/01/08 00:45:36  iang
// + Added selftest(), called by no-args usage.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.5  1997/08/06  David Hopwood
// + Removed catch of InvalidKeyFormatException when creating DES key
//   (this can't happen any more).
//
// Revision 0.1.0.4  1997/08/02  David Hopwood
// + Merge with Raif Naffah's changes:
// + Use a local DES Cipher to do the actual password computation.
// + Use DESKeyGenerator object to handle user key (original).
// + Moved the crypt3 code proper to cryptix.security.DES as a public
//   method with same name.
// + Allow calling the class with 1 argument (original) defaulting
//   salt to "AA".
// + Removed the NullPointerException from the constructor.
// + Use new DES() and new DESKeyGenerator(), rather than getInstance.
// + Constructor and crypt method no longer throw Throwable.
//
// Revision 0.1.0.3  1997/07/27  David Hopwood
// + Renamed this class to UnixCrypt (from Crypt).
//
// Revision 0.1.0.2  1997/07/19  David Hopwood
// + Changed 'salt' to non-static fields salt0/salt1, and 'original' to
//   an argument passed to the crypt method. This prevents multiple uses
//   of Crypt from interfering with each other, making the class more
//   re-usable. It also potentially allows pre-computation for passwords
//   with a common salt, even though this is not currently done.
// + Mask each salt character with 0x7F.
// + Crypt no longer extends Thread.
// + Added documentation.
//
// Revision 0.1.0.1  1997/07/16  Raif Naffah
// + Use initialisation code from cryptix.security.DES class.
// + Formatting changes.
// + Renamed to cryptix.tools.Crypt.
//
// Revision 0.1.0.0  1997/?/0?  John F. Dumas
//   Initial version (named jcrypt.java).
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.tools;

import java.security.KeyException;
import xjava.security.InvalidKeyFormatException;

import cryptix.provider.cipher.DES;
import cryptix.provider.key.DESKeyGenerator;

/**
 * A Java-based implementation of the Unix crypt(3) function call, used
 * for hashing user passwords in many Unix dialects.
 * <p>
 * Based on C source code written by Eric Young (eay@psych.uq.oz.au).
 * <p>
 * The crypt(3) algorithm is <strong>not recommended</strong> for new
 * applications that require password hashing and do not need to be
 * compatible with Unix, because it has the following weaknesses:
 * <ul>
 *   <li> Only the first 8 characters of the password are significant.
 *        The rest is silently truncated. This may mislead the user
 *        into thinking that an uncrackable password has been chosen,
 *        even though the first 8 characters may be crackable.
 *   <li> Only the low 7 bits of the ASCII code of each character are
 *        used, which does not take advantage of additional entropy in
 *        non-US-ASCII passwords.
 *   <li> The salt has a total of 12 significant bits. This is not enough
 *        to prevent a massive precomputation attack, where a dictionary
 *        of common passwords is hashed using all 4096 salts, after which
 *        individual passwords from the dictionary can be cracked quickly.
 *   <li> The amount of computation needed is arguably not sufficient,
 *        taking into account improvements in processor speed since the
 *        algorithm was developed. A better approach would be to allow a
 *        variable number of iterations, with this number being stored
 *        with the salt.
 * </ul>
 * <p>
 * <b>Copyright</b> &copy; 1995-1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  John F. Dumas (jdumas@zgs.com)
 * @author  Raif Naffah
 * @author  David Hopwood
 * @since   Cryptix 2.2.2
 */
public class UnixCrypt
{

// Variables and constants
//...........................................................................

    private char salt0, salt1;

    private static final byte[] CON_SALT = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 
        0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 
        0x0A, 0x0B, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 
        0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 
        0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 
        0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 
        0x23, 0x24, 0x25, 0x20, 0x21, 0x22, 0x23, 0x24, 
        0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 
        0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34, 
        0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 
        0x3D, 0x3E, 0x3F, 0x00, 0x00, 0x00, 0x00, 0x00};

    private static final char[] COV_2CHAR = {
        0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 
        0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 
        0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 
        0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54, 
        0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x61, 0x62, 
        0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 
        0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 
        0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A};

    /**
     * A DES Cipher used to compute the password.
     */
    private DES des;

    /**
     * A DESKeyGenerator object.
     */
    private DESKeyGenerator dkg;


// Constructor
//...........................................................................

    /**
     * Constructs a UnixCrypt instance with the given salt value. If
     * needed this value is appended with enough A's.
     * <p>
     * If <code>salt</code> is null then "AA" is taken as the
     * salt value.
     *
     * @param salt  the salt value as a String
     */
    public UnixCrypt(String salt) {
        if (salt == null)
            salt = "";
        salt += "AA";
        salt0 = (char) (salt.charAt(0) & 0x7F);
        salt1 = (char) (salt.charAt(1) & 0x7F);

        // des = (DES) Cipher.getInstance("DES", "Cryptix");
        des = new DES();
        // dkg = (DESKeyGenerator) KeyGenerator.getInstance("DES", "Cryptix");
        dkg = new DESKeyGenerator();
    }


// Own methods
//...........................................................................

    /**
     * Processes <i>original</i> and the salt value passed in the constructor
     * using the crypt(3) algorithm, and returns the resulting hash as a
     * String.
     *
     * @param original  the plaintext password
     * @return the hashed password
     */
    public synchronized String crypt(String original) {
        int i, j;
 
        byte[] key = new byte[8];
        for (i = 0; i < key.length && i < original.length(); i++)
            key[i] = (byte)(original.charAt(i) << 1);

        dkg.setWeakAllowed(true); // weak keys allowed
        try {
            des.initEncrypt(dkg.generateKey(key));
        } catch (KeyException e) {
            throw new InternalError(e.toString());
        }

        int[] out =
            des.crypt3(CON_SALT[salt0] & 0xFF, (CON_SALT[salt1] & 0xFF) << 4);

        i = out[0];
        j = out[1];

        byte[] b = {
            (byte) i, (byte)(i >>> 8), (byte)(i >>> 16), (byte)(i >>> 24),
            (byte) j, (byte)(j >>> 8), (byte)(j >>> 16), (byte)(j >>> 24),
            0};
        int y = 0;
        int u = 0x80;
        int c;

        char[] buffer = new char[13];

        i = 0;
        buffer[i++] = salt0;
        buffer[i++] = salt1;
        while (i < buffer.length) {
            for (j = 0, c = 0; j < 6; j++) {
                c <<= 1;
                if (((int) b[y] & u) != 0)
                    c |= 1;
                u >>>= 1;
                if (u == 0) {
                    y++;
                    u = 0x80;
                }
            }
            buffer[i++] = COV_2CHAR[c];
        }
        return new String(buffer);
    }


// Main
//...........................................................................

    /**
     * Calculates the hash of a salt and password given on the command line.
     * <p>
     * Usage:
     * <pre>
     *    java cryptix.tools.UnixCrypt [&lt;salt&gt;] &lt;clear-password&gt;
     * </pre>
     */
    public static void main(String[] args) {
        String salt = null;
        String original;

        switch (args.length) {
            case 2:
                salt = args[0];
                original = args[1];
                break;

            case 1:
                salt = "";
                original = args[0];
                break;

            default:
                System.out.println(
                    "Usage:\n" +
                    "    java cryptix.tools.UnixCrypt [<salt>] <clear-password>");
                return;
        }
        try {
            UnixCrypt jc = new UnixCrypt(salt);
            System.out.print(
                "[" + (salt + "AA").substring(0, 2) + "] " +
                "[" + original + "] => ");
            System.out.println(
                "[" + jc.crypt(original) + "]");
        } catch (Exception e) {
            e.printStackTrace();
	    System.exit(1);
        }
    }
}
