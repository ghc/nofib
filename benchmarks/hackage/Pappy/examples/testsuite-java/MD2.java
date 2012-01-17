// This file is currently unlocked (change this line if you lock the file)
//
// $Log: MD2.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.4  1999/06/30 23:27:21  edwin
// In fixing the synchronized class (which isn't allowed) I made every method synchronized. According to iang this isn't necessairy so I'm removing all synchronized modifiers.
//
// Revision 1.3  1999/06/30 22:43:55  edwin
// classes are evil
//
// Revision 1.2  1998/01/05 03:41:19  iang
// Added references only.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.0  1997/07/14  R. Naffah
// + original version
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.md;


import java.security.MessageDigest;

/**
 * Implements the MD2 message digest algorithm in Java.
 * <p>
 * <b>References:</b>
 * <ol>
 *   <li> Burton S. Kaliski Jr,
 *        "<a href="http://rfc.fh-koeln.de/rfc/html/rfc1319.html">
 *        The MD2 Message-Digest Algorithm</a>",
 *        IETF RFC-1319 (informational).
 * </ol>
 *
 * <p><b>$Revision: 1.1 $</b>
 * @revision 1.0.0 --1997/08/14
 * @author   Raif S. Naffah
 */
public class MD2
extends MessageDigest
implements Cloneable
{
// MD2 specific object variables
//...........................................................................

    /**
     * The size in bytes of the input block to the tranformation algorithm.
     */
    private static final int BLOCK_LENGTH = 16;       //    = 128 / 8;
    
    /**
     * 16 8-bit entities also.
     */
    private int[] checksum = new int[BLOCK_LENGTH];
    
    /**
     * Number of bytes modulo 16.
     */
    private int count;
    
    /**
     * 128 bits input buffer = 16 x 8-bit entities holds until reaches 128 bits.
     */
    private int[] buffer = new int[BLOCK_LENGTH];
    
    /**
     * 3 * 128 bits work buffer = 3 * 16 x 8-bit entities. We'll use the
     * lower byte of each int. The first chunck of 128-bit is the context.
     */
    private int[] X = new int[3 * BLOCK_LENGTH];

    /**
     * Permutation of 0..255 constructed from the digits of pi. It gives a
     * 'random' nonlinear byte substitution operation.
     */
    private static final int[] S = {
     41,  46,  67, 201, 162, 216, 124,   1,  61,  54,  84, 161, 236, 240,   6,  19,
     98, 167,   5, 243, 192, 199, 115, 140, 152, 147,  43, 217, 188,  76, 130, 202,
     30, 155,  87,  60, 253, 212, 224,  22, 103,  66, 111,  24, 138,  23, 229,  18,
    190,  78, 196, 214, 218, 158, 222,  73, 160, 251, 245, 142, 187,  47, 238, 122,
    169, 104, 121, 145,  21, 178,   7,  63, 148, 194,  16, 137,  11,  34,  95,  33,
    128, 127,  93, 154,  90, 144,  50,  39,  53,  62, 204, 231, 191, 247, 151,   3,
    255,  25,  48, 179,  72, 165, 181, 209, 215,  94, 146,  42, 172,  86, 170, 198,
     79, 184,  56, 210, 150, 164, 125, 182, 118, 252, 107, 226, 156, 116,   4, 241,
     69, 157, 112,  89, 100, 113, 135,  32, 134,  91, 207, 101, 230,  45, 168,   2,
     27,  96,  37, 173, 174, 176, 185, 246,  28,  70,  97, 105,  52,  64, 126,  15,
     85,  71, 163,  35, 221,  81, 175,  58, 195,  92, 249, 206, 186, 197, 234,  38,
     44,  83,  13, 110, 133,  40, 132,   9, 211, 223, 205, 244,  65, 129,  77,  82,
    106, 220,  55, 200, 108, 193, 171, 250,  36, 225, 123,   8,  12, 189, 177,  74,
    120, 136, 149, 139, 227,  99, 232, 109, 233, 203, 213, 254,  59,   0,  29,  57,
    242, 239, 183,  14, 102,  88, 208, 228, 166, 119, 114, 248, 235, 117,  75,  10,
     49,  68,  80, 180, 143, 237,  31,  26, 219, 153, 141,  51, 159,  17, 131,  20
    };


// Constructors
//...........................................................................

    public MD2 () {
        super("MD2");
        engineReset();
    }

    /**
     *    This constructor is here to implement cloneability of this class.
     */
    private MD2 (MD2 md) {
        this();
        X = (int[])md.X.clone();
        checksum = (int[])md.checksum.clone();
        buffer = (int[])md.buffer.clone();
        count = md.count;
    }


// Cloneable method implementation
//...........................................................................

    /**
     * Returns a copy of this MD object.
     */
    public Object clone() { return new MD2(this); }


// JCE methods
//...........................................................................

    /**
     * Resets this object disregarding any temporary data present at the
     * time of the invocation of this call.
     */
    public void engineReset () {
        count = 0;
        for (int i = 0; i < BLOCK_LENGTH; i++) {
            X[i] = 0;
            checksum[i] = 0;
        }
    }

    /**
     * Continues an MD2 message digest using the input byte.
     */
    public void engineUpdate (byte input) {
        buffer[count] = input & 0xFF;
        if (count == BLOCK_LENGTH - 1) {
            transform(buffer, 0);
            count = 0;
        } else
            count += 1;            // update number of bytes modulo 16
    }

    /**
     * MD2 block update operation.
     * <p>
     * Continues an MD2 message digest operation, by filling the buffer,
     * transform(ing) data in 128-bit message block(s), updating the variables
     * context and count, and leaving (buffering) the remaining bytes in buffer
     * for the next update or finish.
     *
     * @param    input    input block
     * @param    offset   start of meaningful bytes in input
     * @param    len      count of bytes in input block to consider
     */
    public void engineUpdate (byte[] input, int offset, int len) {
        // make sure we don't exceed input's allocated size/length
        if (offset < 0 || len < 0 || (long)offset + len > input.length)
            throw new ArrayIndexOutOfBoundsException();

        // compute number of bytes still unhashed; ie. present in buffer
        int partLen = BLOCK_LENGTH - count,
            i = 0;
        if (len >= partLen) {
            // fill the buffer and hash it
            for (int j = 0; j < partLen; j++)
                buffer[count + j] = input[offset + j] & 0xFF;
            transform(buffer, 0);
            count = 0;

            // hash as many BLOCK_LENGTH from remaining input as feasible
            for (i = partLen; i + BLOCK_LENGTH - 1 < len; i+= BLOCK_LENGTH)
                transform(input, offset + i);
        }
        // buffer remaining input
        if (i < len) {
            for (int j = 0; j < len - i; j++)
                buffer[count + j] = input[offset + i + j] & 0xFF;
            count += len - i;    // update number of bytes
        }
    }

    /**
     * Completes the hash computation by performing final operations such
     * as padding. At the return of this engineDigest, the MD engine is
     * reset.
     *
     * @return    the array of bytes for the resulting hash value.
     */
    public byte[] engineDigest () {
        // pad output to 0 mod 16;
        int padLen = BLOCK_LENGTH - count;

        // padding is n bytes each of value n (similar to PKCS#5)
        // but we reserve an extra 16-bytes for the checksum
        for (int i = count; i < BLOCK_LENGTH; i++)
            buffer[i] = (byte)padLen;
    
        transform(buffer, 0);

        // encrypt and process checksum as the last block for this digest op.
        transform(checksum, 0);

        byte[] result = new byte[BLOCK_LENGTH];
        // cast this MD2's context (first 16 ints of X) into an array of 16 bytes.
        for (int i = 0; i < BLOCK_LENGTH; i++)
            result[i] = (byte)X[i];

        // reset the engine
        engineReset();
        return result;
    }


// own methods
//...........................................................................

    /**
     * MD2 basic transformation.
     * <p>
     * Transforms context based on 128 bits from input block starting
     * from the offset'th byte.
     *
     * @param    block    input sub-array.
     * @param    offset   starting position of sub-array.
     */
    private void transform (int[] block, int offset) {
        for (int i = 0; i < BLOCK_LENGTH; i++) {
            X[16 + i] = block[offset + i] & 0xFF;
            X[32 + i] = X[i] ^ X[16 + i];
        }
        // Encrypt block (18 rounds)
        int t = 0;
        for (int i = 0; i < 18; i++) {
            for (int j = 0; j < 48; j++)
                t = X[j] ^= S[t];
            t = (t + i) & 0xFF;
        }
        // update checksum
        t = checksum[BLOCK_LENGTH - 1];
        for (int i = 0; i < BLOCK_LENGTH; i++)
            t = checksum[i] ^= S[(block[offset + i] & 0xFF) ^ t];
    }

    /**
     *    convenience method to handle byte array on input rather than int array.
     */
    private void transform (byte[] block, int offset) {
        for (int i = 0; i < BLOCK_LENGTH; i++) {
            X[16 + i] = block[offset + i] & 0xFF;
            X[32 + i] = X[i] ^ X[16 + i];
        }
        // Encrypt block (18 rounds)
        int t = 0;
        for (int i = 0; i < 18; i++) {
            for (int j = 0; j < 48; j++)
                t = X[j] ^= S[t];
            t = (t + i) & 0xFF;
        }
        // update checksum
        t = checksum[BLOCK_LENGTH - 1];
        for (int i = 0; i < BLOCK_LENGTH; i++)
            t = checksum[i] ^= S[(block[offset + i] & 0xFF) ^ t];
    }
}
