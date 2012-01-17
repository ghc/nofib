// $Id: BlockMessageDigest.java,v 1.1 2002/09/05 19:27:05 baford Exp $
//
// $Log: BlockMessageDigest.java,v $
// Revision 1.1  2002/09/05 19:27:05  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.4  1998/01/19 23:40:54  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1998/01/19  hopwood
// + Fix from Aldo Eisma: incorrect input data was used to calculate the digest.
//   'engineTransform(data)' should be 'engineTransform(buffer)'.
//
// Revision 1.3  1997/12/19 05:44:13  hopwood
// + Committed changes below.
//
// Revision 1.2.1  1997/12/18  hopwood
// + Made count variable long.
// + Made sure that count does not overflow (although it's unlikely that anyone
//   could feasibly hash 2^61 bytes).
// + engineGetDataLength should be abstract.
// + Cache data length for minor efficiency gain.
// + Cosmetics.
//
// Revision 1.2  1997/12/16 21:58:25  iang
// + MD5, SHA{01} debugged, got working, internal self_tests ok.
// + BlockMessageDigest.bitcount() made long, was int, check calling
//   where it is assumed by digest algorithms to be a long.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.2  1997/08/27  David Hopwood
// + engineGetDigest must not be abstract, because that would cause
//   an incompatible class change problem when Java 1.2 is released.
//
// Revision 0.1.0.1  1997/08/14  David Hopwood
// + Moved to cryptix.provider.md package.
// + Changed to extend java.security.MessageDigest directly.
// + Removed deprecated methods.
//
// Revision 0.1.0.0  1997/06/29  David Hopwood
// + Initial version, based on cryptix.security.MessageDigest.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.md;

import cryptix.CryptixException;

import java.security.MessageDigest;

/**
 * This is a superclass for message digests that operate internally on
 * blocks of data. It is not intended directly for use by application
 * programmers.
 * <p>
 * <b>Copyright</b> &copy; 1995-1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 * @since   Cryptix 2.2.2
 */
abstract class BlockMessageDigest
extends MessageDigest
{
    /**
     * The buffer used to store the last incomplete block.
     */
    private byte[] buffer;

    /**
     * The number of bytes currently stored in <code>buffer</code>.
     */
    private int buffered;

    /**
     * The number of bytes that have been input to the digest.
     */
    private long count;

    private static final long MAX_COUNT = (1L << 61)-1L;

    /**
     * The length of a data block for this algorithm.
     */
    private int data_length;

    /**
     * Constructs a message digest with the specified algorithm name.
     *
     * @param algorithm the standard name of the digest algorithm.
     */
    protected BlockMessageDigest(String algorithm)
    {
        super(algorithm);
        data_length = engineGetDataLength();
        buffer = new byte[data_length];
    }

    /**
     * @return number of bits hashed so far?
     */
    protected long bitcount() {
        return count * 8;
    }

    /**
     * <b>SPI</b>: Resets the digest. Subclasses that override <code>engineReset</code>
     * should always call this implementation using <code>super.engineReset()</code>.
     */
    protected void engineReset()
    {
        buffered = 0;
        count = 0;
    }

    /**
     * <b>SPI</b>: Updates the message digest with a byte of new data.
     *
     * @param b     the byte to be added.
     */
    protected void engineUpdate(byte b)
    {
        byte[] data = { b };
        engineUpdate(data, 0, 1);
    }

    /**
     * <b>SPI</b>: Updates the message digest with new data.
     *
     * @param data      the data to be added.
     * @param offset    the start of the data in the array.
     * @param length    the number of bytes of data to add.
     */
    protected void engineUpdate(byte[] data, int offset, int length)
    {
        count += length;
        if (count > MAX_COUNT)
            throw new CryptixException(getAlgorithm() + ": Maximum input length exceeded");

        int datalen = data_length;
        int remainder;

        while (length >= (remainder = datalen - buffered)) {
            System.arraycopy(data, offset, buffer, buffered, remainder);
            engineTransform(buffer);
            length -= remainder;
            offset += remainder;
            buffered = 0;
        }

        if (length > 0) {
            System.arraycopy(data, offset, buffer, buffered, length);
            buffered += length;
        }
    }

    /**
     * <b>SPI</b>: Calculates the final digest. BlockMessageDigest
     * subclasses should not usually override this method.
     *
     * @return the digest as a byte array.
     */
    protected byte[] engineDigest()
    {
        return engineDigest(buffer, buffered);
    }

    //
    // Override int engineDigest(byte[] buf, int offset, int len)
    // from Java 1.2 preview docs? For the time being no - it should work
    // anyway.
    //

    /**
     * <b>SPI</b> (for BlockMessageDigests only): Calculates the
     * final digest. <code>data[0..length-1]</code> contains the last
     * incomplete input block. <i>length</i> will be less than
     * <code>engineDataLength()</code>.
     *
     * @param data      the last incomplete block.
     * @param length    the length in bytes of the last block.
     * @return the digest as a byte array.
     */
    protected abstract byte[] engineDigest(byte[] data, int length);

    /**
     * <b>SPI</b> (for BlockMessageDigests only): Performs a
     * transformation on the given data, which is always one block long.
     */
    protected abstract void engineTransform(byte[] data);

    /**
     * <b>SPI</b>: Returns the length of the block that this hash
     * function operates on.
     */
    protected abstract int engineGetDataLength();
}
