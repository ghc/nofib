// $Id: TestBase64Stream.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestBase64Stream.java,v $
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
// Revision 1.7  1998/02/01 05:08:45  hopwood
// + Committed changes below.
//
// Revision 1.6.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.6  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.5  1998/01/12 04:10:39  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.4  1997/12/22 03:27:24  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/12/22  hopwood
// + Replaced use of temporary files with ByteArrayIn/OutputStream.
// + Replaced use of input file with random data.
// + Use BaseTest API.
//
// Revision 1.3  1997/12/21 20:26:44  iang
// + Hacked to assume file that exists for testing.
//
// Revision 1.2  1997/12/07 06:42:21  hopwood
// + Committed changes below.
//
// Revision 1.1.1  1997/12/04  hopwood
// + Temp files now have a ".tmp" filetype.
// + This class is now non-public.
//
// Revision 1.1  1997/12/03 01:18:21  raif
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.test;

import cryptix.util.mime.Base64InputStream;
import cryptix.util.mime.Base64OutputStream;
import cryptix.util.core.ArrayUtil;
import cryptix.util.test.BaseTest;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Random;

/**
 * This class tests the <samp><a href=cryptix.util.mime.Base64InputStream.html>
 * cryptix.util.mime.Base64InputStream</a></samp> and
 * <samp><a href=cryptix.util.mime.Base64OutputStream>Base64OutputStream</a></samp>
 * classes.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
public class TestBase64Stream
extends BaseTest
{

// Test methods
//................................................................................

    public static void main (String[] args) {
        new TestBase64Stream().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(1);

        Random r = new Random();
        int inputlen = 1000+(r.nextInt() % 100);
        byte[] input = new byte[inputlen];
        r.nextBytes(input);

        int i = 0;
/*
        while (true) {
            i += random.nextByte() & 0x7F;
            if (i > input.length - 2) break;
            input[i++] = '\r';
            input[i++] = '\n';
        }
*/
        out.println("Asciifying...");
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        Base64OutputStream b64out = new Base64OutputStream(baos, true);

        int len = 0;
        for (i = 0; i < inputlen; i += len) {
            len = r.nextInt() & 0x0F;
            if (len > inputlen - i)
                len = inputlen - i;
            b64out.write(input, i, len);
        }
        b64out.close();
        byte[] output = baos.toByteArray();

        out.println("De-asciifying...");
        ByteArrayInputStream bais = new ByteArrayInputStream(output);
        Base64InputStream b64in = new Base64InputStream(bais, true);

        byte[] input2 = new byte[input.length];
        len = 0;
        i = 0;
        while (len != -1) {
            i += len;
            len = b64in.read(input2, i, r.nextInt() & 0x0F);
        }
        b64in.close();

        passIf(ArrayUtil.areEqual(input, input2), "Compare decoded to original");
    }
}
