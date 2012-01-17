// $Id: TestInstall.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestInstall.java,v $
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
// Revision 1.3  2000/08/16 23:07:22  edwin
// Remove reference to library path, which is not used anymore since the
// .properties files are now in the jars.
//
// Revision 1.2  1998/02/01 05:08:44  hopwood
// + Committed changes below.
//
// Revision 1.1.1  1998/02/01  hopwood
// + Checked for security, and made public.
//
// Revision 1.1  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 0.1.0  1998/01/16  hopwood
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.CryptixProperties;
import cryptix.provider.Cryptix;
import cryptix.util.test.BaseTest;

import java.security.Provider;
import java.security.Security;

/**
 * Tests whether Cryptix is installed correctly.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 */
public class TestInstall
extends BaseTest
{

// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestInstall().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(1);

        out.println(CryptixProperties.getVersionString());

        Provider provider = Security.getProvider("Cryptix");
        boolean ok = provider != null && provider instanceof cryptix.provider.Cryptix;
        passIf(ok, "Cryptix installed?");
        if (!ok) {
            out.println("Cryptix is not installed as a provider in the java.security file.");
            out.println("Enter \"java cryptix.provider.Install\" to correct this.");
        }

    }
}
