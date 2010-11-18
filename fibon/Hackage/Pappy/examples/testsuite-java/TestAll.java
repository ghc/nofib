// $Id: TestAll.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestAll.java,v $
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
// Revision 1.11  2000/10/02 15:07:26  edwin
// - Add Rijndael
// - Add a readme for the 3.1.3/3.2.0 releases
//
// Revision 1.10  1999/07/21 01:33:24  edwin
// Removed RPK
//
// Revision 1.9  1999/07/13 18:14:48  edwin
// Added missing tests
//
// Revision 1.8  1998/02/22 04:23:07  hopwood
// + Committed changes below.
//
// Revision 1.7.1  1998/02/14  hopwood
// + Case is now not significant in -allVerbose option.
//
// Revision 1.7  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.6.2  1998/01/28  hopwood
// + More changes to match BaseTest.
// + Use BaseTest.skip to flag skipped tests.
// + Added "Install" test.
//
// Revision 1.6.1  1998/01/13  hopwood
// + Changes to co-operate with the new delayed output feature of
//   BaseTest.
// + Added -allverbose option.
//
// Revision 1.6  1998/01/12 04:10:39  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.5  1997/12/07 06:42:21  hopwood
// + Added TestElGamal, various other changes.
//
// Revision 1.4  1997/11/29 17:47:08  hopwood
// + Minor changes to test API.
// + Added better support for BaseTest subclasses in TestAll (doesn't work
//   yet).
//
// Revision 1.3  1997/11/29 11:51:42  raif
// + Changed the call statement to invoke a constructor with
//   no argument. Now works with some Test* classes which, in
//   their turn invoke local test*() methods from their constructor;
//   ie. TestIDEA, TestSAFER, etc...
//
// Revision 1.2  1997/11/29 05:12:22  hopwood
// + Changes to use new test API (BaseTest).
//
// Revision 1.1.1.1  1997/11/03 22:36:55  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.0  1997/09/14  David Hopwood
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.test;

import cryptix.util.test.BaseTest;
import cryptix.util.test.TestException;

/**
 * Class for running all tests in the cryptix.test package.
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
class TestAll
extends BaseTest
{

// Variables
//................................................................................

    private boolean allVerbose; // defaults to false


// Constants
//................................................................................

    private static String[] tests = {
        "Install",
        "3LFSR", "Base64Stream", "BR", // simple tests first
        "IJCE",
        "Blowfish", "CAST5", "DES", "DES_EDE3", "IDEA", "LOKI91", 
        "RC2", "RC4", "Rijndael", "SAFER", "SPEED", "Square",
        "HAVAL", "MD2", "MD4", "MD5", "RIPEMD128", "RIPEMD160", "SHA0", "SHA1",
        "HMAC", 
        "Scar", "UnixCrypt",
        "RSA", "ElGamal"
    };


// Command-line parsing
//................................................................................

    protected void parseOption(String option) throws TestException {
        if (option.equalsIgnoreCase("-allVerbose"))
            allVerbose = true;
        else
            super.parseOption(option);
    }

    public String describeOptions() {
        return super.describeOptions() +
            "    -allVerbose: print full output for each test class.\n";
    }


// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestAll().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(tests.length);

        if (allVerbose) setVerbose(true);
        for (int i = 0; i < tests.length; i++) {
            out.println("---------------------------------------------------------------------------");
            String classname = "cryptix.test.Test" + tests[i];
            status.print("\n>>> " + classname);
            status.flush();

            try {
                Object obj = Class.forName(classname).newInstance();
                if (obj instanceof BaseTest) {
                    BaseTest test = (BaseTest) obj;
                    test.setOutput(out);
                    test.setVerbose(allVerbose);
                    test.test();
                    passIf(test.isOverallPass(), classname);
                } else {
                    error("Test class does not extend cryptix.util.test.BaseTest");
                }
            } catch (ClassNotFoundException e) {
                skip("Class not found");
            } catch (TestException e) {
                fail(e.getMessage());
            } catch (Exception e) {
                error(e);
            }
        }
    }
}
