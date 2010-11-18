// $Id: Test3LFSR.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: Test3LFSR.java,v $
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
// Revision 1.10  1998/02/01 05:08:44  hopwood
// + Committed changes below.
//
// Revision 1.9.1  1998/02/01  hopwood
// + Replaced use of temporary file 't.tmp' with ByteArrayIn/OutputStreams.
// + Checked for security, and made public.
//
// Revision 1.9  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.8.1  1998/01/13  hopwood
// + Set number of expected passes.
//
// Revision 1.8  1998/01/12 04:10:39  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.7  1997/11/29 11:23:02  raif
// + hard-wired the cowboy hack. For a complete, and lengthy,
//   test for all values use TestRPK3.
// + replace equals() by isSameValue() to keep same semantics.
// + tests OK.
//
// Revision 1.6  1997/11/29 05:12:23  hopwood
// + Changes to use new test API (BaseTest).
//
// Revision 1.5  1997/11/22 07:05:40  raif
// *** empty log message ***
//
// Revision 1.4  1997/11/22 06:16:28  iang
// + Added .math in text
//
// Revision 1.3  1997/11/22 06:03:52  iang
// + math ==> util.math
//
// Revision 1.2  1997/11/20 18:15:52  iang
// + Added cowboy hack to stop testing after L=127 so test is reasonably quick.
//
// Revision 1.1  1997/11/07 05:53:25  raif
// *** empty log message ***
//
// Revision 1.1  1997/09/29 12:47:02  raif
// *** empty log message ***
//
// Revision 0.1.0  1997/09/23  R. Naffah
// + Original version.
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.util.math.BigRegister;
import cryptix.util.math.TrinomialLFSR;
import cryptix.util.test.BaseTest;

import java.io.*;

/**
 * This class tests the <samp>cryptix.util.math.TrinomialLFSR</samp> class.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
public class Test3LFSR
extends BaseTest
{

// Test methods
//................................................................................

    public static void main(String[] args) {
        new Test3LFSR().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(3);

        TrinomialLFSR xx = new TrinomialLFSR(4, 1);
        xx.resetX(1);
        out.println(" before shift left: " + xx);
        xx.shiftLeft(1); out.println(" after shift left (1): " + xx);
        xx.resetX(1); xx.shiftLeft(2); out.println(" after shift left (2): " + xx);
        xx.resetX(1); xx.shiftLeft(3); out.println(" after shift left (3): " + xx);
        xx.resetX(1); xx.shiftLeft(4); out.println(" after shift left (4): " + xx);
        xx.resetX(1); xx.shiftLeft(5); out.println(" after shift left (5): " + xx);
        xx.resetX(0);
        out.println(" before shift right: " + xx);
        xx.shiftRight(1); out.println(" after shift right (1): " + xx);
        xx.resetX(0); xx.shiftRight(2); out.println(" after shift right (2): " + xx);
        xx.resetX(0); xx.shiftRight(3); out.println(" after shift right (3): " + xx);
        xx.resetX(0); xx.shiftRight(4); out.println(" after shift right (4): " + xx);
        xx.resetX(0); xx.shiftRight(5); out.println(" after shift right (5): " + xx);

        // example 6.10, p.196 in [HAC]

        TrinomialLFSR r = new TrinomialLFSR(4, 3);      // f(x) = x4 + x3 + 1
        out.println(r);                                 // test toString()

        r.setX(2);                                      // test setX()
        r.setX(3);
        out.println("Initialising to (1001) x3 + x2: " + r);

        String output = "";
        for (int i = 0; i < 15; i++) {
            output += "" + r.next(1) + ", ";
            out.println("LFSR state @" + (i+1) + ": " + r);
        }
        out.println("Output sequence is = " + output + "...\n");
        passIf(output.equals("1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, "),
            "LFSR state test");

        // examples in infosec.doc <infosec@certicom.ca>

        r = new TrinomialLFSR(4, 1);                    // f(x) = x4 + x + 1
        r.resetX(0);                                    // test resetX()
        TrinomialLFSR a = (TrinomialLFSR) r.clone();    // test clone()
        a.setX(0);
        a.setX(1);
        out.println("Generating powers of x (mod(f(x))...");
        int c;
        for (int i = 0; i < 15; i++) {
            r.clock(1);
            out.print(" State @" + (i+1) + ": " + r.toPolynomial());
            c = r.compareTo(a);                         // test compareTo()
            out.println((c == -1 ? "<" : (c == 0 ? "==" : ">")) +
                a.toPolynomial());                      // test toPolynomial()
        }

        TrinomialLFSR x = r.trinomialX();
        out.println("\nSame using pow()...");
        for (int i = 0; i < 15; i++) {
            x = r.trinomialX();
            out.print(x.toPolynomial() + "** " + (i) + " =");
            x.pow(x.valueOf(i));
            out.println(x.toPolynomial());
        }

        TrinomialLFSR b = (TrinomialLFSR) r.clone();
        a.resetX(3);
        a.setX(2);
        a.setX(0);                                      // a = x3 + x2 + 1; (0111)
        b.resetX(3);
        b.setX(0);                                      // b = x3 + 1; (0011)
        out.println("\nNow working in GF[2**4] with f(x) = x4 + x + 1 ...");
        out.println("Defining 'a' set to (0111) x3 + x2 + 1: " + a.toPolynomial());
        out.println("Defining 'b' set to (0011) x3 + 1: " + b.toPolynomial());
        a.multiply(b);
        out.println("Computing a * b (mod(f(x)): " + a.toPolynomial());

        // repeat to test if a * b == b * a
        TrinomialLFSR aa = (TrinomialLFSR) r.clone();
        TrinomialLFSR bb = (TrinomialLFSR) r.clone();
        aa.resetX(3);
        aa.setX(2);
        aa.setX(0);
        bb.resetX(3);
        bb.setX(0);
        out.println("\nDefining 'aa' set to (0111) x3 + x2 + 1: " + aa.toPolynomial());
        out.println("Defining 'bb' set to (0011) x3 + 1: " + bb.toPolynomial());
        bb.multiply(aa);
        out.println("Computing bb * aa (mod(f(x)): " + bb.toPolynomial());
        passIf(a.isSameValue(bb), "a * b == bb * aa?");

        // test serialisation
        try {
            r.atRandom();
            out.println("\nAbout to serialize R...");
            out.println("R: " + r);

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(r);
            oos.flush();
            baos.close();
            byte[] serialized = baos.toByteArray();
 
            out.println("Finished serialization. Now resetting R...");
            TrinomialLFSR r1 = (TrinomialLFSR) r.clone();
            r1.reset();
            out.println("R: " + r1);

            out.println("About to deserialize R...");
            ByteArrayInputStream bais = new ByteArrayInputStream(serialized);
            ObjectInputStream ois = new ObjectInputStream(bais);

            r1 = (TrinomialLFSR) ois.readObject();
            bais.close();

            out.println("R: " + r1);
            passIf(r.isSameValue(r1), "Serialization test");
        } catch (Exception e) {
            error(e);
        }

        // test Galois counter behaviour
        int[] mersenne = {89, 127, 521, 607, 1279, 2281, 3217};
        int[][] taps = {
            {  38},
            {  63,  30,  15,   7},
            { 168, 158,  48,  32},
            { 273, 147, 105},
            { 418, 216},
            {1029, 915, 715},
            { 576,  67}
        };

        out.println("Testing few monic primitive trinomials as Galois counters...");
        TrinomialLFSR y, z;
        BigRegister exp;
        int limit, tt;
//        for (int i = 0; i < mersenne.length; i++) {
        for (int i = 0; i < 2; i++) {
            int L = mersenne[i];
//	        if (127 < L) {   
//	            out.println("Cowboy hack - Stop after fifth Test L="+L);
//		        break;
//	        }
            exp = new BigRegister(L);
            for (int j = 0; j < taps[i].length; j++) {
                int K = taps[i][j];
                y = new TrinomialLFSR(L, K);
                y.resetX(1);
                z = (TrinomialLFSR) y.clone();
                limit = L;
                tt = 1;
                out.println(" ...testing: x"+L+" + x"+K+" + 1...");
                while (limit >= 0) {
                    z.resetX(1);
                    z.pow(exp.valueOf(tt));
//                    out.println("\t(x)"+tt+" =\t"+z.toPolynomial()+"\t"+y.toPolynomial());
//                    if (! y.equals(z)) {
                    if (! y.isSameValue(z)) {
                        fail("LFSR is out of sync...");
                        break;
                    }
                    y.clock(1);
                    tt++;
                    limit--;
                }
            }
        }
    }
}
