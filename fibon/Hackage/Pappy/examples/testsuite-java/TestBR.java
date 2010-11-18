// $Id: TestBR.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestBR.java,v $
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
// Revision 1.6  1998/02/01 05:08:45  hopwood
// + Committed changes below.
//
// Revision 1.5.1  1998/02/01  hopwood
// + Replaced use of temporary file 't.tmp' with ByteArrayIn/OutputStreams.
// + Checked for security, and made public.
//
// Revision 1.5  1998/01/28 05:40:01  hopwood
// + Major update of test classes.
//
// Revision 1.4  1998/01/12 04:10:39  hopwood
// + Made engineTest() protected.
// + Cosmetics.
//
// Revision 1.3  1997/11/29 05:12:22  hopwood
// + Changes to use new test API (BaseTest).
//
// Revision 1.2  1997/11/22 06:03:53  iang
// + math ==> util.math
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
import cryptix.util.test.BaseTest;

import java.io.*;

/**
 * This class tests the <samp>cryptix.util.math.BigRegister</samp> class.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
public class TestBR
extends BaseTest
{

// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestBR().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(8);

        BigRegister r = new BigRegister(64);            // 64-bit register
        out.println(r);                                 // test toString()

        r.setBit(0);                                    // test setBit(I)
        r.setBit(5);
        r.setBit(6);
        out.println("Setting bits 0, 5 and 6...\n" + r);

        r.shiftLeft(10);                                // test shiftLeft(+I)
        out.println("Shift left 10 positions...\n" + r);

        r.shiftLeft(10);                                // test shiftLeft(+I)
        out.println("Shift left 10 positions...\n" + r);

        r.shiftRight(11);                               // test shiftRight(+I)
        out.println("Shift right 11 positions...\n" + r);

        r.shiftRight(9);                                // test shiftRight(+I)
        out.println("Shift right 9 positions...\n" + r);

        r.shiftLeft(-2);                                // test shiftLeft(-I)
        out.println("Shift right 2 positions (using 'shiftLeft')...\n" + r);

        r.shiftRight(-4);                               // test shiftRight(-I)
        out.println("Shift left 4 positions (using 'shiftRight')...\n" + r);

        r.shiftRight(2);                                // test shiftRight(+I)
        out.println("Shift right 2 positions...\n" + r);

        r.rotateLeft(10);                               // test rotateLeft(+I)
        out.println("Rotating left 10 positions...\n" + r);

        r.rotateRight(13);                              // test rotateRight(+I)
        out.println("Rotating right 13 positions...\n" + r);

        r.rotateRight(-10);                             // test rotateRight(-I)
        out.println("Rotating left 10 positions (using 'rotateRight')...\n" + r);

        r.rotateLeft(-13);                              // test rotateLeft(-I)
        out.println("Rotating right 13 positions (using 'rotateLeft')...\n" + r);

        r.rotateLeft(50);                               // test rotateLeft(I)
        out.println("Rotating left 50 positions = right 14...\n" + r);

        r.rotateRight(60);                              // test rotateRight(I)
        out.println("Rotating right 60 positions = left 4...\n" + r);

        r.shiftRight(65);                               // test reset()
        out.println("Shifting right 65 positions...\n" + r);

        // create 1 new register and make two copies
        BigRegister b = new BigRegister(64);            // all zeroes
        BigRegister a = new BigRegister(64);
        a.atRandom();                                   // test atRandom()
        BigRegister aa = (BigRegister) a.clone();       // test clone()

        out.println("Register A (random value): " + a);
        out.println("Register AA (copy of A): " + a);
        out.println("Both A and AA have " +
            a.countSetBits() + " set bits between indices: #" +
            a.lowestSetBit() + " and #" +               // test lowestSetBit()
            a.highestSetBit() + " (inclusive)...");     // test highestSetBit()
        out.println();
        
        out.println("Register B: " + b);
        a.and(b);                                       // test and()
        out.println("Register AA: " + aa);
        out.println("A = A & B: " + a);

        // a should be all zeroes by now.
        passIf(a.countSetBits() == 0, "Register A now has 0 set bits?");

        // load a with its old value
        a.load(aa);                                     // test load(BR)
        // a or b should be == aa
        a.or(b);                                        // test or()
        out.println("Register A: " + a);
        passIf(a.isSameValue(aa), "A == AA using isSameValue?");
                                                        // test isSameValue()

        out.print("How does A compare to AA?");
        int x = a.compareTo(aa);
        out.println(" " + (x == 0 ? "A == AA" : (x == -1 ? "A < AA" : " A > AA")));
        passIf(x == 0, "A == AA using compareTo?");     // test compareTo()
        out.println();

        // invert b
        b.not();                                        // test not()
        out.println("Register B: " + b);
        // should be all 1s.
        passIf(b.countSetBits() == b.getSize(), "Register B is now all 1s?");
                                                        // test getSize()

        // test if a = a xor b xor b. use random values for a and b
        a.atRandom();
        b.atRandom();
        aa = (BigRegister) a.clone();
        out.println("Register A (random value): " + a);
        out.println("Register B (random value): " + b);

        a.xor(b);
        out.println("A = A ^ B: " + a);                 // test xor()
        out.println("Register AA (old value of A): " + aa);
        passIf(!(a.isSameValue(aa)), "A != AA?");

        a.xor(b);
        out.println("(A ^ B) ^ B: " + a);
        passIf(a.isSameValue(aa), "AA = (A ^ B) ^ B?");

        // test serialisation
        try {
            a.atRandom();
            out.println("About to serialize A and B...");
            out.println("Register A (random value): " + a);
            out.println("Register B: " + b);

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(a);
            oos.writeObject(b);
            oos.flush();
            baos.close();
            byte[] serialized = baos.toByteArray();
 
//            out.println("Finished serialization. Now resetting A and B...");
//            a.reset();        out.println("Register A: " + a);
//            b.reset();        out.println("Register B: " + b);

            out.println("About to deserialize A and B...");

            ByteArrayInputStream bais = new ByteArrayInputStream(serialized);
            ObjectInputStream ois = new ObjectInputStream(bais);
            aa = (BigRegister) ois.readObject();
            BigRegister bb = (BigRegister) ois.readObject();
            bais.close();

            out.println("Register AA: " + aa);
            out.println("Register BB: " + bb);
            passIf(a.isSameValue(aa), "A == AA?");
            passIf(b.isSameValue(bb), "B == BB?");
        } catch (Exception e) {
            error(e);
        }

        // test odd size registers
        r = new BigRegister(53);
        out.println(r);

        r.setBit(0);
        r.setBit(5);
        r.setBit(6);
        out.println("Setting bits 0, 5 and 6...\n" + r);

        r.shiftLeft(10);
        out.println("Shift left 10 positions...\n" + r);

        r.rotateRight(13);
        out.println("Rotating right 13 positions...\n" + r);

        r.setBits(45, 5, 0xFF);                         // test setBits()
        out.println("Setting 5 bits starting @45 to 1s...\n" + r);
                                                        // test getBits()
        out.println("2 bits starting @44 have a value of: " + r.getBits(44, 2));
        out.println("4 bits starting @46 have a value of: " + r.getBits(46, 4));
    }
}
