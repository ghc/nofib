// $Id: MCT.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: MCT.java,v $
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
// Revision 1.5  2000/08/17 11:41:04  edwin
// java.* -> xjava.*
//
// Revision 1.4  1998/03/13 11:01:25  raif
// *** empty log message ***
//
// Revision 1.3.1  1998/03/13  raif
// + added support for _Algorithm implementations with variable block size.
//
// Revision 1.3  1998/02/28 07:11:23  raif
// *** empty log message ***
//
// Revision 1.2.1  1998/02/28  raif
// + fixed a bug that appeared when IJCE API was used.
// + added support for a user-defined Provider and key lengths.
//
// Revision 1.2  1998/02/08 21:04:46  raif
// *** empty log message ***
//
// Revision 1.1.1  1998/02/09  raif
// + fixed the VERSION string.
//
// Revision 1.1  1998/01/15 20:30:46  raif
// *** empty log message ***
//
// Revision 0.1  1998/01/10  raif
// + original version.
//
// $Endlog$
/*
 * Copyright (c) 1998 Systemics Ltd on behalf of
 * the Cryptix Development Team. All rights reserved.
 */
package cryptix.tools;

import cryptix.util.core.ArrayUtil;
import cryptix.util.core.Hex;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import xjava.security.Cipher;
import java.security.KeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import xjava.security.SecretKey;
import java.util.StringTokenizer;

/**
 * For a designated symmetric block cipher algorithm, this command generates
 * and exercises Monte Carlo Tests data for both Encryption and Decryption in
 * Electronic Codebook (ECB) and Cipher Block Chaining (CBC) modes.<p>
 *
 * MCT's output file format is in conformance with the layout described in
 * Section 4 of NIST's document "Description of Known Answer Tests and Monte
 * Carlo Tests for Advanced Encryption Standard (AES) Candidate Algorithm
 * Submissions" dated January 7, 1998.<p>
 *
 * If the -p argument is not specified, this command assumes that the name
 * of the designated cipher algorithm is also that of its Security Provider.
 * It always tries processing the user's request using Java Reflection API
 * methods on an XXX_Algorithm class, if such a class exists --XXX being the
 * name of the AES candidate algorithm. When such a class exists, it is
 * assumed to include the following static methods:<ul>
 *  <li>public static int blockSize();<br>
 *      Return the length in bytes of the cipher's input block.
 *  <li>public static synchronized Object makeKey (byte[]);<br>
 *      Expand a user-supplied key material into a cipher's session key.
 *  <li>public static byte[] blockEncrypt (byte[], int, Object);<br>
 *      Encrypt exactly one block of plaintext.
 *  <li>public static byte[] blockDecrypt (byte[], int, Object);<br>
 *      Decrypt exactly one block of plaintext.</ul><p>
 *
 * If an *_Algorithm class was not found, or if found but an exception was
 * thrown during the invocation and/or execution of one of its methods, this
 * command then reverts to using the IJCE API methods for carrying on the
 * user's request.<p>
 *
 * This duality of functionalities is here for performance reasons since
 * speed is faster with the Reflection API than with the IJCE one --on a
 * Pentium 133MHz, without JIT, using JDK-1.1.5 Reflection API brings more
 * than 10% speed improvement.<p>
 *
 * <b>Copyright</b> &copy; 1998
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.<p>
 *
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
public final class MCT
{
// main method
//...........................................................................
    
    public static void main (String[] args) {
        System.out.println(
            "NIST Monte-Carlo Tests data generator/exerciser\n\n" +
            VERSION + "\n" +
            "Copyright (c) 1998 Systemics Ltd. on behalf of\n" +
            "the Cryptix Development Team.  All rights reserved.\n\n");
        MCT cmd = new MCT();
        cmd.processOptions(args);
        cmd.run();
    }


// Fields & constants
//...........................................................................

    static final String VERSION = "$Revision: 1.1 $";
    static final String SUBMITTER = "<as stated on the submission cover sheet>";

    /** Current values of switches as set from the command line arguments. */
    boolean ecb = false ;       // -e  generate ECB Encrypt/Decrypt only
    boolean cbc = false ;       // -c  generate CBC Encrypt/Decrypt only
    String dirName = null;      // -d  output directory if != user.dir
    String keylengths = null;   // -l  comma-separated key lengths

    String provider = null;   // provider name if cipherName != provider name
    String cipherName = null;   // cipher algorithm name == provider
    File destination = null;    // destination directory File object
    int[] keys = new int[] {128, 192, 256}; // key-length values to test with

    final String eeFileName = "ecb_e_m.txt"; // ECB/Encrypt output filename
    final String edFileName = "ecb_d_m.txt"; // ECB/Decrypt output filename
    final String ceFileName = "cbc_e_m.txt"; // CBC/Encrypt output filename
    final String cdFileName = "cbc_d_m.txt"; // CBC/Decrypt output filename

    // will use zeroes for fields that require initial values.
    // could be replaced by random.nextBytes() using
    //
//    static final java.util.Random rand = new java.util.Random();
    //
    // or for cryptographically strong randoms use the following
    //
//    static final java.security.SecureRandom rand = new java.security.SecureRandom();
    //
    // bear in mind that initialising the latter PRNG is a lengthy process

    // statistics fields
    long encBlocks;     // total count of encrypted blocks
    long decBlocks;     // total count of decrypted blocks
    long keyCount;      // total count of key creation requests

    Class algorithm = null;        // fields for using Reflection API methods
    Method blockSize = null;
    Method makeKey = null;
    Method encrypt = null;
    Method decrypt = null;

    Cipher cipher = null;                 // field for using IJCE API methods

    boolean useReflection = true;  // by default we'll use the Reflection API


// Own methods
//...........................................................................

    /** Process command line arguments. */
    void processOptions (String[] args) {
        int argc = args.length;
        if (argc == 0) printUsage();
        System.out.println(
            "(type \"java cryptix.tools.MCT\" with no arguments for help)\n\n");
        int i = -1;
        String cmd = "";
        boolean next = true;
        while (true) {
            if (next) {
                i++;
                if (i >= argc)
                    break;
                else
                    cmd = args[i];
            } else
                cmd = "-" + cmd.substring(2);
            
            if (cmd.startsWith("-e")) {
                ecb = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-c")) {
                cbc = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-l")) {       // key lengths
                keylengths = args[i + 1];
                i++;
                next = true;
            } else if (cmd.startsWith("-d")) {       // destination directory
                dirName = args[i + 1];
                i++;
                next = true;
            } else if (cmd.startsWith("-p")) {       // provider name
                provider = args[i + 1];
                i++;
                next = true;
            } else // it's the cipher
                cipherName = cmd;
        }
        // sanity checks
        if (cipherName == null) halt("Missing cipher algorithm name");
        if (cipherName.length() > 1 &&
                (cipherName.startsWith("\"") || cipherName.startsWith("'")))
            cipherName = cipherName.substring(2, cipherName.length() - 2);

        if (provider == null) provider = cipherName;

        if (keylengths != null) {
            int count = 0;
            int k;
            int[] keystemp = new int[3]; // maximum allowed
            StringTokenizer st = new StringTokenizer(keylengths, ", \t\"");
            while (st.hasMoreTokens()) {
                k = Integer.parseInt(st.nextToken());
                if (k <= 0) halt("Negative key length not allowed: "+k);
                if (count == 3) halt("Only three key-length values are allowed.");
                keystemp[count++] = k;
            }
            if (count != 0) {
                keys = new int[count];
                System.arraycopy(keystemp, 0, keys, 0, count);
            }
        }

        if (!ecb && !cbc) ecb = cbc = true;

        if (dirName == null) dirName = System.getProperty("user.dir");
        destination = new File(dirName);
        if (! destination.isDirectory())
            halt("Destination <" + destination.getName() +
                "> is not a directory");

        // now instantiate both Reflection and IJCE fields
        // start with Reflection
        // to use reflection API we load the *_Algorithm class if one exists
        // look for class fully named XXX.XXX_Algorithm
        try {
            algorithm = Class.forName(
//                cipherName + "." + cipherName + "_Algorithm");
                provider + "." + cipherName + "_Algorithm");
            // inspect the _Algorithm class
            Method[] methods = algorithm.getDeclaredMethods();
            for (i = 0; i < methods.length; i++) {
                String name = methods[i].getName();
                int params = methods[i].getParameterTypes().length;
                if (name.equals("blockSize"))
                    blockSize = methods[i];
                else if (name.equals("makeKey") && (params == 1))
                    makeKey = methods[i];
                else if (name.equals("blockEncrypt") && (params == 3))
                    encrypt = methods[i];
                else if (name.equals("blockDecrypt") && (params == 3))
                    decrypt = methods[i];
            }
            if (blockSize == null) throw new NoSuchMethodException("blockSize()");
            if (makeKey == null)   throw new NoSuchMethodException("makeKey()");
            if (encrypt == null)   throw new NoSuchMethodException("blockEncrypt()");
            if (decrypt == null)   throw new NoSuchMethodException("blockDecrypt()");
        }
        catch (ClassNotFoundException ex1) {
            notify("Unable to find a " + cipherName + "_Algorithm class");
            algorithm = null;
        }
        catch (NoSuchMethodException ex2) {
            notify("Unable to find method " + ex2.getMessage() + " in " +
                cipherName + "_Algorithm class");
            algorithm = null;
        }
        //
        // then IJCE. note that IJCE is mandatory.
        //
        try {
//            cipher = Cipher.getInstance(cipherName + "/ECB", cipherName);
            cipher = Cipher.getInstance(cipherName + "/ECB", provider);
        } catch (NoSuchProviderException ex3) {
//            halt("Unable to locate Security Provider: " + cipherName);
            halt("Unable to locate Security Provider: " + provider);
        } catch (NoSuchAlgorithmException ex4) {
            halt("Unable to locate an implementation for Cipher: " +
                cipherName + "/ECB");
        }
        useReflection = algorithm != null;
    }

    /**
     * Print an error message to System.err and halts execution returning
     * -1 to the JVM.
     *
     * @param  s  a message to output on System.err
     */
    static void halt (String s) {
        System.err.println("\n*** " + s + "...");
        System.exit(-1);
    }

    /**
     * Write a notification message to System.out.
     *
     * @param  s  string to output to System.out.
     */
    static void notify (String s) { System.out.println("MCT: " + s + "..."); }
    
    /** write help text and quit. */
    void printUsage() {
        System.out.println(
        "NAME\n" +
        "  MCT: A Monte Carlo Tests data generator/exerciser for any block\n" +
        "  cipher algorithm.\n\n" +
        "SYNTAX\n" +
        "  java cryptix.tools.MCT\n" +
        "    [ -e | -c ]\n" +
        "    [ -l <comma-separated-key-lengths>]\n" +
        "    [ -d <output-directory>]\n" +
        "    [ -p <provider>]\n" +
        "    <cipher>\n\n" +
        "DESCRIPTION\n" +
        "  For a designated symmetric block cipher algorithm, this command\n" +
        "  generates and exercises Monte Carlo Tests data for both Encryption\n" +
        "  and Decryption in Electronic Codebook (ECB) and Cipher Block Chaining\n" +
        "  (CBC) modes.\n" +
        "  MCT's output file format is in conformance with the layout described\n" +
        "  in Section 4 of NIST's document \"Description of Known Answer Tests\n" +
        "  and Monte Carlo Tests for Advanced Encryption Standard (AES) Candidate\n" +
        "  Algorithm Submissions\" dated January 7, 1998.\n\n" +
        "OPTIONS\n" +
        "  -e   Generate both Encryption and Decryption data for the cipher in\n" +
        "       ECB mode only.  By default MCT generates both ECB and CBC test\n" +
        "       suites.\n\n" +
        "  -c   Generate both Encryption and Decryption data for the cipher in\n" +
        "       CBC mode only.  By default MCT generates both ECB and CBC test\n" +
        "       suites.\n\n" +
        "  -l <comma-separated-key-lengths>\n" +
        "       Comma separated list (maximum of three) of key lengths to use\n" +
        "       for the tests.  If omitted, the following three values are\n" +
        "       assumed: 128, 192 and 256.\n\n" +
        "  -d <output-directory>\n" +
        "       Pathname of the directory where the output files: \"ecb_e_m.txt\",\n" +
        "       \"ecb_d_m.txt\", \"cbc_e_m.txt\" and \"cbc_d_m.txt\" will be generated.\n" +
        "       If this destination directory is not specified, those files will\n" +
        "       be placed in the current user directory.\n\n" +
        "  -p <provider>\n" +
        "       Name of the Security Provider for the designated algorithm.\n" +
        "       If omitted, then assumes provider has the same name as the\n" +
        "       algorithm itself.\n\n" +
        "  <cipher>\n" +
        "       Cipher algorithm name.\n\n" +
        "COPYRIGHT\n" +
        "  Copyright (c) 1998 Systemics Ltd. on behalf of\n" +
        "  the Cryptix Development Team.  All rights reserved.\n");
        System.exit(0);
    }

    /** main action. */
    void run() {
        long time = System.currentTimeMillis();
        try {
            if (ecb) ecbMCT(eeFileName, edFileName);
            if (cbc) cbcMCT(ceFileName, cdFileName);
        }
        catch (KeyException ex1) {
            ex1.printStackTrace();
            halt("Key Exception encountered\n" + ex1.getMessage());
        }
        notify("Java interpreter used: Version " + System.getProperty("java.version"));
        notify("Java Just-In-Time (JIT) compiler: " + System.getProperty("java.compiler"));
        // print timing and stats info
        notify("Total execution time (ms): " + (System.currentTimeMillis() - time));
        notify("During this time, " + cipherName + ":");
        notify("  Encrypted " + encBlocks + " blocks");
        notify("  Decrypted " + decBlocks + " blocks");
        notify("  Created " + keyCount + " session keys");
    }


// ECB Monte Carlo Tests
//...........................................................................

    void ecbMCT (String encName, String decName)
    throws KeyException {
        PrintWriter enc = null;
        File f1 = new File(destination, encName);
        try { enc = new PrintWriter(new FileWriter(f1) , true); }
        catch (IOException ex3) {
            halt("Unable to initialize <" + encName + "> as a Writer:\n" +
                ex3.getMessage());
        }
        PrintWriter dec = null;
        File f2 = new File(destination, decName);
        try { dec = new PrintWriter(new FileWriter(f2) , true); }
        catch (IOException ex4) {
            halt("Unable to initialize <" + decName + "> as a Writer:\n" +
                ex4.getMessage());
        }
        enc.println();                                  // do the common load
        enc.println("=========================");
        enc.println();
        enc.println("FILENAME:  \"" + encName+ "\"");
        enc.println();
        enc.println("Electronic Codebook (ECB) Mode - ENCRYPTION");
        enc.println("Monte Carlo Test");
        enc.println();
        enc.println("Algorithm Name: " + cipherName);
        enc.println("Principal Submitter: " + SUBMITTER);
        enc.println();
        
        dec.println();
        dec.println("=========================");
        dec.println();
        dec.println("FILENAME:  \"" + decName+ "\"");
        dec.println();
        dec.println("Electronic Codebook (ECB) Mode - DECRYPTION");
        dec.println("Monte Carlo Test");
        dec.println();
        dec.println("Algorithm Name: " + cipherName);
        dec.println("Principal Submitter: " + SUBMITTER);
        dec.println();

        int k;
//        boolean useIJCE = false;
        boolean useIJCE = true;
        if (useReflection) {
            try {
//                for (k = 128; k < 257; k += 64) ecbForKeyReflect(k, enc, dec);
                for (k = 0; k < keys.length; k++)
                    ecbForKeyReflect(keys[k], enc, dec);
                useIJCE = false;
            } catch (IllegalAccessException ex1) {
                // soemthing wrong happened while invoking a known method in
                // *_Algorithm. revert to IJCE
                notify("Exception while invoking a method in " + cipherName +
                    "_Algorithm class");
//                useIJCE = true;
            } catch (InvocationTargetException ex2) {
                // no point trying IJCE API. problem is code/data specific
                halt("Exception encountered in a " + cipherName +
                    "_Algorithm method:\n" + ex2.getMessage());
                useIJCE = false;
            }
        }
        if (useIJCE)                                     // use the I/JCE API
//            for (k = 128; k < 257; k += 64) ecbForKeyIjce(k, enc, dec);
            for (k = 0; k < keys.length; k++) ecbForKeyIjce(keys[k], enc, dec);

        enc.println("==========");
        dec.println("==========");
        
        enc.close();
        dec.close();
    }

    void ecbForKeyReflect (int keysize, PrintWriter enc, PrintWriter dec)
    throws IllegalAccessException, InvocationTargetException {
        notify("Processing MCT in ECB mode (long); key size: " + keysize);
        notify("Using Reflection API methods");

        enc.println("==========");
        enc.println();
        enc.println("KEYSIZE=" + keysize);
        enc.println();
        
        dec.println("==========");
        dec.println();
        dec.println("KEYSIZE=" + keysize);
        dec.println();

        Object[] args = {};            //actual arguments
        int keylen = keysize / 8;      // number of bytes in user key
        byte[] keyMaterial = new byte[keylen];
        // cipher block size in bytes
        int size = ((Integer) blockSize.invoke(null, args)).intValue();
        byte[] pt = new byte[size];    // plaintext
        byte[] cpt;                    // computed plaintext
        byte[] ct;                     // ciphertext @round j
        byte[] ct_1;                   // ciphertext @round j-1
        int j, k, count;               // temp vars
        String ks, cts; // hexadecimal strings used more than once
        Object skeys;                  // algorithm secret key

        // step 1 (both). will use all zeroes.
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(pt);

        for (int i = 0; i < 400; i++) {                      // step 2 (both)
            ks = Hex.toString(keyMaterial);
            args = new Object[] { keyMaterial };
            skeys = makeKey.invoke(null, args);
            keyCount++;

            //...............................................................
            // Encryption
            //...............................................................
            enc.println("I="   + i);                              // step 2.a
            enc.println("KEY=" + ks);
            enc.println("PT="  + Hex.toString(pt));
            args = new Object[] {pt, new Integer(0), skeys};
            ct_1 = (byte[]) encrypt.invoke(null, args);           // step 2.b
            for (j = 1; j < 9999; j++) {
                args[0] = ct_1;
                ct_1 = (byte[]) encrypt.invoke(null, args);
                encBlocks++;
            }
            args[0] = ct_1;
            ct = (byte[]) encrypt.invoke(null, args);
            encBlocks++;
            cts = Hex.toString(ct);
            enc.println("CT=" + cts);                             // step 2.c

            //...............................................................
            // Decryption
            //...............................................................
            dec.println("I="   + i);                              // step 2.a
            dec.println("KEY=" + ks);
            dec.println("CT="  + cts);
            args[0] = ct;
            cpt = (byte[]) decrypt.invoke(null, args);           // step 2.b
            decBlocks++;
            for (j = 1; j < 10000; j++) {
                args[0] = cpt;
                cpt = (byte[]) decrypt.invoke(null, args);
                decBlocks++;
            }
            dec.println("PT=" + Hex.toString(cpt));               // step 2.c

            if (! ArrayUtil.areEqual(pt, cpt)) {    // check if results match
                enc.println(" *** ERROR ***");
                dec.println(" *** ERROR ***");
                halt("ECB Encryption/Decryption mismatch");
            }
            enc.println();
            dec.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            j = 0;                                         // step 2.d (both)
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= ct_1[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= ct[k++];

            System.arraycopy(ct, 0, pt, 0, size);          // step 2.e (both)
        }
    }

    void ecbForKeyIjce (int keysize, PrintWriter enc, PrintWriter dec)
    throws KeyException {
        notify("Processing MCT in ECB mode (long); key size: " + keysize);
        notify("Using IJCE API methods");

        enc.println("==========");
        enc.println();
        enc.println("KEYSIZE=" + keysize);
        enc.println();
        
        dec.println("==========");
        dec.println();
        dec.println("KEYSIZE=" + keysize);
        dec.println();

        int keylen = keysize / 8;      // number of bytes in user key
        byte[] keyMaterial = new byte[keylen];
        SecretKey key;                 // algorithm secret key object
        int size = cipher.blockSize(); // cipher block size in bytes
        byte[] pt = new byte[size];    // plaintext
        byte[] cpt;                    // computed plaintext
        byte[] ct;                     // ciphertext @round 9999
        byte[] ct_1;                   // ciphertext @round 9998
        int j, k, count;               // temp vars
        String ks, cts; // hexadecimal strings used more than once

        // step 1 (both). will use all zeroes.
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(pt);

        for (int i = 0; i < 400; i++) {                      // step 2 (both)
            ks = Hex.toString(keyMaterial);
            key = new MCT_Key(keyMaterial);

            //...............................................................
            // Encryption
            //...............................................................
            enc.println("I="   + i);                              // step 2.a
            enc.println("KEY=" + ks);
            enc.println("PT="  + Hex.toString(pt));
            cipher.initEncrypt(key);
            keyCount++;
            ct_1 = cipher.crypt(pt);                              // step 2.b
            encBlocks++;
            for (j = 1; j < 9999; j++) {
                ct_1 = cipher.crypt(ct_1);
                encBlocks++;
            }
            ct = cipher.crypt(ct_1);
            encBlocks++;
            cts = Hex.toString(ct);
            enc.println("CT=" + cts);                             // step 2.c

            //...............................................................
            // Decryption
            //...............................................................
            dec.println("I="   + i);                              // step 2.a
            dec.println("KEY=" + ks);
            dec.println("CT="  + cts);
            cipher.initDecrypt(key);
            keyCount++;
            cpt = cipher.crypt(ct);                               // step 2.b
            decBlocks++;
            for (j = 1; j < 10000; j++) {
                cpt = cipher.crypt(cpt);
                decBlocks++;
            }
            dec.println("PT=" + Hex.toString(cpt));               // step 2.c

            if (! ArrayUtil.areEqual(pt, cpt)) {    // check if results match
                enc.println(" *** ERROR ***");
                dec.println(" *** ERROR ***");
                halt("ECB Encryption/Decryption mismatch");
            }
            enc.println();
            dec.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            j = 0;                                         // step 2.d (both)
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= ct_1[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= ct[k++];

            System.arraycopy(ct, 0, pt, 0, size);          // step 2.e (both)
        }
    }


// CBC Monte Carlo Tests
//...........................................................................

    void cbcMCT (String encName, String decName) throws KeyException {
        cbcEncrypt(encName);
        cbcDecrypt(decName);
    }


// CBC-Encryption methods
//...........................................................................

    void cbcEncrypt (String encName) throws KeyException {
        PrintWriter pw = null;    // instantiate a PrintWriter for Encryption
        File f = new File(destination, encName);
        try { pw = new PrintWriter(new FileWriter(f) , true); }
        catch (IOException ex1) {
            halt("Unable to initialize <" + encName + "> as a Writer:\n" +
                ex1.getMessage());
        }
        pw.println();
        pw.println("=========================");
        pw.println();
        pw.println("FILENAME:  \"" + encName+ "\"");
        pw.println();
        pw.println("Cipher Block Chaining (CBC) Mode - ENCRYPTION");
        pw.println("Monte Carlo Test");
        pw.println();
        pw.println("Algorithm Name: " + cipherName);
        pw.println("Principal Submitter: " + SUBMITTER);
        pw.println();

        int k;
//        boolean useIJCE = false;
        boolean useIJCE = true;
        if (useReflection) {
            try {
//                for (k = 128; k < 257; k += 64) cbcEncForKeyReflect(k, pw);
                for (k = 0; k < keys.length; k++)
                    cbcEncForKeyReflect(keys[k], pw);
                useIJCE = false;
            } catch (IllegalAccessException ex1) {
                // soemthing wrong happened while invoking a known method in
                // *_Algorithm. revert to IJCE
                notify("Exception while invoking a method in " + cipherName +
                    "_Algorithm class");
//                useIJCE = true;
            } catch (InvocationTargetException ex2) {
                // no point trying other API. problem lies with code/data
                halt("Exception encountered in a " + cipherName +
                    "_Algorithm method:\n" + ex2.getMessage());
                useIJCE = false;
            }
        }
        if (useIJCE)                                     // use the I/JCE API
//            for (k = 128; k < 257; k += 64) cbcEncForKeyIjce(k, pw);
            for (k = 0; k < keys.length; k++) cbcEncForKeyIjce(keys[k], pw);

        pw.println("==========");
        pw.close();
    }

    void cbcEncForKeyReflect (int keysize, PrintWriter pw)
    throws IllegalAccessException, InvocationTargetException {
        notify("Processing MCT in CBC-Encrypt mode (long); key size: " + keysize);
        notify("Using Reflection API methods");

        pw.println("==========");
        pw.println();
        pw.println("KEYSIZE=" + keysize);
        pw.println();

        Object[] args = {};            //actual arguments
        int keylen = keysize / 8;      // number of bytes in user key material
        byte[] keyMaterial = new byte[keylen];
        int size = ((Integer) blockSize.invoke(null, args)).intValue();
        byte[] pt = new byte[size];    // plaintext
        byte[] ct = new byte[size];    // ciphertext
        byte[] iv = new byte[size];    // initialization vector
        int j, k, count;               // temp vars
        Object skeys;                  // algorithm secret key

        // step 1. will use all zeroes.
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(iv);
//        rand.nextBytes(pt);

        // we do this cause we don't distinguish between j = 0 or other
        // in fact we don't need it at all if we start with zero values
        System.arraycopy(iv, 0, ct, 0, size);

        for (int i = 0; i < 400; i++) {                             // step 2
                        // step 2.a is implicit since we're handling cv as iv
            pw.println("I="   + i);                               // step 2.b
            pw.println("KEY=" + Hex.toString(keyMaterial));
            pw.println("IV="  + Hex.toString(iv));
            pw.println("PT="  + Hex.toString(pt));

            args = new Object[] { keyMaterial };
            skeys = makeKey.invoke(null, args);  // the cipher's session keys
            keyCount++;

            args = new Object[3];
            args[1] = new Integer(0);
            args[2] = skeys;

            for (j = 0; j < 10000; j++) {                         // step 2.c
                for (k = 0; k < size; k++) iv[k] ^= pt[k];      // step 2.c.i
                System.arraycopy(ct, 0, pt, 0, size); // copy ct@(j-1) into pt
                args[0] = iv;
                ct = (byte[]) encrypt.invoke(null, args);      // step 2.c.ii
                encBlocks++;
                System.arraycopy(ct, 0, iv, 0, size);        // set new iv/cv
            }
            pw.println("CT=" + Hex.toString(ct));                 // step 2.d
            pw.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            //
            // remember: we keep ct@(j-1) values in pt...
            j = 0;                                                // step 2.e
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= pt[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= ct[k++];
        }
    }

    void cbcEncForKeyIjce (int keysize, PrintWriter pw) throws KeyException {
        notify("Processing MCT in CBC-Encrypt mode (long); key size: " + keysize);
        notify("Using IJCE API methods");

        pw.println("==========");
        pw.println();
        pw.println("KEYSIZE=" + keysize);
        pw.println();

        int keylen = keysize / 8;      // number of bytes in user key material
        byte[] keyMaterial = new byte[keylen];
        int size = cipher.blockSize(); // cipher block size in bytes
        byte[] pt = new byte[size];    // plaintext
        byte[] ct = new byte[size];    // ciphertext
        byte[] iv = new byte[size];    // initialization vector
        int j, k, count;               // temp vars
        SecretKey key;                 // algorithm secret key object

        // step 1. will use all zeroes.
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(iv);
//        rand.nextBytes(pt);

        // we do this cause we don't distinguish between j = 0 or other
        // in fact we don't need it at all if we start with zero values
        System.arraycopy(iv, 0, ct, 0, size);

        for (int i = 0; i < 400; i++) {                             // step 2
                        // step 2.a is implicit since we're handling cv as iv
            pw.println("I="   + i);                               // step 2.b
            pw.println("KEY=" + Hex.toString(keyMaterial));
            pw.println("IV="  + Hex.toString(iv));
            pw.println("PT="  + Hex.toString(pt));

            key = new MCT_Key(keyMaterial);
            cipher.initEncrypt(key);
            keyCount++;
            for (j = 0; j < 10000; j++) {                         // step 2.c
                for (k = 0; k < size; k++) iv[k] ^= pt[k];      // step 2.c.i
                System.arraycopy(ct, 0, pt, 0, size); // copy ct@(j-1) into pt
                ct = cipher.crypt(iv);                         // step 2.c.ii
                encBlocks++;
                System.arraycopy(ct, 0, iv, 0, size);        // set new iv/cv
            }
            pw.println("CT=" + Hex.toString(ct));                 // step 2.d
            pw.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            //
            // remember: we keep ct@(j-1) values in pt...
            j = 0;                                                // step 2.e
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= pt[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= ct[k++];
        }
    }


// CBC-Decryption methods
//...........................................................................
    
    void cbcDecrypt (String decName) throws KeyException {
        PrintWriter pw = null;    // instantiate a PrintWriter for Encryption
        File f = new File(destination, decName);
        try { pw = new PrintWriter(new FileWriter(f) , true); }
        catch (IOException ex1) {
            halt("Unable to initialize <" + decName + "> as a Writer:\n" +
                ex1.getMessage());
        }
        pw.println();
        pw.println("=========================");
        pw.println();
        pw.println("FILENAME:  \"" + decName+ "\"");
        pw.println();
        pw.println("Cipher Block Chaining (CBC) Mode - DECRYPTION");
        pw.println("Monte Carlo Test");
        pw.println();
        pw.println("Algorithm Name: " + cipherName);
        pw.println("Principal Submitter: " + SUBMITTER);
        pw.println();

        int k;
        boolean useIJCE = false;
        if (useReflection) {
            try { for (k = 128; k < 257; k += 64) cbcDecForKeyReflect(k, pw); }
            catch (IllegalAccessException ex1) {
                // soemthing wrong happened while invoking a known method in
                // *_Algorithm. revert to IJCE
                notify("Exception while invoking a method in " + cipherName +
                    "_Algorithm class");
                useIJCE = true;
            }
            catch (InvocationTargetException ex2) {
                // no point trying other API. problem lies with code/data
                halt("Exception encountered in a " + cipherName +
                    "_Algorithm method:\n" + ex2.getMessage());
            }
        }
        if (useIJCE)                                     // use the I/JCE API
            for (k = 128; k < 257; k += 64) cbcDecForKeyIjce(k, pw);

        pw.println("==========");
        pw.close();
    }

    void cbcDecForKeyReflect (int keysize, PrintWriter pw)
    throws IllegalAccessException, InvocationTargetException {
        notify("Processing MCT in CBC-Decrypt mode (long); key size: " + keysize);
        notify("Using Reflection API methods");
        
        pw.println("==========");
        pw.println();
        pw.println("KEYSIZE=" + keysize);
        pw.println();

        Object[] args = {};            //actual arguments
        int keylen = keysize / 8;      // number of bytes in user key material
        byte[] keyMaterial = new byte[keylen];
        int size = ((Integer) blockSize.invoke(null, args)).intValue();
        byte[] pt = new byte[size];    // plaintext
        byte[] ct = new byte[size];    // ciphertext
        byte[] iv = new byte[size];    // initialization vector
        int j, k, count;               // temp vars
        Object skeys;                  // algorithm secret key object

        // step 1. will use all zeroes.
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(iv);
//        rand.nextBytes(ct);

        for (int i = 0; i < 400; i++) {                             // step 2
                        // step 2.a is implicit since we're handling cv as iv
            pw.println("I="   + i);                               // step 2.b
            pw.println("KEY=" + Hex.toString(keyMaterial));
            pw.println("IV="  + Hex.toString(iv));
            pw.println("CT="  + Hex.toString(ct));

            args = new Object[] { keyMaterial };
            skeys = makeKey.invoke(null, args);  // the cipher's session keys
            keyCount++;

            args = new Object[3];
            args[1] = new Integer(0);
            args[2] = skeys;

            for (j = 0; j < 10000; j++) {                         // step 2.c
                args[0] = ct;
                pt = (byte[]) decrypt.invoke(null, args); // steps 2.c.i + ii
                decBlocks++;
                for (k = 0; k < size; k++) pt[k] ^= iv[k];    // step 2.c.iii
                System.arraycopy(ct, 0, iv, 0, size);          // step 2.c.iv
                System.arraycopy(pt, 0, ct, 0, size);
            }
            pw.println("PT=" + Hex.toString(pt));                 // step 2.d
            pw.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            //
            // remember: iv contains values of pt@(j-1)
            j = 0;                                                // step 2.e
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= iv[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= pt[k++];
        }
    }

    void cbcDecForKeyIjce (int keysize, PrintWriter pw) throws KeyException {
        notify("Processing MCT in CBC-Decrypt mode (long); key size: " + keysize);
        notify("Using IJCE API methods");
        
        pw.println("==========");
        pw.println();
        pw.println("KEYSIZE=" + keysize);
        pw.println();

        int keylen = keysize / 8;      // number of bytes in user key material
        byte[] keyMaterial = new byte[keylen];
        int size = cipher.blockSize(); // cipher block size in bytes
        byte[] pt = new byte[size];    // plaintext
        byte[] ct = new byte[size];    // ciphertext
        byte[] iv = new byte[size];    // initialization vector
        int j, k, count;               // temp vars
        SecretKey key;                 // algorithm secret key object

        // step 1. use all-zeroes values
//        rand.nextBytes(keyMaterial);
//        rand.nextBytes(iv);
//        rand.nextBytes(ct);

        for (int i = 0; i < 400; i++) {                             // step 2
                        // step 2.a is implicit since we're handling cv as iv
            pw.println("I="   + i);                               // step 2.b
            pw.println("KEY=" + Hex.toString(keyMaterial));
            pw.println("IV="  + Hex.toString(iv));
            pw.println("CT="  + Hex.toString(ct));

            key = new MCT_Key(keyMaterial);
            cipher.initDecrypt(key);
            keyCount++;
            for (j = 0; j < 10000; j++) {                         // step 2.c
                pt = cipher.crypt(ct);                // steps 2.c.i + 2.c.ii
                decBlocks++;
                for (k = 0; k < size; k++) pt[k] ^= iv[k];    // step 2.c.iii
                System.arraycopy(ct, 0, iv, 0, size);          // step 2.c.iv
                System.arraycopy(pt, 0, ct, 0, size);
            }
            pw.println("PT=" + Hex.toString(pt));                 // step 2.d
            pw.println();

            // may throw ArrayIndexOutOfBoundsException with
            // non-AES ciphers; ie. those for which:
            // keylen < size || keylen > 2*size
            //
            // remember: iv contains values of pt@(j-1)
            j = 0;                                                // step 2.e
            if (keylen > size) {
                count = keylen - size; 
                k = size - count;
                while (j < count) keyMaterial[j++] ^= iv[k++];
            }
            k = 0;
            while (j < keylen) keyMaterial[j++] ^= pt[k++];
        }
    }


// ==========================================================================
// MCT_Key inner class
// ==========================================================================

    final class MCT_Key implements SecretKey
    {
        byte[] key; // copy of user supplied key material

        public MCT_Key (byte[] data) { key = (byte[]) data.clone(); }
    //
    // java.security.Key methods
    //
        public String getAlgorithm() { return "<ANY>"; }
        public String getFormat() { return "RAW"; }
        public byte[] getEncoded() { return (byte[]) key.clone(); }
    }
}
