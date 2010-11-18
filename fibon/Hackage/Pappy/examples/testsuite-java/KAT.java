// $Id: KAT.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: KAT.java,v $
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
// Revision 1.6  2000/08/17 11:41:04  edwin
// java.* -> xjava.*
//
// Revision 1.5  1998/03/13 11:01:25  raif
// *** empty log message ***
//
// Revision 1.4.1  1998/03/13  raif
// + added support for _Algorithm implementations with variable block size.
//
// Revision 1.4  1998/02/28 07:11:22  raif
// *** empty log message ***
//
// Revision 1.3.1  1998/02/28  raif
// + fixed a bug that appeared when IJCE API was used.
// + added support for a user-defined Provider and key lengths.
//
// Revision 1.3  1998/02/08 21:04:45  raif
// *** empty log message ***
//
// Revision 1.2.1  1998/02/09  raif
// + fixed the VERSION string.
//
// Revision 1.2  1998/01/17 04:37:43  raif
// *** empty log message ***
//
// Revision 1.1.1  1998/01/16 raif
// + removed duplicate para in dox.
// + corrected spelling of some method names.
//
// Revision 1.1  1998/01/15 20:30:46  raif
// *** empty log message ***
//
// Revision 0.1  1998/01/13  raif
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
 * and exercises Known Answer Tests data for both Variable Key and Variable
 * Text suites.<p>
 *
 * KAT's output file format is in conformance with the layout described in
 * Section 3 of NIST's document "Description of Known Answer Tests and Monte
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
 * The duality of functionalities are there for performance reasons since
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
public final class KAT
{
// main method
//...........................................................................
    
    public static void main (String[] args) {
        System.out.println(
            "NIST Known Answer Tests data generator/exerciser\n\n" +
            VERSION + "\n" +
            "Copyright (c) 1998 Systemics Ltd. on behalf of\n" +
            "the Cryptix Development Team.  All rights reserved.\n\n");
        KAT cmd = new KAT();
        cmd.processOptions(args);
        cmd.run();
    }


// Fields & constants
//...........................................................................

    static final String VERSION = "$Revision: 1.1 $";
    static final String SUBMITTER = "<as stated on the submission cover sheet>";

    // current values of switches as set from the command line arguments
    boolean varKey = false ;  // -k  generate variable-key data
    boolean varText = false ; // -t  generate variable-text data
    String dirName = null;    // -d  output directory if != user.dir
    String keylengths = null; // -l  comma-separated key lengths

    String provider = null;   // provider name if cipherName != provider name
    String cipherName = null; // cipher algorithm name, default == provider
    File destination = null;  // destination directory File object
    int[] keys = new int[] {128, 192, 256}; // key-length values to test with
    
    final String vkFileName = "ecb_vk.txt"; // variable-key output filename
    final String vtFileName = "ecb_vt.txt"; // variable-text output filename

    // statistics fields
    long encBlocks; // total count of encrypted blocks
    long decBlocks; // total count of decrypted blocks
    long keyCount;  // total count of key creation requests

    Class algorithm = null;        // fields for using Reflection API methods
    Method blockSize = null;
    Method makeKey = null;
    Method encrypt = null;
    Method decrypt = null;

    Cipher cipher = null;                 // field for using IJCE API methods

    boolean useReflection = true;  // by default we'll use the Reflection API


// Own methods
//...........................................................................

    /** Process command line arguments and initialise instance fields. */
    private void processOptions (String[] args) {
        int argc = args.length;
        if (argc == 0) printUsage();
        System.out.println(
            "(type \"java cryptix.tools.KAT\" with no arguments for help)\n\n");
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
            
            if (cmd.startsWith("-k")) {
                varKey = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-t")) {
                varText = true;
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

        if (!varKey && !varText) varKey = varText = true;

        if (dirName == null) dirName = System.getProperty("user.dir");
        destination = new File(dirName);
        if (! destination.isDirectory())
            halt("Destination <" + destination.getName() +
                "> is not a directory");

        // now instantiate both Reflection and IJCE fields
        // start with Reflection
        // to use reflection API we load the *_Algorithm class if one exists
        // look for class fully named <provider>.<cipherName>_Algorithm
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
    static void notify (String s) { System.out.println("KAT: " + s + "..."); }
    
    /** write help text and quit. */
    void printUsage() {
        System.out.println(
        "NAME\n" +
        "  KAT: A Known Answer Tests data generator/exerciser for any block\n" +
        "  cipher algorithm.\n\n" +
        "SYNTAX\n" +
        "  java cryptix.tools.KAT\n" +
        "    [ -k | -t ]\n" +
        "    [ -l <comma-separated-key-lengths>]\n" +
        "    [ -d <output-directory>]\n" +
        "    [ -p <provider>]\n" +
        "    <cipher>\n\n" +
        "DESCRIPTION\n" +
        "  For a designated symmetric block cipher algorithm, KAT generates\n" +
        "  and exercises Known Answer Tests data for both Variable Key and\n" +
        "  Variable Text suites.\n" +
        "  KAT's output file format conforms to the layout described in\n" +
        "  Section 3 of NIST's document \"Description of Known Answer Tests\n" +
        "  and Monte Carlo Tests for Advanced Encryption Standard (AES)\n" +
        "  Candidate Algorithm Submissions\" dated January 7, 1998.\n\n" +
        "OPTIONS\n" +
        "  -k   Generate data for variable-key tests only.  By default KAT\n" +
        "       generates both variable-key and variable-text test uites.\n\n" +
        "  -t   Generate data for variable-text tests only. By default KAT\n" +
        "       generates both variable-key and variable-text test suites.\n\n" +
        "  -l <comma-separated-key-lengths>\n" +
        "       Comma separated list (maximum of three) of key lengths to use\n" +
        "       for the tests.  If omitted, the following three values are\n" +
        "       assumed: 128, 192 and 256.\n\n" +
        "  -d <output-directory>\n" +
        "       Pathname of the directory where output files: \"ecb_vk.txt\"\n" +
        "       and \"ecb_vt.txt\" will be generated.  If this destination\n" +
        "       directory is not specified, those files will be placed in\n" +
        "       the current user directory.\n\n" +
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
            if (varKey)  vkKAT(vkFileName);
            if (varText) vtKAT(vtFileName);
        }
        catch (KeyException ex1) {
            ex1.printStackTrace();
            halt("Key Exception encountered:\n" + ex1.getMessage());
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


// Variable Key KAT methods
//...........................................................................

    void vkKAT (String fileName) throws KeyException {
        File f = new File(destination, fileName);
        PrintWriter out = null;
        try { out = new PrintWriter(new FileWriter(f) , true); }
        catch (IOException ex1) {
            halt("Unable to initialize <" + fileName + "> as a Writer:\n" +
                ex1.getMessage());
        }
        out.println();
        out.println("=========================");
        out.println();
        out.println("FILENAME:  \"" + fileName + "\"");
        out.println();
        out.println("Electronic Codebook (ECB) Mode");
        out.println("Variable Key Known Answer Tests");
        out.println();
        out.println("Algorithm Name: " + cipherName);
        out.println("Principal Submitter: " + SUBMITTER);
        out.println();

        int k;
//        boolean useIJCE = false;
        boolean useIJCE = true;
        if (useReflection) {
            try {
//                for (k = 128; k < 257; k += 64) vkForKeyReflect(k, out);
                for (k = 0; k < keys.length; k++) vkForKeyReflect(keys[k], out);
                useIJCE = false;
            } catch (IllegalAccessException ex1) {
                // soemthing wrong happened while invoking a known method in
                // *_Algorithm. revert to IJCE
                notify("Exception while invoking a method in " + cipherName +
                    "_Algorithm class");
//                useIJCE = true;
            } catch (InvocationTargetException ex3) {
                // no point trying IJCE API. problem lies with code/data
                halt("Exception encountered in a " + cipherName +
                    "_Algorithm method:\n" + ex3.getMessage());
                useIJCE = false;
            }
        }
        if (useIJCE)                                     // use the I/JCE API
//            for (k = 128; k < 257; k += 64) vkForKeyIjce(k, out);
            for (k = 0; k < keys.length; k++) vkForKeyIjce(keys[k], out);

        out.println("==========");
        out.close();
    }

    void vkForKeyReflect (int keysize, PrintWriter out)
    throws IllegalAccessException, InvocationTargetException {
        notify("Generating and testing Variable Key KAT (short); key size: " +
            keysize);
        notify("Using Reflection API methods");

        Object[] args = {};         //actual arguments
        int count = keysize / 8;    // number of bytes in key material
        int size = ((Integer) blockSize.invoke(null, args)).intValue();
        byte[] keyMaterial = new byte[count];
        byte[] pt = new byte[size]; // plaintext
        byte[] cpt;                 // computed plaintext
        byte[] ct;                  // ciphertext
        int round = 0;              // current round ord. number
        int i, j;                   // temp vars
        Object skeys;               // algorithm secret key
        
        out.println("==========");
        out.println();
        out.println("KEYSIZE=" + keysize);
        out.println();
        out.println("PT=" + Hex.toString(pt));
        out.println();

        // The key bytes are organised and numbered as follows:
        //
        // |<- byte 0 ->|<- byte 1 ->|<- ... ->|<- byte n ->|
        // |<------------- bit_(n-1) to bit_0 ------------->|
        //
        for (i = 0; i < count; i++) {
            for (j = 0; j < 8; j++) {
                round++;
                out.println("I=" + round);
                keyMaterial[i] = (byte)(1 << (7 - j));
                out.println("KEY=" + Hex.toString(keyMaterial));

                args = new Object[] { keyMaterial };
                skeys = makeKey.invoke(null, args);
                keyCount++;

                args = new Object[] {pt, new Integer(0), skeys};
                ct = (byte[]) encrypt.invoke(null, args);
                encBlocks++;

                out.print("CT=" + Hex.toString(ct));

                args[0] = ct;
                cpt = (byte[]) decrypt.invoke(null, args);
                decBlocks++;

                if (! ArrayUtil.areEqual(pt, cpt))  // check if results match
                    out.print(" *** ERROR ***");

                out.println();
                out.println();
            }
            keyMaterial[i] = 0x00;
        }
    }

    void vkForKeyIjce (int keysize, PrintWriter out)
    throws KeyException {
        notify("Generating and testing Variable Key KAT (short); key size: " +
            keysize);
        notify("Using IJCE API methods");

        int count = keysize / 8; // number of bytes in key material
        int size = cipher.blockSize();
        byte[] keyMaterial = new byte[count];
        byte[] pt = new byte[size]; // plaintext
        byte[] ct;                  // ciphertext
        byte[] cpt;                 // computed plaintext
        int round = 0;              // current round ord. number
        int i, j;                   // temp vars
        SecretKey key;              // algorithm secret key

        out.println("==========");
        out.println();
        out.println("KEYSIZE=" + keysize);
        out.println();
        out.println("PT=" + Hex.toString(pt));
        out.println();

        // The key bytes are organised and numbered as follows:
        //
        // |<- byte 0 ->|<- byte 1 ->|<- ... ->|<- byte n ->|
        // |<------------- bit_(n-1) to bit_0 ------------->|
        //
        for (i = 0; i < count; i++) {
            for (j = 0; j < 8; j++) {
                round++;
                out.println("I=" + round);
                keyMaterial[i] = (byte)(1 << (7 - j));
                out.println("KEY=" + Hex.toString(keyMaterial));

                key = new KAT_Key(keyMaterial);

                cipher.initEncrypt(key);
                keyCount++;
                ct = cipher.crypt(pt);
                encBlocks++;
                out.print("CT=" + Hex.toString(ct));

                cipher.initDecrypt(key);
                keyCount++;
                cpt = cipher.crypt(ct);
                decBlocks++;

                if (! ArrayUtil.areEqual(pt, cpt))  // check if results match
                    out.print(" *** ERROR ***");

                out.println();
                out.println();
            }
            keyMaterial[i] = 0x00;
        }
    }


// Variable Text KAT methods
//...........................................................................

    void vtKAT (String fileName) throws KeyException {
        File f = new File(destination, fileName);
        PrintWriter out = null;
        try { out = new PrintWriter(new FileWriter(f) , true); }
        catch (IOException ex1) {
            halt("Unable to initialize <" + fileName + "> as a Writer:\n" +
                ex1.getMessage());
        }
        out.println();
        out.println("=========================");
        out.println();
        out.println("FILENAME:  \"" + fileName + "\"");
        out.println();
        out.println("Electronic Codebook (ECB) Mode");
        out.println("Variable Text Known Answer Tests");
        out.println();
        out.println("Algorithm Name: " + cipherName);
        out.println("Principal Submitter: " + SUBMITTER);
        out.println();

        // use reflection API to load the *_Algorithm class if one exists
        // look for class fully named <provider>.<cipherName>_Algorithm
        int k;
//        boolean useIJCE = false;
        boolean useIJCE = true;
        if (useReflection) {
            try {
//                for (k = 128; k < 257; k += 64) vtForKeyReflect(k, out);
                for (k = 0; k < keys.length; k++) vtForKeyReflect(keys[k], out);
                useIJCE = false;
            } catch (IllegalAccessException ex1) {
                // soemthing wrong happened while invoking a known method in
                // *_Algorithm. revert to IJCE
                notify("Exception while invoking a method in " + cipherName +
                    "_Algorithm class");
//                useIJCE = true;
            } catch (InvocationTargetException ex3) {
                // no point trying other API. problem lies with code/data
                halt("Exception encountered in a " + cipherName +
                    "_Algorithm method:\n" + ex3.getMessage());
                useIJCE = false;
            }
        }
        if (useIJCE)                                     // use the I/JCE API
//            for (k = 128; k < 257; k += 64) vtForKeyIjce(k, out);
            for (k = 0; k < keys.length; k++) vtForKeyIjce(keys[k], out);

        out.println("==========");
        out.close();
    }

    void vtForKeyReflect (int keysize, PrintWriter out)
    throws IllegalAccessException, InvocationTargetException {
        notify("Generating and testing Variable Text KAT (short); key size: " +
            keysize);
        notify("Using Reflection API methods");

        Object[] args = {};             //actual arguments
        byte[] keyMaterial = new byte[keysize / 8];
        int count = ((Integer) blockSize.invoke(null, args)).intValue();
        byte[] pt = new byte[count];    // plaintext
        byte[] ct;                      // ciphertext
        byte[] cpt;                     // computed plaintext
        int round = 0;                  // current round ord. number
        int i, j;                       // temp vars

        args = new Object[] { keyMaterial };
        Object skeys = makeKey.invoke(null, args); // the cipher's session keys
        keyCount++;

        out.println("==========");
        out.println();
        out.println("KEYSIZE=" + keysize);
        out.println();
        out.println("KEY=" + Hex.toString(keyMaterial));
        out.println();

        args = new Object[3];
        args[1] = new Integer(0);
        args[2] = skeys;

        // The plaintext bytes are organised and numbered as follows:
        //
        // |<- byte 0 ->|<- byte 1 ->|<- ... ->|<- byte n ->|
        // |<------------- bit_(n-1) to bit_0 ------------->|
        //
        for (i = 0; i < count; i++) {
            for (j = 0; j < 8; j++) {
                round++;
                out.println("I=" + round);
                pt[i] = (byte)(1 << (7 - j));
                out.println("PT=" + Hex.toString(pt));

                args[0] = pt;
                ct = (byte[]) encrypt.invoke(null, args);
                encBlocks++;

                out.print("CT=" + Hex.toString(ct));

                args[0] = ct;
                cpt = (byte[]) decrypt.invoke(null, args);
                decBlocks++;

                if (! ArrayUtil.areEqual(pt, cpt))  // check if results match
                    out.print(" *** ERROR ***");

                out.println();
                out.println();
            }
            pt[i] = 0x00;
        }
    }

    void vtForKeyIjce (int keysize, PrintWriter out) throws KeyException {
        notify("Generating and testing Variable Text KAT (short); key size: " +
            keysize);
        notify("Using IJCE API methods");

        byte[] keyMaterial = new byte[keysize / 8];
        int count = cipher.blockSize(); // the cipher's block size
        byte[] pt = new byte[count];    // plaintext
        byte[] ct;                      // ciphertext
        byte[] cpt;                     // computed plaintext
        int round = 0;                  // current round ord. number
        int i, j;                       // temp vars
        SecretKey key = new KAT_Key(keyMaterial);

        out.println("==========");
        out.println();
        out.println("KEYSIZE=" + keysize);
        out.println();
        out.println("KEY=" + Hex.toString(keyMaterial));
        out.println();

        // The plaintext bytes are organised and numbered as follows:
        //
        // |<- byte 0 ->|<- byte 1 ->|<- ... ->|<- byte n ->|
        // |<------------- bit_(n-1) to bit_0 ------------->|
        //
        for (i = 0; i < count; i++) {
            for (j = 0; j < 8; j++) {
                round++;
                out.println("I=" + round);
                pt[i] = (byte)(1 << (7 - j));
                out.println("PT=" + Hex.toString(pt));

                cipher.initEncrypt(key);
                keyCount++;
                ct = cipher.crypt(pt);
                encBlocks++;

                out.print("CT=" + Hex.toString(ct));

                cipher.initDecrypt(key);
                keyCount++;
                cpt = cipher.crypt(ct);
                decBlocks++;

                if (! ArrayUtil.areEqual(pt, cpt))  // check if results match
                    out.print(" *** ERROR ***");

                out.println();
                out.println();
            }
            pt[i] = 0x00;
        }
    }


// ==========================================================================
// KAT_Key inner class
// ==========================================================================

    final class KAT_Key implements SecretKey
    {
        byte[] key; // copy of user supplied key material

        public KAT_Key (byte[] data) { key = (byte[]) data.clone(); }
    //
    // java.security.Key methods
    //
        public String getAlgorithm() { return "<ANY>"; }
        public String getFormat() { return "RAW"; }
        public byte[] getEncoded() { return (byte[]) key.clone(); }
    }
}
