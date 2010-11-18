// $Id: Scar.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: Scar.java,v $
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
// Revision 1.4  2000/08/17 11:41:04  edwin
// java.* -> xjava.*
//
// Revision 1.3  1998/02/22 01:45:09  zox
// Changed few privates to publics and cut down unnecesary output so it can be
//   invoked from TestScar.
//
// Revision 1.2  1998/01/15 20:30:47  raif
// *** empty log message ***
//
// Revision 1.1.1  1998/01/12  raif
// + cosmetics.
//
// Revision 1.1  1997/12/30 11:05:36  raif
// *** empty log message ***
//
// Revision 1.4.1  1997/12/29  raif
// + modified to work with any security provider using the IJCE.
// + added debugging and tracing.
// + use SecureRandom to generate a random iv and use it as the first
//   encryption block when creating the archive.  Recommended since we're
//   using the cipher in CBC mode.  Doing so will generate a different
//   output for the same input and key.
// + renamed it 'scar' for Strong Cryptographic ARchiver!
// + use OpenPGP-like simple/salted/iterated S2K algorithms. Used the
//   draft-ietf-openpgp-formats.txt document dated November 97 for
//   implementation details.
// + embedded CryptorInputStream and CryptorOutputStream as inner classes
//   and changed their names to ScarInputStream and ScarOutputStream
//   respectively.
// + added support for individual user-specific properties file.
// + fixed some values that were platform specific.
// + made it final.
// + amended dox.
//
// Revision 1.4  1997/12/07 06:59:53  hopwood
// + Committed changes below.
//
// Revision 1.3.1  1997/12/04  hopwood
// + Changed variables to instance, not static.
// + Fixed some typos.
// + Added "to do" points in documentation (use random IV, iterate the
//   hash function, use salt). Also added disclaimer, since it won't be
//   cryptographically strong until these three points are fixed.
// + Noted that the format may change incompatibly.
// + Made this class non-public.
//
// Revision 1.3  1997/12/03 01:16:26  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/20 22:54:22  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.1  1997/11/07 14:32:46  raif
// *** empty log message ***
//
// $Endlog$
/*
 * Copyright (c) 1997, 1998 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */
package cryptix.tools;

import cryptix.CryptixException;
import cryptix.util.io.DosFilter;
import cryptix.util.checksum.PRZ24;
import cryptix.provider.key.RawKeyGenerator;

import java.io.CharConversionException;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.PushbackInputStream;
import xjava.security.Cipher;
import xjava.security.CipherInputStream;
import xjava.security.CipherOutputStream;
import xjava.security.FeedbackCipher;
import java.security.KeyException;
import xjava.security.KeyGenerator;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import xjava.security.SecretKey;
import xjava.security.WeakKeyException;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * A command line utility to (a) compress, encrypt and asciify files and/or
 * directories (with or without directory recursion), and (b) accomplish the
 * inverse with user-specified option for recreating a source tree directory.<p>
 *
 * Uses java.zip tools to deflate and inflate data, Cryptix IJCE for the cipher
 * and message digest (used to compute cipher keys from user plain ascii
 * passphrase) algorithms, and a PGP-style Base-64 armour with P. R. Zimmermann
 * 24-bit CRC method (PRZ24 class) for the [de-]asciification.<p>
 *
 * Hard-wired default values for cipher (Square) and Simple String To Key (S2K)
 * specifier with message digest (RIPEMD-160) algorithms are used. These and
 * other default values can be individually modified for each user by setting
 * the appropriate properties in a <i>scar.properties</i> file placed in the
 * user's home directory.<p>
 *
 * Current scar properties that the user can alter are:
 *      <dt>scar.header
 *      <dd>The text that will be enclosed between a pair of ----- to visually
 *          indicate the start of an asciified scar. Current default is "BEGIN
 *          SCAR ARCHIVE".
 *
 *      <dt>scar.comment
 *      <dd>A text that will follow the text "Comment: " in a line following
 *          scar version information in an asciified scar. Current default is
 *          "scar by Cryptix...".
 *
 *      <dt>scar.footer
 *      <dd>The text that will be enclosed between a pair of ----- to visually
 *          indicate the end of an asciified scar. Current default is "END SCAR
 *          ARCHIVE."
 *
 *      <dt>scar.cipher.algorithm
 *      <dd>The name of a symmetric cipher algorithm installed and accessible
 *          by the user Java VM. Current default is "Square".  Square is a
 *          symmetric block cipher algorithm developed by Joan Daemen
 *          <Daemen.J@banksys.com> and Vincent Rijmen
 *          <vincent.rijmen@esat.kuleuven.ac.be>.
 *
 *      <dt>scar.passphrase
 *      <dd>The text to use as the pass-phrase. This pass-phrase will be used
 *          as the basis for computing a session key. The algorithms used to
 *          generate a session key from the pass-phrase are an implementation
 *          of the proposed S2K Simple, Salted, Iterated and Salted-Iterated
 *          variations described in the OpenPGP IETF draft document dated
 *          November 1997.  The current default is "sub rosa."
 *
 *      <dt>scar.md.algorithm
 *      <dd>The Message Digest algorithm used in the S2K algorithms. "RIPEMD-160"
 *          Is the default. RIPEMD-160 is designed by Hans Dobbertin, Antoon
 *          Bosselaers and Bart Preneel.
 *
 *      <dt>scar.md.salt
 *      <dd>A salt value to use in S2K Salted and Iterated-Salted variants.
 *          Current value is "Cryptix Development Team".
 *
 *      <dt>scar.md.iterations
 *      <dd>A positive integer to use in S2K Iterated and Salted-Iterated
 *          variants. Current default value is 7.
 *
 * To do:<ul>
 *  <li>Add an option to allow use in distribution applications ('JAR' format
 *      with manifest file).
 *  <li> ...
 * </ul>
 * <strong>Note: <i>this is an alpha release of <i>scar</i>. The format of
 * encrypted archives may (and probably will) change incompatibly in future
 * releases.</i></strong>.<p>
 *
 * <b>Copyright</b> &copy; 1997, 1998
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
public class Scar
extends Thread
{
// Tracing and Debugging methods and fields
//...........................................................................

    public static boolean DEBUG = true;

    static int debuglevel;
    static final PrintWriter err = new PrintWriter(System.out, true);
    static void debug (String s) { if (DEBUG) err.println(">>> scar: " + s); }

    static final boolean TRACE = false;
    static final boolean IN = true, OUT = false;
    static void trace (boolean in, String s) {
        if (TRACE) err.println((in ? "==> " : "<== ") + s); }
    static void trace (String s) { if (TRACE) err.println("<=> " + s); }


// main method
//...........................................................................
    
    public static void main (String[] args)
    {
        System.out.println(
            "scar (Strong Cryptographic ARchiver)\n" +
            VERSION + "\n" +
            "Copyright (c) 1997, 1998 Systemics Ltd. on behalf of\n" +
            "the Cryptix Development Team.  All rights reserved.\n\n");
        Scar jc = new Scar();
        jc.processOptions(args);
        jc.run();
    }


// Constants and fields
//...........................................................................

    /** Current values of switches as set from the command line arguments. */
    private boolean             // switch
        asciify = false,        // -a  asciify
        decrypting = false,     // -d  decrypt
        encrypting = false,     // -e  encrypt
        recursion = false,      // -r  recurse directories
        useDirInfo = false,     // -u  use directory info
        verbose = false,        // -v  verbose
        wipeSource = false;     // -w  wipe source

    /** Configured values of some main parameters set during this instance. */
    private String
        cipherAlgorithm = null, // -c  cipher; property key "scar.cipher.algorithm"
        passPhrase = null,      // -p  passphrase; property key "scar.passphrase"
        mdAlgorithm = null,     // -m  message digest; property key "scar.md.algorithm"
        salt = null;            // -s  salt; property key "scar.md.salt"
    private int iterations;     // -i  md iteration count; property key "scar.md.iterations"
    private String
        input = null,
        output = null;

    /** Some local vars for file/stream pipelining. */
    private File
        inFile =  null,
        outFile = null,
        temp =    null,
        temp2 =   null;

    /** Local buffer to speed compression, read, and write operations. */
    private byte[] buffer = new byte[512];

    /**
     * A java.io.FilenameFilter to allow file name selection using DOS-style
     * wildcards ('*' and '?').
     */
    private DosFilter filter = new DosFilter();

    /** Number of files processed so far. */
    private int count = 0;

    /**
     * Magic string that will be written to archive file at creation time
     * and checked at decryption time to make sure the file was created
     * by us. Saves time if the file is not ours or was peoduced with a
     * different set of properties.
     */
    private static final String MAGIC_STRING = "Que du magnifique...";
    private static final byte[] MAGIC = MAGIC_STRING.getBytes();

    /**
     * User ResourceBundle file for his/her scar.properties. Only
     * user home directory is searched for this properties file.
     */
    PropertyResourceBundle properties;

    //
    // user specific properties and runtime jvm host-specific fields
    //

    /** User runtime jvm host file separator. */
    static String fs;

    /** Header info following ----- in an asciified scar file. */
    String header; // property key "scar.header"

    /** Footer info following ----- in an asciified scar file. */
    String footer; // property key "scar.footer"

    /** Comment data. */
    String comment; // property key "scar.comment"

    /** Source of randomness. */
    static final SecureRandom random = new SecureRandom();

    /** Default default values! */
    static final String DEFAULT_HEADER =      "BEGIN SCAR ARCHIVE";
    static final String DEFAULT_FOOTER =      "END SCAR ARCHIVE";
    static final String DEFAULT_COMMENT =     "scar by Cryptix...";
    static final String DEFAULT_CIPHER =      "Square";
    static final String DEFAULT_PASS_PHRASE = "sub rosa";
    static final String DEFAULT_MD =          "RIPEMD-160";
    static final String DEFAULT_SALT =        "BEGIN SCAR ARCHIVE";
    static final int DEFAULT_ITERATIONS =     7;
    


// Constructor
//...........................................................................

    public Scar ()
    {
        trace(IN, "Scar()");
        //
        // look for 'scar.properties' file in user.home
        //
        fs = System.getProperty("file.separator");
        String home = System.getProperty("user.home");
        try {                            // read user default scar properties
            String resource = home + "scar.properties";
            properties = new
                PropertyResourceBundle(new FileInputStream(resource));
        }
        catch (FileNotFoundException ex1) {
            debug("File \"scar.properties\" was not found in " + home +
                ". Using default properties");
            initDefaults();
            trace(OUT, "Scar()");
            return;
        }
        catch (IOException ex2) {
            debug("I/O exception occured while loading \"scar.properties\"" +
                " file. Using default properties");
            initDefaults();
            trace(OUT, "Scar()");
            return;
        }
        //
        // load properties setting hard-coded values if related
        // property key was not found in the 'scar.properties' file
        //
        try { header = properties.getString("scar.header"); }
        catch (MissingResourceException ex3) { header = DEFAULT_HEADER; }

        try { footer = properties.getString("scar.footer"); }
        catch (MissingResourceException ex4) { footer = DEFAULT_FOOTER; }

        try { comment = properties.getString("scar.comment"); }
        catch (MissingResourceException ex5) { comment = DEFAULT_COMMENT; }

        try { cipherAlgorithm = properties.getString("scar.cipher.algorithm"); }
        catch (MissingResourceException ex6) { cipherAlgorithm = DEFAULT_CIPHER; }

        try { passPhrase = properties.getString("scar.passphrase"); }
        catch (MissingResourceException ex7) { passPhrase = DEFAULT_PASS_PHRASE; }

        try { mdAlgorithm = properties.getString("scar.md.algorithm"); }
        catch (MissingResourceException ex8) { mdAlgorithm = DEFAULT_MD; }

        try { salt = properties.getString("scar.md.salt"); }
        catch (MissingResourceException ex9) { salt = DEFAULT_SALT; }

        try { iterations = Integer.parseInt(properties.getString("scar.md.iterations")); }
        catch (MissingResourceException ex10) { iterations = DEFAULT_ITERATIONS; }

        debug("Default properties [...");
        debug("      header line: \"-----" + header + "-----\"");
        debug("     comment line: \"Comment: " + comment + "\"");
        debug("      footer line: \"-----" + footer + "-----\"");
        debug(" cipher algorithm: \"" + cipherAlgorithm + "\"");
        debug("      pass-phrase: \"" + passPhrase + "\"");
        debug("   message digest: \"" + mdAlgorithm + "\"");
        debug("          md salt: \"" + salt + "\"");
        debug("    md iterations: " + iterations);
        debug("...]");

        trace(OUT, "Scar()");
    }


// Own methods
//...........................................................................

    /** Set default properties. */
    void initDefaults ()
    {
        trace(IN, "initDefaults()");

        header =          DEFAULT_HEADER;
        footer =          DEFAULT_FOOTER;
        comment =         DEFAULT_COMMENT;
        cipherAlgorithm = DEFAULT_CIPHER;
        passPhrase =      DEFAULT_PASS_PHRASE;
        mdAlgorithm =     DEFAULT_MD;
        salt =            DEFAULT_SALT;
        iterations =      DEFAULT_ITERATIONS;

        trace(OUT, "initDefaults()");
    }

    /** Process command line arguments. */
    public void processOptions (String[] args)
    {
        trace(IN, "processOptions()");

        int argc = args.length;

        debug("Command line arguments [...");
        for (int i = 0; i < argc; i++) debug(" args["+(i+1)+"]: " + args[i]);
        debug("...]");

        if (argc == 0) printUsage();
//        System.out.println(
//            "(type \"java cryptix.tools.Scar\" with no arguments for help)\n\n");

        Vector files = new Vector();
        int i = -1;
        String cmd = "";
        boolean next = true;
        filter.reset();

        // we'll use the next counter to count the number of crypto-related
        // args designated on the command line. If the count at the end of
        // processing the command line remains 0 then we'll issue a warning
        // to the user that it's bad practice to have all these arguments
        // used from values stored in the clear on the hard disk.
        int cargs = 0;

        while (true) {
            if (next) {
                i += 1;
                if (i >= argc)
                    break;
                else
                    cmd = args[i];
            } else
                cmd = "-" + cmd.substring(2);
            
            if (cmd.startsWith("-a")) {
                asciify = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-d")) {
                decrypting = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-e")) {
                encrypting = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-r")) {
                recursion = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-u")) {
                useDirInfo = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-v")) {
                verbose = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-w")) {
                wipeSource = true;
                next = (cmd.length() == 2);
            } else if (cmd.startsWith("-c")) {        // cipher algorithm
                cipherAlgorithm = args[i + 1];
                i += 1;
                next = true;
                cargs++;
            } else if (cmd.startsWith("-p")) {        // pass-phrase
                passPhrase = args[i + 1];
                i += 1;
                next = true;
                cargs++;
            } else if (cmd.startsWith("-m")) {        // md algorithm
                mdAlgorithm = args[i + 1];
                i += 1;
                next = true;
                cargs++;
            } else if (cmd.startsWith("-s")) {        // md salt
                salt = args[i + 1];
                i += 1;
                next = true;
                cargs++;
            } else if (cmd.startsWith("-i")) {        // md iterations
                iterations = Integer.parseInt(args[i + 1]);
                i += 1;
                next = true;
                cargs++;
            }
            //
            // in some cases java expands the input (if it consist of "*.*")
            // and sometimes it doesn't
            //
            else if (! files.contains(cmd))
                files.addElement(cmd);
        }
        if (decrypting) {
            if (encrypting || recursion || asciify)
                halt("Found at least one conflicting option to Decryption");
        } else
            encrypting = true;

        if (cipherAlgorithm.length() > 1 &&
                (cipherAlgorithm.startsWith("\"") ||
                cipherAlgorithm.startsWith("'")))
            cipherAlgorithm =
                cipherAlgorithm.substring(2, cipherAlgorithm.length() - 2);

        if (passPhrase.length() > 1 &&
                (passPhrase.startsWith("\"") || passPhrase.startsWith("'")))
            passPhrase = passPhrase.substring(2, passPhrase.length() - 2);

        if (mdAlgorithm.length() > 1 &&
                (mdAlgorithm.startsWith("\"") || mdAlgorithm.startsWith("'")))
            mdAlgorithm = mdAlgorithm.substring(2, mdAlgorithm.length() - 2);

        if (salt.length() > 1 &&
                (salt.startsWith("\"") || salt.startsWith("'")))
            salt = salt.substring(2, salt.length() - 2);

        // now the filenames
        files.trimToSize();
        if (files.size() == 0)          // neither input nor output specified
            halt("Missing <input> path-name");
        else if (files.size() == 1) {          // assume input only specified
            input = (String) files.elementAt(0);
            output = System.getProperty(
                "user.dir", new File("." + fs).getAbsolutePath());
        } else if (files.size() == 2) {    // both input and output specified
            input = (String) files.elementAt(0);
            output = (String) files.elementAt(1);
        } else
            halt("Too many files");

        if (input.startsWith("\"") || input.startsWith("'"))
            input = input.substring(2, input.length() - 2);

        // input may be a valid File object reference or a wildcard mask
        if (input.indexOf("*") != -1 || input.indexOf("?") != -1) {
            // it's a mask
            filter.setMask((new File(input)).getName());
            input =
                (new File((new File(input)).getAbsolutePath())).getParent();
        }
        if (output.startsWith("\"") || output.startsWith("'"))
            output = output.substring(2, output.length() - 2);

        if (encrypting)
            if (decrypting || useDirInfo || output == null)
                halt("Found at least one conflicting option to Encryption");

        inFile = new File(input); // Make sure specified source exists
        if (! inFile.exists())
            halt("Input <" + input + "> not found");

        if (! inFile.canRead()) // and is readable
            halt("Input <" + input + "> is unreadable");

        // finally shouldn't be a directory if decrypting
        if (decrypting && inFile.isDirectory()) halt(
            "Decryption required but input <" + input + "> is a directory");

        // output has to be a file when encrypting or
        // a directory when decrypting
        outFile = new File(output);
        if (encrypting && outFile.isDirectory())
            halt("Encryption required but output <" + output +
                "> is a directory");
        else if (decrypting && ! outFile.isDirectory())
            halt("Decryption required but output <" + output +
                "> is not a directory");

        if (cargs == 0) System.out.println(
            "WARNING:\n" +
            "  You did not specify at least one of: cipher algorithm, pass-phrase,\n" +
            "  message digest algorithm, message digest salt value or message\n" +
            "  digest iteration count; instead you are relying on default values\n" +
            "  for these arguments. Please note that it is bad practice not to\n" +
            "  vary at least one of those parameters. Failing to do so reduces\n" +
            "  the efforts of an attacker trying to decrypt your scar.");

        temp = getTempFile(); // create 1 temp file
        
        trace(OUT, "processOptions()");
    }

    /**
     * Print an error message to System.err and halts execution returning
     * -1 to the JVM.
     *
     * @param  s  a message to output on System.err
     */
    private void halt (String s)
    {
        trace("halt()");
        debug("halt() --> " + s);

        System.err.println("\n*** " + s + "...");
        System.exit(-1);
    }
    
    /** write help text and quit. */
    private void printUsage ()
    {
        trace(IN, "printUsage()");
        System.out.println(
        "NAME\n" +
        "  Scar: Strong Cryptographic ARchiver using Cryptix IJCE\n" +
        "  (International Java Cryptography Extensions).\n\n" +
        "SYNTAX\n" +
        "  java cryptix.tools.Scar\n" +
        "    [ -e ]\n" +
        "    [ -a ]\n" +
        "    [ -r ]\n" +
        "    [ -v ]\n" +
        "    [ -w ]\n" +
        "    [ -c cipher]\n" +
        "    [ -p passphrase]\n" +
        "    [ -m s2k_message_digest]\n" +
        "    [ -s s2k_salt]\n" +
        "    [ -i s2k_iterations]\n" +
        "    input\n" +
        "    output\n\n" +
        "  java cryptix.tools.Scar\n" +
        "    -d\n" +
        "    [ -u ]\n" +
        "    [ -v ]\n" +
        "    [ -w ]\n" +
        "    [ -c cipher]\n" +
        "    [ -p passphrase]\n" +
        "    [ -m s2k_message_digest]\n" +
        "    [ -s s2k_salt]\n" +
        "    [ -i s2k_iterations]\n" +
        "    input\n" +
        "    [output]\n\n" +
        "DESCRIPTION\n" +
        "  Scar  reads  and  compresses input and writes the encrypted\n" +
        "  result to output. It also does the inverse operation: reads\n" +
        "  and decrypts input and decompresses the resulting data into\n" +
        "  output.\n\n" +
        "  By default both encryption and decryption are done using the\n" +
        "  'Square' cipher algorithm (designed by Joan Daemen & Vincent\n" +
        "  Rijmen) in Cipher Electronic Codebook (CBC) mode padded with\n" +
        "  the method described in PKCS#7.\n\n" +
        "  The cipher's secret session key is derived from a passphrase\n" +
        "  supplied by the user through an S2K algorithm. The types and\n" +
        "  differences of such S2K algorithms are described in Open-PGP\n" +
        "  I.E.T.F document (draft-ietf-openpgp-formats.txt) dated 9/97.\n" +
        "  This scar uses Simple, Salted and Salted-Iterated S2K variants.\n" +
        "  The default message digest used with all S2K variants is\n" +
        "  RIPEMD-160.\n\n" +
        "  As mentioned earlier, the encryption and decryption are not\n" +
        "  done on the data itself but on a ZIP-ped image.  ZIPping is\n" +
        "  accomplished using the DEFLATE  method at its maximum level\n" +
        "  (best size).\n\n" +
        "  When a command line is entered, scar tries to load a properties\n" +
        "  file named \"scar.properties\" from the user home directory,\n" +
        "  the value of which is returned by Java's property \"user.home\"\n\n" +
        "OPTIONS\n" +
        "  -a   Asciify (Encryption).  Encode the output in Base-64 format\n" +
        "       (RFC-1521) making it suitable for Internet transmission.\n\n" +
        "  -d   Decryption.\n\n" +
        "  -e   Encryption (default).\n\n" +
        "  -r   Recurse (Encryption).  Apply the process repetitively to\n" +
        "       sub-directories found in input.\n\n" +
        "  -u   Use directory information (Decryption).  Recreate original\n" +
        "       directory tree structure.\n\n" +
        "  -v   Verbose.  Print notification messages to System.out.\n\n" +
        "  -w   Wipe the source input after processing.\n\n" +
        "  -c <cipher>\n" +
        "       Cipher algorithm name ('Square' by default).  Other values\n" +
        "       can be any block cipher algorithm installed and accessible\n" +
        "       on user platform  that conforms to Sun(R)'s JCE or Cryptix\n" +
        "       IJCE. With Cryptix security provider installed choices are\n" +
        "       Blowfish, CAST5, RC4, IDEA, SAFER and LOKI91 in addition to\n" +
        "       Square.\n\n" +
        "  -p <passphrase>\n" +
        "       An alphanumeric string with no spaces.  If contains spaces\n" +
        "       then include within double quotes.  If not supplied use \"\".\n\n" +
        "  -m <s2k_message_digest>\n" +
        "       Message digest algorithm name ('RIPEMD-160' by default).\n" +
        "       Other values can be any message digest algorithm installed\n" +
        "       and accessible on user platform that conforms to Sun (R)'s\n" +
        "       JCE or Cryptix's IJCE. With Cryptix security provider this\n" +
        "       can be, in addition to 'RIPEMD-160', HAVAL, MD2, MD4, MD5,\n" +
        "       SHA-1 and RIPEMD-128.\n\n"+
        "  -s <s2k_salt>\n" +
        "       S2K salt value.  If not supplied a Simple or Iterated S2K\n" +
        "       algorithm will be used, depending on whether an iteration\n" +
        "       count was supplied or not.\n\n" +
        "  -i <s2k_iterations>\n" +
        "       S2K iteration count. If a positive value is not provided,\n" +
        "       a Simple or Salted S2K algorithm will be used, depending\n" +
        "       on whether a salt value is given or not.\n\n" +
        "  <input>\n" +
        "       Input file or directory pathname. If the pathname includes\n" +
        "       spaces,  then it should be enclosed within  double quotes.\n" +
        "       Wild characters such as '*' (any number of characters) and\n" +
        "       '?' (any one character) are allowed, in such case <input>\n" +
        "       acts as a filter for actual selection of input file(s).\n" +
        "       When a filter is used, it should be enclosed within \"\".\n\n" +
        "  <output>\n" +
        "       Output file or directory.  When decrypting this should be\n" +
        "       a directory pathname. If absent, in the case of decryption,\n" +
        "       use the current directory.\n\n" +
        "COPYRIGHT\n" +
        "  Copyright (c) 1997, 1998 Systemics Ltd. on behalf of\n" +
        "  the Cryptix Development Team.  All rights reserved.\n");

        trace(OUT, "printUsage()");
        System.exit(0);
    }
    
    /**
     * Method to return a randomly built string to be used as the name of a
     * temporary file in the current working directory
     */
    private File getTempFile ()
    {
        trace(IN, "getTempFile()");

        int x;
        File f;
        do {
            x = (int)(Math.abs(random.nextDouble()) * 1000000);
            f = new File("." + fs, 'F' + String.valueOf(x));
        } while (f.exists());

        debug("getTempFile() --> " + f.getName());
        trace(OUT, "getTempFile()");

        return f;
    }

    /** main action. */
    public void run ()
    {
        trace(IN, "run()");

        notify("\nActual parameters");
        notify("\tcipher algorithm: \"" + cipherAlgorithm + "\"");
        notify("\t     pass-phrase: \"" + passPhrase + "\"");
        notify("\t  message digest: \"" + mdAlgorithm + "\"");
        notify("\t         md salt: \"" + salt + "\"");
        notify("\t   md iterations: " + iterations);
        notify("\t   input file(s): <" + input + ">");
        notify("\t output file/dir: <" + output + ">");
        notify("\tselection filter: <" + filter + ">");

        int n;
        FileInputStream fis = null;
        FileOutputStream fos = null;
        try {
            Cipher cipher = null;
            try {
                cipher = Cipher.getInstance(cipherAlgorithm + "/CBC/PKCS#7");
            }
            catch (NoSuchAlgorithmException ex1) {
                throw new CryptixException(
                    "Unable to instantiate a " + cipherAlgorithm +
                    " cipher object in CBC mode with PKCS#7 padding.");
            }
            int ivLen =
                ((FeedbackCipher) cipher).getInitializationVectorLength();
            byte[] iv = new byte[ivLen];
            
            // IV all zeroes. Not important since we'll start encryption
            // with a random block.
            ((FeedbackCipher) cipher).setInitializationVector(iv);

            SecretKey key = s2k();

            if (encrypting) {
                notify("\nZipping");                           // compressing
                ZipOutputStream zipo = new
                    ZipOutputStream(new FileOutputStream(temp));
                zipo.setComment("Made with scar");
                zipo.setMethod(ZipOutputStream.DEFLATED);
                zipo.setLevel(9);
                zip(inFile, zipo, 0);

                // if considering the selection criteria (as per our filter)
                // no files were found and processed we have to stop here
                // otherwise we'll get a ZipException thrown in our face if
                // we try to close it.
                if (count == 0) halt(
                    "No files were found that satisfy the selection criteria");
                zipo.flush();
                zipo.finish();
                zipo.close();

                notify("\nEncrypting");                         // encrypting
                temp2 = getTempFile();
                fos = new FileOutputStream(temp2);

                cipher.initEncrypt(key);
                CipherInputStream cis = new
                    CipherInputStream(new FileInputStream(temp), cipher);

                // generate a random block as long as the iv, encrypt it
                // and write it as the first block. Doing so, and since
                // we run our cipher in CBC mode, ensures that the same
                // text with the same key will not generate the same
                // output.
                random.nextBytes(iv);
                fos.write(iv);

                // write a magic word to test when decrypting in order
                // to ensure that it was encrypted by us or we used the
                // correct properties
                fos.write(MAGIC);

                // process user file(s)
                while ((n = cis.read(buffer)) != -1) fos.write(buffer, 0, n);
                cis.close();
                fos.close();

                if (asciify) {                        // asciify temp2 to out
                    notify("\nAsciifying");
                    fis = new FileInputStream(temp2);
                    ScarOutputStream cros = new
                        ScarOutputStream(new FileOutputStream(outFile));

                    while ((n = fis.read(buffer)) != -1)
                        cros.write(buffer, 0, n);

                    fis.close();
                    cros.flush();
                    cros.close();
                } else {                               // rename temp2 to out
                    outFile.delete();
                    temp2.renameTo(outFile);
                }
                temp.delete();
                temp2.delete();

            } else {                                               // decrypt
                notify("\nDe-asciifying");      // de-asciify (if applicable)
                fos = new FileOutputStream(temp);
                ScarInputStream cris = null;
                try {
                    cris = new ScarInputStream(new FileInputStream(inFile));
                    while ((n = cris.read(buffer)) != -1)
                        fos.write(buffer, 0, n);
                    cris.close();
                    fos.flush();
                    fos.close();
                    fis = new FileInputStream(temp);
                } catch (IOException e1) {          // most likely not a scar
                    debug("Warning: " + e1.getMessage());
                    notify("Warning: " + e1.getMessage());
                    fis = new FileInputStream(inFile);
                }
                notify("\nDecrypting");                         // decrypting

                temp2 = getTempFile();
                fos = new FileOutputStream(temp2);
                cipher.initDecrypt(key);
                CipherOutputStream cos = new CipherOutputStream(fos, cipher);

                // read a block as long as the iv and discard it.
                n = fis.read(buffer, 0, iv.length);
                debug("length of alleged iv: " + n);
                if (n != iv.length) throw new
                    CryptixException("File too short to be a scar (1).");

                // next check if there is a magic word put there by us during
                // encryption phase. if not then this file is not ours or was
                // produced by a different set of properties.
                n = fis.read(buffer, 0, MAGIC.length);
                if (n == -1 || n != MAGIC.length)
                    throw new CryptixException("File too short to be a scar (2).");
                else {
                    debug("Magic word: " + new String(buffer, 0, MAGIC.length));
                    if (! (new String(buffer, 0, MAGIC.length)).
                        equals(MAGIC_STRING))
                        throw new CryptixException(
                            "File doesn't look to be a scar. If it is, it was " +
                            "produced using a different set of properties.");
                }
                // read the rest
                while ((n = fis.read(buffer)) != -1) cos.write(buffer, 0, n);
                cos.flush();
                cos.close();
                fis.close();
                fos.close();

                notify("\nUnzipping");                           // unzipping
                ZipInputStream zipi = new
                    ZipInputStream(new FileInputStream(temp2));

                unzip(zipi, outFile);
                zipi.close();

                temp.delete();
                temp2.delete();
            }
            if (wipeSource) {                                  // wipe source
                notify("\nDeleting input");
                wipe(inFile, 0);
            }
        }
        catch (CryptixException ex1) {
            debug(ex1.getMessage());
            notify("\n--- Exception: " + ex1.getMessage());
        }
        catch (Exception ex2) {
            ex2.printStackTrace();
        }
        finally {
            if (temp != null)
                try { temp.delete(); }
                catch (Exception e1) {}
            if (temp2 != null)
                try { temp2.delete(); }
                catch (Exception e2) {}
        }
        trace(OUT, "run()");
    }

    /**
     * S2K algorithm. When used with Cryptix security provider it will not
     * throw any exception.
     */
    private SecretKey s2k ()
    throws CloneNotSupportedException, InvalidKeyException
    {
        trace(IN, "s2k()");

        MessageDigest md = null;
        try { md = MessageDigest.getInstance(mdAlgorithm); }
        catch (NoSuchAlgorithmException ex1) {
            throw new CryptixException(
                "Unable to instantiate a " + mdAlgorithm +
                " message digest object");
        }
        //
        // one instance of the md algorithm may not be enough. the
        // total number depends on (a) the output size of the md
        // and (b) the default key length of the cipher we will use.
        //
        // (a) can be obtained by calling the digest() method and
        // measuring the output length while (b), and thanks to the
        // IJCE (is anybody from SUN reading this? ;-), can be
        // obtained by a call to the getDefaultKeyLength() IJCE
        // method from a RawKeyGenerator instance.
        //
        // do (a) and follow it by (b)
        //
        int mdLength = md.digest().length;
        RawKeyGenerator rkg = null;
        try {
            rkg = (RawKeyGenerator) KeyGenerator.getInstance(cipherAlgorithm);
        }
        catch (NoSuchAlgorithmException ex1) {
            throw new CryptixException(
                "Unable to instantiate a " + cipherAlgorithm +
                " key-generator object");
        }
        int keyLength = rkg.getDefaultKeyLength();

        //
        // let's have a look at the ietf-draft. here what it says:
        //
        // If the hash size is greater than or equal to the session key
        // size, the leftmost octets of the hash are used as the key.
        //
        // If the hash size is less than the key size, multiple instances
        // of the hash context are created -- enough to produce the required
        // key data. These instances are preloaded with 0, 1, 2, ... octets
        // of zeroes (that is to say, the first instance has no preloading,
        // the second gets preloaded with 1 octet of zero, the third is
        // preloaded with two octets of zeros, and so forth).
        //
        Vector mds = new Vector();
        int lensofar = 0;
        do {
            mds.addElement(md.clone()); // always add a clone
            md.update((byte) 0x00);
            lensofar += mdLength;
        } while (lensofar < keyLength);

        // 
        // build a salted byte[] consisting of the salt bytes prepended to
        // the pass-phrase ones to process through the previous md(s)
        //
        int s = salt.length();
        int p = passPhrase.length();
        byte[] salted = new byte[s + p];
        if (s != 0) System.arraycopy(salt.getBytes(), 0, salted, 0, s);
        System.arraycopy(passPhrase.getBytes(), 0, salted, s, p);

        int countsofar = 0;
        int mdCount = mds.size();
        do {
            for (int i = 0; i < mdCount; i++)
                ((MessageDigest) mds.elementAt(i)).update(salted);
            countsofar++;
        } while (countsofar < iterations); // does it at least once

        //
        // that's it. time now to assemble the lot and hand it over to
        // the key-generator object.
        //
        byte[] keyData = new byte[keyLength];
        byte[] mdBytes;
        int length;
        lensofar = 0;
        for (int i = 0; i < mdCount; i++) {
            mdBytes = ((MessageDigest) mds.elementAt(i)).digest();
            length = (lensofar + mdLength > keyLength) ?
                keyLength - lensofar :
                mdLength;
            System.arraycopy(mdBytes, 0, keyData, lensofar, length);
            lensofar += length;
        }

        rkg.setWeakAllowed(true); // we don't care about key strength here

        SecretKey key = null;
        try { key = rkg.generateKey(keyData); }
        catch (WeakKeyException ex2) { /* will never happen */ }

        trace(OUT, "s2k()");
        return key;
    }

    /**
     * Write notification message to System.out.
     *
     * @param  s  string to output to System.out.
     */
    private void notify (String s)
    {
        trace(IN, "notify()");
        if (verbose) System.out.println(s + "...");
        trace(OUT, "notify()");
    }

    /**
     * Zip files and/or directories to a ZipOutputStream.
     *
     * @param  source  source file or directory.
     * @param  zip     destination zip output stream.
     * @param  level   depth level in the recursion tree of this method.
     *                 Used to distinguish top level directory from sub-
     *                 directories (whether to apply recursion or not).
     * @exception  IOException if operation fails
     */
    public void zip (File source, ZipOutputStream zip, int level)
    throws FileNotFoundException, IOException
    {
        trace(IN, "zip("+level+")");

        FileInputStream fis = null;        // input stream to read input data
        if (source.isFile()) {
            try {
                count += 1;
                fis = new FileInputStream(source);
                String sf = source.getCanonicalPath();
                int n = sf.indexOf(fs);    // get rid of home drive reference
                if (n != -1) sf = sf.substring(n + 1);

                // PKZIP (if you want to check at this stage) wants
                // '/' as file separator
                sf = sf.replace(fs.charAt(0), '/');

                notify("\t " + sf);
                zip.putNextEntry(new ZipEntry(sf));

                while ((n = fis.read(buffer)) != -1) zip.write(buffer, 0, n);
            }
            finally {
                if (fis != null)
                    try { fis.close(); }
                    catch (IOException e) { e.printStackTrace(); }
            }
        } else if (source.isDirectory()) {
            if (level == 0 || (level != 0 && recursion)) {
                // top level or lower level but recursion required
                String[] entries = source.list(filter);
                for (int i = 0; i < entries.length; i++)
                    zip(new File(source, entries[i]), zip, level + 1);
            }
        }
        trace(OUT, "zip("+level+")");
    }

    /**
     * unzip files and/or directories to a destination.
     *
     * @param  src   source zip stream.
     * @param  dest  destination File object.
     * @exception  IOException if operation fails
     */
    public void unzip (ZipInputStream zip, File dest)
    throws FileNotFoundException, IOException
    {
        trace(IN, "unzip()");

        ZipEntry ze;
        FileOutputStream fos = null;

        while ((ze = zip.getNextEntry()) != null) {
            if (ze.isDirectory()) continue;
            try {
                int n;
                String sf = ze.getName();
                if (! useDirInfo) { // PKZIP uses '/' as file separator
                    n = sf.lastIndexOf("/");
                    if (n != -1) sf = sf.substring(n + 1);
                } else
                    sf = sf.replace('/', fs.charAt(0));

                String destPath = dest.getPath();
                sf = destPath.endsWith(fs) ?
                    destPath + sf :
                    destPath + fs + sf;

                notify("\t " + sf);
                File f = new File(sf);
                if (useDirInfo) new File(f.getParent()).mkdirs();

                fos = new FileOutputStream(f);
                while ((n = zip.read(buffer)) != -1) fos.write(buffer, 0, n);
            }
            finally {
                if (fos != null)
                    try {
                        fos.flush();
                        fos.close();
                    }
                    catch (IOException e) { e.printStackTrace(); }
            }
        }
        trace(OUT, "unzip()");
    }

    /**
     * Wipe source input. If input is a directory than wipe all files
     * inside and, depending on the value set in the 'recursion' switch,
     * repeat same for sub-directories. Finally try to delete top level
     * directory. If this latter is not empty (recursion is inhibited and
     * at leat one sub-directory is still alive) no deletion occurs.<p>
     *
     * Basically delete all files processed earlier by a scar invocation.
     * If at the end the top level and enclosing directories are empty,
     * then they are deleted too.
     *
     * @param  source  File object to be deleted.
     * @param  level   0 if in top level directory.
     */
    private void wipe (File source, int level)
    {
        trace(IN, "wipe("+level+")");

        if (source.isDirectory()) {
            if (level == 0 || (level != 0 && recursion)) {
                // top level or lower level but recursion required
                String[] entries = source.list();
                for (int i = 0; i < entries.length; i++)
                    wipe(new File(source, entries[i]), level + 1);
            }
        }
        try { source.delete(); }
        catch (Exception ex) {
            System.err.println(ex.getMessage());
            ex.printStackTrace();
        }
        trace(OUT, "wipe("+level+")");
    }


// ==========================================================================
// ScarInputStream inner class.
// ==========================================================================

    // lex tokens for local consumption
    static final int CONV_WHITE = -1;
    static final int CONV_PAD   = -2;
    static final int CONV_OTHER = -3;


    final class ScarInputStream
    extends FilterInputStream
    {
    // Variables and constants
    //.......................................................................

        byte[] lineBuffer; // our local buffering buffer!

        boolean finished;
        PRZ24 crc;
        byte[] inBuf = new byte[4];  // Input data buffer
        byte[] outBuf = new byte[3]; // Output data buffer
        int inOff;                     // Bytes in inBuff
        int outOff;                     // index into outBuff
        int outBufMax;                 // outBuff capacity


    // Constructor
    //.......................................................................

        public ScarInputStream (InputStream is) throws IOException
        {
            super(is);
            trace(IN, "ScarInputStream()");
            
            outBufMax = 3;
            inOff = outOff = 0;
            finished = false;
            crc = new PRZ24();

            String line;
            while (true) { // munge lines until header line is found
                line = readLine();
                if (line == null) throw new
                    EOFException("Missing scar header");
                if (line.startsWith("-----" + header + "-----"))
                    break;
            }
            while (true) { // munge header lines: version, comments, etc...
                line = readLine();
                if (line == null)
                    throw new EOFException("Missing scar data");
                else if (line.length() == 0)
                    break;
            }
            trace(OUT, "ScarInputStream()");
        }

    // FilterInputStream methods
    //.......................................................................

        public synchronized int read () throws IOException
        {
            trace(IN, "ScarInputStream.read()");

            if (outOff == 0) { // read another set of 4 bytes
                if (finished) return -1; // see first if we are at end
                int inByte = 0;
                int n = CONV_WHITE;
                while (n == CONV_WHITE) { // skip whitespace
                    inByte = in.read();
                    if (inByte < 0) return -1;
                    n = toNumber(inByte);
                }
                if (n < 0) { // pad or conv other
                    if (n == CONV_OTHER)
                        // this stream does not contain Base64 data after all
                        throw new CharConversionException();
                    long computed = crc.getValue();
                    long actual = 0;
                    crc = null;
                    for (int i = 0; i < 3; i++) { // read checksum bytes
                        inByte = read();
                        if (inByte < 0) throw new EOFException();
                        actual = actual << 8 | inByte;
                    }
                    finished = true;
                    outOff = 0;
                    if (actual != computed) throw new
                        IOException("PRZ24 crc mismatch");
                    return -1;
                }
                for (int i = 0; i < 4; ++i) {
                    if (n == CONV_PAD) {
                        if (i < 2) throw new CharConversionException();
                    } else if (n < 0) // conv char read within a group of 4
                        throw new CharConversionException();
                    else
                        inBuf[inOff++] = (byte) n;

                    if (i != 3) {
                        inByte = in.read();
                        if (inByte < 0) throw new EOFException();
                        n = toNumber(inByte);
                    }
                }
                writeTriplet();
            }
            int b = outBuf[outOff++] & 0xFF;
            if (outOff == outBufMax) outOff = 0;
            if (crc != null) crc.update(b);

            trace(OUT, "ScarInputStream.read()");
            return b;
        }

        public synchronized int read (byte[] buffer, int offset, int length)
        throws IOException
        {
            trace(IN, "ScarInputStream.read(3)");
            debug("ScarInputStream.read("+buffer+", "+offset+", "+length+")");
            for (int i = 0; i < length; ++i) {
                int c = read();
                if (c < 0) return (i == 0) ? -1 : i;
                buffer[offset++] = (byte) c;
            }
            trace(OUT, "ScarInputStream.read(3)");
            return length;
        }

        /**
         * Override close in Base64InputStream to allow detection of
         * scar footer line. Basically munge lines until it gets to footer.
         */
        public synchronized void close () throws IOException
        {
            trace(IN, "ScarInputStream.close()");

            while (true) {
                String line = readLine();
                if (line == null) throw new
                    EOFException("Missing scar footer");
                if (line.startsWith("-----" + footer + "-----"))
                    break;
            }
            finished = true;
            outOff = 0;
            super.close();

            trace(OUT, "ScarInputStream.close()");
        }

    // own methods
    //.......................................................................

        // source for this method is mainly from SUN's source code.
        private String readLine () throws IOException
        {
            InputStream in = this.in;
            byte[] buf = lineBuffer;
            if (buf == null) buf = lineBuffer = new byte[128];
            int room = buf.length,
                offset = 0,
                c;
        loop:
            while (true) {
                switch (c = in.read()) {
                case -1:
                case '\n':
                    break loop;
                case '\r':
                    int c2 = in.read();
                    if (c2 != '\n') {
                        if (! (in instanceof PushbackInputStream))
                            in = this.in = new PushbackInputStream(in);
                        ((PushbackInputStream) in).unread(c2); // push it back
                    }
                    break loop;
                default:
                    if (--room < 0) {
                        buf = new byte[offset + 128];
                        room = buf.length - offset - 1;
                        System.arraycopy(lineBuffer, 0, buf, 0, offset);
                        lineBuffer = buf;
                    }
                    buf[offset++] = (byte) c;
                    break;
                }
            }
            if ((c == -1) && (offset == 0)) return null;

            byte[] lineBytes = new byte[offset];
            System.arraycopy(buf, 0, lineBytes, 0, offset);
            return new String(lineBytes);
        }

        private void writeTriplet ()
        {
            outBufMax = 0;
            outBuf[outBufMax++] = (byte)(inBuf[0] << 2 | inBuf[1] >>> 4);
            if (inOff > 2)
                outBuf[outBufMax++] = (byte)(inBuf[1] << 4 | inBuf[2] >>> 2);
            if (inOff > 3)
                outBuf[outBufMax++] = (byte)(inBuf[2] << 6 | inBuf[3]);
            inOff = 0;
        }

        private int toNumber (int b)
        {
            if (b >= 'a' & b <= 'z')        return b - 'a' + 26;
            else if (b >= 'A' & b <= 'Z')   return b - 'A';
            else if (b >= '0' & b <= '9')   return b - '0' + 52;
            else if (b == '+')              return 62;
            else if (b == '/')              return 63;
            else if (b == '=')              return CONV_PAD;
            else if (b == '\n' || b == '\r'
                || b == ' ' || b == '\t')   return CONV_WHITE;
            else                            return CONV_OTHER;
        }
    
    } // ScarInputStream


// ==========================================================================
// ScarOutputStream inner class.
// ==========================================================================

    static final String VERSION = "Version: Alpha.1 --December 97";
    static final int MAX_LINE_LENGTH = 64;
    static final char[] BASE64 = {
     //     0   1   2   3   4   5   6   7
        'A','B','C','D','E','F','G','H', // 0
        'I','J','K','L','M','N','O','P', // 1
        'Q','R','S','T','U','V','W','X', // 2
        'Y','Z','a','b','c','d','e','f', // 3
        'g','h','i','j','k','l','m','n', // 4
        'o','p','q','r','s','t','u','v', // 5
        'w','x','y','z','0','1','2','3', // 6
        '4','5','6','7','8','9','+','/'  // 7
    };
    static final char PADDING = '=';

    final class ScarOutputStream
    extends FilterOutputStream
    {
        PRZ24 crc;
        byte[] inBuf;   // Internal data buffer
        int inOff;      // Bytes in internal data buffer
        int lineLength; // Number of bytes output so far on line


        public ScarOutputStream (OutputStream os)
        throws IOException
        {
            super(os);
            trace(IN, "ScarOutputStream()");

            inBuf = new byte[3];
            crc = new PRZ24();
            inOff = lineLength = 0;

            out.write(new String("-----" + header + "-----").getBytes());
            writeln();
            out.write(VERSION.getBytes());
            writeln();
            if (comment.length() != 0) {
                out.write(new String("Comment: " + comment).getBytes());
                writeln();
            }
            writeln();
            trace(OUT, "ScarOutputStream()");
        }

    // FilterOutputStream methods
    //.......................................................................

        public void write (int b)
        throws IOException
        {
            inBuf[inOff++] = (byte) b;
            crc.update(b);
            if (inOff == 3) writeQuadruplet();
        }

        public void write (byte[] b, int offset, int length)
        throws IOException
        {
            for (int i = 0; i < length; i++) this.write(b[i + offset]);
        }

        public void close ()
        throws IOException
        {
            trace(IN, "ScarOutputStream.close()");

            if (inOff != 0) { // process remaining chars in buffer
                for (int i = inOff; i < 3; i++) inBuf[i] = 0;
                writeQuadruplet();
            }
            // force a line break unless the current line is empty
            if (lineLength != 0) writeln();
            out.write(PADDING);
            int cks = (int) crc.getValue();
            inBuf[0] = (byte)(cks >> 16);
            inBuf[1] = (byte)(cks >> 8);
            inBuf[2] = (byte) cks;
            inOff = 3;
            writeQuadruplet();
            writeln();

            out.write(new String("-----" + footer + "-----").getBytes());
            writeln();
            super.flush();
            super.close();

            trace(OUT, "ScarOutputStream.close()");
        }

    // Own methods
    //.......................................................................

        private synchronized void writeQuadruplet ()
        throws IOException
        {
            char c = BASE64[(inBuf[0] & 0xFF) >> 2];
            out.write(c);
            c = BASE64[(inBuf[0] & 0x03) << 4 | (inBuf[1] & 0xFF) >> 4];
            out.write(c);
            c = inOff > 1 ?
                BASE64[(inBuf[1] & 0x0F) << 2 | (inBuf[2] & 0xCF) >> 6] :
                PADDING;
            out.write(c);
            c = inOff > 2 ?
                BASE64[inBuf[2] & 0x3F] :
                PADDING;
            out.write(c);
            inOff = 0;
            lineLength += 4;
            if (lineLength >= MAX_LINE_LENGTH) writeln();
        }

        private void writeln () throws IOException
        {
            out.write('\r');
            out.write('\n');
            lineLength = 0;
        }
    } // ScarOutputStream
}
