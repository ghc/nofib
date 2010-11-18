// $Id: NativeLink.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: NativeLink.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 17:36:02  baford
// Checked in my "test suite":
// cryptix32-20001002-r3.2.0/src/cryptix/provider/cipher/*.java
//
// Revision 1.10  1998/01/11 03:33:23  hopwood
// + Look for 'Native.Allowed' property to determine whether any native
//   code is to be used.
// + Changed default for 'Native.Enable.*' to false.
//
// Revision 1.9  1998/01/10 04:12:24  hopwood
// + Committed changes below.
//
// Revision 1.8.1  1998/01/09  hopwood
// + Print no debugging messages (other than unexpected exceptions) below
//   debuglevel 3.
//
// Revision 1.8  1997/12/09 04:43:45  hopwood
// + Various.
//
// Revision 1.7.1  1997/12/07  hopwood
// + Changed debugging message output by fail method to be less 'frightening',
//   since this is not necessarily an error.
//
// Revision 1.7  1997/11/29 04:33:33  hopwood
// + Committed changes below.
//
// Revision 1.6.1  1997/11/28  hopwood
// + Removed spurious file separator before "bin".
//
// Revision 1.6  1997/11/23 03:24:22  hopwood
// + Renamed Disable.Native properties to Native.Enable, and changed
//   their sense.
//
// Revision 1.5  1997/11/20 19:31:41  hopwood
// + cryptix.util.* name changes.
//
// Revision 1.4.1  1997/11/15  David Hopwood
// + Sorted out conflicts between my version and Raif's.
// + Moved isNativeWanted method to this class.
// + Fixed imports.
//
// Revision 1.4  1997/11/10 07:31:32  raif
// + Removed reference to core.Cryptix.
// + Use CryptixProperties.
//
// Revision 1.1.1.1  1997/11/03 22:36:56  hopwood
// + Imported to CVS (tagged as 'start').
//
// Revision 0.1.0.9  1997/09/18  David Hopwood
// + Libraries are now loaded from cryptix-lib/bin, instead of
//   cryptix-lib.
// + Replaced references to Cryptix class with CryptixProperties.
//
// Revision 0.1.0.8  1997/08/13  David Hopwood
// + Copies of NativeLink are now in the cryptix.provider.cipher, and
//   cryptix.provider.md directories.
//
// Revision 0.1.0.7  1997/08/02  David Hopwood
// + Use Debug.getInternalLevel("NativeLink"), instead of
//   Debug.getLevel("_NativeLink_").
// + Cryptix.wantNative has been renamed to isNativeWanted.
//
// Revision 0.1.0.6  1997/07/26  David Hopwood
// + Fix for error messages output by ld on Unix, when a native
//   library cannot be found:
//   - get the directory to load native libraries from using
//   Cryptix.getLibraryPath().
//   - check whether the library file exists before trying to
//   load it. Currently we search for both possible names,
//   i.e. X.dll, then libX.so.
//   In Netscape the UniversalFileRead privilege is needed in
//   order to test this.
//   - load libraries using System.load (which takes an absolute
//   path), not System.loadLibrary.
// + For Netscape, the UniversalLinkAccess privilege is now only
//   requested if a library file is actually found (this helps to
//   avoid unnecessary security dialogs).
// + Since libraries are now associated with a specific installation
//   of classfiles (so PATH/LD_LIBRARY_PATH is not used), the change
//   made in 1.0.5 is not needed, and has been undone.
// + Added debugging support.
//
// Revision 0.1.0.5  1997/07/15  David Hopwood
// + Append "_" + major version number to the library filename. This
//   allows installations of Cryptix with different library versions to
//   co-exist in the PATH or LD_LIBRARY_PATH.
//
// Revision 0.1.0.4  1997/07/10  R. Naffah
// + Enclosed netscape.security calls within try/catch.
// + Use new cryptix.provider.Cryptix class.
//
// Revision 0.1.0.3  1997/07/05  David Hopwood
// + Removed selfTest() method, since it is redundant given check()
//   (check is a better name).
// + Only look for libname, rather than libname_jni and libname_rni.
//   We don't support RNI at the moment (it hasn't been decided whether
//   or not we need to support it).
// + Added documentation for non-public methods (public methods are
//   already documented in cryptix.util.LinkStatus).
//
// Revision 0.1.0.2  1997/07/04  R. Naffah
// + Added check() method to handle all native functions that return
//   an error string if an error occurs or null if OK. 
//
// Revision 0.1.0.1  1997/06/22  David Hopwood
// + Changed so that the class implementing the native code calls
//   getLibMajorVersion(), etc., rather than using the Reflection
//   API.
// + Added checkVersion, selfTest, fail, useNative, checkNative and
//   setNative methods.
//
// Revision 0.1.0.0  1997/06/22  David Hopwood
//   Start of history (Cryptix 2.2.0a).
//
// $Endlog$
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.provider.cipher;

import java.io.PrintWriter;
import java.io.File;
import java.io.IOException;

import cryptix.CryptixProperties;
import cryptix.util.core.Debug;
import cryptix.util.core.LinkStatus;

import netscape.security.PrivilegeManager;

/**
 * A class to handle loading and versioning of native code in a VM-independent way.
 * <p>
 * SECURITY: this class must be package-private, and not accessible to untrusted
 * code. Therefore the source file must be copied to each package that needs it,
 * with the package statement adjusted accordingly. Make sure that any changes are
 * reflected in all copies.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  David Hopwood
 * @author  Raif S. Naffah
 * @since   Cryptix 2.2.2
 */
final class NativeLink
implements LinkStatus
{

// Debugging methods and vars.
//...........................................................................

    private static final boolean DEBUG = Debug.GLOBAL_DEBUG;
    private static final int debuglevel = DEBUG ? Debug.getLevel("NativeLink") : 0;
    private static final PrintWriter err = DEBUG ? Debug.getOutput() : null;
    private static void debug(String s) { err.println("NativeLink: " + s); }


// Constants
//...........................................................................

    private static final int NOT_LOADED = 0;
    private static final int FAILED = 1;
    private static final int OK = 2;


// Variables
//...........................................................................

    private static boolean native_allowed; // defaults to false
    static {
        if (CryptixProperties.NATIVE_ALLOWED) {
            try {
                String s = CryptixProperties.getProperty("Native.Allowed");
                native_allowed = (s == null || s.equalsIgnoreCase("true"));
            } catch (Exception e) { e.printStackTrace(); }
        }
    }

    private int required_major;
    private int required_minor;
    private int actual_major;
    private int actual_minor;
    private String library_name;
    private boolean want_native;
    private int status;         // defaults to NOT_LOADED
    private boolean lib_loaded; // defaults to false
    private String link_error;


// Constructor
//...........................................................................

    /**
     * Constructs a NativeLink object that can be used to load the given
     * native library. <i>libname</i> is the platform-independent part
     * of the name.
     *
     * @param libname   the library name
     * @param major     the required major version number
     * @param minor     the required minor version number
     */
    NativeLink(String libname, int major, int minor) {
        required_major = major;
        required_minor = minor;
        library_name = libname;

        want_native = isNativeWanted(libname);
        link_error = want_native
            ? "Library is not loaded because the class has not yet been used."
            : "Library is not loaded because it is disabled in the properties.";
    }


// LinkStatus methods
//...........................................................................

    public int getRequiredMajorVersion() { return required_major; }
    public int getRequiredMinorVersion() { return required_minor; }
    public String getLibraryName() { return library_name; }
    public int getMajorVersion() { return actual_major; }
    public int getMinorVersion() { return actual_minor; }
    public boolean isLibraryLoaded() { return lib_loaded; }
    public boolean isLibraryCorrect() { return status == OK; }
    public String getLinkErrorString() { return link_error; }


// Own methods
//...........................................................................

    /**
     * Returns true iff native linking is enabled for the given
     * library name.
     */
    private static boolean isNativeWanted(String libname) {
        if (!native_allowed) return false;
        String s = CryptixProperties.getProperty("Native.Enable." + libname);
        if (s == null) s = CryptixProperties.getProperty("Native.Enable.*");
        return s != null && s.equalsIgnoreCase("true");
    }

    /**
     * Sets the actual major and minor version numbers of the library. If they
     * are not compatible with the required version numbers passed to the
     * <samp>NativeLink</samp> constructor, an <samp>UnsatisfiedLinkError</samp>
     * is thrown.
     * <p>
     * The major version must match the required major version exactly, and
     * the minor version must be at least the required minor version, in order
     * for the library to be compatible.
     *
     * @param major the actual major version number
     * @param minor the actual minor version number
     */
    void checkVersion(int major, int minor) {
        actual_major = major;
        actual_minor = minor;
        if (major != required_major ||
            minor < required_minor) {
            status = FAILED;
            throw new UnsatisfiedLinkError("The " + library_name + " native library " +
                actual_major + "." + actual_minor + " is too old (or new) to use. Version " +
                required_major + "." + required_minor + " is required.");
        }
    }

    /**
     * If <i>reason</i> is null, this method returns silently. Otherwise,
     * an <samp>UnsatisfiedLinkError</samp> is thrown with <i>reason</i>
     * included in the detail message.
     * <p>
     * This is useful for handling native methods that return null on
     * success, or an error String on failure.
     *
     * @param reason    either null or an error message
     */
    void check(String reason) {
        if (reason != null) {
            status = FAILED;
            throw new UnsatisfiedLinkError("Unexpected result in " +
                library_name + " native library: " + reason);
        }
    }

    /**
     * Marks the library as having failed a test.
     *
     * @param e the exception that was thrown to indicate the failure.
     */
    void fail(Throwable e) {
        status = FAILED;
        link_error = e.getMessage();
if (DEBUG && debuglevel >= 3) debug(library_name + " library was not loaded: " + link_error);
    }

    public void checkNative() {
        if (!useNative()) throw new UnsatisfiedLinkError(link_error);
    }

    public boolean useNative() { return status == OK && want_native; }

    public void setNative(boolean wanted) { want_native = wanted; }

    /**
     * Tries to load the native library, if it is enabled. Returns true if the
     * library was successfully loaded by this call (but not if it was already
     * loaded).
     */
    boolean attemptLoad() {
        if (status != NOT_LOADED || !want_native) return false;

        String libpath;
        try {
            String fs = File.separator;
            libpath = CryptixProperties.getLibraryPath() + "bin" + fs;
        }
        catch (IOException e) { return false; }
        lib_loaded = attemptLoad(new String[] {
            libpath + library_name + ".dll",
            libpath + "lib" + library_name + ".so",
            // libpath + library_name + "_rni.dll",
        });
        if (!lib_loaded) {
            throw new UnsatisfiedLinkError("The " + library_name +
                " native library could not be loaded.");
        }
        status = OK;
        link_error = null;
        return true;
    }

    /**
     * Tries to load a native library if possible. Each of the elements of
     * <i>libs</i> is the absolute path and filename of a potential library
     * file (which need not exist).
     *
     * @param libs  an array of library filenames to try, in order.
     * @return true if one of the libraries was loaded successfully.
     */
    private boolean attemptLoad(String[] libs) {
if (DEBUG && debuglevel >= 7) debug("entered attemptLoad(String[] libs) for " + library_name);
        try {
            try { PrivilegeManager.enablePrivilege("UniversalFileRead"); }
            catch (NoClassDefFoundError e) {}

if (DEBUG && debuglevel >= 9) {
    for (int i = 0; i < libs.length; i++) debug("libs[" + i + "] = \"" + libs[i] + "\"");
}
            for (int i = 0; i < libs.length; i++) {
                try {
                    File file = new File(libs[i]);
                    if (file.isFile()) {
if (DEBUG && debuglevel >= 5) debug(libs[i] + " exists. Attempting load.");
                        try {
                            try { PrivilegeManager.enablePrivilege("UniversalLinkAccess"); }
                            catch (NoClassDefFoundError e) {}

                            System.load(libs[i]);
                        } finally {
                            // make sure UniversalLinkAccess privilege is always
                            // dropped as soon as possible.
                            try { PrivilegeManager.revertPrivilege("UniversalLinkAccess"); }
                            catch (NoClassDefFoundError e) {}
                        }
if (DEBUG && debuglevel >= 3) debug(library_name + " library loaded successfully.");
                        return true;
                    } else {
if (DEBUG && debuglevel >= 6) debug(libs[i] + " does not exist.");
                        continue;
                    }
                } catch (LinkageError e) {
                    link_error = e.toString();
                } catch (SecurityException e) {
                    link_error = e.toString();
                } catch (Throwable e) {
                    // don't call non-final methods on arbitrary exceptions for
                    // security reasons.
                    try { PrivilegeManager.revertPrivilege("UniversalFileRead"); }
                    catch (NoClassDefFoundError e2) {}
            
                    link_error = e.getClass().getName();
                    debug("unexpected exception while attempting to load " +
                        libs[i] + ": " + link_error);
                }
if (DEBUG && debuglevel >= 3) debug(libs[i] + " failed to load: " + link_error);
            }
            return false;
        } catch (SecurityException e) {
            link_error = e.toString();
if (DEBUG && debuglevel >= 3) debug(library_name + " library failed to load: " + link_error);
            return false;
        }
    }
}
