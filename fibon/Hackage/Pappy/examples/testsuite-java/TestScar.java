// $Id: TestScar.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestScar.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:04  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.1  1998/02/20 05:54:10  zox
// Simple test. Creates temp files, SCARs it, unSCARS it and compares result.
//
//
// $Endlog$
/*
 * Copyright (c) 1997, 1998 Systemics Ltd
 * on behalf of the Cryptix Development Team.  All rights reserved.
 */

package cryptix.test;

import cryptix.tools.UnixCrypt;
import cryptix.tools.Scar;
import cryptix.util.test.BaseTest;

import java.io.*;

/**
 * This class tests the <samp><a href=cryptix.tools.Scar.html>
 * cryptix.tools.Scar</a></samp> class.<br>
 * <b>Will fail when unable to write into testing directory.</b>
 *
 * <p>
 * <b>Copyright</b> &copy; 1997, 1998
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Zoran Rajic
 */
public class TestScar extends BaseTest
{
  //public static boolean DEBUG = true;
  public static boolean DEBUG = false;

  public final static String tmpArch     = "tmpARCH.scar";
  public final static String tmpDirS_in  = "scarIN.tmp";
  public final static String tmpDirS_out = "scarOUT.tmp";
  public final static String tFile_1     = "file_1";
  public final static String tFile_2     = "file_2";
  public final static String tFile_3     = "file_3";

  public final static String testData_1 = "Marry had a little lamb";
  public final static String testData_2 = "Burp!, pardon me...";
  public final static char[] testData_3 = { 0x01, 0x02, 0xF3, 0xF4 };

  public final static String[] inParams  = { "-er", "-p", "blabla", tmpDirS_in, tmpArch };
  public final static String[] outParams = {  "-d", "-p", "blabla", tmpArch, tmpDirS_out };

  /**
   * @param toWrite File to be written
   * @param contents String or char[] contents to be written into file
   */
  public void localWriter(File toWrite, Object contents) throws IOException
  {
    FileWriter tmpWriter = new FileWriter(toWrite);
    Class      tmpClass  = contents.getClass();
    String     tmpString = (tmpClass.getComponentType() == null) ? null : tmpClass.getComponentType().toString();

    if (DEBUG)
    {
      out.print("TestScar.localWriter(file, ");
      if (tmpString == null)
	out.println(tmpClass.getName() + ")");
      else
	out.println(tmpString + "[])");
    }

    if (contents instanceof String)
      tmpWriter.write((String )contents);
    else if (tmpString.equals("char"))
      tmpWriter.write((char[] )contents);
    else
      tmpWriter.write(">> Unknown data of " + tmpString + "[] type given as input <<");
    tmpWriter.flush();
    tmpWriter.close();
  }

  /**
   * @param toRead File to be read from
   * @return Contents of a file converted into String
   */
  public String localReader(File toRead) throws IOException
  {
    FileReader tmpReader = new FileReader(toRead);
    StringBuffer tempSB  = new StringBuffer();
    int tempChar;

    while ((tempChar = tmpReader.read()) > -1)
      tempSB.append((char )tempChar);
    
    return (tempSB.length() > 0) ? tempSB.toString() : null;
  }

  /**
   * Test that is run by distribution to make sure everything is OK!
   * It writes some test files, makes Scar archive from them, unarchives files
   * and compares results.
   */
  protected void engineTest() throws Exception
  {
    File tempDir = new File(tmpDirS_in);
    File filek_1 = new File(tmpDirS_in, tFile_1);
    File filek_2 = new File(tmpDirS_in, tFile_2);
    File filek_3 = new File(tmpDirS_in, tFile_3);

    setExpectedPasses(3);

    try
    {
      if (tempDir.mkdirs() && DEBUG)
       	out.println("\nDirectory "+ tmpDirS_in + " created.");

      localWriter(filek_1, testData_1);
      localWriter(filek_2, testData_2);
      localWriter(filek_3, testData_3);
    }
    catch (Exception e)
    {
      error("Can't write test files: " + e.getMessage());
      System.exit(1);
    }

    if (DEBUG)
      out.println("TestScar> Starting Scar()");

    Scar.DEBUG = false;
    Scar jc = new Scar();
    jc.processOptions(inParams);
    jc.run();

    if (DEBUG)
      out.println("Encrypted. Now decrypting...");
    
    if (filek_1.delete() && filek_2.delete() && filek_3.delete() && tempDir.delete())
    {
      if (DEBUG)
	out.println("TestScar> Test files deleted.");
    }
    else
      System.err.println("TestScar> Warning: Unable to delete all test files!");

    tempDir = new File(tmpDirS_out);
    filek_1 = new File(tmpDirS_out, tFile_1);
    filek_2 = new File(tmpDirS_out, tFile_2);
    filek_3 = new File(tmpDirS_out, tFile_3);

    if (tempDir.mkdirs() && DEBUG)
      out.println("Directory "+ tmpDirS_out + " created.");


    //Need to do it this way, cause processOptions() doesn't reinitialise all params
    jc = new Scar();
    jc.processOptions(outParams);
    jc.run();

    passIf(localReader(filek_1).equals(testData_1), "Scar file 1 OK");
    passIf(localReader(filek_2).equals(testData_2), "Scar file 2 OK");
    passIf(localReader(filek_3).equals(new String(testData_3)), "Scar file 3 OK");

    if (filek_1.delete() && filek_2.delete() && filek_3.delete() && tempDir.delete() && (new File(tmpArch)).delete())
    {
      if (DEBUG)
	out.println("TestScar> Test files deleted.");
    }
    else
      System.err.println("TestScar> Warning: Unable to delete all test files!");
  }

  public static void main(String argv[])
  {
    new TestScar().commandline(argv);
  }
}

