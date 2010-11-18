package cryptix.tools;

import java.io.*;
import cryptix.util.core.Hex;

/**
 * Reads the file specified as an argument and displays it as hex.
 *
 * @since Cryptix 3.1
 * @author Ian Brown
 * @see cryptix.util.core.Hex
 */
public class HexDump
{
	public static void main(String args[])
	throws IOException
	{
		if (args.length != 1)
		{
			System.err.println("Usage: java HexDump filename");
			System.exit(1);
		}

		InputStream in = new FileInputStream(args[0]);
		byte[] data = new byte[in.available()];
		in.read(data);

		System.out.println(Hex.dumpString(data));
	}
}