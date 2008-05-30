package org.kalypso.model.km;

import java.io.File;

public class Main
{

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{

		System.out
				.println("This program calculates Kalinin-Miljukov-Parameter from 1D-simulation result files");
		System.out.println();
		System.out
				.println("usage: java -jar km.jar <kmStart> <kmEnd> <nkm> <path>");
		System.out
				.println("example to print result to file name result.txt:\n usage: java -jar km.jar <kmStart> <kmEnd> <nkm> <path> >result.txt");
		System.out.println();
		System.out
				.println("kmStart: KM-channel start-position [km]. It might be before first profile");
		System.out
				.println("kmEnd: KM-channel end-position [km]. It might be after last profile");
		System.out.println("nkm: number of KM-parameterset to be generated.");
		System.out
				.println("path: path to directory of result files. files must have suffix \"*.km\".");
		System.out.println();
		System.out
				.println("Example java -jar km.jar 58.8 58.853 5 C:\\TMP\\results");
		System.out.println();
		try
		{
			if (args.length != 4)
				throw new Exception("wrong count of parameters");
			System.out.println("your parameters:");
			double kmStart = Double.parseDouble(args[0]);
			System.out.println("kmStart: " + kmStart + " [km]");

			double kmEnd = Double.parseDouble(args[1]);
			System.out.println("kmEnd:   " + kmEnd + " [km]");
			int nkm = Integer.parseInt(args[2]);
			System.out.println("nkm:     " + nkm);

			final File path = new File(args[3]);
			System.out.println("path:    " + path.getAbsolutePath());
			if (!path.exists())
				throw new Exception("unknown directory: " + path.toString());
			System.out.println();
			final ProfileDataSet pSet = ProfileFactory.createProfileSet(path,
					kmStart, kmEnd);
			final AbstractKMValue[] kmValues = pSet.getKMValues();
			System.out.println("Ergebnis:");
			for (int i = 0; i < kmValues.length; i++)
			{
				final AbstractKMValue value = kmValues[i];
				System.out.println(value);
			}
		} catch (Exception e)
		{
			System.out.println("error, check input parameter\n");
			System.out.println("hint: " + e.getMessage() + "\n");
			System.out.println("hint: " + e.getLocalizedMessage() + "\n");
		}
	}
}
