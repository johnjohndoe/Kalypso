package org.kalypso.model.km;

import java.io.File;

import junit.framework.TestCase;

public class KMTest extends TestCase
{
	public void testKMCalculation() throws Exception
	{
		try
		{
			final File profileDir = new File("C:/daten/WQ");
			final String args[] = new String[]
			{ "58.800",// start
					"58.853",// end
					"5", // ,count
					profileDir.getAbsolutePath() // path
			};
			Main.main(args);
			// final ProfileDataSet pSet = ProfileFactory.createProfileSet(
			// profileDir, 58.8000, 59.00);
			// final AbstractKMValue[] kmValues = pSet.getKMValues(5);
			// System.out.println("Ergebnis:");
			// for (int i = 0; i < kmValues.length; i++)
			// {
			//
			// final AbstractKMValue value = kmValues[i];
			// System.out.println(value);
			// }
		} catch (Exception e)
		{
			e.printStackTrace();
			throw e;
		}
	}
}