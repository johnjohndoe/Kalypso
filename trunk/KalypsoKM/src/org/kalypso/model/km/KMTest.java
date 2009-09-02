package org.kalypso.model.km;

import java.io.File;

import org.kalypso.model.km.i18n.Messages;

import junit.framework.TestCase;

public class KMTest extends TestCase
{
	public void testKMCalculation() throws Exception
	{
		try
		{
			final File profileDir = new File("C:/daten/WQ"); //$NON-NLS-1$
			final String args[] = new String[]
			{ "58.800",// start //$NON-NLS-1$
					"58.853",// end //$NON-NLS-1$
					"5", // ,count //$NON-NLS-1$
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