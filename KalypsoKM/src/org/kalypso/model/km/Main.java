package org.kalypso.model.km;

import java.io.File;

import org.kalypso.model.km.i18n.Messages;

public class Main
{

	/**
	 * @param args
	 */
	public static void main(String[] args)
	{

		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.0")); //$NON-NLS-1$
		System.out.println();
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.1")); //$NON-NLS-1$
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.2")); //$NON-NLS-1$
		System.out.println();
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.3")); //$NON-NLS-1$
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.4")); //$NON-NLS-1$
		System.out.println(Messages.getString("org.kalypso.model.km.Main.5")); //$NON-NLS-1$
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.6")); //$NON-NLS-1$
		System.out.println();
		System.out
				.println(Messages.getString("org.kalypso.model.km.Main.7")); //$NON-NLS-1$
		System.out.println();
		try
		{
			if (args.length != 4)
				throw new Exception(Messages.getString("org.kalypso.model.km.Main.8")); //$NON-NLS-1$
			System.out.println(Messages.getString("org.kalypso.model.km.Main.9")); //$NON-NLS-1$
			double kmStart = Double.parseDouble(args[0]);
			System.out.println("kmStart: " + kmStart + " [km]"); //$NON-NLS-1$ //$NON-NLS-2$

			double kmEnd = Double.parseDouble(args[1]);
			System.out.println("kmEnd:   " + kmEnd + " [km]"); //$NON-NLS-1$ //$NON-NLS-2$
			int nkm = Integer.parseInt(args[2]);
			System.out.println("nkm:     " + nkm); //$NON-NLS-1$

			final File path = new File(args[3]);
			System.out.println("path:    " + path.getAbsolutePath()); //$NON-NLS-1$
			if (!path.exists())
				throw new Exception(Messages.getString("org.kalypso.model.km.Main.16") + path.toString()); //$NON-NLS-1$
			System.out.println();
			final ProfileDataSet pSet = ProfileFactory.createProfileSet(path,
					kmStart, kmEnd);
			final AbstractKMValue[] kmValues = pSet.getKMValues();
			System.out.println(Messages.getString("org.kalypso.model.km.Main.17")); //$NON-NLS-1$
			for (int i = 0; i < kmValues.length; i++)
			{
				final AbstractKMValue value = kmValues[i];
				System.out.println(value);
			}
		} catch (Exception e)
		{
			System.out.println(Messages.getString("org.kalypso.model.km.Main.18")); //$NON-NLS-1$
			System.out.println(Messages.getString("org.kalypso.model.km.Main.19") + e.getMessage() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
			System.out.println(Messages.getString("org.kalypso.model.km.Main.21") + e.getLocalizedMessage() + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
}
