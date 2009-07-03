package org.kalypso.model.km;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.math.LinearEquation.SameXValuesException;

public class ProfileDataSet
{
	private final SortedSet<ProfileData> m_profileSort = new TreeSet<ProfileData>(
			new Comparator<ProfileData>()
			{
				public int compare(ProfileData p1, ProfileData p2)
				{
					return Double.compare(p1.getPosition(), p2.getPosition());
				}
			});

	private final double m_startPosition;

	private final double m_endPosition;

	public ProfileDataSet(final File[] profileFiles)
	{
		init(profileFiles, false);
		m_startPosition = m_profileSort.first().getPosition();
		m_endPosition = m_profileSort.last().getPosition();
	}

	public ProfileDataSet(final File[] profileFiles, double min, double max)
	{
		m_startPosition = min;
		m_endPosition = max;
		init(profileFiles, true);
	}

	public double getStartPosition()
	{
		return m_startPosition;
	}

	public double getEndPosition()
	{
		return m_endPosition;
	}

	private void init(final File[] profileFiles, boolean validatePosition)
	{
		for (int i = 0; i < profileFiles.length; i++)
		{
			final File file = profileFiles[i];
			final ProfileData qwProfile;
			try
			{
				qwProfile = ProfileFactory.createQWProfile(file,
						m_startPosition, m_endPosition);
				if (validatePosition)
				{
					final double profilePos = qwProfile.getPosition();
					if (m_startPosition <= profilePos
							&& profilePos <= m_endPosition)
						m_profileSort.add(qwProfile);
				} else
					m_profileSort.add(qwProfile);

			} catch (IOException e)
			{
				Logger.getAnonymousLogger().log(
						Level.WARNING,
						"skip profile, could not read: "
								+ file.getAbsolutePath());
				e.printStackTrace();
			}
		}
		final ProfileData[] profiles = m_profileSort
				.toArray(new ProfileData[m_profileSort.size()]);
		for (int i = 0; i < profiles.length; i++)
		{
			ProfileData data = profiles[i];
			if (i > 0)
				data.setPrev(profiles[i - 1]);
			if (i + 1 < profiles.length)
				data.setNext(profiles[i + 1]);
		}
	}

	public Iterator<ProfileData> getAllProfiles()
	{
		return m_profileSort.iterator();
	}

	public AbstractKMValue[] getKMValues(int numberOfDischarges)
			throws SameXValuesException
	{
		final ProfileData data = m_profileSort.first();

		// generate kmValues for each q;
		int maxValues = data.getNumberKMValues();
		final List<AbstractKMValue> kmMergedForIndexOverProfiles = new ArrayList<AbstractKMValue>();
		for (int index = 0; index < maxValues; index++)
		{
			final ProfileData[] profiles = (ProfileData[]) m_profileSort
					.toArray(new ProfileData[m_profileSort.size()]);
			// collect KMValues for one index over all profiles
			final List<AbstractKMValue> kmForIndexOverProfiles = new ArrayList<AbstractKMValue>();
			for (int counterProfile = 0; counterProfile < profiles.length; counterProfile++)
			{
				final ProfileData profile = profiles[counterProfile];
				kmForIndexOverProfiles.add(profile.getKMValue(index));
			}
			// merge collection to one kmvalue
			final AbstractKMValue[] kmForIndexOverProfileArray = (AbstractKMValue[]) kmForIndexOverProfiles
					.toArray(new AbstractKMValue[kmForIndexOverProfiles.size()]);
			kmMergedForIndexOverProfiles.add(new MulitKMValue(
					kmForIndexOverProfileArray));
		}

		final AbstractKMValue[] kmMerged = (AbstractKMValue[]) kmMergedForIndexOverProfiles
				.toArray(new AbstractKMValue[kmMergedForIndexOverProfiles
						.size()]);

		final SortedSet<AbstractKMValue> sort = new TreeSet<AbstractKMValue>(
				new Comparator<AbstractKMValue>()
				{

					public int compare(AbstractKMValue km1, AbstractKMValue km2)
					{
						return Double.compare(km1.getQSum(), km2.getQSum());
					}

				});
		for (int i = 0; i < kmMerged.length; i++)
			sort.add(kmMerged[i]);

		final AbstractKMValue kmFirst = kmMerged[0];
		final AbstractKMValue kmLast = kmMerged[kmMerged.length - 1];
		double qFirst = kmFirst.getQSum();
		double qLast = kmLast.getQSum();
		double qbankfull;
		final List<AbstractKMValue> result = new ArrayList<AbstractKMValue>();
		try
		{
			final AbstractKMValue kmBankFull = getBankFull(kmMerged);
			qbankfull = kmBankFull.getQSum();
		} catch (Exception e)
		{
			qbankfull = (qLast + qFirst) / 2;
		}
		 
//		double dq1 = (qbankfull - qFirst) / (numberOfDischarges / 2);
		int max1=numberOfDischarges/2;
		for(int i=0;i<max1;i++)
		{
			double q=qFirst+i*(qbankfull - qFirst)/max1; 
			result.add(getKM(sort, q));
		}
		for(int i=max1;i<numberOfDischarges;i++)
		{
			double q=qbankfull+(i-(max1))*(qLast - qbankfull)/(numberOfDischarges-(max1+1)); 
			result.add(getKM(sort, q));
		}
//		for (double q = qFirst; q <= qbankfull; q += dq1)
//			result.add(getKM(sort, q));
//		double dq2 = (qLast - qbankfull) / ((double)numberOfDischarges / 2d);
//		for (double q = qbankfull + dq2; q <= qLast; q += dq2)
//			result.add(getKM(sort, q));
		return (AbstractKMValue[]) result.toArray(new AbstractKMValue[result
				.size()]);
	}

	private AbstractKMValue getKM(SortedSet<AbstractKMValue> sort, double q)
			throws SameXValuesException
	{

		final DummyKMValue value = new DummyKMValue(q);
		final SortedSet<AbstractKMValue> headSet = sort.headSet(value);
		if (headSet.isEmpty())
		{
			return sort.first();
		}
		final AbstractKMValue km1 = headSet.last();
		final AbstractKMValue km2 = sort.tailSet(value).first();
		return new KMValueFromQinterpolation(q, km1, km2);
	}

	private AbstractKMValue getBankFull(final AbstractKMValue[] kmValues)
			throws Exception
	{
		for (int i = 0; i < kmValues.length; i++)
		{
			final AbstractKMValue km = kmValues[i];
			if (km.getAlpha() < 1 && i == 0)
				throw new Exception("no bankfull, min q is more than q bankfull");
			else if (km.getAlpha() < 1 && i > 0)
				return kmValues[i - 1];
		}
		throw new Exception("no bankfull, check boundary conditions or max q is less than q bankfull");
	}

	private class DummyKMValue extends AbstractKMValue
	{
		private final double m_q;

		public DummyKMValue(double q)
		{
			m_q = q;
		}

		public double getLength()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getAlpha()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getK()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getN()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getKForeland()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getNForeland()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getQ()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getQForeland()
		{
			// TODO Auto-generated method stub
			return 0;
		}

		public double getQSum()
		{
			return m_q;
		}

	}
}
