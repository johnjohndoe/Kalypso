package org.kalypso.statistics.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.statistics.types.EDischargeTimestepArea;
import org.kalypso.statistics.types.EHydrologicalInterval;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.AppUtils;

public class TimeserieProcessor {

	private final TimeserieProfile m_timeserieProfile;

	public TimeserieProcessor(final TimeserieProfile timeserieProfile, boolean createCopy) {
		if (createCopy) {
			m_timeserieProfile = TimeserieProfile.clone(timeserieProfile, false, true);
		} else {
			m_timeserieProfile = timeserieProfile;
		}
	}

	public void toIntervalMaximums(int numberOfMaximumsPerInterval, EHydrologicalInterval interval) {
		if (getTimeserieProfile().getEntries().size() == 0) {
			return;
		}
		final long millis24h = 24 * 60 * 60 * 1000;
		boolean nextPeriodStarted = true;
		List<TimeserieProfileEntry> newEntries = new ArrayList<TimeserieProfileEntry>();
		List<TimeserieProfileEntry> oldEntries = new ArrayList<TimeserieProfileEntry>();
		oldEntries.addAll(getTimeserieProfile().getEntries());
		getTimeserieProfile().getEntries().clear();
		Calendar startCalendar = oldEntries.get(0).getTime();
		Calendar[] period = getNextInterval(startCalendar, interval);
		Calendar processBegin = period[0];
		TimeserieProfileEntry prevEntry = null;
		for (TimeserieProfileEntry e : oldEntries) {
			Calendar time = e.getTime();
			// skip until first interval entry
			if (time.before(processBegin)) {
				continue;
			}
			if (nextPeriodStarted) {
				if (time.after(period[1])) {
					// period finished, find maximums
					getTimeserieProfile().getEntries().addAll(getMaximums(newEntries, numberOfMaximumsPerInterval, millis24h));
					// prepare for the next period
					newEntries.clear();
					nextPeriodStarted = false;
					if (prevEntry != null) {
						period = getNextInterval(prevEntry.getTime(), interval);
					} else {
						period = getNextInterval(time, interval);
					}
				} else {
					newEntries.add(e);
				}
			}
			if (!nextPeriodStarted) {
				if (time.before(period[0])) {
					continue;
				} else {
					nextPeriodStarted = true;
					newEntries.add(e);
				}
			}
			prevEntry = e;
		}
		// check if the last period is finished properly
		if (nextPeriodStarted) {
			Calendar dayAfterLast = Calendar.getInstance();
			dayAfterLast.setTimeInMillis(newEntries.get(newEntries.size() - 1).getTime().getTimeInMillis());
			dayAfterLast.add(Calendar.DAY_OF_MONTH, 1);
			if (dayAfterLast.after(period[1])) {
				getTimeserieProfile().getEntries().addAll(getMaximums(newEntries, numberOfMaximumsPerInterval, millis24h));
			}
		}
		// sort entries by date (natural order)
		Collections.sort(getTimeserieProfile().getEntries(), new Comparator<TimeserieProfileEntry>() {
			@Override
			public int compare(TimeserieProfileEntry o1, TimeserieProfileEntry o2) {
				return o1.getTime().compareTo(o2.getTime());
			}
		});
	}

	private Calendar[] getNextInterval(Calendar currentTime, EHydrologicalInterval interval) {
		Calendar[] calendars = new Calendar[2];
		calendars[0] = Calendar.getInstance();
		calendars[1] = Calendar.getInstance();
		Calendar begin = interval.getintervalBegin();
		Calendar end = interval.getintervalEnd();
		calendars[0].setTimeInMillis(begin.getTimeInMillis());
		int periodBeginYear = currentTime.get(Calendar.YEAR);
		calendars[0].set(Calendar.YEAR, periodBeginYear);
		if (!currentTime.before(calendars[0])) {
			// we are already into or after the period, get the next one
			periodBeginYear++;
			calendars[0].set(Calendar.YEAR, periodBeginYear);
		}
		calendars[1].setTimeInMillis(end.getTimeInMillis());
		if (interval.isSameYearPeriod()) {
			calendars[1].set(Calendar.YEAR, periodBeginYear);
		} else {
			calendars[1].set(Calendar.YEAR, periodBeginYear + 1);
		}
		// because of Calendar bug, re-load time in millis
		calendars[0].setTimeInMillis(calendars[0].getTimeInMillis());
		calendars[1].setTimeInMillis(calendars[1].getTimeInMillis());

		System.out.println("----");
		System.out.println(currentTime.getTime().toString());
		System.out.println(calendars[0].getTime().toString());
		System.out.println(calendars[1].getTime().toString());
		return calendars;
	}

	public List<TimeserieProfile> transformToDischargeAreaSeries(TimeserieProfile baseProfile, EDischargeTimestepArea dischargeTimestepArea,
			IProgressMonitor monitor) {
		final List<TimeserieProfile> profiles = new ArrayList<TimeserieProfile>();
		if (monitor.isCanceled())
			return profiles;
		if (baseProfile.getEntries().size() == 0)
			return profiles;
		double[] corrections = new double[] { 1.14, 1.07, 1.04, 1.03, 1.03, 1.02 };
		// here we assume that the baseProfile timestep is correct
		List<TimeserieProfileEntry> entries = baseProfile.getEntries();
		for (int multFactor : dischargeTimestepArea.getMultiplicators()) {
			TimeserieProfile newProfile = TimeserieProfile.clone(baseProfile, false, false);
			newProfile.setDescription(String.format("Derived from '%s' (%s - %s)", baseProfile.getName(),
					AppUtils.DISPLAY_DATE_FORMAT.format(baseProfile.getTimeFrom().getTime()),
					AppUtils.DISPLAY_DATE_FORMAT.format(baseProfile.getTimeTo().getTime())));
			profiles.add(newProfile);
			List<TimeserieProfileEntry> list = newProfile.getEntries();
			final int size = entries.size();
			if (size > 0) {
				TimeZone timeZone = entries.get(0).getTime().getTimeZone();
				for (int i = 0; i < size; i += multFactor) {
					double sum = 0.0;
					for (int k = 0; k < multFactor; k++) {
						int index = i + k;
						if (index < size)
							sum += entries.get(index).getValue();
					}
					if (multFactor <= corrections.length) {
						sum *= corrections[multFactor - 1];
					}
					Calendar time = Calendar.getInstance(timeZone);
					time.setTimeInMillis(entries.get(i).getTime().getTimeInMillis() + multFactor * dischargeTimestepArea.getTimestepMillis());
					TimeserieProfileEntry entry = new TimeserieProfileEntry(0, time, sum);
					list.add(entry);
				}
			}
			newProfile.setTimeFrom(list.get(0).getTime());
			newProfile.setTimeTo(list.get(list.size() - 1).getTime());
			if (list.size() > 1) {
				newProfile.setTimestepMillis(list.get(1).getTime().getTimeInMillis() - list.get(0).getTime().getTimeInMillis());
			}
			newProfile.setNumberOfEntries(list.size());
		}
		return profiles;
	}

	public void removeValue(double value) {
		int index = -1;
		List<TimeserieProfileEntry> entries = getTimeserieProfile().getEntries();
		for (int i = 0; i < entries.size(); i++) {
			if (entries.get(i).getValue().doubleValue() == value) {
				index = i;
				break;
			}
		}
		if (index > -1) {
			entries.remove(index);
		}
	}

	public void exportProfileToZML(TimeserieProfile profile, String fileName) throws SensorException {
		profile.getEntries();
		IAxis[] axes = createAxis(profile);
		final Object[][] tuppleData = new Object[profile.getEntries().size()][2];
		int i = 0;
		for (TimeserieProfileEntry entry : profile.getEntries()) {
			tuppleData[i][0] = entry.getTime().getTime();
			tuppleData[i++][1] = entry.getValue();
		}
		ITupleModel tupleModel = new SimpleTupleModel(axes, tuppleData);
		final SimpleObservation observation = new SimpleObservation("href", profile.getName(), new MetadataList(), tupleModel); //$NON-NLS-1$ //$NON-NLS-2$
		ZmlFactory.writeToFile(observation, new File(fileName));
	}

	private IAxis[] createAxis(TimeserieProfile profile) {
		final IAxis dateAxis = new DefaultAxis("Datum", ITimeseriesConstants.TYPE_DATE, "", Date.class, true); //$NON-NLS-1$ //$NON-NLS-2$
		String valueType = profile.getTimeseriesType().getAbbreviation();
		TimeseriesUtils.getUnit(valueType);
		final IAxis valueAxis = new DefaultAxis(TimeseriesUtils.getName(valueType), valueType, TimeseriesUtils.getUnit(valueType), Double.class, false);
		final IAxis[] axis = new IAxis[] { dateAxis, valueAxis };
		return axis;
	}

	private List<TimeserieProfileEntry> getMaximums(List<TimeserieProfileEntry> entries, int numberOfMaximums, long minTimeDistanceMillis) {
		final List<TimeserieProfileEntry> myEntries = new ArrayList<TimeserieProfileEntry>();
		final List<TimeserieProfileEntry> myMaximums = new ArrayList<TimeserieProfileEntry>();
		myEntries.addAll(entries);
		Collections.sort(myEntries, new Comparator<TimeserieProfileEntry>() {
			@Override
			public int compare(TimeserieProfileEntry o1, TimeserieProfileEntry o2) {
				// we want descending order
				return o2.getValue().compareTo(o1.getValue());
			}
		});
		int entriesAdded = 0;
		while (entriesAdded < numberOfMaximums && myEntries.size() > 0) {
			final TimeserieProfileEntry currentMax = myEntries.remove(0);
			boolean shouldAdd = true;
			// check if the time distance is enough
			for (TimeserieProfileEntry max : myMaximums) {
				long difference = Math.abs(max.getTime().getTimeInMillis() - currentMax.getTime().getTimeInMillis());
				if (difference < minTimeDistanceMillis) {
					shouldAdd = false;
					break;
				}
			}
			if (shouldAdd) {
				myMaximums.add(currentMax);
				entriesAdded++;
			}
		}
		return myMaximums;
	}

	/**
	 * @return the timeserieProfile
	 */
	public TimeserieProfile getTimeserieProfile() {
		return m_timeserieProfile;
	}

}
