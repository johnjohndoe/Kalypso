package org.kalypso.statistics.test;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.statistics.tools.GrubbsTest;
import org.kalypso.statistics.tools.Trend;
import org.kalypso.statistics.types.EDischargeTimestepArea;
import org.kalypso.statistics.types.EHydrologicalInterval;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.ETrendAdjustmentType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;

public class TestProcessor {

	private TestProcessor() {
	}

	public static void main(String[] args) throws SensorException, ParseException {
		System.out.println(System.currentTimeMillis());
		if (1 + 1 == 2)
			return;
		TestProcessor processor = new TestProcessor();
		// TimeserieProfile profile =
		// processor.loadProfile("./T_Fuhlsbuettel.zml");
		// TimeserieProfile profile =
		// processor.loadProfile("./N_Fuhlsbuettel.zml");

		// IObservation observation =
		// ZmlFactory.parseXML(TestProcessor.class.getResource("./statistics.zml"));
		// ITupleModel values = observation.getValues(null);
		// TimeserieProfile statsReport =
		// processor.loadProfile("./statistics.zml");

		// Calendar from = Calendar.getInstance();
		// Calendar to = Calendar.getInstance();
		// from.setTimeInMillis(new
		// SimpleDateFormat("dd.MM.yyyy HH:mm").parse("01.07.2002 00:00").getTime());
		// to.setTimeInMillis(new
		// SimpleDateFormat("dd.MM.yyyy HH:mm").parse("31.07.2002 23:59").getTime());
		// TimeserieProfile profile =
		// processor.loadProfile("./Wendlohstrasse_5min.zml", from, to);
		TimeserieProfile profile = processor.loadProfile("./Wendlohstrasse_5min.zml", null, null);
		System.out.println("Observation type: " + TimeseriesUtils.getName(profile.getTimeseriesType().getLabel()));
		System.out.println("Number of entries: " + profile.getEntries().size());
		System.out.println(profile.getEntries().get(0).getTime().getTime().toString());
		System.out.println(profile.getEntries().get(profile.getEntries().size() - 1).getTime().getTime().toString());
		List<TimeserieProfile> areaProfiles = processor.transformToDischargeAreaSeries(profile, EDischargeTimestepArea.AREA_DAYS);
		for (int i = 0; i < areaProfiles.size(); i++) {
			TimeserieProfile areaProfile = areaProfiles.get(i);
			System.out.println(String.format("Number of entries in area %d: %d", i, areaProfile.getEntries().size()));
			processor.saveProfile(areaProfile, String.format("./out/values_%dmin.zml", i));
			TimeserieProfile maximums = processor.getYearlyMaximums(areaProfile, 5, EHydrologicalInterval.CALENDAR_YEAR);
			if (maximums.getEntries().size() < 600) {
				double grubbsMax001 = GrubbsTest.getGrubbsMax(maximums, 0.01);
				double grubbsMax005 = GrubbsTest.getGrubbsMax(maximums, 0.05);
				if ((!new Double(grubbsMax001).isNaN()))
					System.out.println("1st Grubbs max (0.01): " + grubbsMax001);
				else
					System.out.println("No Grubbs max (0.01)");
				if ((!new Double(grubbsMax005).isNaN()))
					System.out.println("1st Grubbs max (0.05): " + grubbsMax005);
				else
					System.out.println("No Grubbs max (0.05)");
			}
			if (maximums.getEntries().size() < 600) {
				for (;;) {
					double grubbsMax001 = GrubbsTest.getGrubbsMax(maximums, 0.05);
					if (new Double(grubbsMax001).isNaN()) {
						break;
					} else {
						processor.removeValue(maximums, grubbsMax001);
						System.out.println("Outlier removed: " + grubbsMax001);
					}
				}
			}
			processor.saveProfile(maximums, String.format("./out/maximums_%dmin.zml", i));
			TimeserieProfile trend = Trend.getTrend(areaProfile);
			TimeserieProfile maxtrend = Trend.getTrend(maximums);
			processor.saveProfile(trend, String.format("./out/trend_%dmin.zml", i));
			processor.saveProfile(maxtrend, String.format("./out/maxTrend_%dmin.zml", i));
			TimeserieProfile trendAdjustedProfile = Trend.doTrendAdjustment(areaProfile, ETrendAdjustmentType.TIMESERIE_END);
			TimeserieProfile trendAdjustedMax = Trend.doTrendAdjustment(maximums, ETrendAdjustmentType.TIMESERIE_END);
			processor.saveProfile(trendAdjustedProfile, String.format("./out/trendAdj_%dmin.zml", i));
			processor.saveProfile(trendAdjustedMax, String.format("./out/trendAdjMax_%dmin.zml", i));
		}

	}

	private TimeserieProfile loadProfile(final String resource) throws SensorException {
		return loadProfile(resource, null, null);
	}

	private TimeserieProfile loadProfile(final String resource, Calendar from, Calendar to) throws SensorException {
		IObservation srcObservation = ZmlFactory.parseXML(TestProcessor.class.getResource(resource));
		String obsName = srcObservation.getName();
		if (obsName == null)
			obsName = "";
		// final MetadataList obsMetadataList =
		// srcObservation.getMetadataList();
		// for (final Entry<Object, Object> entry :
		// obsMetadataList.entrySet()) {
		// final String mdKey = (String) entry.getKey();
		// final String mdValue = (String) entry.getValue();
		// System.out.println(String.format("%s --> %s", mdKey, mdValue));
		// }
		final IAxis[] axes = srcObservation.getAxes();
		String valueType = null;
		for (IAxis axis : axes) {
			if (Double.class.isAssignableFrom(axis.getDataClass())) {
				valueType = axis.getType();
				break;
			}
		}
		if (valueType == null) {
			throw new IllegalArgumentException("Timeseries structure currently not supported [1]");
		}
		final TimeserieProfile profile = new TimeserieProfile(0, obsName, ETimeseriesType.getTypeFor(valueType));
		final ITupleModel model = srcObservation.getValues(null);
		boolean skip = false;
		for (int i = 0; i < model.size(); i++) {
			Calendar time = null;
			Double value = null;
			for (final IAxis axis : srcObservation.getAxes()) {
				if (axis.isPersistable()) {
					if (Date.class.isAssignableFrom(axis.getDataClass())) {
						time = Calendar.getInstance();
						time.setTimeInMillis(((Date) model.get(i, axis)).getTime());
						if (from != null)
							skip = from.after(time);
						if (!skip && to != null)
							skip = to.before(time);
					} else if (Double.class.isAssignableFrom(axis.getDataClass())) {
						value = (Double) model.get(i, axis);
					} else {
						throw new IllegalArgumentException("Data type currently not supported");
					}
				}
			}
			if (time != null && value != null) {
				if (!skip) {
					profile.getEntries().add(new TimeserieProfileEntry(0, time, value));
				}
			} else {
				throw new IllegalArgumentException("Timeseries structure currently not supported [2]");
			}
		}
		return profile;
	}

	private TimeserieProfile getYearlyMaximums(TimeserieProfile profile, int numberOfMaximumsPerYear, EHydrologicalInterval interval) {
		System.out.println(String.format("%s: %s", "Period", interval.getLabelFormatted()));
		final long millis24h = 24 * 60 * 60 * 1000;
		boolean yearStarted = false;
		List<TimeserieProfileEntry> entries = new ArrayList<TimeserieProfileEntry>();
		TimeserieProfile tp = TimeserieProfile.clone(profile, false, false);
		if (profile.getEntries().size() == 0) {
			return tp;
		}
		Calendar startCalendar = profile.getEntries().get(0).getTime();
		Calendar[] period = getInterval(startCalendar.get(Calendar.YEAR), interval);
		for (TimeserieProfileEntry e : profile.getEntries()) {
			Calendar time = e.getTime();
			// skip until first interval entry
			if (!yearStarted) {
				if (time.before(period[1])) {
					continue;
				} else {
					yearStarted = true;
					period = getInterval(time.get(Calendar.YEAR), interval);
				}
			}
			if (yearStarted) {
				if (time.after(period[1])) {
					// year finished, find maximums
					tp.getEntries().addAll(getMaximums(entries, numberOfMaximumsPerYear, millis24h));
					// restart period
					period = getInterval(time.get(Calendar.YEAR), interval);
					entries.clear();
				} else {
					entries.add(e);
				}
			}
		}
		// check if the last period is finished properly
		Calendar dayAfterLast = Calendar.getInstance();
		dayAfterLast.setTimeInMillis(entries.get(entries.size() - 1).getTime().getTimeInMillis());
		dayAfterLast.add(Calendar.DAY_OF_MONTH, 1);
		if (dayAfterLast.after(period[1])) {
			tp.getEntries().addAll(getMaximums(entries, numberOfMaximumsPerYear, millis24h));
		}

		// sort entries by date (natural order)
		Collections.sort(tp.getEntries(), new Comparator<TimeserieProfileEntry>() {
			@Override
			public int compare(TimeserieProfileEntry o1, TimeserieProfileEntry o2) {
				return o1.getTime().compareTo(o2.getTime());
			}
		});
		System.out.println("Maximums: ");
		for (TimeserieProfileEntry e : tp.getEntries()) {
			System.out.println(String.format("%s --> %.3f", e.getTime().getTime().toString(), e.getValue()));
		}
		return tp;
	}

	private Calendar[] getInterval(int beginYear, EHydrologicalInterval interval) {
		Calendar[] calendars = new Calendar[2];
		calendars[0] = Calendar.getInstance();
		calendars[1] = Calendar.getInstance();
		Calendar begin = interval.getintervalBegin();
		Calendar end = interval.getintervalEnd();
		calendars[0].set(beginYear, begin.get(Calendar.MONTH), begin.get(Calendar.DATE), begin.get(Calendar.HOUR_OF_DAY), begin.get(Calendar.MINUTE));
		calendars[1].set(interval.isSameYearPeriod() ? beginYear : beginYear + 1, end.get(Calendar.MONTH), end.get(Calendar.DATE),
				end.get(Calendar.HOUR_OF_DAY), end.get(Calendar.MINUTE));
		return calendars;
	}

	private List<TimeserieProfile> transformToDischargeAreaSeries(TimeserieProfile baseProfile, EDischargeTimestepArea dischargeTimestepArea) {
		final List<TimeserieProfile> profiles = new ArrayList<TimeserieProfile>();
		if (baseProfile.getEntries().size() == 0)
			return profiles;
		double[] corrections = new double[] { 1.14, 1.07, 1.04, 1.03, 1.03, 1.02 };
		// here we assume that the baseProfile timestep is correct
		List<TimeserieProfileEntry> entries = baseProfile.getEntries();
		for (int multFactor : dischargeTimestepArea.getMultiplicators()) {
			TimeserieProfile newProfile = new TimeserieProfile(0, baseProfile.getName(), baseProfile.getTimeseriesType());
			newProfile.setDescription("Mult: " + multFactor);
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
			newProfile.setTimeFrom(entries.get(0).getTime());
			newProfile.setTimeFrom(entries.get(entries.size() - 1).getTime());
		}
		return profiles;
	}

	private TimeserieProfile transformToDischargeType3(TimeserieProfile profile) {
		if (profile.getEntries().size() == 0)
			return profile;
		// TODO check if values are 5min
		int[] multiplicators = new int[] { 1, 2, 3, 4, 6 };
		double[] corrections = new double[] { 0.0, 1.14, 1.07, 1.04, 1.03, 1.03, 1.02 };
		int shortestListSize = Integer.MAX_VALUE;
		final Map<Integer, List<Double>> componentsMap = new HashMap<Integer, List<Double>>();
		List<TimeserieProfileEntry> entries = profile.getEntries();
		for (int i : multiplicators) {
			ArrayList<Double> list = new ArrayList<Double>();
			componentsMap.put(i, list);
			int size = entries.size();
			for (int j = 0; j < size; j += i) {
				double sum = 0.0;
				for (int k = 0; k < i; k++) {
					int index = j + k;
					if (index < size)
						sum += entries.get(index).getValue();
				}
				if (i < corrections.length) {
					sum *= corrections[i];
				}
				list.add(sum);
			}
			if (shortestListSize > list.size()) {
				shortestListSize = list.size();
			}
		}
		ArrayList<TimeserieProfileEntry> list = new ArrayList<TimeserieProfileEntry>();
		long firstMillis = entries.get(0).getTime().getTimeInMillis();
		long lastMillis = entries.get(entries.size() - 1).getTime().getTimeInMillis();
		TimeZone timeZone = entries.get(0).getTime().getTimeZone();
		long step = (lastMillis - firstMillis) / shortestListSize;
		for (int i = 0; i < shortestListSize; i++) {
			double sum = 0.0;
			for (int j : multiplicators) {
				sum += componentsMap.get(j).get(i);
			}
			Calendar time = Calendar.getInstance(timeZone);
			time.setTimeInMillis(firstMillis + i * step);
			list.add(new TimeserieProfileEntry(0, time, sum));
		}
		TimeserieProfile newProfile = new TimeserieProfile(0, profile.getName(), profile.getTimeseriesType());
		newProfile.getEntries().addAll(list);
		return newProfile;
	}

	private void removeValue(TimeserieProfile profile, double value) {
		int index = -1;
		List<TimeserieProfileEntry> entries = profile.getEntries();
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

	private void saveProfile(TimeserieProfile profile, String fileName) throws SensorException {
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

}
