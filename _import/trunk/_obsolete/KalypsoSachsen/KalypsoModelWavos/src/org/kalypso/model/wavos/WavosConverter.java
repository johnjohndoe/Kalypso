/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wavos;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.xml.sax.InputSource;

/**
 * @author thuel2
 */
public class WavosConverter {

	private static final ObjectFactory OF = new ObjectFactory();

	private static final JAXBContext JC = JaxbUtilities.createQuiet(OF
			.getClass());

	private final static Logger m_logger = Logger
			.getLogger(WavosInputWorker.class.getName());

	/**
	 * @param obsZml
	 * @param fleWavos
	 * @param header
	 */
	private static void zml2At(IObservation obsZml, File fleWavos, String header)
			throws Exception {
		final Writer wrtrAT = new OutputStreamWriter(new FileOutputStream(
				fleWavos), WavosConst.WAVOS_CODEPAGE);

		zml2At(obsZml, wrtrAT, header);
		IOUtils.closeQuietly(wrtrAT);
	}

	/**
	 * @param obsZml
	 * @param wrtrAT
	 * @param header
	 */
	private static void zml2At(final IObservation obsZml, final Writer wrtrAT,
			final String header) throws Exception {
		final PrintWriter pWrtr;
		MetadataList metadataList = obsZml.getMetadataList();
		if (metadataList.containsKey(TimeserieConstants.MD_WQTABLE)) {
			final String wqtable = metadataList
					.getProperty(TimeserieConstants.MD_WQTABLE);

			final WQTableSet tableSet = WQTableFactory.parse(new InputSource(
					new StringReader(wqtable)));
			final WQTable[] tables = tableSet.getTables();
			if (tables.length > 0) {
				// TODO extract as method (TimeserieUtils)
				final IAxis[] axisList = obsZml.getAxisList();
				final ITuppleModel resultValues = obsZml.getValues(null);
				final IAxis resultDateAxis = ObservationUtilities
						.findAxisByClass(axisList, Date.class);
				final IAxisRange rangeFor = resultValues
						.getRangeFor(resultDateAxis);

				final WQTable tbleBegin = tableSet.getFor((Date) rangeFor
						.getLower());
				final WQTable table = tableSet.getFor((Date) rangeFor
						.getUpper());
				if (!tbleBegin.equals(table))
					m_logger.info("");

				pWrtr = new PrintWriter(wrtrAT);
				final Date validity = table.getValidity();
				final WQPair[] wqPairs = table.getPairs();
				int count = wqPairs.length;
				pWrtr.println(header + "\tWmin: " + wqPairs[0].getW()
						+ " Wmax: " + wqPairs[count - 1].getW()
						+ " gültig ab: " + validity.toLocaleString());

				// 2nd line of header
				pWrtr.println(String.format(Locale.ENGLISH,
						WavosConst.WAVOS_COUNT_WQ_PAIRS_FORMAT, Integer
								.valueOf(Integer.toString(count))));
				
				for (int ii = 0; ii < count; ii++) {
					final WQPair pair = wqPairs[ii];

					pWrtr.println(String.format(Locale.ENGLISH,
							WavosConst.WAVOS_COUNT_WQ_FORMAT, Double
									.valueOf(Double.toString(pair.getW())),
							Double.valueOf(Double.toString(pair.getQ()))));
				}

				IOUtils.closeQuietly(pWrtr);
			}
		}

	}

	/**
	 * @param zmlUrl
	 * @param tafelDir
	 * @param fileNme
	 * @param zmlId
	 * @param overWrite
	 */
	public static void zml2At(final URL zmlUrl, final File tafelDir,
			final String fileNme, final String zmlId, final boolean overWrite)
			throws Exception {
		final IObservation obsZml = ZmlFactory.parseXML(zmlUrl, zmlId);
		final URLConnection urlConTest = UrlUtilities.connectQuietly(zmlUrl);

		if (urlConTest != null) {
			MetadataList metadataList = obsZml.getMetadataList();
			if (overWrite
					&& metadataList.containsKey(TimeserieConstants.MD_WQTABLE)) {
				final File fleWavos = new File(tafelDir, fileNme);
				final String sHeader = fleWavos.getName() + ": ";

				zml2At(obsZml, fleWavos, sHeader);
			} else {
				m_logger
						.info("\t\tFür die Zeitreihe "
								+ zmlId
								+ " konnte keine aktuelle AT-Datei geschrieben werden. Es wird die Standard-Datei verwendet.");
			}

			return;
		}
		m_logger
				.info("Zeitreihe "
						+ zmlUrl.toString()
						+ " kann nicht geöffnet werden (da vielleicht nicht vorhanden).");
		throw new Exception(
				"Zeitreihe "
						+ zmlUrl.toString()
						+ " kann nicht geöffnet werden (da vielleicht nicht vorhanden).");
	}

	/**
	 * @param nativeOutVorherDir
	 * @param outputZmlDir
	 * @param props
	 * @param metaMap
	 * @throws SensorException
	 * @throws IOException
	 * @throws JAXBException
	 * @throws FactoryException
	 */
	public static void convertVorher2Zml(final File nativeOutVorherDir,
			final File outputZmlDir, final Properties props, final Map metaMap,
			final boolean writeUmhuellende) throws SensorException,
			IOException, JAXBException, FactoryException, SimulationException {
		final Map mapKennObs = new HashMap();
		// GML: alle Pegel holen
		final GMLWorkspace wks = (GMLWorkspace) props.get(WavosConst.DATA_GML);
		final Date startForecast = (Date) props
				.get(WavosConst.DATA_STARTFORECAST_DATE);

		String sFleVorher = WavosUtils.createInputFleName(startForecast);
		// und füllen...
		final File fleVorher = new File(nativeOutVorherDir, sFleVorher);
		final ArrayList zeilen = new ArrayList();
		FileReader readVorher = null;
		LineNumberReader lneNumReadVorher = null;
		FileOutputStream stream = null;
		try {
			readVorher = new FileReader(fleVorher);
			lneNumReadVorher = new LineNumberReader(readVorher);

			// StringTokenizer über die erste Zeile
			// --- Datum wegschmeißen
			// --- Liste der internen Kennungen (Key=Nummer, value=kenn)
			final String ersteZeile = lneNumReadVorher.readLine();
			final StringTokenizer tokErsteZeile = new StringTokenizer(
					ersteZeile, " ");
			final Map mapKennPos = new HashMap();
			int cntToks = 0;
			while (tokErsteZeile.hasMoreTokens()) {
				cntToks++;
				final String nextToken = tokErsteZeile.nextToken();
				if (cntToks > 5) {
					mapKennPos.put(Integer.toString(cntToks), nextToken);
				}
			}
			// alle anderen Zeilen lesen
			// erste Spalte Datum
			while (lneNumReadVorher.ready()) {
				final String inputline = lneNumReadVorher.readLine();
				if (inputline == null)
					break;

				zeilen.add(new StringTokenizer(inputline, " "));
			}
			final int cntZeilen = zeilen.size();
			final FeatureList pegelList = (FeatureList) wks
					.getFeatureFromPath("pegelMember[VorhersagePegel]");
			for (final Iterator iter = pegelList.iterator(); iter.hasNext();) {
				final Feature pegel = (Feature) iter.next();

				final String kenn = String.format(Locale.ENGLISH,"%s", pegel
						.getProperty("kenn"));

				// ZMLs dazu erzeugen (Metadaten nicht vergessen...)
				final ZmlInfo info = (ZmlInfo) metaMap.get(kenn);
				final IObservation zmlObs = createResultObsForPegel(pegel,
						info, cntZeilen);
				mapKennObs.put(kenn, zmlObs);
			}
			for (int ii = 0; ii < cntZeilen; ii++) {
				final StringTokenizer tokZeile = (StringTokenizer) zeilen
						.get(ii);
				cntToks = 0;
				VorherDate vDte = new VorherDate();

				while (tokZeile.hasMoreTokens()) {
					cntToks++;
					final String nextToken = tokZeile.nextToken();
					if (cntToks <= 5)
						// Datum extrahieren
						vDte.addDatePart(nextToken);
					else
					// Werte
					{
						// in richtige Obs schreiben
						// ganz schön durch die Brust ins Auge...
						// TODO? ggf. schneller, wenn die Werte erst
						// zwischengespeichert werden in ArrayLists und dann
						// geschrieben
						// werden...
						final Date date = vDte.getDate();
						final Double value = Double.valueOf(nextToken);
						final String kenn = (String) mapKennPos.get(Integer
								.toString(cntToks));
						final ITuppleModel values = ((IObservation) mapKennObs
								.get(kenn)).getValues(null);
						final IAxis[] axisList = values.getAxisList();

						values.setElement(ii, date, axisList[0]);
						values.setElement(ii, value, axisList[1]);
					}
				}
			}
			// ZMLs serialisieren
			for (final Iterator iter = pegelList.iterator(); iter.hasNext();) {
				final Feature pegel = (Feature) iter.next();

				final String kenn = String.format(Locale.ENGLISH,"%s", pegel
						.getProperty("kenn"));
				final String name = String.format(Locale.ENGLISH,"%s", pegel
						.getProperty("name"));

				final File fleZml = new File(outputZmlDir, name + ".zml");
				final IObservation zmlObs = (IObservation) mapKennObs.get(kenn);
				stream = new FileOutputStream(fleZml);
				final OutputStreamWriter writer = new OutputStreamWriter(
						stream, WavosConst.WAVOS_CODEPAGE);
				// TODO: eventually add some prefixes via overloaded
				// createMarshaller method
				final Marshaller marshaller = JaxbUtilities.createMarshaller(
						JC, true);
				marshaller.marshal(ZmlFactory.createXML(zmlObs, null), writer);
				writer.close();

				// und noch Umhüllende dran...
				if (writeUmhuellende) {
					final String sZmlFileBaseName = fleZml.getName()
							.replaceAll(".zml", "");
					final Object objAccuracy = pegel
							.getProperty("accuracyPrediction");
					double accuracy = LhwzHelper
							.getDefaultUmhuellendeAccuracy();
					if (objAccuracy instanceof Double)
						accuracy = ((Double) objAccuracy).doubleValue();

					// und Umhüllende "_unten", "_oben"
					final URL is = fleZml.toURL();
					final IObservation obsZml = ZmlFactory.parseXML(is, "");
					final IAxis[] axisList = obsZml.getAxisList();
					final String axisType;
					if (ObservationUtilities.hasAxisOfType(axisList,
							TimeserieConstants.TYPE_RUNOFF))
						axisType = TimeserieConstants.TYPE_RUNOFF;
					else if (ObservationUtilities.hasAxisOfType(axisList,
							TimeserieConstants.TYPE_WATERLEVEL))
						axisType = TimeserieConstants.TYPE_WATERLEVEL;
					else
						throw new SimulationException(
								"Ergebniszeitreihe enthält weder Abfluss noch Wasserstand, Umhüllendenberechnung nicht möglich.",
								null);
					// get first and last date of observation
					final IAxis dateAxis = ObservationUtilities.findAxisByType(
							axisList, TimeserieConstants.TYPE_DATE);
					final IAxis valueAxis = ObservationUtilities
							.findAxisByType(axisList, axisType);
					final ITuppleModel values = obsZml.getValues(null);
					final int valueCount = values.getCount();
					if (valueCount > 1) {
						final org.kalypso.ogc.sensor.DateRange forecastRange = TimeserieUtils
								.isForecast(obsZml);
						if (forecastRange != null) {
							final Date startPrediction = forecastRange
									.getFrom();

							// final Date endPrediction = forecastRange.getTo();
							// sicher ist sicher...
							final Date endPrediction = (Date) values
									.getElement(valueCount - 1, dateAxis);
							final Double endValue = (Double) values.getElement(
									valueCount - 1, valueAxis);

							final Calendar calBegin = Calendar.getInstance();
							calBegin.setTime(startPrediction);

							final Calendar calEnd = Calendar.getInstance();
							calEnd.setTime(endPrediction);

							final long millisOf60hours = 1000 * 60 * 60 * 60;

							final double endAccuracy = accuracy
									* (((double) (endPrediction.getTime() - startPrediction
											.getTime())) / ((double) millisOf60hours));

							final double endOffset = Math.abs(endValue
									.doubleValue()
									* endAccuracy / 100);

							final IRequest request = new ObservationRequest(
									calBegin.getTime(), calEnd.getTime());

							TranProLinFilterUtilities.transformAndWrite(obsZml,
									calBegin, calEnd, 0, endOffset, "-",
									axisType, KalypsoStati.BIT_DERIVATED,
									new File(fleZml.getParentFile(),
											sZmlFileBaseName + "_unten.zml"),
									"- Spur Unten", request);
							TranProLinFilterUtilities.transformAndWrite(obsZml,
									calBegin, calEnd, 0, endOffset, "+",
									axisType, KalypsoStati.BIT_DERIVATED,
									new File(fleZml.getParentFile(),
											sZmlFileBaseName + "_oben.zml"),
									"- Spur Oben", request);

						}
					}
				}
			}
		} finally {
			IOUtils.closeQuietly(lneNumReadVorher);
			IOUtils.closeQuietly(readVorher);
			IOUtils.closeQuietly(stream);
		}
	}

	/**
	 * @param pegel
	 * @param info
	 */
	private static IObservation createResultObsForPegel(final Feature pegel,
			final ZmlInfo info, final int vorhersageCount) {

		final String forecastAxis = TimeserieConstants.TYPE_WATERLEVEL;
		IObservation obsOut = null;
		MetadataList metaDataList = new MetadataList();
		metaDataList.putAll(info.getMetadata());

		IAxis[] axis = createAxis(forecastAxis);
		Object[][] tuppleData = new Object[vorhersageCount][2];
		ITuppleModel tplWerte = new SimpleTuppleModel(axis, tuppleData);

		final String kenn = (String) pegel.getProperty("kenn");
		final String name = (String) pegel.getProperty("name");
		obsOut = new SimpleObservation("href", kenn, name, false, null,
				metaDataList, axis, tplWerte);

		return obsOut;
	}

	public static IAxis[] createAxis(final String sValueType) {
		final IAxis dateAxis = new DefaultAxis("Datum",
				TimeserieConstants.TYPE_DATE, "", Date.class, true);
		TimeserieUtils.getUnit(sValueType);
		final IAxis valueAxis = new DefaultAxis(TimeserieUtils
				.getName(sValueType), sValueType, TimeserieUtils
				.getUnit(sValueType), Double.class, false);
		final IAxis[] axis = new IAxis[] { dateAxis, valueAxis };
		return axis;
	}
}
