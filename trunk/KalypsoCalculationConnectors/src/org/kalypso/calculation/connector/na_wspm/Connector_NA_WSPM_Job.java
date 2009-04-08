package org.kalypso.calculation.connector.na_wspm;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_NA_WSPM;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class Connector_NA_WSPM_Job extends AbstractInternalStatusJob implements ISimulation {
    public Connector_NA_WSPM_Job() {
    }

    @Override
    public URL getSpezifikation() {
	return getClass().getResource("resources/modelSpecification.xml");
    }

    @Override
    public void run(final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor) throws SimulationException {
	final URL urlModelNA = (URL) inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.NA_Model.name());
	final URL urlControlModelNA = (URL) inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.NA_ControlModel.name());
	final URL urlModelWSPM = (URL) inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.WSPM_Model.name());
	final URL urlStatisticalReportNA = (URL) inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.NA_StatisticalReport.name());
	final Object riverCode = inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.NA_RiverCode.name());
	final String naRiverCode = riverCode == null ? "" : riverCode.toString();
	final String runOffEventID = inputProvider.getInputForID(MODELSPEC_CONNECTOR_NA_WSPM.WSPM_RunoffEventID.name()).toString();
	try {
	    final File outputFile = File.createTempFile("outTempWspmModel", ".gml", tmpdir);
	    final File gmlModelFileNA = new File(urlModelNA.getPath());
	    if (gmlModelFileNA.exists()) {
		try {
		    final TupleResult wspmRunoffEventTupleResult;
		    final List<BigDecimal> kilometerValuesList = new ArrayList<BigDecimal>();
		    final GMLWorkspace workspaceNA = GmlSerializer.createGMLWorkspace(urlModelNA, null);
		    final GMLWorkspace workspaceControlModelNA = GmlSerializer.createGMLWorkspace(urlControlModelNA, null);
		    final GMLWorkspace workspaceWSPM = GmlSerializer.createGMLWorkspace(urlModelWSPM, null);

		    Feature wspmObservationFeature = null;
		    org.kalypso.observation.IObservation<TupleResult> wspmObservation = null;
		    final IFeatureType wspmRunoffEventFeatureType = workspaceWSPM.getGMLSchema().getFeatureType(new QName(IWspmConstants.NS_WSPMRUNOFF, "RunOffEvent"));
		    final Feature[] wspmRunoffEvents = workspaceWSPM.getFeatures(wspmRunoffEventFeatureType);
		    for (final Feature feature : wspmRunoffEvents) {
			if (runOffEventID.equals(feature.getId())) {
			    wspmObservationFeature = feature;
			    break;
			}
		    }
		    if (wspmObservationFeature != null) {
			wspmObservation = ObservationFeatureFactory.toObservation(wspmObservationFeature);
			wspmRunoffEventTupleResult = wspmObservation.getResult();
		    } else
			throw new SimulationException("Undefined WSPM runoff event ID specified.");
		    
		    wspmRunoffEventTupleResult.clear();
		    int stationComp = -1;
		    int abflussComp = -1;
		    final IComponent[] wspmRunoffEventTupleResultComponents = wspmRunoffEventTupleResult.getComponents();
		    for (int i = 0; i < wspmRunoffEventTupleResultComponents.length; i++) {
			final IComponent comp = wspmRunoffEventTupleResultComponents[i];
			// TODO: get component via phenomenon
			if (comp.getName().startsWith("Abfluss")) {
			    abflussComp = i;
			}
			if (comp.getName().startsWith("Station")) //$NON-NLS-1$
			{
			    stationComp = i;
			    wspmRunoffEventTupleResult.setSortComponents(new IComponent[] { comp });
			}
		    }

		    final IObservation maxDischargesObservation = ZmlFactory.parseXML(urlStatisticalReportNA, null);
		    final IAxis[] axisList = maxDischargesObservation.getAxisList();
		    IAxis axisNodeNr = null;
		    IAxis axisMaxDischarge = null;
		    // Observation structure:
		    // "Knoten - Nr.", Integer
		    // "Stationierung", String
		    // "Datum", Date
		    // "max. Abfluss", Double
		    // (unfortunately, this values are hard-coded in the
		    // observation...)
		    for (final IAxis element : axisList) {
			if ("Knoten - Nr.".equals(element.getName())) {
			    axisNodeNr = element;
			}
			if ("max. Abfluss".equals(element.getName())) {
			    axisMaxDischarge = element;
			}
		    }
		    if (axisNodeNr == null || axisMaxDischarge == null) {
			// TODO error report & exit
			setStatus(STATUS.ERROR, "No proper axis found.");
			return;
		    }

		    final ITuppleModel tuppleModel = maxDischargesObservation.getValues(null);

		    final IFeatureType naNodeFeatureType = workspaceNA.getGMLSchema().getFeatureType(NaModelConstants.NODE_ELEMENT_FT);
		    final Feature[] naNodes = workspaceNA.getFeatures(naNodeFeatureType);

		    for (int i = 0; i < tuppleModel.getCount(); i++) {
			final String nodeNr = tuppleModel.getElement(i, axisNodeNr).toString();
			final Double maxDischarge = Double.parseDouble(tuppleModel.getElement(i, axisMaxDischarge).toString());
			for (final Feature feature : naNodes) {
			    if (feature.getName().equals(nodeNr)) {
				if (naRiverCode != null && naRiverCode.length() > 0) {
				    final Object nodeRiverCodeProperty = feature.getProperty(NaModelConstants.NODE_RIVER_CODE_PROP);
				    if (nodeRiverCodeProperty == null || !naRiverCode.equals(nodeRiverCodeProperty)) {
					continue;
				    }
				}
				final Object riverKilometerProp = feature.getProperty(NaModelConstants.NODE_RIVER_KILOMETER_PROP);
				if (riverKilometerProp != null) {
				    final Double nodeRiverKilometerProperty = Double.parseDouble(riverKilometerProp.toString());
				    final BigDecimal bigDecimalRiverKilometer = new BigDecimal(nodeRiverKilometerProperty).setScale(IProfileFeature.STATION_SCALE, RoundingMode.HALF_UP);
				    final BigDecimal bigDecimalDischarge = new BigDecimal(maxDischarge).setScale(3, RoundingMode.HALF_UP);

				    if (kilometerValuesList.contains(bigDecimalRiverKilometer)) {
					// TODO: what to do if there are
					// duplicate values for river kilometer?
					// It is not allowed as WSPM input!
					setStatus(STATUS.ERROR, "Duplicate value for river kilometer (formatted to 0.0000): " + bigDecimalRiverKilometer);
					return;
				    } else {
					kilometerValuesList.add(bigDecimalRiverKilometer);
				    }

				    final IRecord record = wspmRunoffEventTupleResult.createRecord();
				    wspmRunoffEventTupleResult.add(record);
				    record.setValue(stationComp, bigDecimalRiverKilometer);
				    record.setValue(abflussComp, bigDecimalDischarge);
				}
			    }
			}
		    }
		    ObservationFeatureFactory.toFeature(wspmObservation, wspmObservationFeature);
		    final Object returnPeriod = workspaceControlModelNA.getRootFeature().getProperty(NaModelConstants.CONTROL_RETURN_PERIOD_PROP);
		    final QName returnPeriodQName = new QName(IWspmConstants.NS_WSPMRUNOFF, "returnPeriod");
		    wspmObservationFeature.setProperty(returnPeriodQName, (returnPeriod != null && ((Integer) returnPeriod) > 0) ? returnPeriod : 1);
		    GmlSerializer.serializeWorkspace(outputFile, workspaceWSPM, "UTF-8");
		    setStatus(STATUS.OK, "Success");
		} catch (final Exception e) {
		    setStatus(STATUS.ERROR, e.getLocalizedMessage());
		}
	    } else {
		setStatus(STATUS.ERROR, "no input file");
	    }
	    if (isOkStatus()) {
		resultEater.addResult(MODELSPEC_CONNECTOR_NA_WSPM.WSPM_Model.name(), outputFile);
	    }
	} catch (final IOException e) {
	    e.printStackTrace();
	}
    }

}
