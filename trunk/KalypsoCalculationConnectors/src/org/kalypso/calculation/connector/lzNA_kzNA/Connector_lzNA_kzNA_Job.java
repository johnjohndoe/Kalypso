package org.kalypso.calculation.connector.lzNA_kzNA;

import java.io.File;
import java.net.URL;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_LZNA_KZNA;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class Connector_lzNA_kzNA_Job extends AbstractInternalStatusJob implements ISimulation {

    public Connector_lzNA_kzNA_Job() {
    }

    @Override
    public URL getSpezifikation() {
	return getClass().getResource("resources/modelSpecification.xml");
    }

    @Override
    public void run(final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor) throws SimulationException {
	final URL urlControlModelKZ = (URL) inputProvider.getInputForID(MODELSPEC_CONNECTOR_LZNA_KZNA.KZNA_CALCULATION.name());
	try {
	    final File resultsFolderLZ = new File(inputProvider.getInputForID(MODELSPEC_CONNECTOR_LZNA_KZNA.LZNA_ERGEBNISSE_AKTUEL_ANFANGWERTE.name()).toString());
	    final GMLWorkspace workspaceControlModelKZ = GmlSerializer.createGMLWorkspace(urlControlModelKZ, null);
	    
	    // TODO @dejan - unknown property exception - startsimulation should be {http://www.tuhh.de/initialValues}iniDate
      // ?!?
	    final XMLGregorianCalendar calendar = (XMLGregorianCalendar) workspaceControlModelKZ.getRootFeature().getProperty(NaModelConstants.CONTROL_STARTSIMULATION);
	    final String fileName = String.format("%1$tY%1$tm%1$td(%1$tH).gml", calendar.toGregorianCalendar().getTime());
	    final File lzSim = new File(resultsFolderLZ, fileName);
	    if (lzSim.exists())
		resultEater.addResult(MODELSPEC_CONNECTOR_LZNA_KZNA.KZNA_ANFANGWERTE_LZSIM.name(), lzSim);
	} catch (final Exception e) {
	    throw new SimulationException(e.getLocalizedMessage());
	}
    }
}