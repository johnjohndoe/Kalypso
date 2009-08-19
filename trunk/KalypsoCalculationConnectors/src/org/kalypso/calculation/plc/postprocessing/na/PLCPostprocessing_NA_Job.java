package org.kalypso.calculation.plc.postprocessing.na;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class PLCPostprocessing_NA_Job extends AbstractInternalStatusJob implements ISimulation {

    @Override
    public URL getSpezifikation() {
	return getClass().getResource("resources/modelSpecification.xml");
    }

    @Override
    public void run(final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor) throws SimulationException {
	File inputSteadyNodesFolder = FileUtils.toFile((URL) inputProvider.getInputForID("SteadyStateNodesFolder"));
	File inputCalculatedNodesFolder = FileUtils.toFile((URL) inputProvider.getInputForID("CalculatedNodesFolder"));

	try {
	    
	    // TODO not finished, create shape!
//	    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("naModel"), null);
	    

	    final File outputSubfolderSteady = new File(tmpdir, "izNodes");
	    final File outputSubfolderCalculated = new File(tmpdir, "sudsNodes");
	    outputSubfolderSteady.mkdirs();
	    outputSubfolderCalculated.mkdirs();
	    if (!inputCalculatedNodesFolder.isDirectory() || !inputSteadyNodesFolder.isDirectory()) {
		setStatus(STATUS.ERROR, "Input folder(s) does not exists.");
		return;
	    }
	    for (final File file : inputSteadyNodesFolder.listFiles()) {
		final String name = file.getName();
		if ("Knoten".equals(name) || "Node".equals(name)) {
		    inputSteadyNodesFolder = file;
		    break;
		}
	    }
	    for (final File file : inputCalculatedNodesFolder.listFiles()) {
		final String name = file.getName();
		if ("Knoten".equals(name) || "Node".equals(name)) {
		    inputCalculatedNodesFolder = file;
		    break;
		}
	    }

	    final File[] inputSteadyNodesSubfolder = inputSteadyNodesFolder.listFiles();
	    final File[] inputCalculatedNodesSubfolder = inputCalculatedNodesFolder.listFiles();
	    if (inputSteadyNodesSubfolder.length != inputCalculatedNodesSubfolder.length) {
		setStatus(STATUS.ERROR, "Input folders are not comparable.");
		return;
	    }
	    for (int i = 0; i < inputSteadyNodesSubfolder.length; i++) {
		final File steadyNodeFolder = inputSteadyNodesSubfolder[i];
		final File calculatedNodeFolder = inputCalculatedNodesSubfolder[i];
		FileUtils.copyDirectoryToDirectory(steadyNodeFolder, outputSubfolderSteady);
		FileUtils.copyDirectoryToDirectory(calculatedNodeFolder, outputSubfolderCalculated);
		final String nodeID = steadyNodeFolder.getName();
		final Obsdiagview view = NodeResultsComparisonViewCreator.createView("Gesamtabfluss: " + nodeID, "", "izNodes/" + nodeID + "/Gesamtabfluss.zml", "sudsNodes/" + nodeID + "/Gesamtabfluss.zml", nodeID);
		final File odtFile = new File(tmpdir, nodeID + ".odt");
		final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(odtFile), "UTF-8"));
		DiagViewUtils.saveDiagramTemplateXML(view, out);
	    }

	} catch (final Exception e) {
	    setStatus(STATUS.ERROR, e.getLocalizedMessage());
	    e.printStackTrace();

	}
	resultEater.addResult("OutputFolder", tmpdir);
	setStatus(STATUS.OK, "Success");
    }

}
