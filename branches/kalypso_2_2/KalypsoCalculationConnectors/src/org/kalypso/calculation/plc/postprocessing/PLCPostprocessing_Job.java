package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public class PLCPostprocessing_Job extends AbstractInternalStatusJob implements ISimulation {

    private final static String RESULT_NS = "org.kalypso.calculation.plc.postprocessing";
    private final static QName INUNDATION_STATUSQUO_COVERAGECOLLECTION = new QName(RESULT_NS, "inundationStatusQuoCoverageCollection");
    private final static QName INUNDATION_CALCULATED_COVERAGECOLLECTION = new QName(RESULT_NS, "inundationCalculatedCoverageCollection");
    private final static QName INUNDATION_DIFFERENCE_COVERAGECOLLECTION = new QName(RESULT_NS, "inundationDifferenceCoverageCollection");
    private final static QName RISK_STATUSQUO_COVERAGECOLLECTION = new QName(RESULT_NS, "riskStatusQuoCoverageCollection");
    private final static QName RISK_CALCULATED_COVERAGECOLLECTION = new QName(RESULT_NS, "riskCalculatedCoverageCollection");
    private final static QName RISK_DIFFERENCE_COVERAGECOLLECTION = new QName(RESULT_NS, "riskDifferenceCoverageCollection");

    /*
     * <input id="riskStatusQuoRasterDataModel" /> <input
     * id="riskStatusQuoRasterFolder" /> <input
     * id="riskCalculatedRasterDataModel" /> <input
     * id="riskCalculatedRasterFolder" /> <input
     * id="riskDifferenceRasterDataModel" /> <input
     * id="riskDifferenceRasterFolder" />
     * 
     * <output id="outputFolder" />
     */

    @Override
    public URL getSpezifikation() {
	return getClass().getResource("resources/modelSpecification.xml");
    }

    public URL getTemplate() {
	return getClass().getResource("resources/template.gml");
    }

    @Override
    public void run(final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor) throws SimulationException {
	try {

	    final GMLWorkspace riskStatusQuoRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskStatusQuoRasterDataModel"), null);
	    final GMLWorkspace riskCalculatedRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskCalculatedRasterDataModel"), null);
	    final GMLWorkspace riskDifferenceRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskDifferenceRasterDataModel"), null);

	    final File naResultsFolder = FileUtils.toFile((URL) inputProvider.getInputForID("naResultsFolder"));
	    final File riskStatusQuoRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskStatusQuoRasterFolder"));
	    final File riskCalculatedRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskCalculatedRasterFolder"));
	    final File riskDifferenceRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskDifferenceRasterFolder"));

	    final GMLWorkspace resultsWorkspace = GmlSerializer.createGMLWorkspace(getTemplate(), null);
	    final ICoverageCollection riskStatusQuoCoverageCollection = (ICoverageCollection) resultsWorkspace.getRootFeature().getProperty(RISK_STATUSQUO_COVERAGECOLLECTION);
	    final ICoverageCollection riskCalculatedCoverageCollection = (ICoverageCollection) resultsWorkspace.getRootFeature().getProperty(RISK_CALCULATED_COVERAGECOLLECTION);
	    final ICoverageCollection riskDifferenceCoverageCollection = (ICoverageCollection) resultsWorkspace.getRootFeature().getProperty(RISK_DIFFERENCE_COVERAGECOLLECTION);

	    final IRasterDataModel riskStatusQuoRasterDataModel = (IRasterDataModel) riskStatusQuoRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);
	    final IRasterDataModel riskCalculatedRasterDataModel = (IRasterDataModel) riskCalculatedRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);
	    final IRasterDataModel riskDifferenceRasterDataModel = (IRasterDataModel) riskDifferenceRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);

	    for (final ICoverage coverage : riskStatusQuoRasterDataModel.getRiskZonesCoverage())
		riskStatusQuoCoverageCollection.add(coverage);
	    for (final ICoverage coverage : riskCalculatedRasterDataModel.getRiskZonesCoverage())
		riskCalculatedCoverageCollection.add(coverage);
	    for (final ICoverage coverage : riskDifferenceRasterDataModel.getRiskZonesCoverage())
		riskDifferenceCoverageCollection.add(coverage);

	    final File propertiesFile = new File(tmpdir, "statistics.properties");
	    final Properties properties = new Properties();
	    properties.put("TOTAL_COST", 1.1E+6);
	    properties.store(new FileOutputStream(propertiesFile), "Scenario statistics");

	    final File naFolder = new File(tmpdir, "rrm");
	    final File riskFolder = new File(tmpdir, "risk");
	    final File file = new File(riskFolder, "result.gml");
	    GmlSerializer.serializeWorkspace(file, resultsWorkspace, "UTF-8");
	    FileUtils.copyDirectoryToDirectory(naFolder, naResultsFolder);
	    FileUtils.copyDirectoryToDirectory(riskFolder, riskStatusQuoRasterFolder);
	    FileUtils.copyDirectoryToDirectory(riskFolder, riskCalculatedRasterFolder);
	    FileUtils.copyDirectoryToDirectory(riskFolder, riskDifferenceRasterFolder);

	} catch (final Exception e) {
	    e.printStackTrace();
	    setStatus(STATUS.ERROR, e.getLocalizedMessage());
	}
	resultEater.addResult("outputFolder", tmpdir);
	setStatus(STATUS.OK, "Success");
    }
}
