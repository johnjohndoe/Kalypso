package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.net.URL;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.UrlCatalog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

public class PLCPostprocessing_Job extends AbstractInternalStatusJob implements ISimulation {

    private final static QName INUNDATION_STATUSQUO_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "inundationStatusQuoCoverageCollection");
    private final static QName INUNDATION_CALCULATED_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "inundationCalculatedCoverageCollection");
    private final static QName INUNDATION_DIFFERENCE_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "inundationDifferenceCoverageCollection");
    private final static QName RISK_STATUSQUO_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "riskStatusQuoCoverageCollection");
    private final static QName RISK_CALCULATED_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "riskCalculatedCoverageCollection");
    private final static QName RISK_DIFFERENCE_COVERAGECOLLECTION = new QName(UrlCatalog.NS_CCHAINRESULTS, "riskDifferenceCoverageCollection");

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
	    /**
        	<input id="naResultsFolder" />
        	<input id="riskStatusQuoRasterDataModel" />
        	<input id="riskStatusQuoRasterFolder" />
        	<input id="riskCalculatedRasterDataModel" />
        	<input id="riskCalculatedRasterFolder" />
        	<input id="riskDifferenceStatistics" />
        	<input id="riskDifferenceRasterDataModel" />
        	<input id="riskDifferenceRasterFolder" />
        	
        	<output id="outputFolder" />
	     */
	    
	    final GMLWorkspace riskStatusQuoRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskStatusQuoRasterDataModel"), null);
	    final GMLWorkspace riskCalculatedRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskCalculatedRasterDataModel"), null);
	    final GMLWorkspace riskDifferenceRasterDataModelWS = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("riskDifferenceRasterDataModel"), null);

	    final File naResultsFolder = FileUtils.toFile((URL) inputProvider.getInputForID("naResultsFolder"));
	    final File statisticsFile = FileUtils.toFile((URL) inputProvider.getInputForID("riskDifferenceStatistics"));
	    final File riskStatusQuoRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskStatusQuoRasterFolder"));
	    final File riskCalculatedRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskCalculatedRasterFolder"));
	    final File riskDifferenceRasterFolder = FileUtils.toFile((URL) inputProvider.getInputForID("riskDifferenceRasterFolder"));
	    
//	    final File outputModel = FileUtils.toFile(getTemplate());

	    final GMLWorkspace resultsWorkspace = GmlSerializer.createGMLWorkspace(getTemplate(), null);
	    final ICoverageCollection riskStatusQuoCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty(RISK_STATUSQUO_COVERAGECOLLECTION)).getAdapter( ICoverageCollection.class );
	    final ICoverageCollection riskCalculatedCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty(RISK_CALCULATED_COVERAGECOLLECTION)).getAdapter( ICoverageCollection.class );
	    final ICoverageCollection riskDifferenceCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty(RISK_DIFFERENCE_COVERAGECOLLECTION)).getAdapter( ICoverageCollection.class );

	    final IRasterDataModel riskStatusQuoRasterDataModel = (IRasterDataModel) riskStatusQuoRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);
	    final IRasterDataModel riskCalculatedRasterDataModel = (IRasterDataModel) riskCalculatedRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);
	    final IRasterDataModel riskDifferenceRasterDataModel = (IRasterDataModel) riskDifferenceRasterDataModelWS.getRootFeature().getAdapter(IRasterDataModel.class);

	    for (final ICoverage coverage : riskStatusQuoRasterDataModel.getRiskZonesCoverage())
		riskStatusQuoCoverageCollection.add(coverage);
	    for (final ICoverage coverage : riskCalculatedRasterDataModel.getRiskZonesCoverage())
		riskCalculatedCoverageCollection.add(coverage);
	    for (final ICoverage coverage : riskDifferenceRasterDataModel.getRiskZonesCoverage())
		riskDifferenceCoverageCollection.add(coverage);

	    final File naFolder = new File(tmpdir, "rrm");
	    final File riskFolder = new File(tmpdir, "risk");
	    final File file = new File(riskFolder, "result.gml");
	    naFolder.mkdirs();
	    riskFolder.mkdirs();
	    file.createNewFile();
	    GmlSerializer.serializeWorkspace(file, resultsWorkspace, "UTF-8");
	    FileUtils.copyFileToDirectory(statisticsFile, riskFolder);
	    FileUtils.copyDirectoryToDirectory(riskStatusQuoRasterFolder, riskFolder);
	    FileUtils.copyDirectoryToDirectory(riskCalculatedRasterFolder, riskFolder);
	    FileUtils.copyDirectoryToDirectory(riskDifferenceRasterFolder, riskFolder);
	    FileUtils.copyDirectoryToDirectory(naResultsFolder, naFolder);

	} catch (final Exception e) {
	    e.printStackTrace();
	    setStatus(STATUS.ERROR, e.getLocalizedMessage());
	}
	resultEater.addResult("outputFolder", tmpdir);
	setStatus(STATUS.OK, "Success");
    }
}
