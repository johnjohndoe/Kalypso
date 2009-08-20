package org.kalypso.calculation.plc.postprocessing.na;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

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
	    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("naModel"), null);
	    final Feature catchmentCollection = (Feature) modelWorkspace.getRootFeature().getProperty(NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP);
	    final FeatureList catchmentList = (FeatureList) catchmentCollection.getProperty(NaModelConstants.CATCHMENT_MEMBER_PROP);
	    final Feature nodeCollection = (Feature) modelWorkspace.getRootFeature().getProperty(NaModelConstants.NODE_COLLECTION_MEMBER_PROP);
	    final FeatureList nodeList = (FeatureList) nodeCollection.getProperty(NaModelConstants.NODE_MEMBER_PROP);
	    final Feature channelCollection = (Feature) modelWorkspace.getRootFeature().getProperty(NaModelConstants.CHANNEL_COLLECTION_MEMBER_PROP);
	    final FeatureList channelList = (FeatureList) channelCollection.getProperty(NaModelConstants.CHANNEL_MEMBER_PROP);

	    /*
	     * Create feature type which describes what data the shape file
	     * contains
	     */
	    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
	    
	    final IMarshallingTypeHandler lineTypeHandler = typeRegistry.getTypeHandlerForTypeName(GeometryUtilities.QN_GEOMETRY);
	    final IMarshallingTypeHandler nameTypeHandler = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_STRING);
	    
	    final QName shapeTypeQName = new QName("anyNS", "shapeType"); //$NON-NLS-1$ //$NON-NLS-2$
	    
	    final IValuePropertyType lineType = GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "location"), lineTypeHandler, 1, 1, false); //$NON-NLS-1$ //$NON-NLS-2$
	    final IValuePropertyType nameType = GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "name"), nameTypeHandler, 1, 1, false); //$NON-NLS-1$ //$NON-NLS-2$

	    final IPropertyType[] properties = new IPropertyType[] { lineType, nameType };
	    final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType(shapeTypeQName, properties);

	    /* Create the shape root feature, we need it to create the children. */
	    final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature(shapeFT, ShapeConst.SHAPE_TYPE_POINT);
	    final GMLWorkspace workspace = shapeRootFeature.getWorkspace();
	    final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty(ShapeSerializer.PROPERTY_FEATURE_MEMBER);

	    /* Now create some features of this type */
	    int fid = 0;
	    for (final Object catchmentObj : catchmentList) {
		final Feature catchment = (Feature) catchmentObj;
		final GM_Object catchmentGeometry = catchment.getDefaultGeometryPropertyValue();
		final String channel = (String) catchment.getProperty(NaModelConstants.LINK_CATCHMENT_CHANNEL);
		if(channel==null || channel.length()==0)continue;
		for (final Object o : channelList) {
		    if (channel.equals(((Feature)o).getId())) {
			final String nodeID = (String) ((Feature)o).getProperty(NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE);
			String nodeName = "";
			for (final Object n : nodeList) {
			    if (nodeID.equals(((Feature)n).getId())) {
				nodeName = ((Feature)n).getName();
				break;
			    }
			}
			final Object[] data = new Object[] { catchmentGeometry, nodeName };
			final Feature feature = FeatureFactory.createFeature(shapeRootFeature, shapeParentRelation, "FeatureID" + fid++, shapeFT, data); //$NON-NLS-1$
			workspace.addFeatureAsComposition(shapeRootFeature, shapeParentRelation, -1, feature);
			break;
		    }
		}
	    }

	    final File shapeFile = new File(tmpdir, "catchments"); //$NON-NLS-1$
	    ShapeSerializer.serialize(workspace, shapeFile.getAbsolutePath(), null);

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
