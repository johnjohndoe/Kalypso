package org.kalypso.calculation.plc.postprocessing.na;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

public class PLCPostprocessing_NA_Job extends AbstractInternalStatusJob implements ISimulation {

    private class DischargeData {
	private final double m_value;
	private final Date m_date;

	public DischargeData(final double value, final Date date) {
	    m_value = value;
	    m_date = date;
	}

	public Date getDate() {
	    return m_date;
	}

	public double getValue() {
	    return m_value;
	}
    }

    @Override
    public URL getSpezifikation() {
	return getClass().getResource("resources/modelSpecification.xml");
    }

    @Override
    public void run(final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor) throws SimulationException {
	File statusQuoResultsFolder = FileUtils.toFile((URL) inputProvider.getInputForID("StatusQuoResultsFolder"));
	File calculatedResultsFolder = FileUtils.toFile((URL) inputProvider.getInputForID("CalculatedResultsFolder"));

	try {
	    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace((URL) inputProvider.getInputForID("naModel"), null);
	    final Feature nodeCollection = (Feature) modelWorkspace.getRootFeature().getProperty(NaModelConstants.NODE_COLLECTION_MEMBER_PROP);
	    final FeatureList nodeList = (FeatureList) nodeCollection.getProperty(NaModelConstants.NODE_MEMBER_PROP);

	    final File outputSubfolderSteady = new File(tmpdir, "izNodes");
	    final File outputSubfolderCalculated = new File(tmpdir, "sudsNodes");
	    outputSubfolderSteady.mkdirs();
	    outputSubfolderCalculated.mkdirs();
	    if (!calculatedResultsFolder.isDirectory() || !statusQuoResultsFolder.isDirectory()) {
		setStatus(STATUS.ERROR, "Input folder(s) does not exists.");
		return;
	    }
	    for (final File file : statusQuoResultsFolder.listFiles()) {
		final String name = file.getName();
		if ("Knoten".equals(name) || "Node".equals(name)) {
		    statusQuoResultsFolder = file;
		    break;
		}
	    }
	    for (final File file : calculatedResultsFolder.listFiles()) {
		final String name = file.getName();
		if ("Knoten".equals(name) || "Node".equals(name)) {
		    calculatedResultsFolder = file;
		    break;
		}
	    }

	    final File[] inputSteadyNodesSubfolder = statusQuoResultsFolder.listFiles();
	    final File[] inputCalculatedNodesSubfolder = calculatedResultsFolder.listFiles();
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

	    /* read statistics: max discharge / date of max discharge */
	    final Map<Integer, DischargeData> izNodesMaxData = new HashMap<Integer, DischargeData>();
	    final Map<Integer, DischargeData> calcNodesMaxData = new HashMap<Integer, DischargeData>();

	    final IObservation obs1 = ZmlFactory.parseXML(new File(statusQuoResultsFolder, "Reports/nodesMax.zml").toURI().toURL(), "ID1"); //$NON-NLS-1$ //$NON-NLS-2$
	    final IObservation obs2 = ZmlFactory.parseXML(new File(calculatedResultsFolder, "Reports/nodesMax.zml").toURI().toURL(), "ID2"); //$NON-NLS-1$ //$NON-NLS-2$

	    final IAxis[] axes1 = obs1.getAxisList();
	    final IAxis[] axes2 = obs2.getAxisList();
	    final IAxis idAxis1 = ObservationUtilities.findAxisByClass(axes1, Integer.class);
	    final IAxis idAxis2 = ObservationUtilities.findAxisByClass(axes2, Integer.class);
	    final IAxis dateAxis1 = ObservationUtilities.findAxisByClass(axes1, Date.class);
	    final IAxis dateAxis2 = ObservationUtilities.findAxisByClass(axes2, Date.class);
	    final IAxis valueAxis1 = ObservationUtilities.findAxisByClass(axes1, Double.class);
	    final IAxis valueAxis2 = ObservationUtilities.findAxisByClass(axes2, Double.class);

	    final ITuppleModel values1 = obs1.getValues(null);
	    final ITuppleModel values2 = obs2.getValues(null);
	    final int cnt1 = values1.getCount();
	    final int cnt2 = values2.getCount();
	    if (cnt1 != cnt2)
		throw new SimulationException("Cannot compare NA results");

	    for (int i = 0; i < cnt1; i++) {
		final int id1 = (Integer) values1.getElement(i, idAxis1);
		final int id2 = (Integer) values2.getElement(i, idAxis2);
		final double val1 = (Double) values1.getElement(i, valueAxis1);
		final double val2 = (Double) values2.getElement(i, valueAxis2);
		final Date date1 = (Date) values1.getElement(i, dateAxis1);
		final Date date2 = (Date) values2.getElement(i, dateAxis2);
		izNodesMaxData.put(id1, new DischargeData(val1, date1));
		calcNodesMaxData.put(id2, new DischargeData(val2, date2));
	    }

	    /*
	     * Create feature type which describes what data the shape file
	     * contains
	     */
	    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
	    final IMarshallingTypeHandler typeHandlerGeometry = typeRegistry.getTypeHandlerForTypeName(GeometryUtilities.QN_GEOMETRY);
	    final IMarshallingTypeHandler typeHandlerString = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_STRING);
	    final IMarshallingTypeHandler typeHandlerInteger = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_INTEGER);
	    final IMarshallingTypeHandler typeHandlerLong = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_LONG);
	    final IMarshallingTypeHandler typeHandlerDouble = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_DOUBLE);
	    final IMarshallingTypeHandler typeHandlerDateTime = typeRegistry.getTypeHandlerForTypeName(XmlTypes.XS_DATETIME);

	    final QName shapeTypeQName = new QName("anyNS", "shapeType"); //$NON-NLS-1$ //$NON-NLS-2$

	    final List<IPropertyType> propertyTypeList = new ArrayList<IPropertyType>();
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "location"), typeHandlerGeometry, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "node"), typeHandlerString, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "izValue"), typeHandlerDouble, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "izDateTime"), typeHandlerDateTime, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "izTimeMillis"), typeHandlerLong, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "awmValue"), typeHandlerDouble, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "awmDateTime"), typeHandlerDateTime, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "awmTimeMillis"), typeHandlerLong, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "valueDifference"), typeHandlerDouble, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "timeMillisDifference"), typeHandlerLong, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "valueInfluence"), typeHandlerInteger, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$
	    propertyTypeList.add(GMLSchemaFactory.createValuePropertyType(new QName("anyNS", "timeInfluence"), typeHandlerInteger, 1, 1, false)); //$NON-NLS-1$ //$NON-NLS-2$

	    // valueInfluence - "1" if AW- measure had a positive (good)
	    // influence on discharge, "0" for no influence, "-1" for negative
	    // (bad) influence
	    // timeInfluence - same for time of max. discharge

	    final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType(shapeTypeQName, propertyTypeList.toArray(new IPropertyType[] {}));

	    /* Create the shape root feature, we need it to create the children. */
	    final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature(shapeFT, ShapeConst.SHAPE_TYPE_POINT);
	    final GMLWorkspace workspace = shapeRootFeature.getWorkspace();
	    final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty(ShapeSerializer.PROPERTY_FEATURE_MEMBER);

	    /* Now create some features of this type */
	    int fid = 0;
	    for (final Object n : nodeList) {
		final Feature node = (Feature) n;
		final List<Object> dataList = new ArrayList<Object>();
		dataList.add(node.getDefaultGeometryPropertyValue());
		dataList.add(node.getName());
		final DischargeData max1 = izNodesMaxData.get(Integer.parseInt(node.getName()));
		final DischargeData max2 = calcNodesMaxData.get(Integer.parseInt(node.getName()));
		dataList.add(max1.getValue());
		dataList.add(max1.getDate());
		dataList.add(max1.getDate().getTime());
		dataList.add(max2.getValue());
		dataList.add(max2.getDate());
		dataList.add(max2.getDate().getTime());
		final double valueDifference = max1.getValue() - max2.getValue();
		long timeDifference = max1.getDate().getTime() - max2.getDate().getTime();
		dataList.add(valueDifference);
		dataList.add(timeDifference);
		if (valueDifference == 0.0)
		    dataList.add(0);
		else
		    dataList.add(valueDifference > 0.0 ? 1 : -1);
		if (timeDifference == 0)
		    dataList.add(0);
		else
		    dataList.add(timeDifference < 0 ? 1 : -1);

		final Feature feature = FeatureFactory.createFeature(shapeRootFeature, shapeParentRelation, "FeatureID" + fid++, shapeFT, dataList.toArray()); //$NON-NLS-1$
		workspace.addFeatureAsComposition(shapeRootFeature, shapeParentRelation, -1, feature);
		break;
	    }

	    final File shapeFile = new File(tmpdir, "catchments"); //$NON-NLS-1$
	    ShapeSerializer.serialize(workspace, shapeFile.getAbsolutePath(), null);

	} catch (final Exception e) {
	    setStatus(STATUS.ERROR, e.getLocalizedMessage());
	    e.printStackTrace();

	}
	resultEater.addResult("OutputFolder", tmpdir);
	setStatus(STATUS.OK, "Success");
    }
}
