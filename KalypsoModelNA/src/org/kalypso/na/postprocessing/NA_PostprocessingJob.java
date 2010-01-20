package org.kalypso.na.postprocessing;

import java.io.BufferedOutputStream;
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
import org.kalypso.simulation.core.AbstractInternalStatusJob;
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
import org.kalypsodeegree_impl.tools.GMLConstants;

/**
 * This class is the post processor for standard rainfall-runoff calculation. The behaviour of it depends on the
 * information given by model specification:
 * <ul>
 * <li>If the "CalculationNature" parameter is set to "PLC", this class produces the set of data representing the
 * difference in the discharge on its nodes. The structure of the output is compatible with PlanerClient application.
 * <ul>
 * <li><b>TODO:</b></li>
 * <li>If the planing area is defined, only the nodes located in the planing area and downstream nodes will be included
 * in the result set; otherwise, all the nodes are included.</li>
 * <li>Planing area is defined by setting the "planingAreaMember" property of "suds.gml", a part of rainfall-runoff
 * model. See {@link org.kalypso.planer.client.server.landuse.helper.AwMeasureHelper#setPlaningAreaMember}.</li>
 * </ul>
 * </li>
 * <li>If the value of optional parameter "CalculationNature" is <code>null</code> or different than "PLC", the class
 * does nothing.</li>
 * </ul>
 * This behaviour can be easily extended for different purposes.<br />
 * <br />
 * 
 * @author Dejan Antanaskovic
 */

public class NA_PostprocessingJob extends AbstractInternalStatusJob implements ISimulation
{

  private class DischargeData
  {
    private final double m_value;

    private final Date m_date;

    public DischargeData( final double value, final Date date )
    {
      m_value = value;
      m_date = date;
    }

    public Date getDate( )
    {
      return m_date;
    }

    public double getValue( )
    {
      return m_value;
    }
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final boolean hasCalculationNature = inputProvider.hasID( "CalculationNature" );
    if( !hasCalculationNature )
    {
      setStatus( STATUS.OK, "Success" );
      return;
    }
    // TODO: Workaround for PLC Ticket #374, switch after fixing to the commented line
    // final String calculationNature = (String) inputProvider.getInputForID( "CalculationNature" );
    String calculationNature = (String) inputProvider.getInputForID( "CalculationNature" );
    calculationNature = calculationNature.substring( calculationNature.lastIndexOf( "/" ) + 1 );

    if( "PLC".equals( calculationNature ) )
    {
      File statusQuoResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( "StatusQuoResultsFolder" ) );
      File calculatedResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( "CalculatedResultsFolder" ) );

      try
      {
        /* read statistics: max discharge / date of max discharge */
        final Map<String, DischargeData> izNodesMaxData = new HashMap<String, DischargeData>();
        final Map<String, DischargeData> calcNodesMaxData = new HashMap<String, DischargeData>();

        final IObservation obs1 = ZmlFactory.parseXML( new File( statusQuoResultsFolder, "Reports/nodesMax.zml" ).toURI().toURL(), "ID1" ); //$NON-NLS-1$ //$NON-NLS-2$
        final IObservation obs2 = ZmlFactory.parseXML( new File( calculatedResultsFolder, "Reports/nodesMax.zml" ).toURI().toURL(), "ID2" ); //$NON-NLS-1$ //$NON-NLS-2$

        final IAxis[] axes1 = obs1.getAxisList();
        final IAxis[] axes2 = obs2.getAxisList();
        final IAxis idAxis1 = ObservationUtilities.findAxisByClass( axes1, Integer.class );
        final IAxis idAxis2 = ObservationUtilities.findAxisByClass( axes2, Integer.class );
        final IAxis dateAxis1 = ObservationUtilities.findAxisByClass( axes1, Date.class );
        final IAxis dateAxis2 = ObservationUtilities.findAxisByClass( axes2, Date.class );
        final IAxis valueAxis1 = ObservationUtilities.findAxisByClass( axes1, Double.class );
        final IAxis valueAxis2 = ObservationUtilities.findAxisByClass( axes2, Double.class );

        final ITuppleModel values1 = obs1.getValues( null );
        final ITuppleModel values2 = obs2.getValues( null );
        final int cnt1 = values1.getCount();
        final int cnt2 = values2.getCount();
        if( cnt1 != cnt2 )
          throw new SimulationException( "Cannot compare NA results" );

        for( int i = 0; i < cnt1; i++ )
        {
          final String id1 = values1.getElement( i, idAxis1 ).toString();
          final String id2 = values2.getElement( i, idAxis2 ).toString();
          final double val1 = (Double) values1.getElement( i, valueAxis1 );
          final double val2 = (Double) values2.getElement( i, valueAxis2 );
          final Date date1 = (Date) values1.getElement( i, dateAxis1 );
          final Date date2 = (Date) values2.getElement( i, dateAxis2 );
          izNodesMaxData.put( id1, new DischargeData( val1, date1 ) );
          calcNodesMaxData.put( id2, new DischargeData( val2, date2 ) );
        }

        final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "naModel" ), null );
        final Feature nodeCollection = (Feature) modelWorkspace.getRootFeature().getProperty( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
        final FeatureList nodeList = (FeatureList) nodeCollection.getProperty( NaModelConstants.NODE_MEMBER_PROP );

        final File outputSubfolderSteady = new File( tmpdir, "izNodes" );
        final File outputSubfolderCalculated = new File( tmpdir, "sudsNodes" );
        outputSubfolderSteady.mkdirs();
        outputSubfolderCalculated.mkdirs();
        if( !calculatedResultsFolder.isDirectory() || !statusQuoResultsFolder.isDirectory() )
        {
          setStatus( STATUS.ERROR, "Input folder(s) does not exists." );
          return;
        }
        for( final File file : statusQuoResultsFolder.listFiles() )
        {
          final String name = file.getName();
          if( "Knoten".equals( name ) || "Node".equals( name ) )
          {
            statusQuoResultsFolder = file;
            break;
          }
        }
        for( final File file : calculatedResultsFolder.listFiles() )
        {
          final String name = file.getName();
          if( "Knoten".equals( name ) || "Node".equals( name ) )
          {
            calculatedResultsFolder = file;
            break;
          }
        }

        final File[] inputSteadyNodesSubfolder = statusQuoResultsFolder.listFiles();
        final File[] inputCalculatedNodesSubfolder = calculatedResultsFolder.listFiles();
        for( int i = 0; i < inputSteadyNodesSubfolder.length; i++ )
        {
          final File steadyNodeFolder = inputSteadyNodesSubfolder[i];
          final File calculatedNodeFolder = inputCalculatedNodesSubfolder[i];
          FileUtils.copyDirectoryToDirectory( steadyNodeFolder, outputSubfolderSteady );
          FileUtils.copyDirectoryToDirectory( calculatedNodeFolder, outputSubfolderCalculated );
          final String nodeID = steadyNodeFolder.getName();
          final Obsdiagview view = NodeResultsComparisonViewCreator.createView( "Gesamtabfluss: " + nodeID, "", "izNodes/" + nodeID + "/Gesamtabfluss.zml", "sudsNodes/" + nodeID
              + "/Gesamtabfluss.zml", nodeID );
          final File odtFile = new File( tmpdir, nodeID + ".odt" );
          final OutputStreamWriter out = new OutputStreamWriter( new BufferedOutputStream( new FileOutputStream( odtFile ) ), "UTF-8" );
          DiagViewUtils.saveDiagramTemplateXML( view, out );
        }

        /*
         * Create feature type which describes what data the shape file contains
         */
        final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
        final IMarshallingTypeHandler typeHandlerGeometry = typeRegistry.getTypeHandlerForTypeName( GMLConstants.QN_POINT );
        final IMarshallingTypeHandler typeHandlerString = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );
        final IMarshallingTypeHandler typeHandlerInt = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
        final IMarshallingTypeHandler typeHandlerLong = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_LONG );
        final IMarshallingTypeHandler typeHandlerDouble = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_DOUBLE );

        final QName shapeTypeQName = new QName( "anyNS", "shapeType" ); //$NON-NLS-1$ //$NON-NLS-2$

        final List<IPropertyType> propertyTypeList = new ArrayList<IPropertyType>();
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "LOCATION" ), typeHandlerGeometry, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "NODE_ID" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "GW_ID" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "GW_KM" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_IZ" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_IZ" ), typeHandlerLong, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_AW" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_AW" ), typeHandlerLong, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_DIF" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_DIF" ), typeHandlerLong, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_INF" ), typeHandlerInt, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
        propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_INF" ), typeHandlerInt, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$

        // valueInfluence - "1" if AW- measure had a positive (good)
        // influence on discharge, "0" for no influence, "-1" for
        // negative
        // (bad) influence
        // timeInfluence - same for time of max. discharge

        final IFeatureType shapeFT = GMLSchemaFactory.createFeatureType( shapeTypeQName, propertyTypeList.toArray( new IPropertyType[] {} ) );

        /*
         * Create the shape root feature, we need it to create the children.
         */
        final Feature shapeRootFeature = ShapeSerializer.createWorkspaceRootFeature( shapeFT, ShapeConst.SHAPE_TYPE_POINT );
        final GMLWorkspace workspace = shapeRootFeature.getWorkspace();
        final IRelationType shapeParentRelation = (IRelationType) shapeRootFeature.getFeatureType().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

        /* Now create some features of this type */
        int fid = 0;
        for( final Object n : nodeList )
        {
          final Feature node = (Feature) n;
          final List<Object> dataList = new ArrayList<Object>();
          dataList.add( node.getDefaultGeometryPropertyValue() );
          dataList.add( node.getName() );

          final String riverCode = (String) node.getProperty( NaModelConstants.NODE_RIVER_CODE_PROP );
          dataList.add( riverCode == null ? "" : riverCode );
          final Double riverKm = (Double) node.getProperty( NaModelConstants.NODE_RIVER_KILOMETER_PROP );
          dataList.add( riverKm == null ? Double.NaN : riverKm );

          final DischargeData max1 = izNodesMaxData.get( node.getName() );
          final DischargeData max2 = calcNodesMaxData.get( node.getName() );
          if( max1 == null || max2 == null )
            continue;
          dataList.add( max1.getValue() );
          dataList.add( max1.getDate().getTime() );
          dataList.add( max2.getValue() );
          dataList.add( max2.getDate().getTime() );
          final double valueDifference = max1.getValue() - max2.getValue();
          final long timeDifference = max1.getDate().getTime() - max2.getDate().getTime();
          dataList.add( valueDifference );
          dataList.add( timeDifference );
          if( valueDifference == 0.0 )
            dataList.add( 0 );
          else
            dataList.add( valueDifference > 0.0 ? 1 : -1 );
          if( timeDifference == 0 )
            dataList.add( 0 );
          else
            dataList.add( timeDifference < 0 ? 1 : -1 );

          final Feature feature = FeatureFactory.createFeature( shapeRootFeature, shapeParentRelation, "FeatureID" + fid++, shapeFT, dataList.toArray() ); //$NON-NLS-1$
          workspace.addFeatureAsComposition( shapeRootFeature, shapeParentRelation, -1, feature );
        }

        final File shapeFile = new File( tmpdir, "difference" ); //$NON-NLS-1$
        ShapeSerializer.serialize( workspace, shapeFile.getAbsolutePath(), null );

      }
      catch( final Exception e )
      {
        e.printStackTrace();
        setStatus( STATUS.ERROR, e.getLocalizedMessage() );
      }
      resultEater.addResult( "OutputFolder", tmpdir );
    }
    setStatus( STATUS.OK, "Success" );
  }
}
