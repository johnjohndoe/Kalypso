package org.kalypso.model.hydrology.internal.simulation.planerclient.postprocessing;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.tableview.TableViewUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypso.template.obstableview.Obstableview;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.io.shpapi.ShapeConst;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * This class is the post processor for standard rainfall-runoff calculation. It produces the set of data representing
 * the difference in the discharge on its nodes. The structure of the output is compatible with PlanerClient
 * application.
 * <ul>
 * <li><b>TODO:</b></li>
 * <li>If the planing area is defined, only the nodes located in the planing area and downstream nodes will be included
 * in the result set; otherwise, all the nodes are included.</li>
 * <li>Planing area is defined by setting the "planingAreaMember" property of "suds.gml", a part of rainfall-runoff
 * model. See {@link org.kalypso.planer.client.server.landuse.helper.AwMeasureHelper#setPlaningAreaMember}.</li>
 * </ul>
 *
 * @author Dejan Antanaskovic
 */
public class NA_PostprocessingJob extends AbstractInternalStatusJob implements ISimulation
{
  /**
   * Helper class for preparing the discharge info for writing to report shape file.
   *
   * @author Dejan Antanaskovic
   */
  private class DischargeData
  {
    private final double m_valueMaximum;

    private final Date m_dateMaximum;

    private final double m_volume;

    /**
     * rounds values to sensible precision
     */
    public DischargeData( final double valueMaximum, final Date dateMaximum, final double volume )
    {
      // round maximum value to 2 digits
      m_valueMaximum = Math.round( valueMaximum * 100 ) / 100.0;

      // round date to minutes
      m_dateMaximum = (Date) dateMaximum.clone();
      final long timeMillis = m_dateMaximum.getTime();
      m_dateMaximum.setTime( Math.round( timeMillis * 1000 * 60 ) / (1000 * 60) );

      // round volume
      m_volume = Math.round( volume );
    }

    public final double getValueMaximum( )
    {
      return m_valueMaximum;
    }

    public final Date getDateMaximum( )
    {
      return m_dateMaximum;
    }

    public final double getVolume( )
    {
      return m_volume;
    }

    /**
     * Returns the date formated with "yyyyMMddHHmmssZ" format. The result is converted to Kalypso time zone.
     */
    public String getDateMaximumFormatted( )
    {
      final TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
      final SimpleDateFormat dfm = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss Z" ); //$NON-NLS-1$
      dfm.setTimeZone( timeZone );
      return dfm.format( m_dateMaximum );
    }
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final TransformVisitor transformVisitor = new TransformVisitor( targetCRS );

    final File statusQuoResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( "StatusQuoResultsFolder" ) ); //$NON-NLS-1$
    final File calculatedResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( "CalculatedResultsFolder" ) ); //$NON-NLS-1$

    if( !calculatedResultsFolder.isDirectory() || !statusQuoResultsFolder.isDirectory() )
    {
      setStatus( STATUS.ERROR, "Input data not available." ); //$NON-NLS-1$
      return;
    }

    try
    {
      final GMLWorkspace sudsWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "sudsModel" ), null ); //$NON-NLS-1$
      sudsWorkspace.accept( transformVisitor, sudsWorkspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "naModel" ), null ); //$NON-NLS-1$
      final NaModell naModel = (NaModell) modelWorkspace.getRootFeature();

      modelWorkspace.accept( transformVisitor, naModel, FeatureVisitor.DEPTH_INFINITE );

      /* read statistics: max discharge / date of max discharge */
      final Map<String, String> izNodesPath = new HashMap<String, String>();
      final Map<String, String> calculatedNodesPath = new HashMap<String, String>();
      final Map<String, DischargeData> izNodesMaxData = new HashMap<String, DischargeData>();
      final Map<String, DischargeData> calcNodesMaxData = new HashMap<String, DischargeData>();

      final IObservation obs1 = ZmlFactory.parseXML( new File( statusQuoResultsFolder, "Report/statistics.zml" ).toURI().toURL() ); //$NON-NLS-1$ //$NON-NLS-2$
      final IObservation obs2 = ZmlFactory.parseXML( new File( calculatedResultsFolder, "Report/statistics.zml" ).toURI().toURL() ); //$NON-NLS-1$ //$NON-NLS-2$

      final IAxis[] axes1 = obs1.getAxes();
      final IAxis[] axes2 = obs2.getAxes();
      final IAxis idAxis1 = ObservationUtilities.findAxisByName( axes1, "NODE_ID" ); //$NON-NLS-1$
      final IAxis idAxis2 = ObservationUtilities.findAxisByName( axes2, "NODE_ID" ); //$NON-NLS-1$
      final IAxis dateAxis1 = ObservationUtilities.findAxisByName( axes1, "DATE" ); //$NON-NLS-1$
      final IAxis dateAxis2 = ObservationUtilities.findAxisByName( axes2, "DATE" ); //$NON-NLS-1$
      final IAxis valueAxis1 = ObservationUtilities.findAxisByName( axes1, "DISCHARGE" ); //$NON-NLS-1$
      final IAxis valueAxis2 = ObservationUtilities.findAxisByName( axes2, "DISCHARGE" ); //$NON-NLS-1$
      final IAxis volumeAxis1 = ObservationUtilities.findAxisByName( axes1, "VOLUME" ); //$NON-NLS-1$
      final IAxis volumeAxis2 = ObservationUtilities.findAxisByName( axes2, "VOLUME" ); //$NON-NLS-1$
      IAxis pathAxis1 = ObservationUtilities.findAxisByName( axes1, "PATH" ); //$NON-NLS-1$
      final IAxis pathAxis2 = ObservationUtilities.findAxisByName( axes2, "PATH" ); //$NON-NLS-1$
      if( pathAxis2 == null )
      {
        Logger.getAnonymousLogger().log( Level.SEVERE, "Unable to detect node result path. Postprocessing aborted." ); //$NON-NLS-1$
        return;
      }
      final boolean useOnlyResult = pathAxis1 == null;
      if( useOnlyResult )
        pathAxis1 = pathAxis2;

      final ITupleModel values1 = obs1.getValues( null );
      final ITupleModel values2 = obs2.getValues( null );
      final int cnt1 = values1.size();
      final int cnt2 = values2.size();
      if( cnt1 != cnt2 )
        Logger.getAnonymousLogger().log( Level.WARNING, "Compared NA results are not of the same size; check calculation config!" ); //$NON-NLS-1$

      for( int i = 0; i < Math.max( cnt1, cnt2 ); i++ )
      {
        if( i < values1.size() )
        {
          final String id1 = values1.get( i, idAxis1 ).toString();
          final double val1 = (Double) values1.get( i, valueAxis1 );
          final double vol1 = (Double) values1.get( i, volumeAxis1 );
          final Date date1 = (Date) values1.get( i, dateAxis1 );
          izNodesMaxData.put( id1, new DischargeData( val1, date1, vol1 ) );
          final String path = values1.get( i, pathAxis1 ).toString();
          if( !useOnlyResult )
            izNodesPath.put( id1, path );
        }
        if( i < values2.size() )
        {
          final String id2 = values2.get( i, idAxis2 ).toString();
          final double val2 = (Double) values2.get( i, valueAxis2 );
          final double vol2 = (Double) values2.get( i, volumeAxis2 );
          final Date date2 = (Date) values2.get( i, dateAxis2 );
          calcNodesMaxData.put( id2, new DischargeData( val2, date2, vol2 ) );
          final String path = values2.get( i, pathAxis2 ).toString();
          calculatedNodesPath.put( id2, path );
          if( useOnlyResult )
            izNodesPath.put( id2, path );
        }
      }

      final File outputSubfolderSteady = new File( tmpdir, "izNodes" ); //$NON-NLS-1$
      final File outputSubfolderCalculated = new File( tmpdir, "sudsNodes" ); //$NON-NLS-1$
      outputSubfolderSteady.mkdirs();
      outputSubfolderCalculated.mkdirs();

      final List<Node> affectedNodes = naModel.getNodes();

      if( affectedNodes.size() == 0 )
      {
        final String message = "No catchment is affected. Please redefine the model/planing area."; //$NON-NLS-1$
        // Logger.getAnonymousLogger().log( Level.WARNING, message );
        setStatus( STATUS.ERROR, message );
        throw new SimulationException( message );
      }

      for( final Feature node : affectedNodes )
      {
        String name = node.getName();
        if( name == null || name.length() == 0 )
          name = node.getId();
        final String izNodePath = izNodesPath.get( name );
        final String calculatedNodePath = calculatedNodesPath.get( name );
        if( izNodePath == null || calculatedNodePath == null )
          continue;
        final File izFile = new File( statusQuoResultsFolder, izNodePath );
        final File calcFile = new File( calculatedResultsFolder, calculatedNodePath );
        final File izFolder = new File( outputSubfolderSteady, name );
        final File calcFolder = new File( outputSubfolderCalculated, name );
        izFolder.mkdirs();
        calcFolder.mkdirs();
        FileUtils.copyFileToDirectory( izFile, izFolder );
        FileUtils.copyFileToDirectory( calcFile, calcFolder );
        final String izPath = String.format( Locale.US, "izNodes/%s/%s", name, izFile.getName() ); //$NON-NLS-1$
        final String calcPath = String.format( Locale.US, "sudsNodes/%s/%s", name, calcFile.getName() ); //$NON-NLS-1$
        final Obsdiagview view = NodeResultsComparisonViewCreator.createView( "Gesamtabfluss: " + name, "", izPath, calcPath, name ); //$NON-NLS-1$ //$NON-NLS-2$
        final Obstableview table = NodeResultsComparisonViewCreator.createTableView( izPath, calcPath );
        final File odtFile = new File( tmpdir, name + ".odt" ); //$NON-NLS-1$
        final File ottFile = new File( tmpdir, name + ".ott" ); //$NON-NLS-1$
        final BufferedWriter outDiag = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( odtFile ), "UTF-8" ) ); //$NON-NLS-1$
        final BufferedWriter outTable = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( ottFile ), "UTF-8" ) ); //$NON-NLS-1$
        DiagViewUtils.saveDiagramTemplateXML( view, outDiag );
        TableViewUtils.saveTableTemplateXML( table, outTable );
      }

      /*
       * Create feature type which describes what data the shape file contains
       */
      final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      final IMarshallingTypeHandler typeHandlerGeometry = typeRegistry.getTypeHandlerForTypeName( GM_Point.POINT_ELEMENT );
      final IMarshallingTypeHandler typeHandlerString = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_STRING );
      final IMarshallingTypeHandler typeHandlerInt = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_INT );
      final IMarshallingTypeHandler typeHandlerDouble = typeRegistry.getTypeHandlerForTypeName( XmlTypes.XS_DOUBLE );

      final QName shapeTypeQName = new QName( "anyNS", "shapeType" ); //$NON-NLS-1$ //$NON-NLS-2$

      final List<IPropertyType> propertyTypeList = new ArrayList<IPropertyType>();
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "LOCATION" ), typeHandlerGeometry, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "NODE_ID" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "GW_ID" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "GW_KM" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_IZ" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_IZ" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_AW" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_AW" ), typeHandlerString, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VALUE_INF" ), typeHandlerInt, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "TIME_INF" ), typeHandlerInt, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VOLUME_IZ" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      propertyTypeList.add( GMLSchemaFactory.createValuePropertyType( new QName( "anyNS", "VOLUME_AW" ), typeHandlerDouble, 1, 1, false ) ); //$NON-NLS-1$ //$NON-NLS-2$

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
      for( final Feature node : affectedNodes )
      {
        final List<Object> dataList = new ArrayList<Object>();
        dataList.add( node.getDefaultGeometryPropertyValue() );
        dataList.add( node.getName() );

        final String riverCode = (String) node.getProperty( INode.PROPERTY_RIVER_CODE );
        dataList.add( riverCode == null ? "" : riverCode ); //$NON-NLS-1$
        final Double riverKm = (Double) node.getProperty( INode.PROPERTY_RIVER_KM );
        dataList.add( riverKm == null ? Double.NaN : riverKm );

        final String name = node.getName();
        DischargeData izMax = null;
        DischargeData calcMax = null;
        if( name != null && name.length() > 0 )
        {
          izMax = izNodesMaxData.get( name );
          calcMax = calcNodesMaxData.get( name );
        }
        if( izMax == null )
        {
          izMax = izNodesMaxData.get( node.getId() );
        }
        if( calcMax == null )
        {
          calcMax = calcNodesMaxData.get( node.getId() );
        }
        if( izMax == null || calcMax == null )
          continue;

        dataList.add( izMax.getValueMaximum() );
        dataList.add( izMax.getDateMaximumFormatted() );
        dataList.add( calcMax.getValueMaximum() );
        dataList.add( calcMax.getDateMaximumFormatted() );
        dataList.add( Double.compare( izMax.getValueMaximum(), calcMax.getValueMaximum() ) );
        dataList.add( calcMax.getDateMaximum().compareTo( izMax.getDateMaximum() ) );
        dataList.add( izMax.getVolume() );
        dataList.add( calcMax.getVolume() );

        final Feature feature = FeatureFactory.createFeature( shapeRootFeature, shapeParentRelation, "FeatureID" + fid++, shapeFT, dataList.toArray() ); //$NON-NLS-1$
        workspace.addFeatureAsComposition( shapeRootFeature, shapeParentRelation, -1, feature );
      }

      // FIXME: check if this workspace is empty and give a better error message
      final File shapeFile = new File( tmpdir, "difference" ); //$NON-NLS-1$
      ShapeSerializer.serialize( workspace, shapeFile.getAbsolutePath(), targetCRS );
    }
    catch( final Exception e )
    {
      setStatus( STATUS.ERROR, e.getLocalizedMessage() );
      throw new SimulationException( Messages.getString( "NA_PostprocessingJob_0" ), e ); //$NON-NLS-1$
    }
    resultEater.addResult( "OutputFolder", tmpdir ); //$NON-NLS-1$
    setStatus( STATUS.OK, "Success" ); //$NON-NLS-1$
  }
}
