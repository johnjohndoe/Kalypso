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
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.dss.utils;

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.optimize.CalcDataProviderDecorater;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.sort.JMSpatialIndex;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.TopologyException;

public class MeasuresHelper
{
  private static final double m_energyLosses = 0.65d;

  private static final double m_g = 9.91d;

  final static int m_intervals = 10;

  private static Double m_lowestPointInBasin = new Double( 0 );

  private static Double m_defaultDiameter = new Double( 0.3d );

  /**
   * @param measureWorkspace
   * @param originalDataProvider
   * @param result
   * @param tmpDir
   * @throws Exception
   */

  public static void insertStorageChannelMeasure( final URL rhbURL, final GMLWorkspace modelWorkspace, final Logger logger ) throws SimulationException, IOException, Exception
  {
    final GMLWorkspace measureRhbWorkspace = GmlSerializer.createGMLWorkspace( rhbURL, null );
    // *Get available Measuers*/
    final IGMLSchema rhbMeasureSchema = measureRhbWorkspace.getGMLSchema();
    final IFeatureType sealingFT = rhbMeasureSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_FT ) );
    final Feature[] measureRhbFEs = measureRhbWorkspace.getFeatures( sealingFT );
    // final FeatureList measureRhbFEs = (FeatureList) measureRhbWorkspace.getFeatureFromPath( MeasuresConstants. );
    if( measureRhbFEs.length == 0 )
    {
      logger.info( "measure " + MeasuresConstants.RHB_MEASURE_FT + " is empty, continue normal simulation without this measure" );
      return;
    }
    final FeatureList storageChannelList = (FeatureList) modelWorkspace.getFeatureFromPath( "ChannelCollectionMember/channelMember[StorageChannel]" );
    final FeatureList catchementList = (FeatureList) modelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    // for geometry operations modelWorkspace and rhbMeasureWorkspace must use the same coordinatessystem,
    // so lets transform the measures workspace to the one rrm model uses. (less work than other way)
    if( storageChannelList.size() > 0 )
    {
      final Feature feature = (Feature) storageChannelList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      measureRhbWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( int i = 0; i < measureRhbFEs.length; i++ )
    {
      final Feature measureRhbFE = measureRhbFEs[i];
      int c_success = 0;
      int c_error = 0;

      final GM_Object measureRhbGEOM = (GM_Object) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
      final GM_Envelope rbENV = measureRhbGEOM.getEnvelope();
      // TODO was passiert wenn das RHB nicht eindeutig in einem catchment liegt (hier wird angenommen das es
      // komplet
      // im Chatchment ist)
      final List<JMSpatialIndex> catchmentInENV = catchementList.query( rbENV, null );
      for( Iterator iter = catchmentInENV.iterator(); iter.hasNext(); )
      {
        Feature catchment = (Feature) iter.next();
        GM_Object catchmentGEOM = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
        if( catchmentGEOM.contains( measureRhbGEOM ) )
        {
          boolean succseeded = addRHBinCatchment( modelWorkspace, catchment, storageChannelList, measureRhbFE );
          if( !succseeded )
            c_error++;
          else
            c_success++;
        }
      }
      logger.info( "Fehler Rückhaltebecken Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }

  }

  /**
   * This method adds a storage channel into an existing net structure. There are two possibilties to insert a new
   * storage basin measure into the RRM. <br>
   * <code>
   *    original:
   *    
   *       #Catchment#
   *           |
   *           V
   *    -> |Channel(1)| -> o(1) -> |Channel(2)| ->
   *   
   *   
   *   
   *    Option one:
   *    
   *       #Catchment#
   *           |
   *           V
   *       |VChannel(new)| -> o(new) -> |StorageChannel(new)|
   *                                             |
   *                                             V
   *                          -> |Channel(1)| -> o(1) -> |Channel(2)| ->
   *    
   *    
   *    Option two:
   *    
   *       #Catchment#
   *           |
   *           V
   *    -> |Channel(1)| -> o(new) -> |StorageChannel(new)| -> o(1) -> |Channel(2)| ->
   *    
   *    
   * </code>
   */
  public static boolean addRHBinCatchment( final GMLWorkspace modelworkspace, final Feature catchment, final FeatureList storageChannelList, final Feature measureRhbFE )
  {
    try
    {
      final IGMLSchema rrmSchema = modelworkspace.getGMLSchema();
      /** generate new rhb feature and set properties for new storage channel contineously */
      final Feature channelColFe = storageChannelList.getParentFeature();
      final Feature rhbFE = (Feature) storageChannelList.iterator().next();
      final Feature newRhbFe = modelworkspace.createFeature( channelColFe, rhbFE.getFeatureType() );
      /** Add new storage channel to the channel collection */
      final IFeatureType channelCollectionFT = channelColFe.getFeatureType();
      IRelationType linkChannelCollectionChannelMember = (IRelationType) channelCollectionFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CHANNEL_MEMBER_PROP ) );
      modelworkspace.addFeatureAsComposition( channelColFe, linkChannelCollectionChannelMember, storageChannelList.size() + 1, newRhbFe );
      // generate water stage/volume to discharge relationship for the new storage channel. The basis is the law of
      // toricelli and set all properties from the measure file.
      generateWaterstageVolumeDischargeRealation( measureRhbFE, newRhbFe );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_VMIN_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_SV_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( NS.GML2, NaModelConstants.GML_FEATURE_NAME_PROP ), new String( "Rhb-Measure_" + newRhbFe.getId() ) );
      newRhbFe.setProperty( new QName( NS.GML2, NaModelConstants.GML_FEATURE_DESCRIPTION_PROP ), new String( "automatically generated storage channel-associated with catchmentID= "
          + catchment.getId() ) );
      // get property of inflowTyp to distingish between option one and two
      final String inflowType = (String) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_INFLOWTYP_PROP ) );
      // get common FeatureTyp's and RelationType's to do the inserting business
      final IFeatureType catchmentFt = catchment.getFeatureType();
      final IRelationType catchmentChannelLink = (IRelationType) catchmentFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CATCHMENT_CHANNEL ) );
      final IFeatureType nodeFt = rrmSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_ELEMENT_FT ) );
      final IFeatureType channelFt = rrmSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CHANNEL_ABSTRACT_FT ) );
      final IRelationType channelLinkdownStreamNodeMember = (IRelationType) channelFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE ) );
      final IRelationType nodeLinkDownStreamChannelMember = (IRelationType) nodeFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL ) );
      /**
       * insert new storage channel following option one
       */
      if( inflowType == null || inflowType.equals( MeasuresConstants.RHB_MEASURE_INFLOWTYP_CATCHMENT ) )
      {
        // Catchment -> originalDownstreamChannel ( remove link )
        final Feature originalDischargChannel = modelworkspace.resolveLink( catchment, catchmentChannelLink );
        modelworkspace.removeLinkedAsAggregationFeature( catchment, catchmentChannelLink, originalDischargChannel.getId() );
        // create new vChannel
        final IFeatureType vChannelFt = rrmSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.V_CHANNEL_ELEMENT_FT ) );
        final Feature vChannelFE = modelworkspace.createFeature( channelColFe, vChannelFt );
        modelworkspace.addFeatureAsComposition( channelColFe, linkChannelCollectionChannelMember, 0, vChannelFE );
        // oldCatchment -> newVChannel ( add new link )
        modelworkspace.addFeatureAsAggregation( catchment, catchmentChannelLink, 0, vChannelFE.getId() );
        // newVChannel -> newNode ( add new link )
        final Feature newNodeFe = createNewNode( modelworkspace, true );
        modelworkspace.addFeatureAsAggregation( vChannelFE, channelLinkdownStreamNodeMember, 0, newNodeFe.getId() );
        // newNode -> newRhbChannel (add new link)
        modelworkspace.addFeatureAsAggregation( newNodeFe, nodeLinkDownStreamChannelMember, 0, newRhbFe.getId() );
        // newRhbChannel -> downStreamNodeFromOriginalDischargChannel (add new link)
        final Feature downStreamOriginalNode = modelworkspace.resolveLink( originalDischargChannel, channelLinkdownStreamNodeMember );
        modelworkspace.addFeatureAsAggregation( newRhbFe, channelLinkdownStreamNodeMember, 0, downStreamOriginalNode.getId() );

      }
      /**
       * insert new storage channel following option two
       */
      if( inflowType != null && inflowType.equals( MeasuresConstants.RHB_MEASURE_INFLOWTYP_RIVER ) )
      {
        // originalChannel -> downstreamNode ( remove link )
        final Feature originalDischargChannel = modelworkspace.resolveLink( catchment, catchmentChannelLink );
        final Feature downstreamNode = modelworkspace.resolveLink( originalDischargChannel, channelLinkdownStreamNodeMember );
        modelworkspace.removeLinkedAsAggregationFeature( originalDischargChannel, channelLinkdownStreamNodeMember, downstreamNode.getId() );
        // originalChannel -> newNode (add new link)
        final Feature newNodeFe = createNewNode( modelworkspace, true );
        modelworkspace.addFeatureAsAggregation( originalDischargChannel, channelLinkdownStreamNodeMember, 0, newNodeFe.getId() );
        // newNode -> newRhb ( add new link )
        modelworkspace.addFeatureAsAggregation( newNodeFe, nodeLinkDownStreamChannelMember, 0, newRhbFe.getId() );
        // newRhb -> downstreamNode ( add new link )
        modelworkspace.addFeatureAsAggregation( newRhbFe, channelLinkdownStreamNodeMember, 0, downstreamNode.getId() );
      }

    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  private static void generateWaterstageVolumeDischargeRealation( final Feature measureRhbFE, final Feature newRhbFe ) throws NumberFormatException, GM_Exception
  {
    final GM_Object measureRhbGEOM = (GM_Object) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
    // generate values for discharge-volume relation
    final String slopeEnum = (String) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_SLOPE ) );
    // String from enumeration 1:x => convert to double 1/x
    final Double slope = 1 / Double.parseDouble( (slopeEnum.split( ":" ))[1] );
    final Double depth = (Double) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_DEPTH ) );
    final Double diameter = FeatureHelper.getAsDouble( measureRhbFE, new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_DIAMETER ), m_defaultDiameter );
    final Double intervall = new Double( depth.doubleValue() / m_intervals );
    final Geometry geometry = JTSAdapter.export( measureRhbGEOM );
    final double area = geometry.getArea();
    final double lo = Math.sqrt( area );
    final double lu = lo - 2 * slope.doubleValue() * depth.doubleValue();
    final double maxVol = depth.doubleValue() / 3 * (Math.pow( lo, 2d ) + Math.pow( lu, 2d ) + Math.sqrt( Math.pow( lo, 2d ) * Math.pow( lu, 2d ) ));
    // set max volume of retension basion
    newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_VMAX_PROP ), new Double( maxVol ) );
    // generate observation with the discharge-volume-waterstage function for the new storage channel
    final ZmlInlineTypeHandler typeHandler = (ZmlInlineTypeHandler) MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( "inline.zml.kalypso.org", "ZmlInlineWVQType" ) );
    final IAxis[] axis = TimeserieUtils.createDefaultAxes( typeHandler.getAxisTypes(), true );
    final Object[][] values = new Object[m_intervals + 1][axis.length];
    final Iterator iterator = new ValueIterator( m_lowestPointInBasin, intervall, m_intervals );
    for( int row = 0; row < m_intervals + 1; row++ )
    {
      values[row][0] = iterator.next();
      final double sumHeigth = ((Double) values[row][0]).doubleValue();
      final Double vol = getVolume( sumHeigth, lo, slope.doubleValue(), depth.doubleValue() );
      final Double discharge = getDischarge( sumHeigth, diameter );
      values[row][1] = vol;
      values[row][2] = discharge;
    }
    final ITuppleModel model = new SimpleTuppleModel( axis, values );
    final IObservation obs = new SimpleObservation( null, null, "automatisch generierte WVQ-Bezeihung: rhb measure", true, null, new MetadataList(), axis, model );
    newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_ZMLINLINE_PROP ), obs );
  }

  /**
   * @param sumHeigth
   * @param mu
   * @param diameter
   * @return
   */
  private static Double getDischarge( double sumHeigth, double diameter )
  {
    final double area = Math.pow( diameter, 2d ) * Math.PI / 4;
    final double q = m_energyLosses * area * Math.pow( 2 * m_g * sumHeigth, 2d );
    return new Double( q );
  }

  private static Double getVolume( double waterlevel, double lenght, double slope, double depth )
  {
    double value = (4 / 3 * Math.pow( slope, 2d ) * (Math.pow( waterlevel, 2d ) - 3 * depth * Math.pow( waterlevel, 2d ) + 3 * Math.pow( depth, 2d ) * waterlevel) + 2 * slope * lenght
        * (Math.pow( waterlevel, 2d ) - 2 * depth * waterlevel) + Math.pow( lenght, 2d ) * waterlevel);
    return new Double( value );
  }

  private static Feature createNewNode( final GMLWorkspace modelworkspace, final boolean generateResults ) throws Exception
  {
    final IGMLSchema rrmSchema = modelworkspace.getGMLSchema();
    final IFeatureType nodeColFT = rrmSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_COLLECTION_FT ) );
    final IRelationType linkNodeToNodeCol = (IRelationType) nodeColFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_MEMBER_PROP ) );
    final Feature nodeColFE = (Feature) modelworkspace.getFeatureFromPath( NaModelConstants.NODE_COLLECTION_MEMBER_PROP );
    final IFeatureType nodeFT = rrmSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_ELEMENT_FT ) );
    final Feature newNodeFe = modelworkspace.createFeature( nodeColFE, nodeFT );
    modelworkspace.addFeatureAsComposition( nodeColFE, linkNodeToNodeCol, 0, newNodeFe );
    newNodeFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.GENERATE_RESULT_PROP ), new Boolean( generateResults ) );
    return newNodeFe;
  }

  public static void insertPlanningMeasure( final URL measuresPlanningURL, final GMLWorkspace hydrotopWorkspace, final GMLWorkspace initValuesWorkspace, final CalcDataProviderDecorater rrmInputProvider, final Logger logger ) throws Exception
  {
    final URL paramURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final GMLWorkspace paraWorkspace = GmlSerializer.createGMLWorkspace( paramURL, null );
    final GMLWorkspace planningWorkspace = GmlSerializer.createGMLWorkspace( measuresPlanningURL, null );
    final IGMLSchema planningSchema = planningWorkspace.getGMLSchema();
    final QName qNameGeom = new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GEOM_PROP );
    final QName qNameGRZ = new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GRZ_PROP );
    // get all hydros in the model
    FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    // transform planning workspace coordinate system to match coordinate system of hydrotop files
    if( hydroList.size() > 0 )
    {
      final Feature feature = (Feature) hydroList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      planningWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    // handel GruenFlaech features
    final IFeatureType gruenflFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GRUENFL_FT ) );
    final Feature[] gruenflFE = planningWorkspace.getFeatures( gruenflFT );
    for( int i = 0; i < gruenflFE.length; i++ )
    {
      final Feature gfFE = gruenflFE[i];
      setNewLandUseAndCorrSealingFactor( gfFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_GRUENFL_LANDUSE_NAME, logger );
    }
    // handel VerkehrsFlaech features
    final IFeatureType verkehrsFlaechFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_VERKEHRSFL_FT ) );
    final Feature[] verkersFEs = planningWorkspace.getFeatures( verkehrsFlaechFT );
    for( int i = 0; i < verkersFEs.length; i++ )
    {
      final Feature vfFE = verkersFEs[i];
      setNewLandUseAndCorrSealingFactor( vfFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_VERKEHRSFL_LANDUSE_NAME, logger );
    }
    // handel VerkehrsflaecheBesondererZweckbestimmung features
    final IFeatureType verkehrsflaechBesZweckFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_VERKEHRSFMITBESZWECK_FT ) );
    final Feature[] verkehrsflaechBesZweckFEs = planningWorkspace.getFeatures( verkehrsflaechBesZweckFT );
    for( int i = 0; i < verkehrsflaechBesZweckFEs.length; i++ )
    {
      final Feature vfBZbFE = verkehrsflaechBesZweckFEs[i];
      setNewLandUseAndCorrSealingFactor( vfBZbFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_VERKEHRSFL_LANDUSE_NAME, logger );
    }
    // handel GemeinbedarfsFlaeche features
    final IFeatureType gemeinBedFlaechFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GEMEINBED_FT ) );
    final Feature[] gemeinBedFlaechFEs = planningWorkspace.getFeatures( gemeinBedFlaechFT );
    for( int i = 0; i < gemeinBedFlaechFEs.length; i++ )
    {
      final Feature gbFE = gemeinBedFlaechFEs[i];
      setNewLandUseAndCorrSealingFactor( gbFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_GEMEINBED_LANDUSE_NAME, logger );
    }
    // handel BaugebietsFlaechenTeil (Baugebiete) features
    final IFeatureType bauGebFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_BAUGEBIET_FT ) );
    final Feature[] bauGebFEs = planningWorkspace.getFeatures( bauGebFT );
    for( int i = 0; i < bauGebFEs.length; i++ )
    {
      final Feature bgFE = bauGebFEs[i];
      final Object artBaulicherNutzung = bgFE.getProperty( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_ART_BAULICHNUTZ_PROP ) );
      final String type;
      if( artBaulicherNutzung == null )
        type = MeasuresConstants.XPLANUNG_UNDEFINED_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_RWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_RWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_AWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_AWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_BWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_BWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_DG_PROP ) )
        type = MeasuresConstants.XPLANUNG_DG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_GG_PROP ) || artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_IG_PROP ) )
        type = MeasuresConstants.XPLANUNG_IG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_KG_PROP ) )
        type = MeasuresConstants.XPLANUNG_KG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_MG_PROP ) )
        type = MeasuresConstants.XPLANUNG_MG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_SGE_PROP ) || artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_SGS_PROP ) )
        type = MeasuresConstants.XPLANUNG_SG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_KS_PROP ) )
        type = MeasuresConstants.XPLANUNG_KS_LANDUSE_NAME;
      else
        type = MeasuresConstants.XPLANUNG_UNDEFINED_LANDUSE_NAME;

      setNewLandUseAndCorrSealingFactor( bgFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, type, logger );
    }
  }

  private static void setNewLandUseAndCorrSealingFactor( final Feature planningMeasureFE, final QName sealingProp, final QName sealingGeom, final GMLWorkspace hydrotopWorkspace, final GMLWorkspace paraWorkspace, GMLWorkspace initValuesWorkspace, final String landUseProp, final Logger logger ) throws Exception
  {
    // init counter
    int c_success = 0;
    int c_error = 0;
    // find Landuse for measure
    final IGMLSchema parameterSchema = paraWorkspace.getGMLSchema();
    final IFeatureType landUseFT = parameterSchema.getFeatureType( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_NAME ) );
    final Feature[] landUseFEs = paraWorkspace.getFeatures( landUseFT );
    final QName qNameLandUse = new QName( NS.GML3, NaModelConstants.GML_FEATURE_NAME_PROP );
    Feature landUseFE = null;
    for( int i = 0; i < landUseFEs.length; i++ )
    {
      landUseFE = landUseFEs[i];
      final Object property = landUseFE.getProperty( qNameLandUse );
      if( landUseProp.equals( property ) )
        break;
    }
    // get all hydrotopes in workspace
    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    // do the intersecting business
    final GM_Object measureGEOM = (GM_Object) planningMeasureFE.getProperty( sealingGeom );
    final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
    final QName qNameHydroGeom = new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM );
    final List query = hydroList.query( measureGEOM.getEnvelope(), null );
    final Iterator iter = query.iterator();
    while( iter.hasNext() )
    {
      final Feature hydroFE = (Feature) iter.next();
      if( hydroFE.getId().endsWith( "2782" ) && landUseProp.equals( MeasuresConstants.XPLANUNG_RWG_LANDUSE_NAME ) )
        System.out.println();
      if( hydroFE == null )
        continue;
      final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( qNameHydroGeom );
      final CS_CoordinateSystem storedCrs = hydroGEOM.getCoordinateSystem();
      final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
      Geometry intersection = null;
      Geometry difference = null;
      try
      {

        boolean intersects = jtsMeasureGEOM.intersects( jtsHydroGEOM );
        if( !intersects )
          continue;
        intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
        // keep the diffrence from the original hydrotop
        difference = jtsHydroGEOM.difference( jtsMeasureGEOM );
        c_success++;
      }
      catch( TopologyException e )
      {
        c_error++;
        e.printStackTrace();
        logger.warning( "Fehler beim verschneiden der Hydrotop-Topologien. HydrotopId=" + hydroFE.getId() + " mit measureId=" + planningMeasureFE.getId()
            + ".Es wird trotzdem weitergerechnet diese Operation wurde ingnorier!" );
        continue;
      }
      final Feature newHydroFE;
      if( difference != null && !difference.isEmpty() )
      {
        final Feature hydroRootFeature = hydrotopWorkspace.getRootFeature();
        final IFeatureType rootFT = hydroRootFeature.getFeatureType();
        final IRelationType linkPropHydro = (IRelationType) rootFT.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_MEMBER ) );
        final IFeatureType hydroFT = hydrotopWorkspace.getFeatureTypeFromPath( NaModelConstants.HYDRO_MEMBER );
        newHydroFE = hydrotopWorkspace.createFeature( hydroRootFeature, hydroFT );
        hydrotopWorkspace.addFeatureAsComposition( hydroRootFeature, linkPropHydro, 0, newHydroFE );
        // copy all propterties from the intersected hydrotop to the newly created hydrotop
        FeatureHelper.copyNoRelationPropterty( hydroFE, newHydroFE );

        final GM_Object geomWithOldParam = JTSAdapter.wrap( difference );
        // since JTS has no coordiante system we have to set the old one after a JTSAdapter call
        geomWithOldParam.setCoordinateSystem( storedCrs );
        // set the inverse Geometry of the intersection at the old hydrotop FE
        hydroFE.setProperty( qNameHydroGeom, GeometryUtilities.ensureIsMultiPolygon( geomWithOldParam ) );
      }
      else
        newHydroFE = hydroFE;
      // override copied props that are not valid anymore
      final GM_Object geomWithNewMeasure = JTSAdapter.wrap( intersection );
      // since JTS has no coordiante system we have to set the old one after a JTSAdapter call
      geomWithNewMeasure.setCoordinateSystem( storedCrs );
      /**
       * since the original sealing factor (from landuse feature) can not be alterd, the new sealing factor form the
       * measure is achieved by using the sealing correction factor at each hydrotop. It could be that a correction
       * factors has been used to calibrate the model, hence we have to incorporate the change of sealing and the old
       * correction factor into an new correction factor. Now do the business!
       */
      final IRelationType linkSealing = (IRelationType) landUseFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
      final Feature landuseSealingFE = paraWorkspace.resolveLink( landUseFE, linkSealing );
      final double landuseSealingFactor = FeatureHelper.getAsDouble( landuseSealingFE, new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING ), 0.5d );
      double measureSealingFactor = FeatureHelper.getAsDouble( planningMeasureFE, sealingProp, landuseSealingFactor );
      // TODO: implement the Kappungsgrenze
      // determine the 50% additional area für Nebenflächen und Stellplätze (Regel in Hamburg)
      // final IFeatureType measureFT = planningMeasureFE.getFeatureType();
      // final QName qnameMeasure = measureFT.getQName();
      // boolean plus50ProzentNebenflaechen = false;
      // if( qnameMeasure.equals( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_BAUGEBIET_FT ) )
      // )
      // plus50ProzentNebenflaechen = true;
      // // kappungsgrenze GRZ 0.8
      // if( plus50ProzentNebenflaechen )
      // {
      // double test = measureSealingFactor + measureSealingFactor * 0.5;
      // if( test < 0.8d )
      // measureSealingFactor = test;
      // else
      // measureSealingFactor = 0.8d;
      // }
      final double originalCorrFactor = FeatureHelper.getAsDouble( hydroFE, new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), 1.0d );
      final double newCorrectionFactor = originalCorrFactor * measureSealingFactor / landuseSealingFactor;
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ), GeometryUtilities.ensureIsMultiPolygon( geomWithNewMeasure ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), new Double( newCorrectionFactor ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_AREA ), new Double( intersection.getArea() ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ), landUseFE.getProperty( new QName( NS.GML3, NaModelConstants.GML_FEATURE_NAME_PROP ) ) );
      newHydroFE.setProperty( new QName( NS.GML2, NaModelConstants.GML_FEATURE_DESCRIPTION_PROP ), "automatically generated hydrotop cloned from hydrotopID=" + hydroFE.getId() );
      setInitialValues( newHydroFE, hydroFE, initValuesWorkspace );
    }
    if( c_error > 0 )
      logger.info( "Fehler BPlan-Measure: " + c_error + " Fehler/" + (c_success + c_error) + " Total\n" );
    else
      logger.info( "BPlan-Maßnahme" + planningMeasureFE.getClass().toString() + " erfolgreich eingefügt!" );
  }

  private static void setInitialValues( final Feature newHydroFE, final Feature hydroFE, final GMLWorkspace initValuesWorkspace ) throws Exception
  {
    final IGMLSchema initalValuesSchema = initValuesWorkspace.getGMLSchema();
    final IFeatureType hydIniFT = initalValuesSchema.getFeatureType( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_MEMBER_PROP ) );
    final IPropertyType featureIdPT = hydIniFT.getProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_FEATUREID_PROP ) );
    final Feature[] hydIniFEs = initValuesWorkspace.getFeatures( hydIniFT );
    for( int i = 0; i < hydIniFEs.length; i++ )
    {
      final Feature hydIniFE = hydIniFEs[i];
      final String value = (String) hydIniFE.getProperty( featureIdPT );
      if( hydroFE.getId().equals( value ) )
      {
        final Feature catchementIniFE = hydIniFE.getParent();
        final Feature newHydIniFE = initValuesWorkspace.createFeature( catchementIniFE, hydIniFT );
        FeatureHelper.copyNoRelationPropterty( hydIniFE, newHydIniFE );
        newHydIniFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_FEATUREID_PROP ), newHydroFE.getId() );
        newHydIniFE.setProperty( new QName( NS.GML3, NaModelConstants.GML_FEATURE_DESCRIPTION_PROP ), "automatically generated inital value for hydrotopID=" + newHydroFE.getId() );
        final IRelationType linkCatchmentHydIni = (IRelationType) catchementIniFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_CATCHMENT_LINK_HYD_PROP ) );
        initValuesWorkspace.addFeatureAsComposition( catchementIniFE, linkCatchmentHydIni, 0, newHydIniFE );
        // there is only one match for hydIni.featureID to hydroFE.getId() -> leave the loop now
        break;
      }

    }
  }

  /**
   * This method merges the sealing measure GML-file with the original hydrotop file. The corrFactor in the hydrotop is
   * adjusted to account for the measure.
   * 
   * @param sealingURL
   *          URL of the sealingMeasure.gml
   * @param result
   *          dataProvider
   * @param logger
   *          logger
   * @throws Exception
   */
  public static void insertSealingChangeMeasure( final URL sealingURL, final GMLWorkspace hydrotopWorkspace, final CalcDataProviderDecorater result, final Logger logger ) throws SimulationException, IOException, Exception
  {
    GMLWorkspace sealingWS = GmlSerializer.createGMLWorkspace( sealingURL, null );

    final URL parameterURL = (URL) result.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    // Versiegelungsgrad Measure
    final IGMLSchema sealingSchema = sealingWS.getGMLSchema();
    final IFeatureType sealingFT = sealingSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_FT ) );
    final Feature[] sealingFEs = sealingWS.getFeatures( sealingFT );
    if( sealingFEs.length == 0 )
    {
      logger.info( "measure " + MeasuresConstants.SEALING_MEASURE_FT + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace paraWorkspace = GmlSerializer.createGMLWorkspace( parameterURL, null );
    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );

    // for geometry operations hydroworkspace and measureworkspace must use the same coordinatessystem,
    // so let measure transform to the one hydo uses. (less work than other way)
    if( hydroList.size() > 0 )
    {
      final Feature feature = (Feature) hydroList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      sealingWS.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    int c_success = 0;
    int c_error = 0;
    for( int i = 0; i < sealingFEs.length; i++ )
    {
      final Feature sealFE = sealingFEs[i];
      double sealMeasure = FeatureHelper.getAsDouble( sealFE, new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_SEALINGFACTOR_PROP ), 1.0d );

      final GM_Object measureGEOM = (GM_Object) sealFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_GEOMETRY_PROP ) );
      final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
      final GM_Envelope selENV = sealFE.getEnvelope();
      final List<JMSpatialIndex> hydrosInENV = hydroList.query( selENV, null );
      for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
      {
        final Feature hydroFE = (Feature) iter.next();
        final double originalSealingFactor = getSealingFactorForHydrotop( hydroFE, paraWorkspace ); // TODO abfangen
        // wenn

        final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
        final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
        Geometry intersection = null;
        try
        {
          intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
          if( intersection.isEmpty() )
            continue;
          c_success++;
        }
        catch( Exception e )
        {
          c_error++;
        }
        final double corrSealingHydroOld = FeatureHelper.getAsDouble( hydroFE, new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), 1.0d );
        final double areaIntersection = intersection.getArea();
        final double areaHydro = jtsHydroGEOM.getArea();
        // TODO check for numerical best order, and account for corrFactor from hydrotop element
        // remark: it does not matter, if in next loop the same hydrotop again is affected

        /**
         * The corrSealingHydroOld is the fixed correction factor from the calibrated model. If the measure dose not
         * affect the whole hydrotop then a new overall sealing factor as a mean value has to be calculated. <br>
         * To account for the change of the old and new sealing factor we have to calculate the new correction factor,
         * because we can not change the original sealing factor in the parameter.gml.
         */

        final double sealingAreaIntersection = areaIntersection * sealMeasure * corrSealingHydroOld;
        final double sealingAreaOld = (areaHydro - areaIntersection) * originalSealingFactor * corrSealingHydroOld;
        final double virtualSealingFactor = (sealingAreaIntersection + sealingAreaOld) / areaHydro;
        double corrSealingHydroNew = virtualSealingFactor / (originalSealingFactor * corrSealingHydroOld);
        if( Double.isInfinite( corrSealingHydroNew ) )
        {
          corrSealingHydroNew = corrSealingHydroOld;
        }

        hydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), new Double( corrSealingHydroNew ) );
      }
      logger.info( "Fehler Entsiegelungs-Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }

  }

  /**
   * Get's the sealing factor for a hydrotop from the paramter.gml file
   * 
   * @param hydroFE
   *          the hydrotop as a Feature
   * @param paramWorkspace
   *          workspace containing the parameter-gml
   */
  private static double getSealingFactorForHydrotop( final Feature hydroFE, final GMLWorkspace paramWorkspace )
  {
    final Object property = hydroFE.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ) );
    final FeatureList features = (FeatureList) paramWorkspace.getFeatureFromPath( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    Feature landuseFE = null;
    Iterator it = features.iterator();
    while( it.hasNext() )
    {
      landuseFE = (Feature) it.next();
      final Object paramProperty = landuseFE.getProperty( new QName( NS.GML2, NaModelConstants.GML_FEATURE_NAME_PROP ) );
      if( paramProperty.equals( property ) )
        break;
    }
    final IFeatureType featureType = landuseFE.getFeatureType();
    final IPropertyType linkProp = featureType.getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
    final Feature sealingFE = paramWorkspace.resolveLink( landuseFE, (IRelationType) linkProp );
    return FeatureHelper.getAsDouble( sealingFE, new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING ), 1.0d );
  }

  public static void insertSwaleTrenchMeasure( final URL measuresMrsURL, final GMLWorkspace naModelWorkspace, final GMLWorkspace hydrotopWorkspace, final URL designAreaURL, final Logger logger ) throws Exception
  {
    try
    {
      final GMLWorkspace mrsMeasureWorkspace = GmlSerializer.createGMLWorkspace( measuresMrsURL, null );
      final IGMLSchema mrsMeasureSchema = mrsMeasureWorkspace.getGMLSchema();
      final IFeatureType mrsMeasureFT = mrsMeasureSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_FT ) );
      final Feature[] mrsMeasureFEs = mrsMeasureWorkspace.getFeatures( mrsMeasureFT );
      if( mrsMeasureFEs.length == 0 )
      {
        logger.info( "measure " + MeasuresConstants.MRS_MEASURE_FT + " is empty, continue normal simulation without this measure" );
        return;
      }
      /** Create a swale and trench collection if it does not exist */
      final IGMLSchema naModelSchema = naModelWorkspace.getGMLSchema();
      final Feature naModelRoot = naModelWorkspace.getRootFeature();
      final IFeatureType naModelRootFt = naModelRoot.getFeatureType();
      Feature swaleTrenchCollection = (Feature) naModelWorkspace.getFeatureFromPath( NaModelConstants.MRS_COLLECTION_MEMBER_PROP );
      if( swaleTrenchCollection == null )
      {
        final IFeatureType swaleTrenchColFt = naModelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_COLLECTION_FT ) );
        swaleTrenchCollection = naModelWorkspace.createFeature( naModelRoot, swaleTrenchColFt );
        final IRelationType linkPropertyCol = (IRelationType) naModelRootFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_COLLECTION_MEMBER_PROP ) );
        naModelWorkspace.addFeatureAsComposition( naModelRoot, linkPropertyCol, 0, swaleTrenchCollection );
      }
      // get the necessary featureTypes and proptertyTypes for the swale and trench element
      final IFeatureType swaleTrenchCollectionFT = swaleTrenchCollection.getFeatureType();
      final IRelationType linkPropertyStMemeber = (IRelationType) swaleTrenchCollectionFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_MEMBER_PROP ) );
      final IFeatureType mrsFt = naModelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_FT ) );

      /** Start inserting bussines */
      final GMLWorkspace designAreaWorkspace = GmlSerializer.createGMLWorkspace( designAreaURL, null );
      final FeatureList catchementList = (FeatureList) naModelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
      if( catchementList.size() > 0 )
      {
        // assure that the measures and designArea geometries have the same coordinate system like the model data
        final Feature catchment = (Feature) catchementList.iterator().next();
        final GM_Object targetGeom = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
        final CS_CoordinateSystem targetCS = targetGeom.getCoordinateSystem();
        final TransformVisitor visitor = new TransformVisitor( targetCS );
        mrsMeasureWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
        designAreaWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
      }
      int c_success = 0;
      int c_error = 0;
      for( int i = 0; i < mrsMeasureFEs.length; i++ )
      {
        final Feature mrsMeasure = mrsMeasureFEs[i];
        final Double percentage = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_PERCENTAGE ), 0d );
        final GM_Envelope envMeasure = mrsMeasure.getEnvelope();
        final GM_Object mrsGEOM = (GM_Object) mrsMeasure.getProperty( new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
        final Geometry mrsJTS = JTSAdapter.export( mrsGEOM );
        final List<JMSpatialIndex> queriedCatchmentList = catchementList.query( envMeasure, null );
        for( Iterator iter = queriedCatchmentList.iterator(); iter.hasNext(); )
        {
          final Feature catchmentInEnv = (Feature) iter.next();
          final GM_Object catchmentGEOM = (GM_Object) catchmentInEnv.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
          try
          {
            final Geometry catchmentJTS = JTSAdapter.export( catchmentGEOM );
            final Geometry intersection = catchmentJTS.intersection( mrsJTS );
            if( !intersection.isEmpty() )
            {
              // splits the measure according to the catchment boundary into sub measures
              final GM_Object mrsSubMeasure = JTSAdapter.wrap( intersection );
              // get set properties from the measure element to transfer them to the newly create model element
              final Double diameter = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_DIAMETER ), 250 );
              final Double width = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_WIDTH ), 1.50 );
              final Feature swaleTrenchFE = naModelWorkspace.createFeature( swaleTrenchCollection, mrsFt );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_DIAMETER_PIPE_PROP ), diameter );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_WIDTH_PROP ), width );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_GEOM_PROP ), mrsSubMeasure );
              // this parameter is not used at the moment left in for
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_KF_PIPE_PROP ), new Double( 0 ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_SLOPE_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_SLOPE_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_ROUGHNESS_PIPE_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_ROUGHNESS_PIPE_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_INFLOW_GW_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_INFLOW_GW_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_MAX_PERK_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_MAX_PERK_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_LANDUSE_TYPE_PROP ), NaModelConstants.DEFAULT_MRS_LANDUSE_PROP );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_SOIL_PROFIL_TYPE_PROP ), NaModelConstants.DEFAULT_MRS_SOIL_PROFIL_PROP );
              naModelWorkspace.addFeatureAsComposition( swaleTrenchCollection, linkPropertyStMemeber, 0, swaleTrenchFE );
              // add catchment that intersects with mrs-Element for later handling
              c_success++;
            }
          }
          catch( Exception e )
          {
            c_error++;
          }
        }
        logger.info( "Fehler Mulden-Rigolen Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
        setHydroTypsForSwaleAndTrenchElement( designAreaWorkspace, percentage, (LineString) mrsJTS, hydrotopWorkspace );
      }

    }
    catch( Exception e )
    {
      logger.warning( "Was not able to merge swale and trench measures....skipped!" );
      throw new Exception( "Was not able to merge swale and trench measures....skipped!", e );
    }

  }

  private static void setHydroTypsForSwaleAndTrenchElement( final GMLWorkspace designAreaWorkspace, final Double percentage, final LineString mrsJTS, final GMLWorkspace hydrotopWorkspace ) throws Exception
  {
    /** calculate puffer area for hydTyps */
    final FeatureList designAreaList = (FeatureList) designAreaWorkspace.getFeatureFromPath( MeasuresConstants.DESIGNAREA_MEMBER_PROP );
    if( designAreaList.size() == 0 )
    {
      // logger.info( "no design area defined can not insert swale and trench meausre!..skipped" );
      return;
    }
    // just get first area of planning (there should only be one element in the List)
    final Feature designAreaFe = (Feature) designAreaList.iterator().next();
    final GM_Object designAreaGEOM = (GM_Object) designAreaFe.getProperty( new QName( MeasuresConstants.NS_DESIGNAREA, MeasuresConstants.DESINGAREA_GEOM_PROP ) );
    final Geometry designAreaJTS = JTSAdapter.export( designAreaGEOM );
    // calculate buffer width around mrs
    final double totalLength = mrsJTS.getLength();
    final double area = designAreaJTS.getArea();
    final double bufferWidth = area * percentage.doubleValue() / 100 / totalLength / 2;
    // the buffered area is to large, this inaccuracy is neclegted
    final Geometry bufferedArea = mrsJTS.buffer( bufferWidth );
    final GM_Object geomBufferdArea = JTSAdapter.wrap( bufferedArea );

    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    final List queriedHydroList = hydroList.query( geomBufferdArea.getEnvelope(), null );
    for( Iterator iter = queriedHydroList.iterator(); iter.hasNext(); )
    {
      final Feature hydFe = (Feature) iter.next();
      final GM_Object hydGeom = (GM_Object) hydFe.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
      final Geometry hydGeomInEnv = JTSAdapter.export( hydGeom );
      if( hydGeomInEnv.intersects( bufferedArea ) )
        hydFe.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_HYDTYPE ), NaModelConstants.HYDRO_ENUM_HYDTYPE_SWALETRENCH );
    }
  }
}