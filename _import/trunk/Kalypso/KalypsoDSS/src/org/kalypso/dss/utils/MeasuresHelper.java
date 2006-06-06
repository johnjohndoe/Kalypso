/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

import java.util.Iterator;

import javax.xml.namespace.QName;

import org.eclipse.ui.dialogs.NewFolderDialog;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

public class MeasuresHelper
{
  private static final double m_energyLosses = 0.65d;

  private static final double m_g = 9.91d;

  final static int m_intervals = 10;

  private static Double m_lowestPointInBasin = new Double( 0 );

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
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_C_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_SV_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( XMLHelper.GMLSCHEMA_NS, NaModelConstants.GML_FEATURE_NAME_PROP ), new String( "Rhb-Measure" ) );
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
    final Double diameter = FeatureHelper.getAsDouble( measureRhbFE, new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_DIAMETER ), new Double( 0.5 ) );
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
    final Object[][] values = new Object[m_intervals][axis.length];
    final Iterator iterator = new ValueIterator( m_lowestPointInBasin, intervall, m_intervals );
    for( int row = 0; row < m_intervals; row++ )
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
}