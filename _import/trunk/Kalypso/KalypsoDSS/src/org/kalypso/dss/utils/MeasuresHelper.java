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

import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.convert.namodel.NaModelConstants;
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
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

public class MeasuresHelper
{
  // Elements
  public static final String CATCHMENT_ELEMENT_NAME = "Catchment";

  public static final String NODE_ELEMENT_NAME = "Node";

  public static final String V_CHANNEL_ELEMENT_NAME = "VirtualChannel";

  public static final String KM_CHANNEL_ELEMENT_NAME = "KMChannel";

  public static final String STORAGE_CHANNEL_ELEMENT_NAME = "StorageChannel";

  // Collection props
  // private static final String CHANNEL_COLLECTION_NAME = "ChannelCollectionMember";
  //
  // private static final String CATCHMENT_COLLECTION_NAME = "CatchmentCollectionMember";
  //
  // private static final String NODE_COLLECTION_NAME = "NodeCollectionMember";
  //
  // private static final String NODE_COLLECTION_MEMBER = "nodeMember";
  //
  // private static final String CHANNEL_COLLECTION_MEMBER = "channelMember";

  // Geometrie Properties of Model
  public static final String NODE_GEOM_PROP = "Ort";

  public static final String CHANNEL_GEOM_PROP = "Ort";

  public static final String CATCHMENT_GEOM_PROP = "Ort";

  // general Props
  public static final String GENERATE_RESULT_PROP = "generateResult";

  // return constants
  public static final int OPERATION_FAILED = -1;

  public static final int OPERATION_SUCCEEDED = 0;

  private static final double m_factorHecto = 1 / 100d;

  public static boolean addRHBinCatchment( final GMLWorkspace modelworkspace, final Feature catchment, final FeatureList rbList, final Feature measureRhbFE )
  {
    // throw new UnsupportedOperationException( "still a TODO, not upgraded after java5.0/eclipse3.1" );
    // create new Properties for Storage
    try
    {
      /** generate new rhb feature and set properties for new storage channel contineously */
      final Feature parentFeature = rbList.getParentFeature();
      // retrieve first element to get the FT for the storage channel
      final Feature rhbFE = (Feature) rbList.iterator().next();
      final Feature newRhbFe = modelworkspace.createFeature( parentFeature, rhbFE.getFeatureType() );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_VMIN_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_C_PROP ), new Double( 0 ) );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_SV_PROP ), new Double( 0 ) );
      final GM_Object measureRhbGEOM = (GM_Object) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
      // generate values for discharge-volume relation
      final String slopeEnum = (String) measureRhbFE.getProperty( new QName(MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_SLOPE ) );
      final Double slope = 1/Double.parseDouble((slopeEnum.split(":"))[1]);
      final Double depth = (Double) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_PROP_DEPTH ) );
      final Double min = new Double( 0 );
      final int max = 10;
      final Double intervall = new Double( depth.doubleValue() / max );
      final Geometry geometry = JTSAdapter.export( measureRhbGEOM );
      final double area = geometry.getArea();
      final double lo = Math.sqrt( area );
      final double lu = lo - 2 * slope.doubleValue() * depth.doubleValue();
      // Das Volumen muss immer in hm^3 sein, deshalb wird hier durch hundert geteilt !!!!
      final double maxVol = depth.doubleValue() / 3 * m_factorHecto * (Math.pow( lo, 2d ) + Math.pow( lu, 2d ) + Math.sqrt( Math.pow( lo, 2d ) * Math.pow( lu, 2d ) ));
      // set max volume of retension basion
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_VMAX_PROP ), new Double( maxVol ) );
      // generate observation with the discharge-volume-waterstage function for the new storage channel
      final ZmlInlineTypeHandler typeHandler = (ZmlInlineTypeHandler) MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( "inline.zml.kalypso.org", "ZmlInlineWVQType" ) );
      final IAxis[] axis = TimeserieUtils.createDefaultAxes( typeHandler.getAxisTypes(), true );
      final Object[][] values = new Object[10][axis.length];
      final Iterator iterator = new ValueIterator( min, intervall, max );
      for( int row = 0; row < max; row++ )
      {
        values[row][0] = iterator.next();
        final double sumHeigth = ((Double) values[row][0]).doubleValue();
        final Double vol = getVolume( sumHeigth, lo, slope.doubleValue(), depth.doubleValue(), m_factorHecto );
        final Double discharge = getDischarge( sumHeigth, 0.65, 9.91, 0.5, m_factorHecto );
        values[row][1] = vol;
        values[row][2] = discharge;
      }
      final ITuppleModel model = new SimpleTuppleModel( axis, values );
      final IObservation obs = new SimpleObservation( null, null, "RHB WVQ-Bezeihung", true, null, new MetadataList(), axis, model );
      newRhbFe.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.STORAGE_CHANNEL_ZMLINLINE_PROP ), obs );

      /**
       * resolve links from oldCatchment -> oldChannel -> oldNode <br>
       * insert new retension basin between old catchment and old channel.
       */
      final IFeatureType catchmentFt = catchment.getFeatureType();
      final IRelationType catchmentToChannelLinkProp = (IRelationType) catchmentFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CATCHMENT_CHANNEL ) );
      // oldCatchment -> oldChannel ( remove link )
      final Feature originalDischargChannel = modelworkspace.resolveLink( catchment, catchmentToChannelLinkProp );
      modelworkspace.removeLinkedAsAggregationFeature( catchment, catchmentToChannelLinkProp, originalDischargChannel.getId() );
      // oldCatchment -> newRetensionBasin ( add new link )
      modelworkspace.addFeatureAsAggregation( catchment, catchmentToChannelLinkProp, 0, newRhbFe.getId() );
      final IFeatureType disChannFt = originalDischargChannel.getFeatureType();
      final IRelationType channelToDownStreamNodeLinkProp = (IRelationType) disChannFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE ) );
      final Feature downStreamNode = modelworkspace.resolveLink( originalDischargChannel, channelToDownStreamNodeLinkProp );
      // newChannel -> newNode ( add new link )
      final Feature newNodeFe = modelworkspace.createFeature( downStreamNode.getParent(), downStreamNode.getFeatureType() );
      modelworkspace.addFeatureAsAggregation( newRhbFe, channelToDownStreamNodeLinkProp, 0, newNodeFe.getId() );
      final IFeatureType nodeFt = newNodeFe.getFeatureType();
      // newNode -> oldChannel ( add new link )
      final IRelationType nodeToDownstreamChannelLinkProp = (IRelationType) nodeFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL ) );
      modelworkspace.addFeatureAsAggregation( newNodeFe, nodeToDownstreamChannelLinkProp, 0, originalDischargChannel.getId() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  /**
   * @param sumHeigth
   * @param mu
   * @param g
   * @param diameter
   * @param factorHecto
   * @return
   */
  private static Double getDischarge( double sumHeigth, double mu, double g, double diameter, double factorHecto )
  {
    final double area = Math.pow( diameter, 2d ) * Math.PI / 4;
    final double q = mu * area * Math.pow( 2 * g * sumHeigth, 2d ) * factorHecto;
    return new Double( q );
  }

  /**
   * @param factorHecto
   * @param double1
   * @return
   */
  private static Double getVolume( double waterlevel, double lenght, double slope, double depth, double factorHecto )
  {
    double value = (4 / 3 * Math.pow( slope, 2d ) * (Math.pow( waterlevel, 2d ) - 3 * depth * Math.pow( waterlevel, 2d ) + 3 * Math.pow( depth, 2d ) * waterlevel) + 2 * slope * lenght
        * (Math.pow( waterlevel, 2d ) - 2 * depth * waterlevel) + Math.pow( lenght, 2d ) * waterlevel)
        * factorHecto;
    return new Double( value * factorHecto );
  }
}