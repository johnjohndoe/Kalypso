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
package org.kalypso.model.hydrology.binding;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class NAOptimize extends Feature_Impl
{
  public static final String NS_NAOPTIMIZE = NaModelConstants.NS_NAOPTIMIZE;

  private static final QName PROP_RESULT_TIMESERIESLINK = new QName( NS_NAOPTIMIZE, "qberechnetZR" ); //$NON-NLS-1$

  private static final QName LINK_ROOTNODE = new QName( NS_NAOPTIMIZE, "rootNodeLink" ); //$NON-NLS-1$

  private static final QName PROP_AUTOCALI = new QName( NS_NAOPTIMIZE, "automaticCallibration" ); //$NON-NLS-1$

  private static final QName PROP_PEGEL_ZR_PROP = new QName( NS_NAOPTIMIZE, "pegelZR" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENT_LINK = new QName( NS_NAOPTIMIZE, "catchmentLink" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENTS_BIANF = new QName( NS_NAOPTIMIZE, "CatchmentsBianf" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENTS_FAKTOR_RETOB_TETINT = new QName( NS_NAOPTIMIZE, "CatchmentsFaktorRetobTetint" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENTS_FAKTN = new QName( NS_NAOPTIMIZE, "CatchmentsFaktn" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENTS_FAKTN_PROGNOSE = new QName( NS_NAOPTIMIZE, "CatchmentsFaktnPrognose" ); //$NON-NLS-1$

  private static final QName PROP_CATCHMENTS_FAKTOR_AIGW = new QName( NS_NAOPTIMIZE, "CatchmentsFaktorAigw" ); //$NON-NLS-1$

  private static final QName PROP_KMCHANNEL_LINK = new QName( NS_NAOPTIMIZE, "kmChannelLink" ); //$NON-NLS-1$

  private static final QName PROP_KMCHANNELS_FAKTOR_RKF = new QName( NS_NAOPTIMIZE, "KMChannelsFaktorRkf" ); //$NON-NLS-1$

  private static final QName PROP_KMCHANNELS_FAKTOR_RNF = new QName( NS_NAOPTIMIZE, "KMChannelsFaktorRnf" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Catchment> m_catchmentMembers;

  private final IFeatureBindingCollection<KMChannel> m_kmChannelMembers;

  public NAOptimize( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_catchmentMembers = new FeatureBindingCollection<Catchment>( this, Catchment.class, PROP_CATCHMENT_LINK, true );
    m_kmChannelMembers = new FeatureBindingCollection<KMChannel>( this, KMChannel.class, PROP_KMCHANNEL_LINK, true );
  }

  public IFeatureBindingCollection<Catchment> getCatchmentCollection( )
  {
    return m_catchmentMembers;
  }

  public Catchment[] getCatchments( )
  {
    return m_catchmentMembers.toArray( new Catchment[m_catchmentMembers.size()] );
  }

  public KMChannel[] getKMChannels( )
  {
    return m_kmChannelMembers.toArray( new KMChannel[m_kmChannelMembers.size()] );
  }

  public Node getRootNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, LINK_ROOTNODE, true );
  }

  public ZmlLink getResultLink( )
  {
    return new ZmlLink( this, PROP_RESULT_TIMESERIESLINK );
  }

  public ZmlLink getPegelZRLink( )
  {
    return new ZmlLink( this, PROP_PEGEL_ZR_PROP );
  }

  public boolean doOptimize( )
  {
    return getBoolean( PROP_AUTOCALI, false );
  }

  public Double getFactorRkf( )
  {
    return getProperty( PROP_KMCHANNELS_FAKTOR_RKF, Double.class );
  }

  public Double getFactorRnf( )
  {
    return getProperty( PROP_KMCHANNELS_FAKTOR_RNF, Double.class );
  }

  public Double getBianf( )
  {
    return getProperty( PROP_CATCHMENTS_BIANF, Double.class );
  }

  public Double getFaktorRetobTetint( )
  {
    return getProperty( PROP_CATCHMENTS_FAKTOR_RETOB_TETINT, Double.class );
  }

  public Double getFaktn( )
  {
    return getProperty( PROP_CATCHMENTS_FAKTN, Double.class );
  }

  public double getFaktnPrognose( )
  {
    return getDoubleProperty( PROP_CATCHMENTS_FAKTN_PROGNOSE, 1.0 );
  }

  public Double getFaktorAigw( )
  {
    return getProperty( PROP_CATCHMENTS_FAKTOR_AIGW, Double.class );
  }

}
