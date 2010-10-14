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
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Binding class for {http://www.tuhh.de/hydrotop}Hydrotop<br/>
 * 
 * @author Dejan Antanaskovic
 */
public class Hydrotop extends Feature_Impl implements IHydrotope
{
  private static final QName QNAME_PROP_GEOMETRY = new QName( NS_NAHYDROTOP, "position" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_LANDUSE = new QName( NS_NAHYDROTOP, "landuse" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_SOILTYPE = new QName( NS_NAHYDROTOP, "soiltype" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_CORR_SEALING = new QName( NS_NAHYDROTOP, "corrSealing" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_M_PERKM = new QName( NS_NAHYDROTOP, "m_perkm" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_M_F1GWS = new QName( NS_NAHYDROTOP, "m_f1gws" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_SUD_MEMBERS = new QName( NaModelConstants.NS_NASUDS, "sudLinkMember" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_CATCHMENT_MEMBER = new QName( NaModelConstants.NS_NAMODELL, "catchmentLinkMember" ); //$NON-NLS-1$

  private IFeatureBindingCollection<ISuds> m_suds = null;

  public Hydrotop( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public synchronized IFeatureBindingCollection<ISuds> getSudCollection( )
  {
    if( m_suds == null )
      m_suds = new FeatureBindingCollection<ISuds>( this, ISuds.class, QNAME_PROP_SUD_MEMBERS, true );

    return m_suds;
  }

  @Override
  public Feature[] getSuds( )
  {
    if( m_suds == null )
      return getSudCollection().toArray( new Feature[] {} );
    // if not null, avoid synchronized method
    return m_suds.toArray( new Feature[] {} );
  }

  @Override
  public GM_MultiSurface getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_MultiSurface.class );
  }

  @Override
  public void setGeometry( final GM_MultiSurface geometry )
  {
    setProperty( QNAME_PROP_GEOMETRY, geometry );
  }

  @Override
  public String getLanduse( )
  {
    return getProperty( QNAME_PROP_LANDUSE, String.class );
  }

  @Override
  public void setLanduse( final String value )
  {
    setProperty( QNAME_PROP_LANDUSE, value );
  }

  @Override
  public String getSoilType( )
  {
    return getProperty( QNAME_PROP_SOILTYPE, String.class );
  }

  @Override
  public void setSoilType( final String value )
  {
    setProperty( QNAME_PROP_SOILTYPE, value );
  }

  @Override
  public double getCorrSealing( )
  {
    return getProperty( QNAME_PROP_CORR_SEALING, Double.class );
  }

  @Override
  public void setCorrSealing( final double value )
  {
    setProperty( QNAME_PROP_CORR_SEALING, value );
  }

  @Override
  public double getMaxPerkolationRate( )
  {
    return getProperty( QNAME_PROP_M_PERKM, Double.class );
  }

  @Override
  public void setMaxPerkolationRate( final double value )
  {
    setProperty( QNAME_PROP_M_PERKM, value );
  }

  @Override
  public double getGWFactor( )
  {
    return getProperty( QNAME_PROP_M_F1GWS, Double.class );
  }

  @Override
  public void setGWFactor( final double value )
  {
    setProperty( QNAME_PROP_M_F1GWS, value );
  }

  @Override
  public XLinkedFeature_Impl getCatchmentMember( )
  {
    return getProperty( QNAME_PROP_CATCHMENT_MEMBER, XLinkedFeature_Impl.class );
  }

  @Override
  public void setCatchmentMember( final XLinkedFeature_Impl value )
  {
    setProperty( QNAME_PROP_CATCHMENT_MEMBER, value );
  }

  @Override
  public boolean isEqualByPropertiesWith( final Feature feature )
  {
    if( feature instanceof Hydrotop )
    {
      final IHydrotope other = (IHydrotope) feature;
      if( !getLanduse().equals( other.getLanduse() ) )
        return false;
      if( !getSoilType().equals( other.getSoilType() ) )
        return false;
      if( getCatchmentMember() == null || other.getCatchmentMember() == null || !getCatchmentMember().getFeatureId().equals( other.getCatchmentMember().getFeatureId() ) )
        return false;
      if( getCorrSealing() != other.getCorrSealing() )
        return false;
      if( getMaxPerkolationRate() != other.getMaxPerkolationRate() )
        return false;
      if( getGWFactor() != other.getGWFactor() )
        return false;
      if( getSudCollection().size() != other.getSudCollection().size() )
        return false;
      boolean eq = true;
      final Feature[] suds = getSuds();
      final Feature[] otherSuds = other.getSuds();
      for( int i = 0; i < suds.length; i++ )
      {
        if( suds[i] instanceof XLinkedFeature_Impl && otherSuds[i] instanceof XLinkedFeature_Impl )
          eq &= ((XLinkedFeature_Impl) suds[i]).getFeatureId().equals( ((XLinkedFeature_Impl) otherSuds[i]).getFeatureId() );
        else
          return false;
      }
      return eq;
    }
    return false;
  }

}
