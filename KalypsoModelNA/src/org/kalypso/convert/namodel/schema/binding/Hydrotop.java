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
package org.kalypso.convert.namodel.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Binding class for hyd:HydrotopFeatureType
 * 
 * @author Dejan Antanaskovic
 */
public class Hydrotop extends Feature_Impl
{
  public static enum HYDROTOP_TYPE
  {
    BODENSPEICHER("Bodenspeicher", 0),
    MULDEN_RIGOLE("MuldenRigole", 1),
    DACHBEGRUENUNG("Dachbegruenung", 2);

    private final String m_name;

    private final int m_asciiValue;

    private HYDROTOP_TYPE( final String name, final int asciiValue )
    {
      m_name = name;
      m_asciiValue = asciiValue;
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      return m_name;
    }

    public String getName( )
    {
      return m_name;
    }

    public int getAsciiValue( )
    {
      return m_asciiValue;
    }
  }

  private static final int DEFAULT_ASCII_HYDROTOP_TYPE = 0;

  public static final QName QNAME = new QName( NaModelConstants.NS_NAHYDROTOP, "Hydrotop" );

  public static final QName QNAME_PROP_GEOMETRY = new QName( NaModelConstants.NS_NAHYDROTOP, "position" );

  public static final QName QNAME_PROP_AREA = new QName( NaModelConstants.NS_NAHYDROTOP, "area" );

  public static final QName QNAME_PROP_LANDUSE = new QName( NaModelConstants.NS_NAHYDROTOP, "landuse" );

  public static final QName QNAME_PROP_SOILTYPE = new QName( NaModelConstants.NS_NAHYDROTOP, "soiltype" );

  public static final QName QNAME_PROP_CORR_SEALING = new QName( NaModelConstants.NS_NAHYDROTOP, "corrSealing" );

  public static final QName QNAME_PROP_DRAINAGE_TYPE = new QName( NaModelConstants.NS_NAHYDROTOP, "drainageType" );

  public static final QName QNAME_PROP_M_PERKM = new QName( NaModelConstants.NS_NAHYDROTOP, "m_perkm" );

  public static final QName QNAME_PROP_M_F1GWS = new QName( NaModelConstants.NS_NAHYDROTOP, "m_f1gws" );

  public static final QName QNAME_PROP_HYD_TYPE = new QName( NaModelConstants.NS_NAHYDROTOP, "hydType" );

  public static final QName QNAME_PROP_SUD_MEMBERS = new QName( NaModelConstants.NS_NASUDS, "sudLinkMember" );

  public static final QName QNAME_PROP_CATCHMENT_MEMBER = new QName( NaModelConstants.NS_NAMODELL, "catchmentLinkMember" );

  private final IFeatureBindingCollection<Feature> m_suds = new FeatureBindingCollection<Feature>( this, Feature.class, QNAME_PROP_SUD_MEMBERS );

  public Hydrotop( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public IFeatureBindingCollection<Feature> getSudCollection( )
  {
    return m_suds;
  }

  public Feature[] getSuds( )
  {
    return m_suds.toArray( new Feature[] {} );
  }

  public GM_MultiSurface getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_MultiSurface.class );
  }

  public void setGeometry( final GM_MultiSurface geometry )
  {
    setProperty( QNAME_PROP_GEOMETRY, geometry );
  }

  public String getLanduse( )
  {
    return getProperty( QNAME_PROP_LANDUSE, String.class );
  }

  public void setLanduse( final String value )
  {
    setProperty( QNAME_PROP_LANDUSE, value );
  }

  public String getSoilType( )
  {
    return getProperty( QNAME_PROP_SOILTYPE, String.class );
  }

  public void setSoilType( final String value )
  {
    setProperty( QNAME_PROP_SOILTYPE, value );
  }

  public double getCorrSealing( )
  {
    return getProperty( QNAME_PROP_CORR_SEALING, Double.class );
  }

  public void setCorrSealing( final double value )
  {
    setProperty( QNAME_PROP_CORR_SEALING, value );
  }

  public String getDrainageType( )
  {
    return getProperty( QNAME_PROP_DRAINAGE_TYPE, String.class );
  }

  public void setDrainageType( final String value )
  {
    setProperty( QNAME_PROP_DRAINAGE_TYPE, value );
  }

  public double getMaxPerkolationRate( )
  {
    return getProperty( QNAME_PROP_M_PERKM, Double.class );
  }

  public void setMaxPerkolationRate( final double value )
  {
    setProperty( QNAME_PROP_M_PERKM, value );
  }

  public double getGWFactor( )
  {
    return getProperty( QNAME_PROP_M_F1GWS, Double.class );
  }

  public void setGWFactor( final double value )
  {
    setProperty( QNAME_PROP_M_F1GWS, value );
  }

  public int getAsciiHydrotopType( )
  {
    final HYDROTOP_TYPE hydrotopType = getHydrotopType();
    return hydrotopType == null ? DEFAULT_ASCII_HYDROTOP_TYPE : hydrotopType.getAsciiValue();
  }

  public HYDROTOP_TYPE getHydrotopType( )
  {
    try
    {
      final String property = getProperty( QNAME_PROP_HYD_TYPE, String.class );
      return property == null ? null : HYDROTOP_TYPE.valueOf( property );
    }
    catch( final IllegalArgumentException e )
    {
      return null;
    }
  }

  public void setHydrotopType( final HYDROTOP_TYPE value )
  {
    setProperty( QNAME_PROP_HYD_TYPE, value.getName() );
  }

  public XLinkedFeature_Impl getCatchmentMember( )
  {
    return getProperty( QNAME_PROP_CATCHMENT_MEMBER, XLinkedFeature_Impl.class );
  }

  public void setCatchmentMember( final XLinkedFeature_Impl value )
  {
    setProperty( QNAME_PROP_CATCHMENT_MEMBER, value );
  }

  public boolean isEqualByPropertiesWith( final Feature feature )
  {
    if( feature instanceof Hydrotop )
    {
      final Hydrotop other = (Hydrotop) feature;
      if( !getLanduse().equals( other.getLanduse() ) )
        return false;
      if( !getSoilType().equals( other.getSoilType() ) )
        return false;
      if( getCatchmentMember() == null || other.getCatchmentMember() == null || !getCatchmentMember().getFeatureId().equals( other.getCatchmentMember().getFeatureId() ) )
        return false;
      if( (getDrainageType() == null && other.getDrainageType() == null) || (getDrainageType() != null && !getDrainageType().equals( other.getDrainageType() )) )
        return false;
      if( getCorrSealing() != other.getCorrSealing() )
        return false;
      if( getMaxPerkolationRate() != other.getMaxPerkolationRate() )
        return false;
      if( getGWFactor() != other.getGWFactor() )
        return false;
      final HYDROTOP_TYPE hydrotopType = getHydrotopType();
      if( !(hydrotopType == null && other.getHydrotopType() == null) || (hydrotopType != null && !hydrotopType.equals( other.getHydrotopType() )) )
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
