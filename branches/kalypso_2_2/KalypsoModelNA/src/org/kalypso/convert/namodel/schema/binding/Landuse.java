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

/**
 * Binding class for rrmLanduse:Landuse
 * 
 * @author Gernot Belger
 */
public class Landuse extends Feature_Impl
{
  public static final QName QNAME = new QName( NaModelConstants.NS_NALANDUSE, "Landuse" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_GEOMETRY = new QName( NaModelConstants.NS_NALANDUSE, "location" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_LANDUSE = new QName( NaModelConstants.NS_NALANDUSE, "landuseclassLink" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_CORRSEALING = new QName( NaModelConstants.NS_NAHYDROTOP, "corrSealing" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_DRAINAGETYPE = new QName( NaModelConstants.NS_NAHYDROTOP, "drainageType" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_SUD_MEMBERS = new QName( NaModelConstants.NS_NASUDS, "sudLinkMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Feature> m_suds = new FeatureBindingCollection<Feature>( this, Feature.class, QNAME_PROP_SUD_MEMBERS );

  public Landuse( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
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

  /**
   * @param landuseClass
   *          Must be either a {@link org.kalypsodeegree.model.feature.Feature} (maybe xlinked) or a {@link String}-ref
   *          to a feature.
   */
  public void setLanduse( final Object landuseClass )
  {
    setProperty( QNAME_PROP_LANDUSE, landuseClass );
  }

  public Object getLanduse( )
  {
    return getProperty( QNAME_PROP_LANDUSE );
  }

  public Double getCorrSealing( )
  {
    return getProperty( QNAME_PROP_CORRSEALING, Double.class );
  }

  public void setCorrSealing( final Double corrSealing )
  {
    setProperty( QNAME_PROP_CORRSEALING, corrSealing );
  }

  public String getDrainageType( )
  {
    return getProperty( QNAME_PROP_DRAINAGETYPE, String.class );
  }

  public void setDrainageType( final String drainageType )
  {
    setProperty( QNAME_PROP_DRAINAGETYPE, drainageType );
  }
}
