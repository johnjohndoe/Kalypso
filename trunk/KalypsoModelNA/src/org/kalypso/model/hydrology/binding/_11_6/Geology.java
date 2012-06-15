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
package org.kalypso.model.hydrology.binding._11_6;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for rrmLanduse:Landuse
 * 
 * @author Gernot Belger
 */
public class Geology extends Feature_Impl
{
  public static final QName QNAME = new QName( NaModelConstants.NS_NAGEOLOGIE_11_6, "Geologie" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_GEOMETRY = new QName( NaModelConstants.NS_NAGEOLOGIE_11_6, "location" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_MAXPERKULATIONSRATE = new QName( NaModelConstants.NS_NAHYDROTOP, "m_perkm" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_GWFACTOR = new QName( NaModelConstants.NS_NAHYDROTOP, "m_f1gws" ); //$NON-NLS-1$

  public Geology( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public GM_MultiSurface getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_MultiSurface.class );
  }

  public void setGeometry( final GM_MultiSurface geometry )
  {
    setProperty( QNAME_PROP_GEOMETRY, geometry );
  }

  public void setMaxPerkulationsRate( final double value )
  {
    setProperty( QNAME_PROP_MAXPERKULATIONSRATE, value );
  }

  public Double getMaxPerkulationsRate( )
  {
    return getProperty( QNAME_PROP_MAXPERKULATIONSRATE, Double.class );
  }

  public void setGWFactor( final double value )
  {
    setProperty( QNAME_PROP_GWFACTOR, value );
  }

  public Double getGWFactor( )
  {
    return getProperty( QNAME_PROP_GWFACTOR, Double.class );
  }
}
