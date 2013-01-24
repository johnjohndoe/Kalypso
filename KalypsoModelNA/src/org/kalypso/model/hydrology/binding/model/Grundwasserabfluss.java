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
package org.kalypso.model.hydrology.binding.model;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}grundwasserabfluss.
 * 
 * @author Gernot Belger
 */
public class Grundwasserabfluss extends AbstractNaModelElement
{
  public static final QName FEATURE_GRUNDWASSERABFLUSS = new QName( NS_NAMODELL, "grundwasserabfluss" ); //$NON-NLS-1$

  public static final QName LINK_NGWZU = new QName( NS_NAMODELL, "ngwzu" ); //$NON-NLS-1$

  private static final QName PROP_GWWI = new QName( NS_NAMODELL, "gwwi" ); //$NON-NLS-1$

  public Grundwasserabfluss( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public Catchment getNgwzu( )
  {
    return (Catchment)FeatureHelper.resolveLink( this, LINK_NGWZU, true );
  }

  public void setNgwzu( final Catchment target )
  {
    setLink( LINK_NGWZU, target );
  }

  public double getGwwi( )
  {
    return getDoubleProperty( PROP_GWWI, 0.0 );
  }

  public void setGwwi( final double gwwi )
  {
    setProperty( PROP_GWWI, gwwi );
  }

  public Catchment getParrent( )
  {
    return (Catchment)super.getOwner();
  }
}