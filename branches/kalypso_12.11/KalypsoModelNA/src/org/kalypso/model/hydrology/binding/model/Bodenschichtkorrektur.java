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

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}bodenschichtkorrektur.
 * 
 * @author Gernot Belger
 */
public class Bodenschichtkorrektur extends AbstractNaModelElement
{
  private static final QName PROP_CINH = new QName( NS_NAMODELL, "cinh" ); //$NON-NLS-1$

  private static final QName PROP_CIND = new QName( NS_NAMODELL, "cind" ); //$NON-NLS-1$

  private static final QName PROP_CEX = new QName( NS_NAMODELL, "cex" ); //$NON-NLS-1$

  private static final QName PROP_BMAX = new QName( NS_NAMODELL, "bmax" ); //$NON-NLS-1$

  private static final QName PROP_BANF = new QName( NS_NAMODELL, "banf" ); //$NON-NLS-1$

  private static final QName PROP_FKO = new QName( NS_NAMODELL, "fko" ); //$NON-NLS-1$

  private static final QName PROP_RETLAY = new QName( NS_NAMODELL, "retlay" ); //$NON-NLS-1$

  public Bodenschichtkorrektur( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public double getCinh( )
  {
    return getDoubleProperty( PROP_CINH, 1.0 );
  }

  public double getCind( )
  {
    return getDoubleProperty( PROP_CIND, 1.0 );
  }

  public double getCex( )
  {
    return getDoubleProperty( PROP_CEX, 1.0 );
  }

  public double getBmax( )
  {
    return getDoubleProperty( PROP_BMAX, 1.0 );
  }

  public double getBanf( )
  {
    return getDoubleProperty( PROP_BANF, 1.0 );
  }

  public double getFko( )
  {
    return getDoubleProperty( PROP_FKO, 1.0 );
  }

  public double getRetlay( )
  {
    return getDoubleProperty( PROP_RETLAY, 1.0 );
  }
}
