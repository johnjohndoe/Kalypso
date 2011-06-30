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
 * Binding class for {http://www.tuhh.de/kalypsoNA}KMParameter.
 * 
 * @author Gernot Belger
 */
public class KMParameter extends AbstractNaModelElement
{
  public static final QName FEATURE_KM_PARAMETER = new QName( NS_NAMODELL, "KMParameter" ); //$NON-NLS-1$

  public static final QName PROP_QRK = new QName( NS_NAMODELL, "qrk" ); //$NON-NLS-1$

  public static final QName PROP_RKF = new QName( NS_NAMODELL, "rkf" ); //$NON-NLS-1$

  public static final QName PROP_RNF = new QName( NS_NAMODELL, "rnf" ); //$NON-NLS-1$

  public static final QName PROP_RKV = new QName( NS_NAMODELL, "rkv" ); //$NON-NLS-1$

  public static final QName PROP_RNV = new QName( NS_NAMODELL, "rnv" ); //$NON-NLS-1$

  public static final QName PROP_C = new QName( NS_NAMODELL, "c" ); //$NON-NLS-1$

  public KMParameter( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public double getQrk( )
  {
    return getDoubleProperty( PROP_QRK, 0.0 );
  }

  public void setQrk( final double qrk )
  {
    setProperty( PROP_QRK, qrk );
  }

  public double getRkf( )
  {
    return getDoubleProperty( PROP_RKF, 0.0 );
  }

  public void setRkf( final double rkf )
  {
    setProperty( PROP_RKF, rkf );
  }

  public double getRnf( )
  {
    return getDoubleProperty( PROP_RNF, 0.0 );
  }

  public void setRnf( final double rnf )
  {
    setProperty( PROP_RNF, rnf );
  }

  public double getRkv( )
  {
    return getDoubleProperty( PROP_RKV, 0.0 );
  }

  public void setRkv( final double rkv )
  {
    setProperty( PROP_RKV, rkv );
  }

  public double getRnv( )
  {
    return getDoubleProperty( PROP_RNV, 0.0 );
  }

  public void setRnv( final double rnv )
  {
    setProperty( PROP_RNV, rnv );
  }

  public double getC( )
  {
    return getDoubleProperty( PROP_C, 0.0 );
  }

  public void setC( final double c )
  {
    setProperty( PROP_C, c );
  }

}
