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
package org.kalypso.model.hydrology.binding.parameter;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for {http://www.tuhh.de/parameter}Snow
 * 
 * @author Gernot Belger
 */
public class Snow extends Feature_Impl
{
  private static final String NS_NAPARAMETER = NaModelConstants.NS_NAPARAMETER;

  public static final QName FEATURE_SNOW = new QName( NS_NAPARAMETER, "Snow" ); //$NON-NLS-1$

  private static final QName PROP_XWWO = new QName( NS_NAPARAMETER, "xwwo" ); //$NON-NLS-1$

  private static final QName PROP_XWWMAX = new QName( NS_NAPARAMETER, "xwwmax" ); //$NON-NLS-1$

  private static final QName PROP_XSNOTEM = new QName( NS_NAPARAMETER, "xsnotem" ); //$NON-NLS-1$

  private static final QName PROP_XSNORAD = new QName( NS_NAPARAMETER, "xsnorad" ); //$NON-NLS-1$

  private static final QName PROP_XHO = new QName( NS_NAPARAMETER, "xh0" ); //$NON-NLS-1$

  public Snow( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public double getXwwo( )
  {
    return getDoubleProperty( PROP_XWWO, 0.2 );
  }

  public double getXwwmax( )
  {
    return getDoubleProperty( PROP_XWWMAX, 0.45 );
  }

  public double getXsnotem( )
  {
    return getDoubleProperty( PROP_XSNOTEM, 0.25 );
  }

  public double getXsnorad( )
  {
    return getDoubleProperty( PROP_XSNORAD, 0.35 );
  }

  public double getXh0( )
  {
    return getDoubleProperty( PROP_XHO, 0.0 );
  }
}
