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
package org.kalypso.model.hydrology.binding.suds;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.NaModelConstants;

/**
 * @author Dejan Antanaskovic
 */
public interface ISealing extends ISuds
{
  public QName QN_PROPERTY_ELEMENT_TYPE = new QName( NaModelConstants.NS_NASUDS, "elementType" ); //$NON-NLS-1$

  public QName QN_PROPERTY_SEALING_FACTOR = new QName( NaModelConstants.NS_NASUDS, "sealingFactor" ); //$NON-NLS-1$

  public double SEALING_FACTOR_DEFAULT_VALUE = 1.0;

  public double getSealingFactor( );

  public void setSealingFactor( final Double value );
}
