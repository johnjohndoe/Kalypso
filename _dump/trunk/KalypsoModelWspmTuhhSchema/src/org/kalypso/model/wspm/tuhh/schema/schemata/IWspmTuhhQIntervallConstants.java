/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.tuhh.schema.schemata;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * Constants for the use of the QIntervall-Schema
 * 
 * @author Gernot Belger
 *
 */
public interface IWspmTuhhQIntervallConstants extends IWspmTuhhConstants
{
  public static final QName QNAME_F_QIntervallResultCollection = new QName( NS_WSPM_TUHH, "QIntervallResultCollection" );

  public static final QName QNAME_P_QIntervallResultCollection_resultMember = new QName( NS_WSPM_TUHH, "resultMember" );
  
  
  
  public static final QName QNAME_F_QIntervallResult = new QName( NS_WSPM_TUHH, "QIntervallResult" );
  
  public static final QName QNAME_P_QIntervallResult_pointsMember = new QName( NS_WSPM_TUHH, "pointsMember" );

  public static final QName QNAME_P_QIntervallResult_station = new QName( NS_WSPM_TUHH, "station" );

  
  
  public static final QName QNAME_F_WPointsObservation = new QName( NS_WSPM_TUHH, "WPointsObservation" );
  
}
