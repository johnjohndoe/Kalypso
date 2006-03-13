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
package org.kalypso.ui.rrm.kmupdate;

import javax.xml.namespace.QName;

/**
 * @author doemming
 */
public interface KMUpdateConstants
{
  public static QName QNAME_KMCHANNEL = new QName( "http://www.tuhh.de/kalypsoNA", "KMChannel" );

  public static QName QNAME_KMParameterMember = new QName( "http://www.tuhh.de/kalypsoNA", "KMParameterMember" );

  public static QName QNAME_KMParameter = new QName( "http://www.tuhh.de/kalypsoNA", "KMParameter" );

  public static QName QNAME_qrk = new QName( "http://www.tuhh.de/kalypsoNA", "qrk" );

  public static QName QNAME_rkf = new QName( "http://www.tuhh.de/kalypsoNA", "rkf" );

  public static QName QNAME_rnf = new QName( "http://www.tuhh.de/kalypsoNA", "rnf" );

  public static QName QNAME_rkv = new QName( "http://www.tuhh.de/kalypsoNA", "rkv" );

  public static QName QNAME_rnv = new QName( "http://www.tuhh.de/kalypsoNA", "rnv" );

  public static QName QNAME_c = new QName( "http://www.tuhh.de/kalypsoNA", "c" );

  public static QName QNAME_KMSTART = new QName( "http://www.tuhh.de/kalypsoNA", "startkm" );

  public static QName QNAME_KMEND= new QName( "http://www.tuhh.de/kalypsoNA", "endkm" );

}
