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
package org.kalypsodeegree_impl.gml.schema.schemata;

import java.net.URL;

import org.kalypso.java.net.IUrlCatalog;

/**
 * Dieser Katalog gib fest-verdrahtet die Schemata hier im Code zurück.
 * die gleichen Schemata (zumindest obslink) werden auch fürs binding benutzt ist sind dadurch
 * endlich wirklich nur noch einmal vorhanden.
 * 
 * @author gernot
 */
public class DeegreeUrlCatalog implements IUrlCatalog
{
  /**
   * @see org.kalypso.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public URL getURL( final String key )
  {
    if( "obslink.zml.kalypso.org".equalsIgnoreCase( key ) )
      return getClass().getResource( "obslink/obslink.xsd" );
    else if( "http://www.w3.org/1999/xlink".equalsIgnoreCase( key ) )
      return getClass().getResource( "gml2_2002/xlinks.xsd" );
    else if( "http://www.opengis.net/gml".equalsIgnoreCase( key ) )
      return getClass().getResource( "gml2_2002/feature.xsd" );
//    else if( "".equalsIgnoreCase( key ) )
//      return getClass().getResource( "" );
    
    return null;
  }

}
