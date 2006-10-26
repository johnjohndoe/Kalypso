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
package org.kalypso.model.wspm.schema;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.model.wspm.core.IWspmConstants;

public class WspmUrlCatalog extends AbstractUrlCatalog implements IWspmConstants
{
  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( NS_WSPM, myClass.getResource( "schemata/wspm.xsd" ) );
    prefixes.put( NS_WSPM, "wspm" );

    catalog.put( NS_WSPMCOMMONS, myClass.getResource( "schemata/wspmCommons.xsd" ) );
    prefixes.put( NS_WSPMCOMMONS, "wspmcommon" );

    catalog.put( NS_WSPMPROJ, myClass.getResource( "schemata/wspmProject.xsd" ) );
    prefixes.put( NS_WSPMPROJ, "wspmproj" );

    catalog.put( NS_WSPMPROF, myClass.getResource( "schemata/profile.xsd" ) );
    prefixes.put( NS_WSPMPROF, "prof" );

    catalog.put( NS_WSPMPROF_ASSIGNMENT, myClass.getResource( "schemata/profilePointAssignment.xsd" ) );
    prefixes.put( NS_WSPMPROF_ASSIGNMENT, "profass" );

    catalog.put( NS_WSPMRUNOFF, myClass.getResource( "schemata/runOff.xsd" ) );
    prefixes.put( NS_WSPMRUNOFF, "runoff" );

    catalog.put( NS_NA_WSPM, myClass.getResource( "schemata/couplingNaWspm.xsd" ) );
    prefixes.put( NS_NA_WSPM, "wspmna" );

    catalog.put( NS_WSPM_BREAKLINE, myClass.getResource( "schemata/breakline.xsd" ) );
    prefixes.put( NS_WSPM_BREAKLINE, "breakline" );

    catalog.put( NS_WSPM_BOUNDARY, myClass.getResource( "schemata/floodBoundary.xsd" ) );
    prefixes.put( NS_WSPM_BOUNDARY, "boundary" );

  }
}
