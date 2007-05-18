
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
package org.kalypso.calc2d;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class UrlCatalog2d extends AbstractUrlCatalog
{
  final static public String MODEL_2D_NS = "http://elbe.wb.tu-harburg.de/2dModel/sim2d";

  final static public String MODEL_2D_NS_PREFIX = "sim2d";

  final static private String MODEL_2D_SCHEMA = "schema/2dgml.xsd";

  final static public String MODEL_BC_NS = "http://elbe.wb.tu-harburg.de/2dModel/bc2d";

  final static public String MODEL_BC_NS_PREFIX = "bc2d";

  final static private String MODEL_BC_SCHEMA = "schema/bc_gml2.xsd";

  /**
   * @see org.kalypso.contribs.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    catalog.put( MODEL_2D_NS, myClass.getResource( MODEL_2D_SCHEMA ) );
    prefixes.put( MODEL_2D_NS, MODEL_2D_NS_PREFIX );
    
    catalog.put( MODEL_BC_NS, myClass.getResource( MODEL_BC_SCHEMA ) );
    prefixes.put( MODEL_BC_NS, MODEL_BC_NS_PREFIX );
  }

}
