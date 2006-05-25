/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.Map;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * this catalog resolves all schemas that are original provided by ogc or very close to these
 * 
 * @author doemming
 */
public class UrlCatalogOGC extends AbstractUrlCatalog
{
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    // XLINK
    catalog.put( NS.XLINK, getClass().getResource( "gml2_2002/xlinks.xsd" ) );
    prefixes.put( NS.XLINK, "xlink" );

    // GML
    // Version 2.1
//     catalog.put( "http://www.opengis.net/gml", getClass().getResource( "gml2_2002/feature.xsd" ) );

    // Version 3.1.1. from http://schemas.opengis.net/gml/3.1.1/base/gml.xsd
    catalog.put( NS.GML3, getClass().getResource( "gml/3.1.1/base/gml.xsd" ) );
    prefixes.put( NS.GML3, "gml" );

    // WFS
    catalog.put( NS.WFS, getClass().getResource( "wfs1.1.0/wfs1.1.0.xsd" ) );
    prefixes.put( NS.WFS, "wfs" );

    // SWE & OM things
    catalog.put( NS.SWE, getClass().getResource( "sweCommon/1.0.30/swe.xsd" ) );
    prefixes.put( NS.SWE, "swe" );

    catalog.put( NS.OM, getClass().getResource( "om/1.0.30/observation.xsd" ) );
    prefixes.put( NS.OM, "om" );

  }
}
