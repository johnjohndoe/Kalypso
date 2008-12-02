/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.gml.schema.schemata;

import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * Dieser Katalog gib fest-verdrahtet die Schemata hier im Code zurück. die gleichen Schemata (zumindest obslink) werden
 * auch fürs binding benutzt ist sind dadurch endlich wirklich nur noch einmal vorhanden.<br>
 * this catalog resolves some schemas that a related to kalypso, but not to ogc<br>
 * TODO rename in URLCatalogKalypso
 * 
 * @author gernot
 */
public class DeegreeUrlCatalog extends AbstractUrlCatalog
{
  @Override
  protected void fillCatalog( final Class<?> myClass, final Map<String, URL> catalog, Map<String, String> prefixes )
  {
    // schemas related close to ogc have been moved to URLCatalogOGC
    catalog.put( "obslink.zml.kalypso.org", getClass().getResource( "obslink/obslink.xsd" ) );
    catalog.put( "org.kalypso.dwd.geolayer", myClass.getResource( "dwd/v0.1/dwdGeoLayer.xsd" ) );
    catalog.put( "inline.zml.kalypso.org", getClass().getResource( "obslink/zmlinline.xsd" ) );
  }
}
