/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.rcm.internal;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.model.rcm.RcmConstants;

public class UrlCatalogRcm extends AbstractUrlCatalog implements RcmConstants
{
  @Override
  protected void fillCatalog( final Class< ? > myClass, final Map<String, URL> catalog, final Map<String, String> prefixes )
  {
    try
    {
      catalog.put( NS_OMBROMETER, new URL( "platform:/plugin/org.kalypso.model.rcm/etc/schema/ombrometer.xsd" ) ); //$NON-NLS-1$
      prefixes.put( NS_OMBROMETER, "ombr" ); //$NON-NLS-1$

      catalog.put( NS_THIESSEN, new URL( "platform:/plugin/org.kalypso.model.rcm/etc/schema/thiessenStations.xsd" ) ); //$NON-NLS-1$
      prefixes.put( NS_THIESSEN, "th" ); //$NON-NLS-1$

      catalog.put( NS_RCM, new URL( "platform:/plugin/org.kalypso.model.rcm/etc/schema/rainfallCatchmentModel_v2.xsd" ) ); //$NON-NLS-1$
      prefixes.put( NS_RCM, "rcm" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
  }
}