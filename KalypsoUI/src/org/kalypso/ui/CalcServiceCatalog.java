/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui;

import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.kalypso.java.net.AbstractUrlCatalog;
import org.kalypso.services.proxy.ICalculationService;

/**
 * @author belger
 */
public class CalcServiceCatalog extends AbstractUrlCatalog
{
  /**
   * @see org.kalypso.java.net.AbstractUrlCatalog#fillCatalog(java.lang.Class, java.util.Map)
   */
  protected void fillCatalog( final Class myClass, final Map catalog )
  {
    try
    {
      final Map calcServices = KalypsoGisPlugin.getDefault().getCalculationServiceProxies();
      for( final Iterator mapIt = calcServices.entrySet().iterator(); mapIt.hasNext(); )
      {
        final Map.Entry entry = (Entry)mapIt.next();
        final String name = (String)entry.getKey();
        final ICalculationService calcService = (ICalculationService)entry.getValue();

        final String[] namespaces = calcService.getSupportedSchemata();
        for( int j = 0; j < namespaces.length; j++ )
        {
          final URL url = new URL( CalculationSchemaStreamHandler.PROTOCOL + "://" + name + "/" + namespaces[j] );
          catalog.put( namespaces[j], url );
        }
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      // erstmal ignoreren
      // wenn auf den Service nicht zugegriffen werden kann ist er entweder
      // nicht da
      // oder falsch konfiguriert
      // in beiden Fällen gibts halt nen leeren Katalog
    }
  }
}
