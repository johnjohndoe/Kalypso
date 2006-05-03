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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

import org.kalypso.contribs.java.net.AbstractUrlCatalog;

/**
 * Dieser Katalog gib fest-verdrahtet die Schemata hier im Code zur�ck. die gleichen Schemata (zumindest obslink) werden
 * auch f�rs binding benutzt ist sind dadurch endlich wirklich nur noch einmal vorhanden.
 * 
 * @author gernot
 */
public class DeegreeUrlCatalog extends AbstractUrlCatalog
{
  /**
   * 
   */
  @Override
  protected void fillCatalog( final Class myClass, final Map<String, URL> catalog )
  {
    catalog.put( "obslink.zml.kalypso.org", getClass().getResource( "obslink/obslink.xsd" ) );
    catalog.put( "http://www.w3.org/1999/xlink", getClass().getResource( "gml2_2002/xlinks.xsd" ) );

    // let cvs point to GML2 !

    // GML2-Base
     catalog.put( "http://www.opengis.net/gml", getClass().getResource( "gml2_2002/feature.xsd" ) );

    // GML3-Base
    try
    {
//      catalog.put( "http://www.opengis.net/gml", new URL( "http://schemas.opengis.net/gml/3.1.1/base/gml.xsd" ) );
      catalog.put( "http://www.opengis.net/swe", new URL( "http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd" ) );
      catalog.put( "http://www.opengis.net/om", new URL( "http://dev.bjoernsen.de/ogc/schema/om/1.0.30/observation.xsd" ) );
      // catalog.put( "org.kalypso.model.wspm", (new File(
      // "C:/eclipse3.1_workspace/KalypsoModelEindim/src/org/kalypso/model/eindim/schema/wspm.xsd" )).toURL() );
    }
    catch( MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    catalog.put( "org.kalypso.dwd.geolayer", myClass.getResource( "dwd/v0.1/dwdGeoLayer.xsd" ) );
    catalog.put( "inline.zml.kalypso.org", getClass().getResource( "obslink/zmlinline.xsd" ) );
    catalog.put( "http://www.opengis.net/wfs", getClass().getResource( "wfs1.1.0/wfs1.1.0.xsd" ) );

    // is also in KalypsoDSS
    // catalog.put( "http://www.xplanung.de/bplangml",
    // getClass().getResource( "resources/BPlan-Operationen_2.xsd" ) );
  }
}
