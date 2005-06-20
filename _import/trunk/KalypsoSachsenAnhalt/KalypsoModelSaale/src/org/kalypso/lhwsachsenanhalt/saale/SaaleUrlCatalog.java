/**
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraﬂe 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.saale;

import java.util.Map;

import org.kalypso.java.net.AbstractUrlCatalog;

/**
 * @author belger
 */
public final class SaaleUrlCatalog extends AbstractUrlCatalog
{
  protected final void fillCatalog( final Class myClass, final Map catalog )
  {
    catalog.put( "org.kalypso.saale.modell", myClass.getResource( "schemata/saalemodell.xsd" ) );
    catalog.put( "org.kalypso.saale.pegel", myClass.getResource( "schemata/pegel.xsd" ) );
    catalog.put( "org.kalypso.saale.wlm", myClass.getResource( "schemata/wlm.xsd" ) );
    //    catalog.put( "org.kalypso.bode.ombrometer", myClass.getResource( "schemata/ombrometer.xsd" ) );
    catalog.put( "org.kalypso.saale.speicher", myClass.getResource( "schemata/speicher.xsd" ) );
    catalog.put( "org.kalypso.saale.temp", myClass.getResource( "schemata/temp.xsd" ) );
    catalog.put( "org.kalypso.saale.common", myClass.getResource( "schemata/common.xsd" ) );
    catalog.put( "org.kalypso.saale.control", myClass.getResource( "schemata/control.xsd" ) );
    catalog.put( "org.kalypso.saale.steuerung", myClass.getResource( "schemata/steuerung.xsd" ) );
  }
}
