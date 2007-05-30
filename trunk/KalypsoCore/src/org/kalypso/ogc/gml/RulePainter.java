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
package org.kalypso.ogc.gml;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.graphics.sld.LegendGraphic;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;

/**
 * @author Gernot Belger
 */
public class RulePainter
{
  public static void paint( final Rule rule, final GC gc ) throws Exception
  {
    gc.drawRectangle( 3, 3, 10, 10 );

    final LegendGraphic legendGraphic = rule.getLegendGraphic();
    if( legendGraphic != null )
    {
// final Graphic graphic = legendGraphic.getGraphic();
      // TODO: paint the graphic onto the image
      // do this when we have implemented it for the PointSymbolizer
    }
    else
    {
      final Symbolizer[] symbolizers = rule.getSymbolizers();
      for( final Symbolizer symbolizer : symbolizers )
      {
        symbolizer.paintLegendGraphic( gc );
      }

    }
  }

}
