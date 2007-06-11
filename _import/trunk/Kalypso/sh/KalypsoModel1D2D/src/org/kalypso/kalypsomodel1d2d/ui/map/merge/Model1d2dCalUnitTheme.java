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
package org.kalypso.kalypsomodel1d2d.ui.map.merge;

import java.awt.Graphics;

import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Theme that shows a calculation unit 
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings({"unchecked", "hiding"})
public class Model1d2dCalUnitTheme extends AbstractKalypsoTheme
{
  
  private CalUnitDisplayElement calUnitDisplayElement;
  
  private ICalculationUnit calUnit;
  
  public Model1d2dCalUnitTheme( String name, IMapModell mapModel )
  {
    super( 
        name, 
        "Aktuelle Berechnungseinheit", 
        mapModel);
  }
  
  public void setCalulationUnit( ICalculationUnit calUnit )
  {
    this.calUnit = calUnit;
    if( calUnit != null )
    {
      calUnitDisplayElement = 
            new CalUnitDisplayElement(calUnit);
    }
    else
    {
      calUnitDisplayElement = null;
    }
  }
  
  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    if( calUnit == null )
    {
      return null;      
    }
    GM_Envelope bbox = CalUnitOps.getBoundingBox( calUnit );
    
    return bbox;
    
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics, org.kalypsodeegree.graphics.transformation.GeoTransform, double, org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint(  Graphics g, 
                      GeoTransform p, 
                      double scale, 
                      GM_Envelope bbox, 
                      boolean selected )
  {
    if( selected )
    {
      return;
    }
    if( calUnitDisplayElement != null )
    {
      calUnitDisplayElement.paint( g, p );
    }
   
  }
  
  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return super.isLoaded();
  }
  
   /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getType()
   */
  @Override
  public String getType( )
  {
    return "GML_MERGE";
  }

  
}
