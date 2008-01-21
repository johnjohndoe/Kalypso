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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow;

import java.awt.Graphics2D;
import java.util.Map;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_ALIGNMENT;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_TYPE;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_WIDGET;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author kuch
 */
public class StrokeArrowPainter implements IAdditionalStrokePainter
{
  private final GeoTransform m_projection;

  private final Map< ? , ? > m_cssParameters;

  private final UOM m_uom;

  public StrokeArrowPainter( final Map< ? , ? > cssParameters, final GeoTransform projection, final UOM uom )
  {
    m_cssParameters = cssParameters;
    m_projection = projection;
    m_uom = uom;
  }

  public void paint( final Graphics2D g2, final GM_Curve curve, final int[][] positions )
  {
    final ARROW_TYPE arrowType = StrokeArrowHelper.getArrowType( m_cssParameters );
    final ARROW_WIDGET arrowWidget = StrokeArrowHelper.getArrowWidget( m_cssParameters );
    final ARROW_ALIGNMENT arrowAlignment = StrokeArrowHelper.getArrowAlignment( m_cssParameters );
    final Double arrowSize = StrokeArrowHelper.getArrowSize( m_cssParameters );
    final Double strokeWidth = StrokeArrowHelper.getStrokeWidth( m_cssParameters );

    final IStrokeArrowPaintDelegate painter = AbstractStrokeArrowPaintDelegate.getPaintDelegate( arrowType, arrowWidget, arrowAlignment, arrowSize, strokeWidth );
    painter.paint( g2, m_projection, curve, m_uom );
  }
}
