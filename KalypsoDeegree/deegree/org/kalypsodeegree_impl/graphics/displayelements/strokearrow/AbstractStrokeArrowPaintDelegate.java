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

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_ALIGNMENT;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_TYPE;

/**
 * @author kuch
 */
public abstract class AbstractStrokeArrowPaintDelegate implements IStrokeArrowPaintDelegate
{

  private final ARROW_TYPE m_arrowType;

  private final ARROW_ALIGNMENT m_arrowAlignment;

  private final Double m_arrowSize;

  public AbstractStrokeArrowPaintDelegate( ARROW_TYPE arrowType, ARROW_ALIGNMENT arrowAlignment, Double arrowSize )
  {
    m_arrowType = arrowType;
    m_arrowAlignment = arrowAlignment;
    m_arrowSize = arrowSize;
  }

  protected ARROW_ALIGNMENT getAlignment( )
  {
    return m_arrowAlignment;
  }

  protected double getSize( )
  {
    return m_arrowSize;
  }

  public static IStrokeArrowPaintDelegate getPaintDelegate( ARROW_TYPE arrowType, ARROW_ALIGNMENT arrowAlignment, Double arrowSize, Double strokeWidth )
  {

    switch( arrowType )
    {
      case eLine:
        return new StrokeArrowPaintDelegateLine( arrowType, arrowAlignment, arrowSize, strokeWidth );

      case eSegmentOfLine:
        return new StrokeArrowPaintDelegateSegment( arrowType, arrowAlignment, arrowSize, strokeWidth );

      default:
        throw new NotImplementedException();
    }
  }

}
