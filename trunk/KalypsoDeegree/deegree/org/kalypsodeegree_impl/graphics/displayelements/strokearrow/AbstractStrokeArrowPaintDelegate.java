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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_ALIGNMENT;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_TYPE;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_WIDGET;

/**
 * @author kuch
 */
public abstract class AbstractStrokeArrowPaintDelegate implements IStrokeArrowPaintDelegate
{

  private final ARROW_TYPE m_arrowType;

  private final ARROW_ALIGNMENT m_arrowAlignment;

  private final Double m_arrowSize;

  private final ARROW_WIDGET m_arrowWidget;

  public AbstractStrokeArrowPaintDelegate( final ARROW_TYPE arrowType, final ARROW_WIDGET arrowWidget, final ARROW_ALIGNMENT arrowAlignment, final Double arrowSize )
  {
    m_arrowType = arrowType;
    m_arrowWidget = arrowWidget;
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

  protected ARROW_WIDGET getWidget( )
  {
    return m_arrowWidget;
  }

  public static IStrokeArrowPaintDelegate getPaintDelegate( final ARROW_TYPE arrowType, final ARROW_WIDGET arrowWidget, final ARROW_ALIGNMENT arrowAlignment, final Double arrowSize, final Double strokeWidth )
  {

    switch( arrowType )
    {
      case eLine:
        return new StrokeArrowPaintDelegateLine( arrowType, arrowWidget, arrowAlignment, arrowSize, strokeWidth );

      case eSegmentOfLine:
        return new StrokeArrowPaintDelegateSegment( arrowType, arrowWidget, arrowAlignment, arrowSize, strokeWidth );

      default:
        throw new NotImplementedException();
    }
  }

}
