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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;

final class GridWorkStatusCnCProvider extends LabelProvider implements ITableLabelProvider, IColorProvider
{
  private final Map<java.awt.Color, Color> m_colors = new HashMap<>();

  private final GridWidgetFace m_gridWidgetFace;

  GridWorkStatusCnCProvider( final GridWidgetFace gridWidgetFace )
  {
    m_gridWidgetFace = gridWidgetFace;
  }

  @Override
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    return null;
  }

  @Override
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( element instanceof LinePointCollectorConfig )
      return getColumnText( (LinePointCollectorConfig) element, columnIndex );
    else
      return "" + element; //$NON-NLS-1$
  }

  private String getColumnText( final LinePointCollectorConfig elementConfig, final int columnIndex )
  {
    switch( columnIndex )
    {
      case 0:
        return elementConfig.getName();

      case 1:
      {
        final LinePointCollector configLinePointCollector = elementConfig.getConfigLinePointCollector();
        if( configLinePointCollector == null )
          return "0"; //$NON-NLS-1$
        else
        {
          final int curPointCnt = configLinePointCollector.getCurrentPointCnt();
          return String.valueOf( curPointCnt );
        }
      }

      case 2:
      {
        final LinePointCollector configLinePointCollector = elementConfig.getConfigLinePointCollector();
        if( configLinePointCollector == null )
          return "0"; //$NON-NLS-1$
        else
        {
          final int pointCnt = configLinePointCollector.getPointCnt();
          return String.valueOf( pointCnt );
        }
      }

      default:
      {
        return "" + elementConfig + "_" + columnIndex; //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
  }

  @Override
  public void dispose( )
  {
    for( final Color color : m_colors.values() )
      color.dispose();
    m_colors.clear();

    super.dispose();
  }

  @Override
  public Color getBackground( final Object element )
  {
    return null;
  }

  @Override
  public Color getForeground( final Object element )
  {
    if( element instanceof LinePointCollectorConfig )
    {
      final java.awt.Color awtColor = ((LinePointCollectorConfig) element).getColor().darker();
      return getColor( awtColor );
    }

    return null;
  }

  private Color getColor( final java.awt.Color awtColor )
  {
    if( !m_colors.containsKey( awtColor ) )
    {
    final Color color = SWT_AWT_Utilities.getSWTFromAWT( awtColor, m_gridWidgetFace.getDisplay() );
      m_colors.put( awtColor, color );
    }

    return m_colors.get( awtColor );
  }
}