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
package org.kalypso.ui.editor.sldEditor;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;

/**
 * @author Thomas Jung
 */
public class PolygonColorMapLabelProvider extends LabelProvider implements ITableLabelProvider, ITableColorProvider
{
  private final TableViewer m_viewer;

  private final Map<java.awt.Color, Color> m_colorStorage = new HashMap<java.awt.Color, Color>();

  public PolygonColorMapLabelProvider( final TableViewer viewer )
  {
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.BaseLabelProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    for( Color color : m_colorStorage.values() )
    {
      if( color != null )
        color.dispose();
    }
    super.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( Object element, int columnIndex )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( Object element, int columnIndex )
  {
    final PolygonColorMapEntry entry = (PolygonColorMapEntry) element;

    PolygonColorMapContentProvider.PROPS prop = PolygonColorMapContentProvider.PROPS.values()[columnIndex];

    switch( prop )
    {
      case label:
        return entry.getLabel( null );

      case from:
        return Double.toString( entry.getFrom( null ) );
      case to:
        return Double.toString( entry.getTo( null ) );

      case stroke:
        return "";

      case fill:
        return "";

      default:
        throw new IllegalArgumentException();
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  @Override
  public boolean isLabelProperty( Object element, String property )
  {
    try
    {
      PolygonColorMapContentProvider.PROPS.valueOf( property );
      return true;
    }
    catch( RuntimeException e )
    {
      e.printStackTrace();
      return false;
    }

  }

  /**
   * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
   */
  public Color getBackground( Object element, int columnIndex )
  {
    final PolygonColorMapEntry entry = (PolygonColorMapEntry) element;

    PolygonColorMapContentProvider.PROPS prop = PolygonColorMapContentProvider.PROPS.values()[columnIndex];

    switch( prop )
    {
      case label:
        return null;

      case from:
        return null;
      case to:
        return null;

      case stroke:
      {
        try
        {
          java.awt.Color stroke = entry.getStroke().getStroke( null );
          Color color = m_colorStorage.get( stroke );
          if( color == null )
          {
            color = SWT_AWT_Utilities.getSWTFromAWT( stroke, m_viewer.getControl().getDisplay() );

            // save the color in a Map in order to be able to dispose all created colors
            m_colorStorage.put( stroke, color );

          }
          return color;
        }
        catch( FilterEvaluationException e )
        {
          e.printStackTrace();
          return null;
        }
      }

      case fill:
      {
        try
        {
          final java.awt.Color fill = entry.getFill().getFill( null );
          Color color = m_colorStorage.get( fill );
          if( color == null )
          {
            color = SWT_AWT_Utilities.getSWTFromAWT( fill, m_viewer.getControl().getDisplay() );

            // save the color in a Map in order to be able to dispose all created colors
            m_colorStorage.put( fill, color );

          }
          return color;
        }
        catch( FilterEvaluationException e )
        {
          e.printStackTrace();
          return null;
        }
      }

      default:
        throw new IllegalArgumentException();
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
   */
  public Color getForeground( Object element, int columnIndex )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
