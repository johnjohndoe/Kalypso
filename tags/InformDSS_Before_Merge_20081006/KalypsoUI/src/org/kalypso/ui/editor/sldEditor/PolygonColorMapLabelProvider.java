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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class PolygonColorMapLabelProvider extends LabelProvider implements ITableLabelProvider
{
  private final TableViewer m_viewer;

  private final List<Color> m_colorList = new ArrayList<Color>();

  public PolygonColorMapLabelProvider( final TableViewer viewer )
  {
    m_viewer = viewer;

    viewer.getControl().addListener( SWT.PaintItem, new Listener()
    {
      /**
       * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
       */
      public void handleEvent( final Event event )
      {
        final Object element = event.item.getData();
        paint( event, element );
      }
    } );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    final PolygonColorMapEntry entry = (PolygonColorMapEntry) element;

    final PolygonColorMapContentProvider.PROPS prop = PolygonColorMapContentProvider.PROPS.values()[columnIndex];

    switch( prop )
    {
      case label:
        return entry.getLabel( null );

      case from:
        // TODO: fixed scale is not good; consider examination of all existing values
        return String.format( "%.2f", entry.getFrom( null ) );
      case to:
        return String.format( "%.2f", entry.getTo( null ) );

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
  public boolean isLabelProperty( final Object element, final String property )
  {
    try
    {
      PolygonColorMapContentProvider.PROPS.valueOf( property );
      return true;
    }
    catch( final RuntimeException e )
    {
      e.printStackTrace();
      return false;
    }

  }

  protected void paint( final Event event, final Object element )
  {
    final PolygonColorMapEntry entry = (PolygonColorMapEntry) element;

    final Object property = m_viewer.getColumnProperties()[event.index];
    final PolygonColorMapContentProvider.PROPS prop = PolygonColorMapContentProvider.PROPS.valueOf( property.toString() );

    switch( prop )
    {
      case label:
        return;

      case from:
        return;
      case to:
        return;

      case stroke:
      {
        try
        {
          final Stroke entryStroke = entry.getStroke();
          final java.awt.Color stroke = entryStroke.getStroke( null );
          final double opacity = entryStroke.getOpacity( null );

          drawRect( event, stroke, opacity );
        }
        catch( final FilterEvaluationException e )
        {
          e.printStackTrace();
          return;
        }
      }

      case fill:
      {
        try
        {
          final Fill entryFill = entry.getFill();
          final java.awt.Color fill = entryFill.getFill( null );
          final double opacity = entryFill.getOpacity( null );

          drawRect( event, fill, opacity );

          return;
        }
        catch( final FilterEvaluationException e )
        {
          e.printStackTrace();
          return;
        }
      }

      default:
        throw new IllegalArgumentException();
    }
  }

  public void drawRect( final Event event, final java.awt.Color color, final double opacity )
  {
    final GC gc = event.gc;

    final Color currentColor = gc.getBackground();

    m_colorList.add( currentColor );

    final int currentAlpha = gc.getAlpha();

    final Color newColor = SWT_AWT_Utilities.getSWTFromAWT( color, m_viewer.getControl().getDisplay() );

    m_colorList.add( newColor );

    gc.setBackground( newColor );
    gc.setAlpha( (int) (opacity * 255) );

    gc.fillRectangle( event.getBounds() );

    gc.setBackground( currentColor );
    gc.setAlpha( currentAlpha );

    newColor.dispose();
  }
}
