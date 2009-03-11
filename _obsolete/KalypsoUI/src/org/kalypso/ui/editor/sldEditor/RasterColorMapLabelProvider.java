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
import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class RasterColorMapLabelProvider extends LabelProvider implements ITableLabelProvider
{
  private final TableViewer m_viewer;

  public RasterColorMapLabelProvider( final TableViewer viewer )
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
    final ColorMapEntry entry = (ColorMapEntry) element;

    final RasterColorMapContentProvider.PROPS prop = RasterColorMapContentProvider.PROPS.values()[columnIndex];

    switch( prop )
    {

      case quantity:
        // TODO: fixed scale is not so nice... maybe calculate scale automatically from existing values
        return String.format( "%.2f", entry.getQuantity() ); //$NON-NLS-1$

      case label:
        return entry.getLabel();

      case color:
        return ""; //$NON-NLS-1$

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
      RasterColorMapContentProvider.PROPS.valueOf( property );
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
    final Object property = m_viewer.getColumnProperties()[event.index];
    final RasterColorMapContentProvider.PROPS prop = RasterColorMapContentProvider.PROPS.valueOf( property.toString() );

    switch( prop )
    {
      case quantity:
        return;

      case label:
        return;

      case color:
      {
        final java.awt.Color stroke = ((ColorMapEntry) element).getColor();
        final double opacity = ((ColorMapEntry) element).getOpacity();

        drawRect( event, stroke, opacity );

        return;
      }

      default:
        throw new IllegalArgumentException();
    }
  }

  public void drawRect( final Event event, final java.awt.Color color, final double opacity )
  {
    final GC gc = event.gc;

    final Color currentColor = gc.getBackground();

    final int currentAlpha = gc.getAlpha();

    final Color newColor = SWT_AWT_Utilities.getSWTFromAWT( color, m_viewer.getControl().getDisplay() );

    gc.setBackground( newColor );
    gc.setAlpha( (int) (opacity * 255) );

    gc.fillRectangle( event.getBounds() );

    gc.setBackground( currentColor );
    gc.setAlpha( currentAlpha );

    newColor.dispose();
  }

}
