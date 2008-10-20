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
package org.kalypso.ogc.gml.symbolizer;

import java.awt.Color;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.ogc.gml.TreeObjectImage;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * @author Gernot Belger
 */
public class ColorMapEntryTreeObject implements IWorkbenchAdapter
{
  private static Object[] EMPTY_CHILDREN = new Object[] {};

  private final Object m_parent;

  private final ColorMapEntry m_entry;

  public ColorMapEntryTreeObject( final Object parent, final ColorMapEntry entry )
  {
    m_parent = parent;
    m_entry = entry;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    return EMPTY_CHILDREN;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    final TreeObjectImage treeImage = new TreeObjectImage( 16, 16 );
    try
    {
      final GC gc = treeImage.getGC();
      final Rectangle clipping = gc.getClipping();
      final int alpha = (int) Math.round( m_entry.getOpacity() * 255 );
      final Color awtColor = m_entry.getColor();
      final RGB rgb = new RGB( awtColor.getRed(), awtColor.getGreen(), awtColor.getBlue() );
      final org.eclipse.swt.graphics.Color color = new org.eclipse.swt.graphics.Color( gc.getDevice(), rgb );
      gc.setAlpha( alpha );
      gc.setBackground( color );
      gc.fillRectangle( clipping.x + 1, clipping.y + 1, clipping.width - 2, clipping.height - 2 );
      color.dispose();
      return treeImage.getImageDescriptor();
    }
    finally
    {
      treeImage.dispose();
    }
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    final String label = m_entry.getLabel().trim();
    if( label.isEmpty() )
      return "" + m_entry.getQuantity();

    return label;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object o )
  {
    return m_parent;
  }

  /**
   * Overwritten, else mapping items to object does not work for the tree, as these item get recreated for every call to
   * getChildren.
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof ColorMapEntryTreeObject )
      return m_entry == ((ColorMapEntryTreeObject) obj).m_entry;

    return false;
  }

  /**
   * @see #equals(Object)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return m_entry.hashCode();
  }


}
