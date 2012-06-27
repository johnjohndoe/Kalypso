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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.ui.rrm.internal.results.view.TreeViewerSelectionStack;

/**
 * @author Gernot Belger
 */
public class TreeNodeLabelProvider extends ColumnLabelProvider
{
  private final Map<ImageDescriptor, Image> m_images = new HashMap<>();

  private TreeViewerSelectionStack m_stack;

  public TreeNodeLabelProvider( )
  {

  }

  public void setSelectionStack( final TreeViewerSelectionStack stack )
  {
    m_stack = stack;
  }

  @Override
  public void dispose( )
  {
    for( final Entry<ImageDescriptor, Image> entry : m_images.entrySet() )
      entry.getValue().dispose();

    super.dispose();
  }

  @Override
  public String getText( final Object element )
  {
    return ((TreeNode) element).getLabel();
  }

  public static final Font BOLD = new Font( Display.getDefault(), "Segoe UI", 9, SWT.BOLD ); //$NON-NLS-1$

  @Override
  public Font getFont( final Object element )
  {
    if( isSelected( element ) )
    {
      return BOLD;
    }

    return super.getFont( element );
  }

  private boolean isSelected( final Object element )
  {
    if( m_stack == null )
      return false;

    return m_stack.isSelected( element );
  }

  @Override
  public Image getImage( final Object element )
  {
    final ImageDescriptor imageDescriptor = ((TreeNode) element).getImage();
    if( imageDescriptor == null )
      return null;

    if( !m_images.containsKey( imageDescriptor ) )
      m_images.put( imageDescriptor, imageDescriptor.createImage( true ) );

    return m_images.get( imageDescriptor );
  }
}