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
package org.kalypso.model.wspm.tuhh.core.results;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider.Property;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;

/**
 * @author Gernot Belger
 */
public class WspmResultLabelProvider extends LabelProvider implements ITableLabelProvider
{
  private final ColumnViewer m_viewer;

  private final GMLLabelProvider m_gmlLabelProvider = new GMLLabelProvider();

  private final WorkbenchLabelProvider m_workbenchLabelProvider = new WorkbenchLabelProvider();

  public WspmResultLabelProvider( final ColumnViewer viewer )
  {
    m_viewer = viewer;
  }

  @Override
  public void dispose( )
  {
    m_gmlLabelProvider.dispose();
    m_workbenchLabelProvider.dispose();

    super.dispose();
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof IWspmResultNode )
      return ((IWspmResultNode)element).getLabel();

    return super.getText( element );
  }

  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof IWspmResultNode )
    {
      final IWspmResultNode node = (IWspmResultNode)element;
      final Object object = node.getObject();
      final Image image = m_gmlLabelProvider.getImage( object );
      if( image != null )
        return image;

      if( object instanceof IPath )
      {
        // TODO Do we need a icon in this case?
      }

      return m_workbenchLabelProvider.getImage( object );
    }

    return m_workbenchLabelProvider.getImage( element );
  }

  @Override
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    final Property property = getProperty( columnIndex );
    switch( property )
    {
      case LABEL:
        return getImage( element );

      default:
        return null;
    }
  }

  @Override
  public String getColumnText( final Object element, final int columnIndex )
  {
    final Property property = getProperty( columnIndex );
    switch( property )
    {
      case LABEL:
        return getText( element );

      default:
        return null;
    }
  }

  private Property getProperty( final int columnIndex )
  {
    final Object[] columnProperties = m_viewer.getColumnProperties();
    final Object columnProperty = columnProperties[columnIndex];
    return Property.valueOf( (String)columnProperty );
  }
}