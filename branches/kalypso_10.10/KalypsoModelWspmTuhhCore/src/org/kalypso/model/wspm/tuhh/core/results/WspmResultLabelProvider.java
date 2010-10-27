/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider.Property;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;

/**
 * @author Gernot Belger
 */
public class WspmResultLabelProvider extends LabelProvider implements ITableLabelProvider
{
  private final ColumnViewer m_viewer;

  private final GMLLabelProvider m_gmlLabelProvider = new GMLLabelProvider();

  public WspmResultLabelProvider( final ColumnViewer viewer )
  {
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.BaseLabelProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    m_gmlLabelProvider.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof IWspmResultNode )
      return ((IWspmResultNode) element).getLabel();

    return super.getText( element );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof IWspmResultNode )
    {
      final IWspmResultNode node = (IWspmResultNode) element;
      final Object featureObject = node.getObject();
      return m_gmlLabelProvider.getImage( featureObject );
    }

    return super.getImage( element );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  @Override
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( element instanceof IWspmResultNode )
    {
      final Property property = getProperty( columnIndex );
      switch( property )
      {
        case LABEL:
          return getImage( element );
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  @Override
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( element instanceof IWspmResultNode )
    {
      final IWspmResultNode node = (IWspmResultNode) element;
      final Property property = getProperty( columnIndex );
      switch( property )
      {
        case LABEL:
          return node.getLabel();
      }
    }

    return null;
  }

  private Property getProperty( final int columnIndex )
  {
    final Object[] columnProperties = m_viewer.getColumnProperties();
    final Object columnProperty = columnProperties[columnIndex];
    return Property.valueOf( (String) columnProperty );
  }

}
