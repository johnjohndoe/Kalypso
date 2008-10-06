/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.om.table;

import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;

/**
 * A delegate for {@link org.eclipse.jface.viewers.ITableLabelProvider}s which adds an (empty) which just ignored the
 * empty element of the {@link LastLineLabelProvider}
 * <p>
 * Should be used in union with {@link org.kalypso.ogc.gml.om.table.LastLineContentProvider}.
 * 
 * @author Gernot Belger
 */
public class LastLineLabelProvider implements ITableLabelProvider, IColorProvider
{
  public static final String DUMMY_ELEMENT_TEXT = "Zeile hinzufügen";
  private final ITableLabelProvider m_provider;
  private Color m_backgroundColor;

  public LastLineLabelProvider( final ITableLabelProvider provider, final Color background )
  {
    m_provider = provider;
    m_backgroundColor = background;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( final ILabelProviderListener listener )
  {
    m_provider.addListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose( )
  {
    m_provider.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( element == LastLineContentProvider.DUMMY_ELEMENT )
      return null;
    
    return m_provider.getColumnImage( element, columnIndex );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( element == LastLineContentProvider.DUMMY_ELEMENT )
      return DUMMY_ELEMENT_TEXT;

    return m_provider.getColumnText( element, columnIndex );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( final Object element, final String property )
  {
    if( element == LastLineContentProvider.DUMMY_ELEMENT )
      return true;

    return m_provider.isLabelProperty( element, property );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( final ILabelProviderListener listener )
  {
    m_provider.removeListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
   */
  public Color getForeground( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
   */
  public Color getBackground( final Object element )
  {
    if( element == LastLineContentProvider.DUMMY_ELEMENT )
      return m_backgroundColor;
    
    return null;
  }

}
