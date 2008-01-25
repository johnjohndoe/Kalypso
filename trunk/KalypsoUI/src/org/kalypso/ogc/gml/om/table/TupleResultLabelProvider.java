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

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

/**
 * @author Marc Schlienger
 */
public class TupleResultLabelProvider extends EventManager implements ITableLabelProvider
{
  private final IComponentUiHandler[] m_handlers;

  public TupleResultLabelProvider( final IComponentUiHandler[] handlers )
  {
    m_handlers = TupleResultContentProvider.addFakeHandler( handlers );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( final ILabelProviderListener listener )
  {
    addListenerObject( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose( )
  {
    clearListeners();
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( final Object element, final String property )
  {
    // Maybe ask content provider if this is a valid property?
    return true;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( final ILabelProviderListener listener )
  {
    removeListenerObject( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( columnIndex >= m_handlers.length )
      return null;

    if( element instanceof IRecord )
    {
      try
      {
        final IRecord record = (IRecord) element;
        final IComponentUiHandler handler = m_handlers[columnIndex];
        return handler.getImage( record );
      }
      catch( final IllegalArgumentException e )
      {
        return null;
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( columnIndex >= m_handlers.length )
      return "";

    if( element instanceof IRecord )
    {
      try
      {
        final IRecord record = (IRecord) element;
        final IComponentUiHandler handler = m_handlers[columnIndex];
        return handler.getStringRepresentation( record );
      }
      catch( final IllegalArgumentException e )
      {
        return "";
      }
    }

    return "";
  }
}
