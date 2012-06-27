/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.logviewer;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;

/**
 * The content provider class is responsible for providing objects to the view. It can wrap existing objects in adapters
 * or simply return objects as-is. These objects may be sensitive to the current input of the view, or ignore it and
 * always show the same content (like Task List, for example).
 * 
 * @author Madan
 */
class TableContentProvider implements IStructuredContentProvider
{
  private IFile m_logIFile;

  private TableViewer m_viewer;

  private LogFileReader m_logFileReader;

  public TableContentProvider( )
  {

  }

  public TableContentProvider( final TableViewer viewer, final IFile file )
  {
    m_viewer = viewer;
    if( file != null )
    {
      m_logIFile = file;
    }
  }

  @Override
  public void dispose( )
  {
  }

  private IFile getLogIFile( )
  {
    return m_logIFile;
  }

  private void refresh( )
  {
    if( m_viewer != null )
    {
      m_viewer.getTable().clearAll();
    }
    m_logFileReader = new LogFileReader( getLogIFile() );
  }

  public int getRowCount( )
  {
    if( m_logFileReader == null )
    {
      return 0;
    }
    return m_logFileReader.getRowCount();
  }

  @Override
  public Object[] getElements( final Object parent )
  {
    if( m_logFileReader == null && m_logIFile != null )
    {
      refresh();
      return m_logFileReader.getRows();
    }
    return new Object[0];
  }

  @Override
  public void inputChanged( final Viewer v, final Object oldInput, final Object newInput )
  {
    // nothing?
  }
}