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
package org.kalypso.ogc.gml.table.command;

import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.table.LayerTableSorter;
import org.kalypso.ogc.gml.table.LayerTableViewer;

/**
 * @author gernot
 */
public class ChangeSortingCommand implements ICommand
{
  private final LayerTableViewer m_viewer;

  private final LayerTableSorter m_sorter;

  private final String m_oldPropertyName;

  private final String m_newPropertyName;

  private final boolean m_oldInverse;

  private final boolean m_newInverse;

  public ChangeSortingCommand( final LayerTableViewer viewer, final TableColumn tableColumn )
  {
    m_viewer = viewer;

    final String propertyName = (String)tableColumn.getData( LayerTableViewer.COLUMN_PROP_NAME );

    m_sorter = (LayerTableSorter)m_viewer.getSorter();

    m_oldPropertyName = m_sorter.getPropertyName();
    m_oldInverse = m_sorter.isInverse();

    if( m_oldPropertyName != null && m_oldPropertyName.equals( propertyName ) )
    {
      // falls bereits invers, ausschalten
      if( m_oldInverse )
      {
        m_newPropertyName = null;
        m_newInverse = false;
      }
      else
      {
        m_newInverse = true;
        m_newPropertyName = m_oldPropertyName;
      }
    }
    else
    {
      m_newInverse = false;
      m_newPropertyName = propertyName;
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    changeSorter( m_newInverse, m_newPropertyName );
  }

  private void changeSorter( final boolean bInverse, final String propertyName )
  {
    m_sorter.setInverse( bInverse );
    m_sorter.setPropertyName( propertyName );

    final LayerTableViewer viewer = m_viewer;
    viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.refresh();
      }
    } );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    changeSorter( m_oldInverse, m_oldPropertyName );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return ( m_newInverse ? "absteigend" : "aufsteigend" ) + " sortieren nach: " + m_newPropertyName;
  }

}