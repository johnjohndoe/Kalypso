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
package org.kalypso.ogc.sensor.commands;

import java.util.List;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.util.command.ICommand;

/**
 * RemoveRowCommand
 * 
 * @author schlienger
 */
public class RemoveRowCommand implements ICommand
{
  private final ObservationTableModel m_model;
  private final int m_index;
  private List m_row = null;

  /**
   * Constructor
   * 
   * @param model
   * @param index
   */
  public RemoveRowCommand( final ObservationTableModel model, final int index )
  {
    m_model = model;
    m_index = index;
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    if( m_index < m_model.getRowCount() )
      m_row = m_model.removeRow( m_index );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    m_model.addRow( m_row );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Entfernt die Zeile " + m_index;
  }
}
