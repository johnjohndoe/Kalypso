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

import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class SetColumnVisibleCommand implements ICommand
{
  private final String m_propertyName;

  private final boolean m_bVisible;

  private final LayerTableViewer m_viewer;

  private final boolean m_wasEditable;

  private final int m_oldWidth;

  public SetColumnVisibleCommand( final LayerTableViewer viewer, final String propertyName,
      final boolean bVisible )
  {
    m_viewer = viewer;
    m_propertyName = propertyName;
    m_bVisible = bVisible;
    m_wasEditable = viewer.isEditable( propertyName );
    m_oldWidth = viewer.getWidth( propertyName );
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    doIt( m_viewer, m_propertyName, m_bVisible, 100, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    doIt( m_viewer, m_propertyName, m_bVisible, 100, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    doIt( m_viewer, m_propertyName, !m_bVisible, m_oldWidth, m_wasEditable );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Spalte '" + m_propertyName + "' " + ( m_bVisible ? "anzeigen" : "verstecken" );
  }
  
  private void doIt( final LayerTableViewer viewer, final String propertyName, final boolean bVisible, final int width, final boolean editable )
  {
    m_viewer.getControl().getDisplay().syncExec( new Runnable()
    {
      public void run()
      {
        if( bVisible )
          viewer.addColumn( propertyName, width, editable, true );
        else
          viewer.removeColumn( propertyName );
      }
    } );
  }
}