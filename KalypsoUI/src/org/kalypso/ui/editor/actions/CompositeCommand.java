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
package org.kalypso.ui.editor.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.commons.command.ICommand;

/**
 * This class encapsulates a series of (similar) commands that can only be processed, undone and redone together
 * 
 * @author Stefan Kurzbach
 */
public class CompositeCommand implements ICommand
{

  private final String m_description;

  private final List<ICommand> m_commands;

  private boolean m_canUndo;

  /**
   * Creates a new composite command with given description and command to be executed
   */
  public CompositeCommand( final String description, final ICommand command )
  {
    m_commands = new ArrayList<ICommand>();
    m_description = description;
    m_canUndo = true;
    addCommand( command );
  }

  /**
   * Adds a command to the list of commands
   */
  public void addCommand( final ICommand command )
  {
    m_commands.add( command );
    if( !command.isUndoable() )
      m_canUndo = false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * The composite command is only undoable if all commands that it exectutes can be undone
   * 
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return m_canUndo;
  }

  /**
   * Executes all the commands in order they were added
   * 
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    for( final ICommand command : m_commands )
    {
      command.process();
    }
  }

  /**
   * Executes all the commands in order they were added
   * 
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * Undos the commands in reverse order they were added
   * 
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    for( int i = m_commands.size() - 1; i >= 0; i-- )
    {
      m_commands.get( i ).undo();
    }
  }

}
