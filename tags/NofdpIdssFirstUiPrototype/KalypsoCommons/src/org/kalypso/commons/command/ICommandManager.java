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
package org.kalypso.commons.command;

/**
 * <p>
 * Das Interface zur Implementation eines CommandManagers. Der CommandManager f�hrt eine Liste von Kommandos und
 * koordiniert die Ausf�hrung und den Undo-Mechanismus.
 * </p>
 * <p>
 * Ein Kommando wird mittels {@link ICommandManager#postCommand( ICommand)}dem CommandManager �bergeben und von diesem
 * ausgef�hrt. Das Runnable wird nach dem Kommando zus�tzlich durchgef�hrt
 * </p>
 * Mittels {@link ICommandManager#canUndo()}und {@link ICommandManager#canRedo()}kann ermittelt werden, ob es m�glich
 * ist, das letzt Kommando r�ckg�ngig zu machen oder das zuletzt r�ckg�ngig gemachte wiederherzustellen.
 * <p>
 * Der CommandManager ist ein Publisher, welcher seine Observer �ber die durchgef�hrten Aktionen (postCommand, undo,
 * redo) informiert.
 * </p>
 * 
 * @author von D�mming
 */
public interface ICommandManager
{
  /**
   * F�gt ein Kommando zum Manager hinzu. Ruft {@link ICommand#process()}auf.
   * 
   * @param command
   * @throws Exception
   */
  public void postCommand( final ICommand command ) throws Exception;

  public boolean canUndo();

  public void undo() throws Exception;

  public String getUndoDescription();

  public boolean canRedo();

  public void redo() throws Exception;

  public String getRedoDescription();

  public void addCommandManagerListener( final ICommandManagerListener l );

  public void removeCommandManagerListener( final ICommandManagerListener l );

  public boolean isDirty();

  public void resetDirty();
}