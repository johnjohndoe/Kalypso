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

import org.kalypso.ogc.sensor.template.AbstractObservationTheme;
import org.kalypso.ogc.sensor.template.AbstractViewTemplate;
import org.kalypso.util.command.ICommand;

/**
 * RemoveThemeCommand
 * 
 * @author schlienger
 */
public class RemoveThemeCommand implements ICommand
{
  private final AbstractViewTemplate m_template;

  private final AbstractObservationTheme m_theme;

  public RemoveThemeCommand( AbstractViewTemplate template,
      AbstractObservationTheme theme )
  {
    m_template = template;
    m_theme = theme;
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
    m_template.removeTheme( m_theme );
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
    m_template.addTheme( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Entfernt einen Thema";
  }
}