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
package org.kalypso.ogc.gml.command;

import org.kalypso.commons.command.ICommand;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * @author belger
 */
public class RemoveThemeCommand implements ICommand
{
  private final IMapModell m_mapModell;

  private final IKalypsoTheme m_theme;

  private final boolean m_force;

  public RemoveThemeCommand( final IMapModell mapModell, final IKalypsoTheme theme )
  {
    this( mapModell, theme, false );
  }

  /**
   * @param force
   *          force the deletion of a not deleteable theme
   */
  public RemoveThemeCommand( final IMapModell mapModell, final IKalypsoTheme theme, final boolean force )
  {
    m_mapModell = mapModell;
    m_theme = theme;
    m_force = force;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    /* Check if deleteable, should never fail, all user actions should be aware of this flag. */
    final String deleteableStr = m_theme.getProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( false ) );
    final boolean deletable = Boolean.parseBoolean( deleteableStr );
    if( !m_force && !deletable )
      throw new IllegalStateException( Messages.getString( "org.kalypso.ogc.gml.command.RemoveThemeCommand.0" ) + m_theme.getName() ); //$NON-NLS-1$

    m_mapModell.removeTheme( m_theme );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    m_mapModell.removeTheme( m_theme );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    m_mapModell.addTheme( m_theme );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.ogc.gml.command.RemoveThemeCommand.1" ); //$NON-NLS-1$
  }
}