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
package org.kalypso.ogc.gml.map.handlers;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.MapPanelSourceProvider;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * Helper class for implementors of {@link org.eclipse.core.commands.IHandler} for map commands.
 * 
 * @author Gernot Belger
 */
public class MapHandlerUtils
{
  private MapHandlerUtils( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" );
  }

  public static void postMapCommand( final MapPanel mapPanel, final ChangeExtentCommand command, final Runnable runnable ) throws ExecutionException
  {
    final ICommandTarget commandTarget = mapPanel.getWidgetManager().getCommandTarget();
    if( commandTarget == null )
      throw new ExecutionException( "No active command target" ); //$NON-NLS-1$

    commandTarget.postCommand( command, runnable );
  }

  /**
   * Gets the currently active mapPanel from the handler event.<br>
   * To be more precise, gets the <code>activeMapPanel</code> source from the events context.
   * 
   * @throws ExecutionException
   *           If the current context contains no mapPanel.
   */
  public static MapPanel getMapPanel( final IEvaluationContext context ) throws ExecutionException
  {
    final MapPanel mapPanel = (MapPanel) context.getVariable( MapPanelSourceProvider.ACTIVE_MAPPANEL_NAME );
    if( mapPanel == null )
      throw new ExecutionException( "No mapPanel in context." ); //$NON-NLS-1$

    return mapPanel;
  }

  /**
   * Gets the currently active mapModell from the handler event.<br>
   * To be more precise, gets the <code>activeMapPanel</code> source from the events context, and from it, its map
   * modell.
   * 
   * @throws ExecutionException
   *           If the current context contains no mapPanel.
   */
  public static IMapModell getMapModell( final IEvaluationContext context ) throws ExecutionException
  {
    final MapPanel mapPanel = getMapPanel( context );
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      throw new ExecutionException( "No mapModell in context." ); //$NON-NLS-1$

    return mapModell;
  }


  /**
   * Gets the currently active theme from the handler event.<br>
   * To be more precise, gets the <code>activeMapPanel</code> source from the events context, and from it, its active
   * theme.
   * 
   * @throws ExecutionException
   *           If the current context contains no mapPanel.
   */
  public static IKalypsoTheme getActiveTheme( final IEvaluationContext context ) throws ExecutionException
  {
    final IMapModell mapModell = getMapModell( context );
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme == null )
      throw new ExecutionException( "No active theme in context" ); //$NON-NLS-1$

    return activeTheme;
  }
}
