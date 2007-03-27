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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.IHandler;
import org.kalypso.afgui.workflow.ContextType;
import org.kalypso.afgui.workflow.FeatureViewInputContext;
import org.kalypso.afgui.workflow.MapViewInputContext;
import org.kalypso.afgui.workflow.ThemeContext;
import org.kalypso.afgui.workflow.WorkflowContextHandlerFactory;

/**
 * Extends the capabilities of the {@link WorkflowContextHandlerFactory} by the required context handlers for the
 * specific Kalypso contexts. These are {@link MapViewInputContext}, {@link FeatureViewInputContext} and
 * {@link ThemeContext}.
 * 
 * @author Stefan Kurzbach
 */
public class KalypsoContextHandlerFactory extends WorkflowContextHandlerFactory
{
  /**
   * @see org.kalypso.afgui.workflow.IContextHandlerFactory#getHandler(org.kalypso.afgui.workflow.ContextType)
   */
  @Override
  public IHandler getHandler( final ContextType context )
  {
    if( context instanceof MapViewInputContext )
    {
      final MapViewInputContext inputContext = (MapViewInputContext) context;
      final String input = inputContext.getInput();
      final MapViewInputContextHandler inputContextHandler = new MapViewInputContextHandler( input );
      return inputContextHandler;
    }
    else if( context instanceof FeatureViewInputContext )
    {
      final FeatureViewInputContext inputContext = (FeatureViewInputContext) context;
      final String input = inputContext.getInput();
      final FeatureViewInputContextHandler inputContextHandler = new FeatureViewInputContextHandler( input );
      return inputContextHandler;
    }
    else if( context instanceof ThemeContext )
    {
      final ThemeContext themeContext = (ThemeContext) context;
      final String featureType = themeContext.getFeatureType();
      final ThemeContextHandler contextHandler = new ThemeContextHandler( featureType );
      return contextHandler;
    }
    else
      return super.getHandler( context );
  }
}
