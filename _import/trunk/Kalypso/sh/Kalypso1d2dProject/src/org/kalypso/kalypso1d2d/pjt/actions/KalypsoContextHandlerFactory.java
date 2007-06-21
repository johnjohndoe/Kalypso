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

import java.util.List;

import org.eclipse.core.commands.IHandler;

import de.renew.workflow.contexts.ContextType;
import de.renew.workflow.contexts.ExtensionContext;
import de.renew.workflow.contexts.IContextHandlerFactory;
import de.renew.workflow.contexts.ExtensionContext.Parameter;

/**
 * Extends the capabilities of the {@link WorkflowContextHandlerFactory} by the required context handlers for the
 * specific Kalypso contexts. These are {@link MapViewInputContext}, {@link FeatureViewInputContext} and
 * {@link ThemeContext}.
 * 
 * @author Stefan Kurzbach
 */
public class KalypsoContextHandlerFactory implements IContextHandlerFactory
{
  private static final String PARAM_TYPE = "type";

  private static final String PARAM_INPUT = "input";

  /**
   * @see org.kalypso.afgui.workflow.IContextHandlerFactory#getHandler(org.kalypso.afgui.workflow.ContextType)
   */
  public IHandler getHandler( final ContextType context )
  {
    if( context instanceof ExtensionContext)
    {
      final ExtensionContext extensionContext = (ExtensionContext) context;
      final List<Parameter> parameters = extensionContext.getParameter();
      String type = null;
      String input = null;
      for( final Parameter parameter : parameters )
      {
        if( parameter.getName().equals( PARAM_TYPE ) )
        {
          type = parameter.getValue();
        }
        else if( parameter.getName().equals( PARAM_INPUT ) )
        {
          input = parameter.getValue();
        }
      }
      if( "mapViewInputContext".equals( type ) )
      {
        if( input != null )
        {
          final MapViewInputContextHandler inputContextHandler = new MapViewInputContextHandler( input );
          return inputContextHandler;
        }
      }
      else if( "featureViewInputContext".equals( type ) )
      {
        if( input != null )
        {
          final FeatureViewInputContextHandler inputContextHandler = new FeatureViewInputContextHandler( input );
          return inputContextHandler;
        }
      }
      else if( "themeContext".equals( type ) )
      {
        if( input != null )
        {
          final ThemeContextHandler contextHandler = new ThemeContextHandler( input );
          return contextHandler;
        }
      }
    }
    return null;
  }
}
