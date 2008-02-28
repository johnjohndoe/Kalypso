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
package org.kalypso.afgui.scenarios;

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Stefan Kurzbach
 */
public class ScenarioHelper
{
  public static String getProjectName( final Scenario scenario )
  {
    final String name = scenario.getURI();
    try
    {
      final URI uri = new URI( name );
      return uri.getAuthority();
    }
    catch( final URISyntaxException e )
    {
      return null;
    }
  }

  public static IProject getProject( final Scenario scenario )
  {
    final String name = scenario.getURI();
    try
    {
      final URI uri = new URI( name );
      return ResourcesPlugin.getWorkspace().getRoot().getProject( uri.getAuthority() );
    }
    catch( final URISyntaxException e )
    {
      return null;
    }
  }

  public static IFolder getFolder( final Scenario scenario )
  {
    final String name = scenario.getURI();
    try
    {
      final URI uri = new URI( name );
      return getProject( scenario ).getFolder( uri.getPath() );
    }
    catch( final URISyntaxException e )
    {
      return null;
    }
  }

  /**
   * Retrieves the folder of the currently active scenario via the current evaluation context of the handler service.
   */
  public static IFolder getScenarioFolder( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    return (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
  }

  /**
   * Retrieves the folder of the currently active scenario via the current evaluation context of the handler service.
   */
  public static SzenarioDataProvider getScenarioDataProvider( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    return (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
  }

  /**
   * Find the oldest parent (=root) of the given scenario.<br>
   * If the scenario has no parent, itself is returned.
   */
  public static Scenario findRootScenario( final Scenario scenario )
  {
    final Scenario parentScenario = scenario.getParentScenario();
    if( parentScenario == null )
      return scenario;

    return findRootScenario( parentScenario );
  }
}