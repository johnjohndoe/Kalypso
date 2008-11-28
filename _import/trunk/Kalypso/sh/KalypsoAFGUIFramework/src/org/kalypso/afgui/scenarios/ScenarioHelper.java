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

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Stefan Kurzbach
 */
public class ScenarioHelper
{

  public static IFolder getFolder( final IScenario scenario )
  {
    final String scenarioPath = getScenarioPath( scenario );
    if( scenarioPath == null )
      return null;

    return scenario.getProject().getFolder( scenarioPath );
  }

  /**
   * Retrieves the folder of the currently active scenario via the current evaluation context of the handler service.
   */
  public static IFolder getScenarioFolder( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( workbench == null )
      return null;

    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    if( handlerService == null )
      return null;

    final IEvaluationContext context = handlerService.getCurrentState();
    if( context == null )
      return null;

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
  public static IScenario findRootScenario( final IScenario scenario )
  {
    final IScenario parentScenario = scenario.getParentScenario();
    if( parentScenario == null )
      return scenario;

    return findRootScenario( parentScenario );
  }

  /**
   * Returns the path to a given scenario.
   */
  public static String getScenarioPath( final IScenario scenario )
  {
    try
    {
      if( scenario == null )
        return null;

      final String scenarioUri = scenario.getURI();
      final URI uri = new URI( scenarioUri );
      return URLDecoder.decode( uri.getPath(), "UTF-8" );
    }
    catch( final URISyntaxException e )
    {
      e.printStackTrace();
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  public static IWorkflow findWorkflow( final IScenario scenario, final CaseHandlingProjectNature newProject )
  {
    try
    {
      if( scenario == null || newProject == null )
        return null;

      final WorkflowProjectNature workflowNature = WorkflowProjectNature.toThisNature( newProject.getProject() );
      if( workflowNature == null )
        return null;

      return workflowNature.getCurrentWorklist();
    }
    catch( final CoreException e )
    {
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( e.getStatus() );
    }

    return null;
  }
}