/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.ui.wizards.results;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

import de.renew.workflow.connector.cases.IScenario;

/**
 * @author Gernot
 */
public class SelectResultData extends AbstractModelObject
{
  public enum ShowType
  {
    current( "current scenario only" ),
    project( "all scenarios of the current project" ),
    all( "all projects and scenarios" );

    private final String m_label;

    private ShowType( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  public static final String PROPERTY_SHOW_ALL = "showAllType"; //$NON-NLS-1$

  private static final String PROPERTY_SHOW_OPTIONS = "showOptions"; //$NON-NLS-1$

  public static final String PROPERTY_RESULT_ROOT = "resultRoot"; //$NON-NLS-1$

  private final IResultMeta m_currentScenarioResult;

  private boolean m_showOptions = false;

  private Object m_resultRoot;

  public SelectResultData( final IResultMeta currentScenarioResult )
  {
    m_currentScenarioResult = currentScenarioResult;
    m_resultRoot = m_currentScenarioResult;
  }

  public IResultMeta getCurrentScenarioResult( )
  {
    return m_currentScenarioResult;
  }

  public Object getResultRoot( )
  {
    return m_resultRoot;
  }

  private void setResultRoot( final Object resultRoot )
  {
    final Object oldRoot = m_resultRoot;
    final Object oldType = getShowAllType();

    m_resultRoot = resultRoot;

    // FIXME: reset checked elements

    firePropertyChange( PROPERTY_RESULT_ROOT, oldRoot, resultRoot );
    firePropertyChange( PROPERTY_SHOW_ALL, oldType, getShowAllType() );
  }

  public ShowType getShowAllType( )
  {
    if( m_resultRoot == m_currentScenarioResult )
      return ShowType.current;

    if( m_resultRoot instanceof IProject )
      return ShowType.project;

    if( m_resultRoot instanceof IWorkspaceRoot )
      return ShowType.all;

    throw new IllegalArgumentException();
  }

  public void setShowAllType( final ShowType showAllType )
  {
    setResultRoot( rootForType( showAllType ) );
  }

  private Object rootForType( final ShowType showAllType )
  {
    switch( showAllType )
    {
      case current:
        return m_currentScenarioResult;

      case project:
        final IScenario currentScenario = KalypsoAFGUIFrameworkPlugin.getDataProvider().getScenario();
        return currentScenario.getProject();

      case all:
        return ResourcesPlugin.getWorkspace().getRoot();

      default:
        throw new IllegalArgumentException();
    }
  }

  public boolean getShowOptions( )
  {
    return m_showOptions;
  }

  /**
   * Sets if the additional filter options for the tree should be shown.<br/>
   * In particular, this allows to select results from other scenarios than the current one (which is not supported by all clients yet).<br/>
   * Defaults to <code>false</code>.<br/>
   * TODO: all client should support this feature, so this can be removed as soon as this is achieved.
   */
  public void setShowOptions( final boolean showOptions )
  {
    final boolean oldValue = m_showOptions;

    m_showOptions = showOptions;

    firePropertyChange( PROPERTY_SHOW_OPTIONS, oldValue, showOptions );
  }
}