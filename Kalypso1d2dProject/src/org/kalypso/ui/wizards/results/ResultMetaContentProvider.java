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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ExecutionException;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.views.ScenarioContentProvider;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.UncheckedExecutionException;

import de.renew.workflow.connector.cases.IScenario;

/**
 * REMARK: it is sufficient to extent from {@link BaseWorkbenchContentProvider}, as the {@link org.eclipse.ui.model.WorkbenchContentProvider} only adds support for resource changes, and we can assume,
 * that this does not happen while we work with one result set.
 * 
 * @author Gernot Belger
 */
class ResultMetaContentProvider extends BaseWorkbenchContentProvider
{
  private final ScenarioContentProvider m_scenarioContentProvider = new ScenarioContentProvider( false );

  private final IScenarioResultMeta m_currentScenarioResult;

  private final ScenarioResultLoader m_resultLoader = new ScenarioResultLoader();

  private final LoadingCache<IScenario, IScenarioResultMeta> m_resultCache = CacheBuilder.newBuilder().weakValues().maximumSize( 100 ).removalListener( m_resultLoader ).build( m_resultLoader );

  public ResultMetaContentProvider( final IScenarioResultMeta currentScenarioResult )
  {
    m_currentScenarioResult = currentScenarioResult;
  }

  @Override
  public void dispose( )
  {
    m_resultCache.invalidateAll();
    m_resultCache.cleanUp();

    super.dispose();
  }

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IProject )
      return m_scenarioContentProvider.getChildren( parentElement );

    if( parentElement instanceof IScenario )
      return getScenarioChildren( (IScenario)parentElement );

    return super.getChildren( parentElement );
  }

  private Object[] getScenarioChildren( final IScenario scenario )
  {
    final Object[] childScenarios = m_scenarioContentProvider.getChildren( scenario );
    final Object[] childResults = getScenarioResults( scenario );

    /* combine into single array */
    final Collection<Object> children = new ArrayList<>( childScenarios.length + childResults.length );
    children.addAll( Arrays.asList( childScenarios ) );
    children.addAll( Arrays.asList( childResults ) );
    return children.toArray( new Object[children.size()] );
  }

  private Object[] getScenarioResults( final IScenario scenario )
  {
    /* use already loaded results for current scenario */
    final IScenario currentScenario = KalypsoAFGUIFrameworkPlugin.getDataProvider().getScenario();
    if( scenario.equals( currentScenario ) )
      return m_currentScenarioResult.getChildren().toArray();

    /* fetch from scenario */
    final IScenarioResultMeta scenarioResult = loadScenarioResults( scenario );
    if( scenarioResult == null )
    {
      // REMARK: protect against corrupt projects
      return ArrayUtils.EMPTY_OBJECT_ARRAY;
    }

    return scenarioResult.getChildren().toArray();
  }

  private IScenarioResultMeta loadScenarioResults( final IScenario scenario )
  {
    try
    {
      return m_resultCache.get( scenario );
    }
    catch( final ExecutionException | UncheckedExecutionException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof IProject )
      return m_scenarioContentProvider.getParent( element );

    if( element instanceof IScenario )
      return m_scenarioContentProvider.getParent( element );

    final Object parent = super.getParent( element );
    if( parent instanceof IScenarioResultMeta )
      return scenarioResultParent( (IScenarioResultMeta)parent );

    return parent;
  }

  private Object scenarioResultParent( final IScenarioResultMeta scenarioResult )
  {
    if( scenarioResult.equals( m_currentScenarioResult ) )
      return KalypsoAFGUIFrameworkPlugin.getDataProvider().getScenario();

    final Scenario1D2D scenario = ResultMeta1d2dHelper.findScenarioLocation( scenarioResult );
    if( scenario == null )
      return null;

    try
    {
      return scenario.loadScenario();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}