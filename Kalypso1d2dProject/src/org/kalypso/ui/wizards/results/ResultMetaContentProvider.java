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
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.kalypso.afgui.views.ScenarioContentProvider;

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

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IProject )
      return m_scenarioContentProvider.getChildren( parentElement );

    if( parentElement instanceof IScenario )
    {
      // FIXME: add results..
      return m_scenarioContentProvider.getChildren( parentElement );
    }

    return super.getChildren( parentElement );
  }

  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof IProject )
      return m_scenarioContentProvider.getParent( element );
    if( element instanceof IScenario )
      return m_scenarioContentProvider.getParent( element );

    // TODO: get scenario for root result element
    return super.getParent( element );
  }
}