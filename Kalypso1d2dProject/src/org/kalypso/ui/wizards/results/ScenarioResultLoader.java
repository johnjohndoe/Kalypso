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

import org.eclipse.core.resources.IFile;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import com.google.common.cache.CacheLoader;
import com.google.common.cache.RemovalListener;
import com.google.common.cache.RemovalNotification;

import de.renew.workflow.connector.cases.IScenario;

/**
 * @author Gernot Belger
 */
class ScenarioResultLoader extends CacheLoader<IScenario, IScenarioResultMeta> implements RemovalListener<IScenario, IScenarioResultMeta>
{
  @Override
  public IScenarioResultMeta load( final IScenario scenario ) throws Exception
  {
    final Scenario1D2D wrapper = new Scenario1D2D( scenario.getFolder() );
    final IFile resultMetaFile = wrapper.getResultMetaFile();

    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resultMetaFile );

    return (IScenarioResultMeta)workspace.getRootFeature();
  }

  @Override
  public void onRemoval( final RemovalNotification<IScenario, IScenarioResultMeta> notification )
  {
    final IScenarioResultMeta result = notification.getValue();
    if( result == null )
      return;

    final GMLWorkspace workspace = result.getWorkspace();
    workspace.dispose();
  }
}