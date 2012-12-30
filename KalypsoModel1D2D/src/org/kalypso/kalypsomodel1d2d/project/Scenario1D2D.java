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
package org.kalypso.kalypsomodel1d2d.project;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Path;

/**
 * Wrapper around a {@link de.renew.workflow.connector.cases.IScenario} of a 1D2D project.<br/>
 * Allows to access resources of the scenario.
 * 
 * @author Gernot Belger
 */
public class Scenario1D2D
{
  private static final String FOLDER_MODELS = "models"; //$NON-NLS-1$

  private static final String FILE_RESULT_META = "scenarioResultMeta.gml"; //$NON-NLS-1$

  private final IContainer m_scenarioFolder;

  public Scenario1D2D( final IContainer scenarioFolder )
  {
    m_scenarioFolder = scenarioFolder;
  }

  public IFolder getModelsFolder( )
  {
    return m_scenarioFolder.getFolder( new Path( FOLDER_MODELS ) );
  }

  public IFile getResultMetaFile( )
  {
    return getModelsFolder().getFile( FILE_RESULT_META );
  }
}