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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;

/**
 * Wrapper around a 1D2D-Project that allows access to its resources.
 * 
 * @author Gernot Belger
 */
public class Project1D2D
{
  private static final String FOLDER_TIMESERIES = "timeseries"; //$NON-NLS-1$

  private static final String FOLDER_IMPORTS = "imports"; //$NON-NLS-1$

  private final IProject m_project;

  public Project1D2D( final IProject project )
  {
    m_project = project;
  }

  public IFolder getImportsFolder( )
  {
    return m_project.getFolder( FOLDER_IMPORTS );
  }

  public IFolder getTimeeseriesFolder( )
  {
    return getImportsFolder().getFolder( FOLDER_TIMESERIES );
  }
}