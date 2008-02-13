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
package org.kalypso.model.wspm.sobek.core.utils;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationRepository;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author kuch
 */
public class RepositoryUtils
{
  public static final String NOFDP_TIME_SERIES_REPOSITORY = "timeSeriesRepository";

  public static IRepositoryContainer createRepository( final IProject project, final String factory ) throws CoreException
  {
    /* repository container */
    final IRepositoryContainer reposContainer = KalypsoGisPlugin.getDefault().getRepositoryContainer();

    /* add new project repository */
    final IFolder repos = project.getFolder( NOFDP_TIME_SERIES_REPOSITORY );
    if( !repos.exists() )
      repos.create( true, true, new NullProgressMonitor() );

    final String location = repos.getLocation().toOSString();
    final String configuration = location + "#*.zml";

    // repository still existing?!?
    final IRepository[] repositories = reposContainer.getRepositories();
    for( final Object object : repositories )
    {
      if( object == null || !(object instanceof ZmlObservationRepository) )
        continue;

      final ZmlObservationRepository r = (ZmlObservationRepository) object;
      if( configuration.equals( r.getConfiguration() ) )
        return reposContainer;
    }

    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*.zml" }, false, true, false );

    final ZmlObservationRepository repository = new ZmlObservationRepository( factory, configuration, location, "Time Series", false, filter );
    reposContainer.addRepository( repository );

    return reposContainer;
  }

}
