/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.plugin;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * Risk user have the possibility to rename the risk zones (boundaries, descriptions). On landuse import, risk zone
 * definitions are updated as well. If that happen, the update should be reflected on risk zone definitions.
 * 
 * @author Dejan Antanaskovic
 * 
 */
public class RiskZonesChangeListener implements IResourceChangeListener
{
  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  @Override
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta rootDelta = event.getDelta();
    if( rootDelta == null )
      return;

    final IFolder scenarioFolder = RiskZonesThemeInfo.getScenarioFolder();
    if( scenarioFolder == null )
      return;

    try
    {
      final String riskResourcePath = RiskZonesThemeInfo.getResourcePath( scenarioFolder );
      if( riskResourcePath == null )
        return;
      final IPath resourcePath = scenarioFolder.getProjectRelativePath().append( riskResourcePath );
      final IPath roughnessPath = scenarioFolder.getProject().getFullPath().append( resourcePath );
      final IResourceDelta fileDelta = rootDelta.findMember( roughnessPath );
      if( fileDelta != null )
      {
        RiskZonesThemeInfo.reloadDefinitions();
      }
    }
    catch( final CoreException e )
    {
      Logger.getAnonymousLogger().log( Level.SEVERE, e.getLocalizedMessage() );
    }
  }

}
