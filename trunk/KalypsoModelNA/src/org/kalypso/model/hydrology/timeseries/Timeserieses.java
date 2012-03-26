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
package org.kalypso.model.hydrology.timeseries;

import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;

/**
 * @author Dirk Kuch
 */
public final class Timeserieses
{
  private Timeserieses( )
  {
  }

  public static String getTreeLabel( final ITimeseries timeseries )
  {
    final Period timestep = timeseries.getTimestep();
    final String quality = timeseries.getQuality();

    final String periodName = PeriodUtils.formatDefault( timestep );

    if( StringUtils.isBlank( quality ) )
      return periodName;

    return String.format( "%s (%s)", periodName, quality );
  }

  public static String[] findTimeseriereses( final IStation station )
  {
    final Set<String> fileNames = new LinkedHashSet<>();
    try
    {
      final URL context = station.getWorkspace().getContext();
      final URL urlFolder = UrlResolverSingleton.resolveUrl( context, station.getTimeseriesFoldername() );

      final IFolder source = ResourceUtilities.findFolderFromURL( urlFolder );

      source.accept( new IResourceVisitor()
      {
        @Override
        public boolean visit( final IResource resource )
        {
          if( resource instanceof IFile )
          {
            final IFile file = (IFile) resource;

            if( StringUtils.equalsIgnoreCase( "zml", file.getFileExtension() ) )
            {
              fileNames.add( file.getName() );
            }
          }

          return true;
        }
      } );

    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return fileNames.toArray( new String[] {} );

  }
}
