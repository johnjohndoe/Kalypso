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
package org.kalypso.model.hydrology.util;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.simulation.INaSimulationRunnable;
import org.kalypso.utils.log.StatusLogUtilities;

/**
 * @author Gernot Belger
 */
public class NAProtokollPublisher
{
  public static void publishProtokoll( final INaSimulationData data, final INaSimulationRunnable runnable, final IStatusCollector log )
  {
    if( data == null )
      return;

    final String pegelID = data.getNaOptimize().getId();

    final String logFilename = String.format( "Protokoll_%s.log", pegelID );
    final String protokollFilename = String.format( "Protokoll_%s.gml", pegelID );

    final File protokollFile = new File( runnable.getResultDir(), protokollFilename );
    final File logFile = new File( runnable.getResultDir(), logFilename );

    /* also pusblish exe-log under a separate name */
    final IStatus publishStatus = publishExeLog( runnable, logFile );
    if( !publishStatus.isOK() )
      log.add( publishStatus );

    StatusLogUtilities.writeStatusLogQuietly( log, protokollFile );

    data.dispose();
  }

  private static IStatus publishExeLog( final INaSimulationRunnable runnable, final File targetFile )
  {
    if( runnable == null )
      return Status.OK_STATUS;

    try
    {
      runnable.copyExeLog( targetFile );
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, "Fehler beim Kopieren der Logdatei des Rechenkerns", e );
    }
  }
}