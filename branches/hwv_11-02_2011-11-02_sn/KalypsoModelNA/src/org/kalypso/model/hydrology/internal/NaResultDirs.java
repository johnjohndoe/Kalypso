/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.hydrology.internal;

import java.io.File;

/**
 * Helper class that holds all the directories inside the 'current' result dir
 * 
 * @author Gernot Belger
 */
public class NaResultDirs
{
  public final File currentResultDir;

  public final File logDir;

  public final File bilanzDir;

  public final File anfangswertDir;

  public final File reportDir;

  public final File exe_logs_zip;

  public NaResultDirs( final File resultDir )
  {
    currentResultDir = resultDir;
    anfangswertDir = new File( currentResultDir, "Anfangswerte" );//$NON-NLS-1$
    bilanzDir = new File( currentResultDir, "Bilanz" );//$NON-NLS-1$
    logDir = new File( currentResultDir, "Log" );//$NON-NLS-1$
    exe_logs_zip = new File( logDir, "output.zip" );//$NON-NLS-1$
    reportDir = new File( currentResultDir, "Report" );//$NON-NLS-1$
  }
}
