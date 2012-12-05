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
package org.kalypso.model.hydrology.internal;

import java.io.File;

import org.kalypso.model.hydrology.NaModelConstants;

/**
 * Helper class that holds all the various directories used by the NA simulation.
 * 
 * @author Gernot Belger
 */
public class NaSimulationDirs
{
  public static final String DIR_AKTUELL = "Aktuell"; //$NON-NLS-1$

  public final File simulationDir;

  public final File outputDir;

  public final File asciiDir;

  public final File resultDir;

  public final File currentResultDir;

  public final NaResultDirs currentResultDirs;

  public NaAsciiDirs asciiDirs;

  public NaSimulationDirs( final File simDir )
  {
    simulationDir = simDir;

    asciiDir = new File( simulationDir, NaModelConstants.ASCII_DIR_NAME );
    asciiDirs = new NaAsciiDirs( asciiDir );

    outputDir = new File( simulationDir, NaModelConstants.OUTPUT_DIR_NAME );

    resultDir = new File( outputDir, NaModelConstants.RESULT_DIR_NAME );

    currentResultDir = new File( resultDir, DIR_AKTUELL );
    currentResultDirs = new NaResultDirs( currentResultDir );
  }
}