/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.kalypsomodel1d2d.sim.preprocessing;

import java.io.File;

import org.kalypso.kalypsomodel1d2d.sim.IRMAPreprocessing;

/**
 * @author Gernot Belger
 */
public class PreRMAFiles
{
  private final File m_outputDir;

  public PreRMAFiles( final File outputDir )
  {
    m_outputDir = outputDir;
  }

  public File getMeshFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_MESH );
  }

  public File getControlFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_CONTROL );
  }

  public File getBuildingFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_BUILDINGS );
  }

  public File getBcWqFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_BC_WQ );
  }

  public File getWindFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_WIND );
  }

  public File getWindCoordFile( )
  {
    return new File( m_outputDir, IRMAPreprocessing.OUTPUT_WIND_COORD );
  }
}