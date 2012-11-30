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
package org.kalypso.model.wspm.ewawi.shape.writer.log;

import java.io.File;
import java.io.IOException;

/**
 * @author Holger Albert
 */
public class XyzEwawiLogger extends AbstractEwawiLogger
{
  public XyzEwawiLogger( )
  {
  }

  @Override
  public void init( final File logFile ) throws IOException
  {
    super.init( logFile );

    logLine( "x\ty\tz\tBemerkung\tKategorie\tDistanz" );
  }

  public void logXyzLine( final double x, final double y, final double z, final String comment, final String category, final double distance )
  {
    try
    {
      final String line = String.format( "%f\t%f\t%f\t%s\t%s\t%f", x, y, z, comment, category, distance );

      logLine( line );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }
}