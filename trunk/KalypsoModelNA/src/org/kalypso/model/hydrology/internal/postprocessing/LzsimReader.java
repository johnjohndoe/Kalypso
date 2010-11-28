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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.util.Date;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;

/**
 * @author huebsch
 */
public class LzsimReader
{
  private final Date[] m_initialDates;

  private final File m_outputDir;

  public LzsimReader( final Date[] initialDates, final File outputDir )
  {
    m_initialDates = initialDates;
    m_outputDir = outputDir;
  }

  /**
   * Reads the initial values back from the ascii files, if any have been ordered.
   */
  public void readInitialValues( final IDManager idManager, final HydroHash hydroHash, final File lzsimDir, final Logger logger ) throws Exception
  {
    if( m_initialDates.length == 0 )
      return;

    for( final Date initialDate : m_initialDates )
    {
      final LzsToGml lzsToGml = new LzsToGml( lzsimDir, initialDate, idManager, hydroHash, logger );
      lzsToGml.readLzs();
      lzsToGml.writeGml( m_outputDir );
    }
  }
}