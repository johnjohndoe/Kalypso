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
 * Helper class that holds all the various directories used by the NA simulation.
 * 
 * @author Gernot Belger
 */
public class NaAsciiDirs
{
  public static final String DIR_KLIMA_DAT = "klima.dat"; //$NON-NLS-1$

  public static final String DIR_ZUFLUSS = "zufluss"; //$NON-NLS-1$

  public static final String DIR_HYDRO_TOP = "hydro.top"; //$NON-NLS-1$

  public final File asciiDir;

  public final File startDir;

  public final File outWeNatDir;

  public final File lzsimDir;

  public final File klimaDatDir;

  public final File hydroTopDir;

  public final File zuflussDir;

  public NaAsciiDirs( @SuppressWarnings("hiding") final File asciiDir )
  {
    this.asciiDir = asciiDir;
    startDir = new File( asciiDir, "start" ); //$NON-NLS-1$
    klimaDatDir = new File( asciiDir, DIR_KLIMA_DAT );
    hydroTopDir = new File( asciiDir, DIR_HYDRO_TOP );
    zuflussDir = new File( asciiDir, DIR_ZUFLUSS );
    outWeNatDir = new File( asciiDir, "out_we.nat" ); //$NON-NLS-1$
    lzsimDir = new File( asciiDir, "lzsim" ); //$NON-NLS-1$
  }

}
