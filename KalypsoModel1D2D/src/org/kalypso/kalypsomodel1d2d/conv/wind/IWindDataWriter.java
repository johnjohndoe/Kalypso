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
package org.kalypso.kalypsomodel1d2d.conv.wind;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.apache.commons.vfs.FileObject;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public interface IWindDataWriter
{
  /**
   * configure this writer with the wind model that will provide the wind data for all time steps in it for output
   * 
   * @param pWindModel
   *          the wind data model
   * @throws IllegalArgumentException
   *           id handler parameter is null
   */
  public void setWindDataModel( IWindModel pWindModel ) throws IllegalArgumentException;

  /**
   * 
   * writes inside of the given {@link GM_Envelope} between the given {@link Date} dates 
   * to target directory given by {@link URL} parameter the data containing in {@link IWindDataProvider} 
   * 
   * 
   * 
   * @throws IOException
   *           to signal that an IO exception occurs while writing the files
   */
  public boolean write( final boolean pBoolConstantWind ) throws IOException;

  /**
   * @param  pUrlOuputDirectory{@link URL} 
   *    pointing to output directory to write to
   */
  public void setOutputDirectory( final FileObject pOutputDirectory );
  
  /**
   * @return the {@link List} of dates of written steps 
   */
  public List< Date > getListWritenDates();

  /**
   * @return the {@link RectifiedGridDomain} descriptor of written grid 
   */
  public RectifiedGridDomain getWrittenGrid();
}