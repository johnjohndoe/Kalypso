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
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.Date;

import org.kalypso.commons.java.io.FileUtilities;

public class ZustandBean
{
  final String m_name;

  final String m_waterName;

  final String m_fileName;

  final double m_startStation;

  final double m_endStation;

  final Date m_date;

  public ZustandBean( final String name, final String waterName, final String fileName, final double startStation, final double endStation, final Date date )
  {
    m_name = name;
    m_waterName = waterName;
    m_fileName = fileName;
    m_startStation = startStation;
    m_endStation = endStation;
    m_date = date;
  }

  public Date getDate( )
  {
    return m_date;
  }

  public double getEndStation( )
  {
    return m_endStation;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public String getName( )
  {
    return m_name;
  }

  public double getStartStation( )
  {
    return m_startStation;
  }

  public String getWaterName( )
  {
    return m_waterName;
  }

  public ZustandContentBean readZustand( final File profDir ) throws IOException, ParseException
  {
    return ZustandContentBean.read( new File( profDir, getFileName() ) );
  }

  public RunOffEventBean[] readRunOffs( final File profDir ) throws ParseException, IOException
  {
    final String strFileName = getFileName();
    final String strBaseName = FileUtilities.nameWithoutExtension( strFileName );
    final File qwtFile = new File( profDir, strBaseName + ".qwt" );
    return RunOffEventBean.read( qwtFile );
  }
}