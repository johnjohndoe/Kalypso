/**----------------    FILE HEADER KALYPSO ------------------------------------------
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
*  g.belger@bjoernsen.de
*  m.schlienger@bjoernsen.de
*  v.doemming@tuhh.de
*   
*  ---------------------------------------------------------------------------*/
package org.kalypso.services.calculation.job.impl;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author thuel2
 */
public class InputMap
{
  private final Map m_mapInput;
  private final File m_dirInput;

  public InputMap( final CalcJobDataBean[] input, final File dirInput )
  {
    m_dirInput = dirInput;
    
    m_mapInput = new HashMap( input.length );
    
    for( int i = 0; i < input.length; i++ )
    {
      final CalcJobDataBean bean = input[i];
      m_mapInput.put( bean.getId(), bean );
    }
  }

  /**
   * Gibt die Datei zur entsprechenden ID (wie in modelspec.xml definiert)
   * zurück
   * 
   * @param sId
   * 
   * @return file
   * 
   * @throws CalcJobServiceException
   */
  public File getInputById( final String sId )
      throws CalcJobServiceException
  {
    final CalcJobDataBean bean;
    final File fleInput;

    bean = (CalcJobDataBean)m_mapInput.get( sId );
    if( bean == null )
      throw new CalcJobServiceException( "Eingabedatei für ID < " + sId + "> fehlt", null );

    fleInput = new File( m_dirInput, bean.getPath() );
    if( !fleInput.exists() || !fleInput.isFile() )
      throw new CalcJobServiceException( "Eingabedatei für ID <" + sId + "> fehlt:"
          + fleInput.getAbsolutePath(), null );

    return fleInput;
  }

  
}
