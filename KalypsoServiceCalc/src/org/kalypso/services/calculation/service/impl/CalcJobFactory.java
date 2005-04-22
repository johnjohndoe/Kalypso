/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.calculation.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public class CalcJobFactory
{
  private static final Properties m_jobTypes = new Properties();

  public CalcJobFactory( final File typeFile )
  {
    try
    {
      m_jobTypes.load( new FileInputStream( typeFile ) );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  public String[] getSupportedTypes()
  {
    return (String[])m_jobTypes.keySet().toArray( new String[0] );
  }

  public ICalcJob createJob( final String typeID ) throws CalcJobServiceException
  {
    try
    {
      final String className = m_jobTypes.getProperty( typeID );

      return (ICalcJob)Class.forName( className ).newInstance();
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Konnte Job nicht erzeugen für Typ: " + typeID, e );
    }
  }
}