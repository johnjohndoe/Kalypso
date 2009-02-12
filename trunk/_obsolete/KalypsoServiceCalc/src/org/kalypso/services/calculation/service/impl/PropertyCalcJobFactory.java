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
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public class PropertyCalcJobFactory implements ICalcJobFactory
{
  private static final Properties m_jobTypes = new Properties();

  /**
   * Closes the stream!
   * 
   * @throws IOException
   */
  private PropertyCalcJobFactory( final InputStream typeStream ) throws IOException
  {
    try
    {
      m_jobTypes.load( typeStream );
      typeStream.close();
    }
    finally
    {
      IOUtils.closeQuietly( typeStream );
    }
  }

  public PropertyCalcJobFactory( final File typeFile ) throws IOException
  {
    this( new FileInputStream( typeFile ) );
  }

  public PropertyCalcJobFactory( final URL typeURL ) throws IOException
  {
    this( typeURL.openStream() );
  }

  public PropertyCalcJobFactory( final Properties typeProperties )
  {
    m_jobTypes.putAll( typeProperties );
  }

  public String[] getSupportedTypes()
  {
    return m_jobTypes.keySet().toArray( new String[0] );
  }

  public ICalcJob createJob( final String typeID ) throws CalcJobServiceException
  {
    try
    {
      final String className = m_jobTypes.getProperty( typeID );
      if( className == null )
        throw new CalcJobServiceException( "Unbekannter Job-Typ: " + typeID, null );

      return (ICalcJob)Class.forName( className ).newInstance();
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Konnte Job nicht erzeugen für Typ: " + typeID, e );
    }
  }
}