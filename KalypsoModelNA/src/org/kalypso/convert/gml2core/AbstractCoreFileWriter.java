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
package org.kalypso.convert.gml2core;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * @author Dejan Antanaskovic
 */
public abstract class AbstractCoreFileWriter implements ICoreFileWriter
{
  protected final File m_outputFile;

  protected final StringBuffer m_contentBuffer;

  private boolean m_contentCreated = false;

  private boolean m_error = false;

  public AbstractCoreFileWriter( final File outputFile )
  {
    m_outputFile = outputFile;
    m_contentBuffer = new StringBuffer();
  }

  protected abstract void createContent( ) throws Exception;

  public final void internalCreateContent( )
  {
    try
    {
      createContent();
      m_contentCreated = true;
    }
    catch( final Exception e )
    {
      handleError( e );
    }
  }

  /**
   * @see org.kalypso.convert.gml2core.ICoreFileWriter#write()
   */
  @Override
  public final void write( )
  {
    if( !m_error && !m_contentCreated )
      internalCreateContent();
    final FileOutputStream fileOutputStream;
    try
    {
      fileOutputStream = new FileOutputStream( m_outputFile );
      final DataOutputStream stream = new DataOutputStream( fileOutputStream );
      stream.writeBytes( m_contentBuffer.toString() );
      stream.close();
    }
    catch( final FileNotFoundException e )
    {
      handleError( e );
    }
    catch( final IOException e )
    {
      handleError( e );
    }
  }

  private void handleError( final Exception e )
  {
    m_error = true;
    Logger.getLogger( getClass().getName() ).warning( e.getLocalizedMessage() );
  }
}
