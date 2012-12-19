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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.Locale;

/**
 * @author Gernot Belger
 */
public abstract class AbstractSobekFileExportOperation extends AbstractSobekExportOperation
{
  private final File m_targetFile;

  private Formatter m_formatter;

  public AbstractSobekFileExportOperation( final SobekExportInfo info, final String filename )
  {
    super( info, filename );

    m_targetFile = new File( info.getTargetDir(), filename );
  }

  @Override
  public void initTargetFile( ) throws FileNotFoundException, UnsupportedEncodingException
  {
    m_formatter = new Formatter( m_targetFile, Charset.defaultCharset().name(), Locale.US );
  }

  public Formatter getFormatter( )
  {
    return m_formatter;
  }

  @Override
  protected void close( ) throws IOException
  {
    m_formatter.flush();
    checkIO();
    m_formatter.close();
    checkIO();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.AbstractSobekExportOperation#closeQuiet()
   */
  @Override
  protected void closeQuiet( )
  {
    if( m_formatter != null )
    {
      m_formatter.close();
    }
  }

  private void checkIO( ) throws IOException
  {
    final IOException ioException = m_formatter.ioException();
    if( ioException != null )
      throw ioException;
  }

}
