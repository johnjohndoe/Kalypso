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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.DirectoryWalker;
import org.apache.commons.io.filefilter.FileFilterUtils;

/**
 * Searches for calc case folders.
 *
 * @author Gernot Belger
 */
public class CalcCaseConvertWalker extends DirectoryWalker<File>
{
  private final File m_sourceDir;

  private final File m_templateDir;

  public CalcCaseConvertWalker( final File sourceDir )
  {
    super( FileFilterUtils.directoryFileFilter(), -1 );

    m_sourceDir = sourceDir;
    // Old template dir, will change in next version
    m_templateDir = new File( m_sourceDir, ".templates" ); //$NON-NLS-1$
  }

  public File[] execute( ) throws IOException
  {
    final Collection<File> collector = new ArrayList<>();
    walk( m_sourceDir, collector );
    return collector.toArray( new File[collector.size()] );
  }

  @Override
  protected boolean handleDirectory( final File directory, final int depth, final Collection<File> results )
  {
    if( directory.equals( m_templateDir ) )
      return false;

    /* Happened in Krückau Model: .calculation inside .models folder */
    if( directory.equals( new File( m_sourceDir, INaProjectConstants.FOLDER_MODEL ) ) )
      return false;

    /* Just ignore non calculation dirs. Needs to return true, as we may come from further up. */
    if( !isCalculationDirectory( directory ) )
      return true;

    results.add( directory );
    return false;
  }

  private boolean isCalculationDirectory( final File directory )
  {
    return new File( directory, ".calculation" ).isFile(); //$NON-NLS-1$
  }
}