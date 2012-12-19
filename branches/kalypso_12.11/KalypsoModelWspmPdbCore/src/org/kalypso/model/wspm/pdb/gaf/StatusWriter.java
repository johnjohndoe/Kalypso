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
package org.kalypso.model.wspm.pdb.gaf;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.Strings;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * @author Gernot Belger
 */
public class StatusWriter extends FileWriter
{
  public StatusWriter( final File file ) throws IOException
  {
    super( file );
  }

  public void write( final IStatus status ) throws IOException
  {
    writeStatus( status, 0 );
  }

  private void writeStatus( final IStatus status, final int level ) throws IOException
  {
    writeStatusLine( status, level );

    final IStatus[] children = status.getChildren();
    for( final IStatus child : children )
      writeStatus( child, level + 1 );
  }

  private void writeStatusLine( final IStatus status, final int level ) throws IOException
  {
    for( int i = 0; i < level; i++ )
      write( ' ' );

    final String severity = StatusUtilities.getLocalizedSeverity( status );
    final String message = status.getMessage();

    final String line = String.format( "%s: %s", severity, message ); //$NON-NLS-1$

    final Throwable exception = status.getException();
    if( exception != null )
      write( String.format( " (%s)", exception ) ); //$NON-NLS-1$

    write( line );
    write( Strings.LINE_SEPARATOR );
  }
}
