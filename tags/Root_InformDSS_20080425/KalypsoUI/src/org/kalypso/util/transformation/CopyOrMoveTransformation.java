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
package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;

/**
 * @deprecated use ant task instead in your model-configuration
 * 
 * belger
 */
public class CopyOrMoveTransformation extends AbstractTransformation
{
  public static final int MODE_MOVE = 1;

  public static final int MODE_COPY = 2;

  /** Eingabedatei: Absoluter Pfad im Workspace */
  public final static String PROP_INPUT = "input";

  /** Ausgabedatei: Absoluter Pfad im Workspace */
  public final static String PROP_OUTPUT = "output";

  /** ignore error */
  public final static String PROP_IGNOREERROR = "ignoreError";

  private final int m_mode;

  public CopyOrMoveTransformation( int mode )
  {
    m_mode = mode;
  }

  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "Transform", 3000 );

    final String input = properties.getProperty( PROP_INPUT );
    final String output = properties.getProperty( PROP_OUTPUT );
    final String ignoreError = properties.getProperty( PROP_IGNOREERROR );

    try
    {
      if( input == null )
        throw new TransformationException( "Parameter 'input' nicht gesetzt" );
      if( output == null || output.length() == 0 )
        throw new TransformationException( "Parameter 'output' nicht gesetzt" );

      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IFile inputFile = root.getFile( new Path( input ) );
      if( inputFile == null || !inputFile.exists() )
        throw new TransformationException( "input file doesn't exist or is not a file: " + input );

      final IFile outputFile = root.getFile( new Path( output ) );

      try
      {
        FolderUtilities.mkdirs( outputFile.getParent() );
        if( outputFile.exists() )
          outputFile.delete( false, true, new SubProgressMonitor( monitor, 1000 ) );
        else
          monitor.worked( 1000 );
        inputFile.copy( outputFile.getFullPath(), false, new SubProgressMonitor( monitor, 1000 ) );
        outputFile.setCharset( inputFile.getCharset(), new SubProgressMonitor( monitor, 1000 ) );
        if( m_mode == MODE_MOVE )
          inputFile.delete( false, true, new SubProgressMonitor( monitor, 1000 ) );
      }
      catch( final CoreException e )
      {
        throw new TransformationException( e );
      }
    }
    catch( TransformationException e )
    {
      if( ignoreError != null && "true".equals( ignoreError ) )
        return;
      throw e;
    }
  }
}