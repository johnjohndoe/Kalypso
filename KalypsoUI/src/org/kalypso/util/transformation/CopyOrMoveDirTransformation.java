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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.i18n.Messages;

/**
 * Kopiert oder verschiebt ein Verzeichnis
 * 
 * thül, belger
 * 
 * @deprecated use ant task instead in your model-configuration
 */
public class CopyOrMoveDirTransformation extends AbstractTransformation
{
  public static final int MODE_MOVE = 1;

  public static final int MODE_COPY = 2;

  /** Eingabedatei: Absoluter Pfad im Workspace auf ein Verzeichnis */
  public final static String PROP_INPUT = "input"; //$NON-NLS-1$

  /** Ausgabedatei: Absoluter Pfad im Workspace auf ein Verzeichnis */
  public final static String PROP_OUTPUT = "output"; //$NON-NLS-1$

  /** ignore error */
  public final static String PROP_IGNOREERROR = "ignoreError"; //$NON-NLS-1$

  //  /** Auch Unterverzeichnisse kopieren?
  //   * Muss 'true' oder 'false' sein
  //   * Optional, default ist 'false'.
  //   * */
  //  public final static String PROP_RECURSE = "recurseSubFolders";

  /**
   * Ursprüngliche Dateien löschen? Muss 'true' oder 'false' sein Optional, default ist 'false'.
   */
  public final static String PROP_DELETE_OLD = "deleteOld"; //$NON-NLS-1$

  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( Messages.getString("org.kalypso.util.transformation.CopyOrMoveDirTransformation.4"), 3000 ); //$NON-NLS-1$

    final String input = properties.getProperty( PROP_INPUT );
    final String output = properties.getProperty( PROP_OUTPUT );
    final String ignoreError = properties.getProperty( PROP_IGNOREERROR );

    try
    {
      final boolean deleteOld = Boolean.valueOf( properties.getProperty( PROP_DELETE_OLD, "false" ) ).booleanValue(); //$NON-NLS-1$
      //      final boolean recurse = Boolean.valueOf( properties.getProperty( PROP_RECURSE, "false" ) ).booleanValue();

      if( input == null )
        throw new TransformationException( Messages.getString("org.kalypso.util.transformation.CopyOrMoveDirTransformation.6") ); //$NON-NLS-1$
      if( output == null || output.length() == 0 )
        throw new TransformationException( Messages.getString("org.kalypso.util.transformation.CopyOrMoveDirTransformation.7") ); //$NON-NLS-1$

      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IFolder inputFolder = root.getFolder( new Path( input ) );
      if( inputFolder == null || !inputFolder.exists() )
        throw new TransformationException( Messages.getString("org.kalypso.util.transformation.CopyOrMoveDirTransformation.8") + input ); //$NON-NLS-1$

      final IFolder outputFolder = root.getFolder( new Path( output ) );

      try
      {
        if( deleteOld )
          inputFolder.move( outputFolder.getFullPath(), false, true, new SubProgressMonitor( monitor, 1000 ) );
        else
          inputFolder.copy( outputFolder.getFullPath(), false, new SubProgressMonitor( monitor, 1000 ) );
      }
      catch( final CoreException e )
      {
        throw new TransformationException( e );
      }
    }
    catch( TransformationException e )
    {
      if( ignoreError != null && "true".equals( ignoreError ) ) //$NON-NLS-1$
        return;
      throw e;
    }
  }
}