/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.shape;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.kalypso.model.wspm.ewawi.WspmEwawiPlugin;

/**
 * @author Gernot Belger
 */
public class Ewawi2ShapeApp implements IApplication
{
  @Override
  public Object start( final IApplicationContext context ) throws Exception
  {
    final Map< ? , ? > arguments = context.getArguments();
    final String[] args = (String[])arguments.get( "application.args" ); //$NON-NLS-1$

    final IStatus status = runOperation( args );
    WspmEwawiPlugin.getDefault().getLog().log( status );

    if( !status.isOK() )
    {
      System.out.println( status.getMessage() );
      printUsage();
    }

    return EXIT_OK;
  }

  private static IStatus runOperation( final String[] args )
  {
    try
    {
      final Ewawi2ShapeData data = readArguments( args );

      final Ewawi2ShapeOperation operation = new Ewawi2ShapeOperation( data );

      operation.run( new NullProgressMonitor() );
      return Status.OK_STATUS;
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final InvocationTargetException e )
    {
      return new Status( IStatus.ERROR, WspmEwawiPlugin.PLUGIN_ID, "Fehler beim Konvertieren", e );
    }
  }

  private static Ewawi2ShapeData readArguments( final String[] args ) throws CoreException
  {
    if( args.length < 2 )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmEwawiPlugin.PLUGIN_ID, "Fehlende Kommandozeilenparameter" );
      throw new CoreException( status );
    }

    /* input file */
    final File inputDir = new File( args[0] );
    if( !inputDir.isDirectory() )
    {
      final String message = String.format( "Eingabeverzeichnis ist kein Verzeichnis: %s", args[0] );
      final IStatus status = new Status( IStatus.ERROR, WspmEwawiPlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    /* gew shape */
    final File gewShape = new File( args[1] );
    if( !gewShape.isFile() )
    {
      final String message = String.format( "Gewässershape ist keine Datei: %s", args[0] );
      final IStatus status = new Status( IStatus.ERROR, WspmEwawiPlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    return new Ewawi2ShapeData( inputDir, gewShape );
  }

  private static void printUsage( )
  {
    System.out.println();
    System.out.println();
    System.out.println( "Usage:" );
    System.out.println( "C:>ewawi2shape <input dir> <gew shape>" );
  }

  @Override
  public void stop( )
  {
  }
}