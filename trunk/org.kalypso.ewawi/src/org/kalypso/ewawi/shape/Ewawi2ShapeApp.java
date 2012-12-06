package org.kalypso.ewawi.shape;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.kalypso.ewawi.Activator;

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
    Activator.getDefault().getLog().log( status );

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
      return new Status( IStatus.ERROR, Activator.PLUGIN_ID, "Fehler beim Konvertieren", e );
    }
  }

  private static Ewawi2ShapeData readArguments( final String[] args ) throws CoreException
  {
    if( args.length < 3 )
    {
      final IStatus status = new Status( IStatus.ERROR, Activator.PLUGIN_ID, "Fehlende Kommandozeilenparameter" );
      throw new CoreException( status );
    }

    /* input file */
    final File inputDir = new File( args[0] );
    if( !inputDir.isDirectory() )
    {
      final String message = String.format( "Eingabeverzeichnis ist kein Verzeichnis: %s", args[0] );
      final IStatus status = new Status( IStatus.ERROR, Activator.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    /* gew shape */
    final File gewShape = new File( args[1] );
    if( !gewShape.isFile() )
    {
      final String message = String.format( "Gewässershape ist keine Datei: %s", args[1] );
      final IStatus status = new Status( IStatus.ERROR, Activator.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    /* gew width shape */
    final File gewWidthShape = new File( args[2] );
    if( !gewWidthShape.isFile() )
    {
      final String message = String.format( "Gewässerbreitenshape ist keine Datei: %s", args[2] );
      final IStatus status = new Status( IStatus.ERROR, Activator.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    return new Ewawi2ShapeData( inputDir, gewShape, gewWidthShape );
  }

  private static void printUsage( )
  {
    System.out.println();
    System.out.println();
    System.out.println( "Usage:" );
    System.out.println( "C:>ewawi2shape <input dir> <gew shape> <gew width shape>" );
  }

  @Override
  public void stop( )
  {
  }
}