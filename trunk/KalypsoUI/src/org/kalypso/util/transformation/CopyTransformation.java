package org.kalypso.util.transformation;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.eclipse.core.resources.FolderUtilities;

/**
 * @author belger
 */
public class CopyTransformation extends AbstractTransformation
{
  /** Eingabedatei: Absoluter Pfad im Workspace */
 public final static String PROP_INPUT = "input";
 
  /** Ausgabedatei: Absoluter Pfad im Workspace */
 public final static String PROP_OUTPUT = "output";

/**
  * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, org.eclipse.core.runtime.IProgressMonitor)
  */
  public void transformIntern( final Properties properties, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "Transform", 3000 );
    
    final String input = properties.getProperty( PROP_INPUT );
    final String output = properties.getProperty( "output" );

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
    }
    catch( final CoreException e )
    {
      throw new TransformationException( e );
    }
  }
}
