package org.kalypso.util.transformation;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.java.util.StringUtilities;

/**
 * @author belger
 */
public class ReplaceTransformation extends AbstractTransformation
{
  /**
   * @throws TransformationException
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "Transform", 2000 );

    final Properties replaceProps = new Properties();

    for( Iterator iter = properties.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();

      final String key = (String)entry.getKey();
      final String value = (String)entry.getValue();

      if( key.startsWith( "protocol" ) )
      {
        final String[] strings = value.split( "=" );
        replaceProps.put( strings[0] + ":", strings[1] );
      }
    }

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    for( Iterator iter = properties.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();

      final String key = (String)entry.getKey();
      final String value = (String)entry.getValue();

      if( key.startsWith( "file" ) )
      {
        try
        {
          final IFile file = root.getFile( new Path( value ) );
          replaceFilecontent( file, replaceProps );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          
          throw new TransformationException( e );
        }
      }
    }
  }

  private void replaceFilecontent( final IFile file, final Properties replaceProperties )
      throws FileNotFoundException, IOException, UnsupportedEncodingException, CoreException
  {
    final InputStreamReader r = new InputStreamReader( file.getContents(), file.getCharset() );
    final String string = ReaderUtilities.readStringFromReader( r );
    final String newContent = StringUtilities.replaceAll( string, replaceProperties );

    final PipedOutputStream pos = new PipedOutputStream();
    final PipedInputStream pis = new PipedInputStream( pos );

    final Thread writeThread = new Thread()
    {
      /**
       * @see java.lang.Thread#run()
       */
      public void run()
      {
        try
        {
          final ByteArrayInputStream bis = new ByteArrayInputStream( newContent.getBytes( file
              .getCharset() ) );
          StreamUtilities.streamCopy( bis, pos );
        }
        catch( final Exception e )
        {
          try
          {
            pos.close();
          }
          catch( IOException e1 )
          {
            e1.printStackTrace();
          }
        }

      }
    };
    writeThread.start();

    file.setContents( pis, false, true, new NullProgressMonitor() );
  }

}