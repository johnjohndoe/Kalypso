package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.zml.ObservationType;

/**
 * A specific loader for ZML-Files. Loads <code>ZmlObservation</code> objects.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, IProject project, IProgressMonitor monitor )
      throws LoaderException
  {
    String type = source.getProperty( "TYPE" );
    String location = source.getProperty( "LOCATION" );

    monitor.beginTask( "Laden von ZML-Datei von " + location, 1 );

    URL url = null;

    try
    {
      if( type.equals( "relative" ) )
        url = project.getFile( location ).getLocation().toFile().toURL();
      else if( type.equals( "absolute" ) )
        url = new URL( location );
      else
        throw new LoaderException( "Href Type is not supported: " + type );
    }
    catch( MalformedURLException e )
    {
      try
      {
        // try with file
        url = new File( location ).toURL();
      }
      catch( MalformedURLException e1 )
      {
        throw new LoaderException( e );
      }
    }

    ZmlObservation obs = new ZmlObservation( location, url );

    monitor.worked( 1 );

    return obs;
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( Properties source, IProject project, IProgressMonitor monitor, Object data )
      throws LoaderException
  {
    try
    {
      final String location = source.getProperty( "LOCATION" );
  
      monitor.beginTask( "ZML Speichern: " + location, 2 );
  
      final IFile file = project.getFile( location );
  
      final ObservationType xmlObs = ZmlFactory.createXML( (IObservation)data );
      
      monitor.worked(1);
      
      final PipedOutputStream pos = new PipedOutputStream();
      final PipedInputStream pis = new PipedInputStream( pos );
      
      final Runnable runnable = new Runnable()
      {
        public void run()
        {
          try
          {
            final OutputStreamWriter osw = new OutputStreamWriter( pos, file.getCharset() );

            ZmlFactory.getMarshaller().marshal( xmlObs, osw );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }
          finally
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
      
      final Thread thread = new Thread( runnable, "ZML Save Thread" );
      thread.start();

      file.setContents( pis, false, true, monitor );
      pis.close(); 
      
      monitor.worked(1);
    }
    catch( Exception e )
    {
      throw new LoaderException(e);
    }
  }
  
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ZML";
  }
}