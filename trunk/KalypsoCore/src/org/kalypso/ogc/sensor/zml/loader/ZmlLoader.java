package org.kalypso.ogc.sensor.zml.loader;

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
import org.kalypso.java.net.test.UrlUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;

/**
 * A specific loader for ZML-Files. Loads <code>ZmlObservation</code> objects.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    final String location = source.getProperty( "LOCATION" );

    monitor.beginTask( "Laden von ZML-Datei von " + location, 1 );

    URL url = null; 

    try
    {
      url = UrlUtilities.resolveURL( context, location );
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

    try
    {
      final IObservation obs = ZmlFactory.parseXML( url, url.getFile() );

      monitor.worked( 1 );

      return obs;
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( Properties source, IProject project, IProgressMonitor monitor, Object data )
      throws LoaderException
  {
    PipedInputStream pis = null;

    try
    {
      final String location = source.getProperty( "LOCATION" );

      monitor.beginTask( "ZML Speichern: " + location, 2 );

      final IFile file = project.getFile( location );

      final ObservationType xmlObs = ZmlFactory.createXML( (IObservation)data, null );

      monitor.worked( 1 );

      final PipedOutputStream pos = new PipedOutputStream();
      pis = new PipedInputStream( pos );

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

      monitor.worked( 1 );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new LoaderException( e );
    }
    finally
    {
      if( pis != null )
        try
        {
          pis.close();
        }
        catch( IOException e1 )
        {
          e1.printStackTrace();
        }
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