package org.kalypso.ogc.sensor.loaders;

import java.io.PipedInputStream;
import java.io.Writer;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.ObservationType;

/**
 * A specific loader for ZML-Files. Loads <code>ZmlObservation</code> objects.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  private final UrlResolver m_urlResolver;

  public ZmlLoader()
  {
    m_urlResolver = new UrlResolver();
  
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, URL context,
      IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final URL url = m_urlResolver.resolveURL( context, source );
      
      monitor.beginTask( "Zml laden aus: " + url, IProgressMonitor.UNKNOWN );

      final IObservation obs = ZmlFactory.parseXML( url, url.getFile() );

      // add resource in order to get aware of changes made by tier on it
      IFile file = ResourceUtilities.findFileFromURL( url );
      if( file != null )
        addResource( file, obs );

      return obs;
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new LoaderException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final String source, URL context, IProgressMonitor monitor,
      Object data ) throws LoaderException
  {
    PipedInputStream pis = null;

    try
    {
      final URL url = m_urlResolver.resolveURL( context, source );
      
      monitor.beginTask( "ZML speichern in: " + url, IProgressMonitor.UNKNOWN );

      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file == null )
        throw new IllegalArgumentException(
            "Datei könnte nicht gefunden werden: " + url );

      final ObservationType xmlObs = ZmlFactory.createXML( (IObservation) data,
          null );

      final SetContentThread thread = new SetContentThread( file, !file
          .exists(), false, true, new NullProgressMonitor() )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          ZmlFactory.getMarshaller().marshal( xmlObs, writer );
        }
      };
      thread.start();
      thread.join();

      final CoreException fileException = thread.getFileException();
      if( fileException != null )
        throw fileException;

      final Throwable thrown = thread.getThrown();
      if( thrown != null )
        throw thrown;

      //        
      //        final PipedOutputStream pos = new PipedOutputStream();
      //        pis = new PipedInputStream( pos );
      //
      //        final Runnable runnable = new Runnable()
      //        {
      //          public void run( )
      //          {
      //            try
      //            {
      //              final OutputStreamWriter osw = new OutputStreamWriter( pos, file
      //                  .getCharset() );
      //
      //              ZmlFactory.getMarshaller().marshal( xmlObs, osw );
      //            }
      //            catch( final Exception e )
      //            {
      //              e.printStackTrace();
      //            }
      //            finally
      //            {
      //              IOUtils.closeQuietly( pos );
      //            }
      //          }
      //        };
      //
      //        final Thread thread = new Thread( runnable, "ZML Save Thread" );
      //        thread.start();
      //
      //        file.setContents( pis, false, true, monitor );
    }
    catch( Throwable e ) // generic exception caught for simplicity
    {
      throw new LoaderException( e );
    }
    finally
    {
      monitor.done();

      IOUtils.closeQuietly( pis );
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription( )
  {
    return "ZML";
  }
}