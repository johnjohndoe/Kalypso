package org.kalypso.ogc.sensor.loaders;

import java.io.Writer;
import java.net.URL;

import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
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
      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file != null )
        addResource( file, obs );
      return obs;
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
      // TODO wenn resource geloescht wurde, wird hier ein fehler geworfen
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
    try
    {
      final URL url = m_urlResolver.resolveURL( context, source );
      
      monitor.beginTask( "ZML speichern in: " + url, IProgressMonitor.UNKNOWN );

      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file == null )
        throw new IllegalArgumentException(
            "Datei k�nnte nicht gefunden werden: " + url );

      final ObservationType xmlObs = ZmlFactory.createXML( (IObservation) data,
          null );
      
      // TODO testing, remove
//      final FileWriter wrtr = new FileWriter( new File("c:/temp/test.zml") );
//      ZmlFactory.getMarshaller().marshal( xmlObs, wrtr );
//      wrtr.close();

      // set contents of ZML-file
      final SetContentHelper helper = new SetContentHelper(  )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          System.out.println( "Marschalling started: " + url);
          
          final Marshaller marshaller = ZmlFactory.getMarshaller();
          marshaller.setProperty( Marshaller.JAXB_ENCODING, getCharset() );
          
          marshaller.marshal( xmlObs, writer );
          
          System.out.println( "Marshalling finished." );
        }
      };
      helper.setFileContents(file, false, true, new NullProgressMonitor());
    }
    catch( Throwable e ) // generic exception caught for simplicity
    {
      throw new LoaderException( e );
    }
    finally
    {
      monitor.done();
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