package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.url.UrlResolver;

/**
 * Lädt einen GMLWorkspace aus einem GML
 * 
 * @author Belger
 */
public class GmlLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final Properties source, final URL context,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      monitor.beginTask( "GML laden", 1000 );

      final String schemaPath = source.getProperty( "XSD", "" );
      final URL schemaURL = UrlResolver.resolveURL( context, schemaPath );

      final String gmlPath = source.getProperty( "PATH", "" );
      final URL gmlURL = UrlResolver.resolveURL( context, gmlPath );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );

      final IResource schemaFile = ResourceUtilities.findFileFromURL( schemaURL );
      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );

      if( gmlFile != null )
        addResource( gmlFile, workspace );

      if( schemaFile != null )
        addResource( schemaFile, workspace );

      monitor.done();

      return workspace;
    }
    catch( final LoaderException le )
    {
      le.printStackTrace();

      throw le;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new LoaderException( "Konnte GML nicht laden", e );
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "GML Layer";
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.util.Properties, java.net.URL, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final URL context,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    final String gmlPath = source.getProperty( "PATH", "" );

    try
    {
      final GMLWorkspace workspace = (GMLWorkspace)data;
      final Feature rootFeature = workspace.getRootFeature();

      final URL gmlURL = UrlResolver.resolveURL( context, gmlPath );
  
      // ists im Workspace?
      final IFile file = ResourceUtilities.findFileFromURL( gmlURL );
      if( file != null )
      {
        final SetContentThread thread = new SetContentThread( file, !file.exists(), false, true, new NullProgressMonitor() ) 
        {
          protected void write( final Writer writer ) throws Throwable
          {
            GmlSerializer.serializeFeature( writer, rootFeature, new NullProgressMonitor() );            
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
      }
      else if( file == null && gmlURL.getProtocol().equals( "file" ) )
      {
        final Writer w = new FileWriter( new File( gmlURL.getFile() ) );
        GmlSerializer.serializeFeature( w, rootFeature, monitor );
      }
      else
        throw new LoaderException( "Die URL kann nicht beschrieben werden: " + gmlURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      
      throw new LoaderException( "Der angegebene Pfad ist ungültig: " + gmlPath + "\n" + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
  }
}