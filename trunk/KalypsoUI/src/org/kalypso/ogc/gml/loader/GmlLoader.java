package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.visitors.ResortVisitor;
import org.deegree_impl.model.feature.visitors.TransformVisitor;
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
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.url.UrlResolver;
import org.opengis.cs.CS_CoordinateSystem;

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

      final URL schemaURL = getSchemaURL( source, context );
      final URL gmlURL = getGmlURL( source, context );

      final IResource schemaFile = ResourceUtilities.findFileFromURL( schemaURL );
      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
      
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );

      try
      {
        final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
        workspace.accept( new ResortVisitor(), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }
      
      if( gmlFile != null )
        addResource( gmlFile, workspace );

      if( schemaFile != null )
        addResource( schemaFile, workspace );

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
    finally
    {
      monitor.done();
    }
  }

  private URL getGmlURL( Properties source, URL context ) throws MalformedURLException
  {
    final String gmlPath = source.getProperty( "PATH", "" );
    return UrlResolver.resolveURL( context, gmlPath );
  }

  private URL getSchemaURL( final Properties source, final URL context ) throws MalformedURLException
  {
    final String schemaPath = source.getProperty( "XSD", "" );
    return UrlResolver.resolveURL( context, schemaPath );
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
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace)data;
      final Feature rootFeature = workspace.getRootFeature();

      final URL gmlURL = getGmlURL( source, context );
  
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
      
      throw new LoaderException( "Der angegebene Pfad ist ungültig: " + source.getProperty( "PATH" ) + "\n" + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#compareKeys(java.util.Properties, java.net.URL, java.util.Properties, java.net.URL)
   */
  public int compareKeys( final Properties source1, final URL context1, final Properties source2, final URL context2 )
  {
    try
    {
      final int schemaHash1 = getSchemaURL( source1, context1 ).hashCode();
      final int schemaHash2 = getSchemaURL( source2, context2 ).hashCode();
      
      if( schemaHash1 != schemaHash2 )
        return schemaHash1 - schemaHash2;

      final int gmlHash1 = getGmlURL( source1, context1 ).hashCode();
      final int gmlHash2 = getGmlURL( source2, context2 ).hashCode();
      
      return gmlHash1 - gmlHash2;
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    
    return 0;
  }
}