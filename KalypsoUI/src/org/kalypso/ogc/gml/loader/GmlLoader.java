package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
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
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.url.UrlResolver;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Lädt ein RootedFeature aus einem GML
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
    final String featureName = source.getProperty( "LAYER", null );
    if( featureName == null )
      throw new LoaderException( "Must specify 'LAYER' in source" );

    try
    {
      monitor.beginTask( "GML laden", 1000 );

      final String schemaPath = source.getProperty( "XSD", "" );
      final URL schemaURL = UrlResolver.resolveURL( context, schemaPath );

      final String gmlPath = source.getProperty( "PATH", "" );
      final URL gmlURL = UrlResolver.resolveURL( context, gmlPath );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );

      final FeatureType ft = GMLHelper.getFeatureType( workspace, featureName );
      if( ft == null )
        throw new LoaderException( "Feature-type not found: " + featureName );

      final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();

      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( ft.getName(), ft, crs, workspace );
      // TODO: das folgende könnte eigentlich der Konstruktor vom Layer machen
      final Feature[] features = workspace.getFeatures( ft );
      for( int j = 0; j < features.length; j++ )
      {
        Feature feature = features[j];
        GMLHelper.checkCrs( feature, crs );
        layer.addFeature( feature );
      }
      layer.optimize();
      // TODO: bis hierher

      final IResource schemaFile = ResourceUtilities.findFileFromURL( schemaURL );
      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );

      if( gmlFile != null )
        addResource( gmlFile, workspace );

      if( schemaFile != null )
        addResource( schemaFile, workspace );

      monitor.done();

      return layer;
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
   * @throws LoaderException
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final URL context,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    final String gmlPath = source.getProperty( "PATH", "" );

    try
    {
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)data;
      final Feature rootFeature = layer.getWorkspace().getRootFeature();

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