package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.net.URL;
import java.util.Properties;

import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.visitors.TransformVisitor;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.progress.EclipseProgressMonitor;
import org.kalypso.util.url.UrlResolver;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 *  
 */
public class ShapeLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final Properties source, final URL context, final IProgressMonitor monitor )
      throws LoaderException
  {
    try
    {
      final String sourceLocation = source.getProperty( "PATH", "" );

      IResource shpResource = null;
      IResource dbfResource = null;
      IResource shxResource = null;
      
      final URL sourceURL = UrlResolver.resolveURL( context, sourceLocation );
      final URL shpURL = UrlResolver.resolveURL( context, sourceLocation + ".shp" );
      final URL dbfURL = UrlResolver.resolveURL( context, sourceLocation + ".dbf" );
      final URL shxURL = UrlResolver.resolveURL( context, sourceLocation + ".shx" );
      
      // leider können Shapes nicht aus URL geladen werden -> protocoll checken
      File sourceFile = null;
      final IPath resource = ResourceUtilities.findPathFromURL( sourceURL );
      if( resource != null )
      {
        sourceFile = ResourceUtilities.makeFileFromPath( resource );
        
        shpResource = ResourceUtilities.findFileFromURL( shpURL ); 
        dbfResource = ResourceUtilities.findFileFromURL( dbfURL ); 
        shxResource = ResourceUtilities.findFileFromURL( shxURL ); 
      }
      else
      {
        if( sourceURL.getProtocol().startsWith( "file:" ) )
          sourceFile = new File( sourceURL.getPath() );
      }
      
      if( sourceFile == null )
        throw new LoaderException( "Could not load shape at source: " + sourceLocation );
      

      // Workspace laden
      final String sourceSrs = source.getProperty( "SRS", "" );
      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();

      final CS_CoordinateSystem sourceCrs = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( sourceSrs ) );

      if( sourceCrs == null )
        throw new LoaderException( "Kein Koordinaten-System für Shape gefunden: " + sourceSrs );

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      final GMLWorkspace workspace = ShapeSerializer.deserialize( sourceFile.getAbsolutePath(),
          sourceCrs, new EclipseProgressMonitor( monitor ) );

      try
      {
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }
      
      if( shpResource != null )
        addResource( shpResource, workspace );
      if( dbfResource != null )
        addResource( dbfResource, workspace );
      if( shxResource != null )
        addResource( shxResource, workspace );

      return workspace;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }
}