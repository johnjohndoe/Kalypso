package org.kalypso.loader.impl;

import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.JMSchema;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.xml.XMLTools;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author schlienger
 *  
 */
public final class GMLArrayLoader extends AbstractLoader implements IResourceChangeListener
{
  final AbstractLoaderResourceDeltaVisitor m_visitor = new AbstractLoaderResourceDeltaVisitor( this );

  public GMLArrayLoader()
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
  }

  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected final Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String sourcePath = source.getProperty( "PATH", "" );
      final String schemaPath = source.getProperty( "XSD", "" );

      final IFile file = project.getFile( sourcePath );

      monitor.beginTask( "Features werden geladen...", 4000 );

      final JMSchema schema = getSchema( project, schemaPath,
          new SubProgressMonitor( monitor, 1000 ) );

      final HashMap layerMap = new HashMap();
      final FeatureType[] types = schema.getFeatureTypes();
      final CS_CoordinateSystem layerCrs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();

      for( int i = 0; i < types.length; i++ )
      {
        final FeatureType type = types[i];
        layerMap.put( type, new KalypsoFeatureLayer( type.getName(), type, layerCrs ) );
      }

      final InputStreamReader reader = new InputStreamReader( file.getContents(), file.getCharset() );
      monitor.setTaskName( "Loading gml..." );
      final GMLDocument gml = new GMLDocument_Impl( reader );
      monitor.worked( 1000 );
      reader.close();

      collectFeatures( new SubProgressMonitor( monitor, 1000 ), layerMap, types, layerCrs, gml );
      optimizeLayers( new SubProgressMonitor( monitor, 1000 ), layerMap );

      final Object[] layerArray = layerMap.values().toArray(
          new KalypsoFeatureLayer[layerMap.size()] );

      m_visitor.addResource( file, layerArray );

      return layerArray;
    }

    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }

  private void optimizeLayers( final IProgressMonitor monitor, final HashMap layerMap )
  {
    monitor.beginTask( "Optimiere layer...", layerMap.size()  );
    
    for( final Iterator iter = layerMap.values().iterator(); iter.hasNext(); )
    {
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
      layer.optimize();
      monitor.worked(1);
    }
  }

  private void collectFeatures( final IProgressMonitor monitor, final HashMap layerMap, final FeatureType[] types, final CS_CoordinateSystem layerCrs, final GMLDocument gml ) throws Exception
  {
    final GMLFeatureCollection gmlFC = gml.getRoot();
    GMLFeature[] gmlFeatures = gmlFC.getFeatures();
    //      final int max= gmlFeatures.length < 20 ? gmlFeatures.length:20;
    final int max = gmlFeatures.length;
    if( max < gmlFeatures.length ) // TODO
      System.out.println( "WARNUNG es werden nur " + max + " von " + gmlFeatures.length
          + " Features geladen" );

    monitor.beginTask( "Lade fwatures..." , max );
    
    for( int i = 0; i < max; i++ )
    {
      final Feature feature = FeatureFactory.createFeature( gmlFeatures[i], types );
      GMLHelper.checkCrs( feature, layerCrs );

      final KalypsoFeatureLayer fl = (KalypsoFeatureLayer)layerMap.get( feature.getFeatureType() );
      fl.addFeature( new KalypsoFeature( feature ) );

      monitor.worked( i );
    }
  }

  private JMSchema getSchema( final IProject project, final String schemaPath,
      final IProgressMonitor monitor ) throws Exception, CoreException
  {
    monitor.beginTask( "Schema wird geladen...", 1000 );
    final JMSchema schema = new JMSchema( XMLTools.getAsDOM( project.getFile( schemaPath )
        .getContents() ) );

    monitor.worked( 1000 );

    return schema;
  }

  /**
   * @see org.kalypso.loader.ILoader#release(java.lang.Object)
   */
  public void release( Object object )
  {
    super.release( object );

    m_visitor.releaseResource( object );
  }

  /**
   * 
   * @see org.kalypso.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
   */
  public void save( final Properties source, final Object data ) throws LoaderException
  {
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    if( event.getType() == IResourceChangeEvent.POST_CHANGE )
    {
      final IResourceDelta delta = event.getDelta();
      try
      {
        delta.accept( m_visitor );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }
  }
}