package org.kalypso.loader;

import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.graphics.FeatureLayer;
import org.deegree.graphics.Layer;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.JMSchema;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.loader.ILoader;
import org.kalypso.util.loader.LoaderException;
import org.kalypso.util.xml.XMLTools;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author schlienger
 *  
 */
public class GMLArrayLoader implements ILoader
{
  private Properties m_source;

  /**
   * @see org.kalypso.util.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#load(java.util.Properties,
   *      java.lang.Object)
   */
  public Object load( final Properties source, final Object helper ) throws LoaderException
  {
    try
    {
      final IProject project = (IProject)helper;

      final String sourcePath = source.getProperty( "PATH", "" );
      final String schemaPath = source.getProperty( "XSD", "" );

      final JMSchema schema = new JMSchema( XMLTools.getAsDOM( project.getFile( schemaPath )
          .getContents() ) );

      final IFile file = project.getFile( sourcePath );

      final HashMap layerMap = new HashMap();
      final FeatureType[] types = schema.getFeatureTypes();
      final CS_CoordinateSystem layerCrs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();

      for( int i = 0; i < types.length; i++ )
      {
        FeatureType type = types[i];
        layerMap.put( type, new KalypsoFeatureLayer( type.getName(), type, layerCrs ) );
      }

      final InputStreamReader reader = new InputStreamReader( file.getContents(), file.getCharset() );
      GMLDocument gml = new GMLDocument_Impl( reader );
      reader.close();

      GMLFeatureCollection gmlFC = gml.getRoot();
      GMLFeature[] gmlFeatures = gmlFC.getFeatures();
      //      final int max= gmlFeatures.length < 20 ? gmlFeatures.length:20;
      final int max = gmlFeatures.length;
      if( max < gmlFeatures.length ) // TODO
        System.out.println( "WARNUNG es werden nur " + max + " von " + gmlFeatures.length
            + " Features geladen" );
      for( int i = 0; i < max; i++ )
      {
        if( i % 10 == 0 )
        {
          final double v = ( i * 100 ) / gmlFeatures.length;
          System.out.println( "loaded " + v + "%" );
        }

        final Feature feature = FeatureFactory.createFeature( gmlFeatures[i], types );
        GMLHelper.checkCrs( feature, layerCrs );

        final FeatureLayer fl = (FeatureLayer)layerMap.get( feature.getFeatureType() );
        fl.addFeature( feature );
      }
      for( Iterator iter = layerMap.values().iterator(); iter.hasNext(); )
      {
        final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
        layer.optimize();
      }

      return layerMap.values().toArray( new Layer[layerMap.size()] );
    }

    catch( Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#save(java.util.Properties,
   *      java.lang.Object)
   */
  public void save( final Properties source, final Object data ) throws LoaderException
  {
    // TODO: support it
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * 
   * @see org.kalypso.util.loader.ILoader#setSource(java.util.Properties)
   */
  public void setSource( final Properties source )
  {
    m_source = source;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#getSource()
   */
  public Properties getSource()
  {
    return m_source;
  }

  /**
   * @see org.kalypso.util.loader.ILoader#createControl(java.lang.Object)
   */
  public Object createControl( final Object argument )
  {
    final Composite c = new Composite( (Composite)argument, SWT.NONE );
    return c;
  }

}