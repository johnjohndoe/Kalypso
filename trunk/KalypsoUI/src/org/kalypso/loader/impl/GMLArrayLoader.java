package org.kalypso.loader.impl;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLException;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
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
import org.w3c.dom.Document;

/**
 * @author schlienger
 *  
 */
public final class GMLArrayLoader extends AbstractLoader
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
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected final Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String sourcePath = source.getProperty( "PATH", "" );

      final IFile file = project.getFile( sourcePath );

      monitor.beginTask( "Features werden geladen...", 4000 );

      final InputStreamReader reader = new InputStreamReader( file.getContents(), file.getCharset() );
      monitor.setTaskName( "Loading gml..." );
      final GMLDocument gml = new GMLDocument_Impl( reader );
      monitor.worked( 1000 );
      reader.close();

      // load schema
      final String schemaPath = source.getProperty( "XSD", "" );
      final IFile schemaFile = project.getFile( schemaPath );
      final JMSchema schema = getSchema( schemaFile, new SubProgressMonitor( monitor, 1000 ) );

      final HashMap layerMap = new HashMap();
      final FeatureType[] types = schema.getFeatureTypes();
      final CS_CoordinateSystem layerCrs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();

      for( int i = 0; i < types.length; i++ )
      {
        final FeatureType type = types[i];
        layerMap.put( type, new KalypsoFeatureLayer( type.getName(), type, layerCrs ) );
      }

      collectFeatures( new SubProgressMonitor( monitor, 1000 ), layerMap, types, layerCrs, gml );
      optimizeLayers( new SubProgressMonitor( monitor, 1000 ), layerMap );

      final Object[] layerArray = layerMap.values().toArray(
          new KalypsoFeatureLayer[layerMap.size()] );

      // egal, ob GML oder Schema geändert werden, das LayerArray ist
      // dann nicht mehr gültig
      addResource( schemaFile, layerArray );
      addResource( file, layerArray );

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
    monitor.beginTask( "Optimiere Layer...", layerMap.size() );

    for( final Iterator iter = layerMap.values().iterator(); iter.hasNext(); )
    {
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
      layer.optimize();
      monitor.worked( 1 );
    }
  }

  private void collectFeatures( final IProgressMonitor monitor, final HashMap layerMap,
      final FeatureType[] types, final CS_CoordinateSystem layerCrs, final GMLDocument gml )
      throws Exception
  {
    final GMLFeatureCollection gmlFC = gml.getRoot();
    final GMLFeature[] gmlFeatures = gmlFC.getFeatures();
    //      final int max= gmlFeatures.length < 20 ? gmlFeatures.length:20;
    final int max = gmlFeatures.length;
    if( max < gmlFeatures.length ) // TODO
      System.out.println( "WARNUNG es werden nur " + max + " von " + gmlFeatures.length
          + " Features geladen" );

    monitor.beginTask( "Lade Geometrien...", max );

    for( int i = 0; i < max; i++ )
    {
      final Feature feature = FeatureFactory.createFeature( gmlFeatures[i], types );
      GMLHelper.checkCrs( feature, layerCrs );

      final KalypsoFeatureLayer fl = (KalypsoFeatureLayer)layerMap.get( feature.getFeatureType() );
      fl.addFeature( new KalypsoFeature( feature ) );

      monitor.worked( i );
    }
  }

  private JMSchema getSchema( final IFile schemaFile, final IProgressMonitor monitor )
      throws Exception, CoreException
  {
    monitor.beginTask( "Schema wird geladen...", 1000 );
    final JMSchema schema = new JMSchema( XMLTools.getAsDOM( schemaFile.getContents() ) );

    monitor.worked( 1000 );

    return schema;
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final IProject project,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    monitor.beginTask( "GML wird geschrieben", 2000 );

    final KalypsoFeatureLayer[] layers = (KalypsoFeatureLayer[])data;

    try
    {
      final GMLDocument gmlDoc = createGmlDOM( layers, new SubProgressMonitor( monitor, 1000 ) );

      // GML Schreiben

      final String sourcePath = source.getProperty( "PATH", "" );
      final IFile file = project.getFile( sourcePath );

      writeDOMToFile( gmlDoc, file, new SubProgressMonitor( monitor, 1000 ) );
    }
    catch( final Exception e )
    {
      throw new LoaderException( "Fehler beim Schriben der GML-Datei", e );
    }
  }

  private void writeDOMToFile( final GMLDocument gmlDoc, final IFile file,
      final SubProgressMonitor monitor )
  {
    //    monitor.beginTask( "GML-Datei wird geschrieben", 1000 );

    final Document xmlDOM = gmlDoc.getDocument();

    try
    {
      final StringWriter sw = new StringWriter();

      final PipedOutputStream pos = new PipedOutputStream();
      final PipedInputStream pis = new PipedInputStream( pos );

      final Runnable runnable = new Runnable()
      {
        public void run()
        {
          try
          {
            final Transformer t = TransformerFactory.newInstance().newTransformer();
            t.transform( new DOMSource( xmlDOM ), new StreamResult( pos ) );

          }
          catch( final TransformerConfigurationException e )
          {
            e.printStackTrace();
          }
          catch( TransformerFactoryConfigurationError e )
          {
            e.printStackTrace();
          }
          catch( final TransformerException e )
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
      
      final Thread thread = new Thread( runnable, "GML Save Thread" );
      thread.start();

      file.setContents( pis, false, true, monitor );
      pis.close();

      System.out.println( sw.toString() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  private GMLDocument createGmlDOM( final KalypsoFeatureLayer[] layers,
      final IProgressMonitor monitor ) throws GMLException
  {
    int featureCount = 0;
    for( int i = 0; i < layers.length; i++ )
      featureCount += layers[i].getSize();

    monitor.beginTask( "Features werden konvertiert", featureCount );

    final GMLFeatureCollection gmlCollection = GMLFactory.createGMLFeatureCollection( "collection" );
    final GMLDocument gmlDoc = new GMLDocument_Impl();

    GM_Envelope boundingBox = null;
    for( int i = 0; i < layers.length; i++ )
    {
      final KalypsoFeatureLayer layer = layers[i];

      if( boundingBox == null )
        boundingBox = layer.getBoundingBox();
      else
        boundingBox.merge( layer.getBoundingBox() );

      final KalypsoFeature[] features = layer.getAllFeatures();
      for( int j = 0; j < features.length; j++ )
      {
        GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc.getDocument(), features[j] );
        gmlFeature.setId( features[j].getId() );
        gmlCollection.addFeature( gmlFeature );

        monitor.worked( 1 );
      }
    }

    final double minx = boundingBox.getMin().getX();
    final double maxx = boundingBox.getMax().getX();
    final double miny = boundingBox.getMin().getY();
    final double maxy = boundingBox.getMax().getY();
    gmlCollection.setBoundingBox( minx, miny, maxx, maxy );

    // muss NACH dem adden aller Features und dem setzen der Bounding Box
    // passieren
    gmlDoc.setRoot( gmlCollection );

    monitor.done();

    return gmlDoc;
  }

}