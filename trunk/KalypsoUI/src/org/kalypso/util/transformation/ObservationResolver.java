package org.kalypso.util.transformation;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author belger
 */
public class ObservationResolver extends AbstractTransformation
{

  private static final String PROP_SOURCEOBS = "sourceObservation";

  private static final String PROP_TARGETOBS = "targetObservation";

  private static final String PROP_HREF = "href";

  private static final String PROP_TYPE = "type";

  private static final String PROP_PROJECT = "project";

  private static final String PROP_TARGETFOLDER = "targetFolder";

  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected void transformIntern( final Properties properties, final IProgressMonitor monitor )
      throws TransformationException
  {
    monitor.beginTask( "Zeitreihen auflösen", 3000 );

    // PROPS parsen
    final String projectName = properties.getProperty( PROP_PROJECT, "" );
    final String type = properties.getProperty( PROP_TYPE, "" );
    final String href = properties.getProperty( PROP_HREF, "" );
    final String sourceObsName = properties.getProperty( PROP_SOURCEOBS, "" );
    final String targetObsName = properties.getProperty( PROP_TARGETOBS, "" );
    final String targetFolderName = properties.getProperty( PROP_TARGETFOLDER, "" );

    try
    {
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IFolder targetFolder = root.getFolder( new Path( targetFolderName ) );
      if( !targetFolder.exists() )
        targetFolder.create( false, true, new SubProgressMonitor( monitor, 1000 ) );

      final IProject project = root.getProject( projectName );

      final Properties sourceProps = PropertiesHelper.parseFromString( href, '#' );

      final Properties replaceProperties = new Properties();
      replaceProperties.setProperty( project.getFullPath().toString(), "" );
      final Properties newSourceProps = PropertiesHelper.replaceValues( sourceProps,
          replaceProperties );

      final ILoaderFactory loaderFactory = KalypsoGisPlugin.getDefault().getLoaderFactory(
          KalypsoFeatureLayer.class );
      final ILoader loaderInstance = loaderFactory.getLoaderInstance( type );

      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)loaderInstance.load( newSourceProps,
          project, new SubProgressMonitor( monitor, 1000 ) );
      loaderInstance.release( layer );

      resolveTimeseries( project, layer, sourceObsName, targetObsName, targetFolder,
          new SubProgressMonitor( monitor, 1000 ) );

      // GML wieder speichern
      loaderInstance.save( newSourceProps, project, monitor, layer );
    }
    catch( final Exception e )
    {
      throw new TransformationException( e );
    }
  }

  private void resolveTimeseries( final IProject project, final KalypsoFeatureLayer layer,
      final String sourceName, final String targetName, final IFolder targetFolder,
      final IProgressMonitor monitor ) throws TransformationException
  {
    checkColumn( layer, sourceName );
    checkColumn( layer, targetName );

    final String zmlPrefix = layer.getName() + "-" + sourceName + "-";

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );
    final ObjectFactory factory = new ObjectFactory();

    final KalypsoFeature[] allFeatures = layer.getAllFeatures();

    monitor.beginTask( "Zeitreihen auslösen", allFeatures.length * 2 );

    for( int i = 0; i < allFeatures.length; i++ )
    {
      final KalypsoFeature feature = allFeatures[i];

      final TimeseriesLink obslink = (TimeseriesLink)feature.getProperty( sourceName );
      if( obslink == null )
        continue;

      final IPoolableObjectType key = new PoolableObjectType( obslink.getLinktype(), obslink
          .getHref(), project );

      try
      {
        final IObservation obs = (IObservation)pool.getObject( key, monitor );

        final IFile newZmlFile = targetFolder.getFile( zmlPrefix + i + ".zml" );
        writeTimeserieToFile( newZmlFile, obs );

        // write property
        final Properties newSourceProps = new Properties();
        newSourceProps.setProperty( "TYPE", "relative" );
        newSourceProps.setProperty( "LOCATION", newZmlFile.getProjectRelativePath().toString() );

        final TimeseriesLink newLink = factory.createTimeseriesLink();
        newLink.setLinktype( "zml" );
        newLink.setActuate( "onRequest" );
        newLink.setType( "simple" );
        newLink.setHref( PropertiesHelper.format( newSourceProps, '#' ) );
        newLink.setTimeaxis( obslink.getTimeaxis() );
        newLink.setValueaxis( obslink.getValueaxis() );

        feature.setProperty( FeatureFactory.createFeatureProperty( targetName, newLink ) );

        monitor.worked( 1 );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

  }

  private void checkColumn( final KalypsoFeatureLayer layer, final String sourceName )
      throws TransformationException
  {
    final String linkclassname = TimeseriesLink.class.getName();

    final FeatureTypeProperty sourceFTP = layer.getFeatureType().getProperty( sourceName );
    if( sourceFTP == null || !sourceFTP.getType().equals( linkclassname ) )
      throw new TransformationException( "Spalte existiert nicht oder ist nicht vom Typ "
          + linkclassname + ": " + sourceName );
  }

  private void writeTimeserieToFile( final IFile newZmlFile, final IObservation obs ) throws CoreException, FactoryException, IOException
  {
    final ObservationType observationType = ZmlFactory.createXML( obs );

    final PipedOutputStream pos = new PipedOutputStream();
    final PipedInputStream pis = new PipedInputStream( pos );

    final Thread writeThread = new Thread( "Output-Stream-Thread" )
    {
      /**
       * @see java.lang.Thread#run()
       */
      public void run()
      {
        try
        {
          ZmlFactory.getMarshaller().marshal( observationType, pos );
        }
        catch( JAXBException e )
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
    writeThread.start();

    newZmlFile.create( pis, false, new NullProgressMonitor() );
  }
}