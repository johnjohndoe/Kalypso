package org.kalypso.util.transformation;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Writer;
import java.net.URL;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
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
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
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
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author belger
 */
public class ObservationResolver extends AbstractTransformation
{

  private static final String PROP_SOURCEOBS = "sourceObservation";

  private static final String PROP_TARGETOBS = "targetObservation";

  private static final String PROP_GML = "gml";
  private static final String PROP_XSD = "xsd";
  private static final String PROP_FEATURE = "feature";

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
    final String gmlPath = properties.getProperty( PROP_GML, "" );
    final String schemaPath = properties.getProperty( PROP_XSD, "" );
    final String featureName = properties.getProperty( PROP_FEATURE, "" );
    final String sourceObsName = properties.getProperty( PROP_SOURCEOBS, "" );
    final String targetObsName = properties.getProperty( PROP_TARGETOBS, "" );
    final String targetFolderName = properties.getProperty( PROP_TARGETFOLDER, "" );

    try
    {
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IFolder targetFolder = root.getFolder( new Path( targetFolderName ) );
      if( !targetFolder.exists() )
        targetFolder.create( false, true, new SubProgressMonitor( monitor, 1000 ) );

      final IProject project = targetFolder.getProject();
      final IFile gmlFile = root.getFile( new Path( gmlPath ) );
      if( gmlFile == null )
        throw new TransformationException( "Datei nicht gefunden: " + gmlPath );
      
      final URL gmlURL = gmlFile.getLocation().toFile().toURL();

      final IFile schemaFile = root.getFile( new Path( schemaPath ) );
      if( schemaFile == null )
        throw new TransformationException( "Datei nicht gefunden: " + schemaPath );
      final URL schemaURL = schemaFile.getLocation().toFile().toURL();

      // TODO: read via replaceToken
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );
      final FeatureType ft = workspace.getSchema().getFeatureType( featureName );
      if( ft == null )
        throw new TransformationException( "Featurename unbekannt: " + featureName );
      
      final Feature[] features = workspace.getFeatures( ft );

      resolveTimeseries( project, features, sourceObsName, targetObsName, targetFolder,
          new SubProgressMonitor( monitor, 1000 ) );

      // GML wieder speichern
      final SetContentThread thread = new SetContentThread( gmlFile, false, false, true, new  NullProgressMonitor() )
      {
        protected void writeStream() throws Throwable
        {
          final Writer writer = new OutputStreamWriter( getOutputStream(), gmlFile.getCharset() );
          GmlSerializer.serializeFeature( writer, workspace.getRootFeature(), new NullProgressMonitor() );
        }
      };
      thread.start();
      thread.join();

      monitor.done(  );
    }
    catch( final Exception e )
    {
      throw new TransformationException( e );
    }
  }

  private void resolveTimeseries( final IProject project, final Feature[] features,
      final String sourceName, final String targetName, final IFolder targetFolder,
      final IProgressMonitor monitor ) throws TransformationException
  {
    if( features.length == 0 )
      return;
    
    final FeatureType featureType = features[0].getFeatureType();
    checkColumn( featureType, sourceName );
    checkColumn( featureType, targetName );

    final String zmlPrefix = featureType.getName() + "-" + sourceName + "-";

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );
    final ObjectFactory factory = new ObjectFactory();

    monitor.beginTask( "Zeitreihen auslesen", features.length * 2 );

    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];

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

        final TimeseriesLinkType newLink = factory.createTimeseriesLinkType();
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

  private void checkColumn( final FeatureType ft, final String sourceName )
      throws TransformationException
  {
    final String linkclassname = TimeseriesLink.class.getName();

    final FeatureTypeProperty sourceFTP = ft.getProperty( sourceName );
    if( sourceFTP == null || !sourceFTP.getType().equals( linkclassname ) )
      throw new TransformationException( "Spalte existiert nicht oder ist nicht vom Typ "
          + linkclassname + ": " + sourceName );
  }

  private void writeTimeserieToFile( final IFile newZmlFile, final IObservation obs ) throws CoreException, FactoryException, IOException
  {
    final ObservationType observationType = ZmlFactory.createXML( obs, null );

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