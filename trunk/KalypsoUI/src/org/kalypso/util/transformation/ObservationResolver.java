package org.kalypso.util.transformation;

import java.io.Writer;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.eclipse.core.resources.FolderUtilities;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author belger
 */
public class ObservationResolver extends AbstractTransformation
{
  private static final String PROP_SOURCEOBS = "sourceObservation";

  private static final String PROP_TARGETOBS = "targetObservation";

  private static final String PROP_GML = "gml";

  private static final String PROP_FEATURE = "feature";

  private static final String PROP_TARGETFOLDER = "targetFolder";

  private static final String PROP_STARTSIM = "startsim";

  private static final String PROP_ENDSIM = "endsim";

  private static final String PROP_STARTFORECAST = "startforecast";

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
    final String featureName = properties.getProperty( PROP_FEATURE, "" );
    final String sourceObsName = properties.getProperty( PROP_SOURCEOBS, "" );
    final String targetObsName = properties.getProperty( PROP_TARGETOBS, "" );
    final String targetFolderName = properties.getProperty( PROP_TARGETFOLDER, "" );
    final String startsimString = properties.getProperty( PROP_STARTSIM, "" );
    final String endsimString = properties.getProperty( PROP_ENDSIM, "" );
    final String startforecastString = properties.getProperty( PROP_STARTFORECAST, "" );

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

      final URL gmlURL = ResourceUtilities.createURL( gmlFile );

      final UrlResolver resolver = createResolver( project, targetFolder, startsimString, startforecastString, endsimString );
      
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, resolver );
      final FeatureType ft = workspace.getFeatureType( featureName );
      if( ft == null )
        throw new TransformationException( "Featurename unbekannt: " + featureName );

      final Feature[] features = workspace.getFeatures( ft );

      if( monitor.isCanceled() )
        throw new OperationCanceledException();
      
      resolveTimeseries( gmlURL, features, sourceObsName, targetObsName, targetFolder,
          new SubProgressMonitor( monitor, 1000 ) );

      monitor.done();
    }
    catch( final Throwable e )
    {
      throw new TransformationException( e );
    }
  }

  private UrlResolver createResolver( final IProject project, final IFolder calcdir, final String startsim, final String startforecast, final String endsim )
  {
    final UrlResolver resolver = new UrlResolver(  );
    
    resolver.addReplaceToken( "project",  "platform:/resource/" + project.getName() + "/" );
    resolver.addReplaceToken( "calcdir",  "platform:/resource/" + calcdir.getFullPath().toString() + "/" );
    resolver.addReplaceToken( "startsim", startsim );
    resolver.addReplaceToken( "startforecast", startforecast );
    resolver.addReplaceToken( "endsim", endsim );
    
    return resolver;
  }

  /**
   * funktioniert nur, wenn der TimeSeriesLink des Target eine relative URL hat
   * (== relativer Pfad)
   */
  private void resolveTimeseries( final URL baseURL, final Feature[] features, final String sourceName, final String targetName,
      final IFolder targetFolder, final IProgressMonitor monitor ) throws TransformationException
  {
    if( features.length == 0 )
      return;

    final FeatureType featureType = features[0].getFeatureType();
    checkColumn( featureType, sourceName );
    checkColumn( featureType, targetName );

    monitor.beginTask( "Zeitreihen auslesen", features.length * 2 );

    for( int i = 0; i < features.length; i++ )
    {
      if( monitor.isCanceled() )
        throw new OperationCanceledException();
      
      final Feature feature = features[i];

      final TimeseriesLink sourcelink = (TimeseriesLink)feature.getProperty( sourceName );
      final TimeseriesLink targetlink = (TimeseriesLink)feature.getProperty( targetName );
      if( sourcelink == null || targetlink == null )
        continue;

      try
      {
        String sourceref = sourcelink.getHref();

        // TODO... Gernot, this is the way to go
        
        // Date from = new Date();
        // Date to = new Date();
        // sourceref = ZmlURL.insertDateRange( sourceref, new DateRangeArgument( from, to ) );
        
        final URL sourceURL = new UrlResolver().resolveURL( baseURL, sourceref );

        final IObservation obs = ZmlFactory.parseXML( sourceURL, targetName );
        
        // TODO here: merge the two observations that you got from the previous steps
        // NOTE: the order is important: 
        // obs( i ) has a higher priority than obs( i + 1 )
        // with 'i' the index in the observations array...
        
        // final ForecastFilter fc = new ForecastFilter();
        // fc.initFilter( new IObservation[] { obs, obs }, null );
        
        final IFile targetfile = targetFolder.getFile( new Path( targetlink.getHref() ) );
        FolderUtilities.mkdirs( targetfile.getParent() );
        
        final SetContentThread thread = new SetContentThread( targetfile, !targetfile.exists(), false, true,
            new NullProgressMonitor() )
        {
          protected void write( final Writer w ) throws Throwable
          {
            final ObservationType type = ZmlFactory.createXML( obs, null );
            ZmlFactory.getMarshaller().marshal( type, w );
          }
        };
        thread.start();
        thread.join();

        final Throwable thrown = thread.getThrown();
        if( thrown != null )
          thrown.printStackTrace();
        
        final CoreException fileException = thread.getFileException();
        if( fileException != null )
          fileException.printStackTrace();
        // todo: handle errors?

        monitor.worked( 1 );
      }
      catch( final Throwable e )
      {
        e.printStackTrace();
        // todo: report to user!
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
}