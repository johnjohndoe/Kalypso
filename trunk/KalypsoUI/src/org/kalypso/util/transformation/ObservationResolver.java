package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.Mapper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.eclipse.core.resources.FolderUtilities;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoProcolWriter;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * Diese Transformation f�hrt das 'Resolven' der Zeitreihen durch. Dies
 * passiert, indem alle Features einer Feature-List der Reihe nach durchlaufen
 * werden, und f�r jedes Feature zwei Zeitreihen vom Server geholt und als eine
 * gemergte Zeitreihe lokal abgelegt wird.
 * 
 * @author belger
 */
public class ObservationResolver extends AbstractTransformation
{
  private static final String PROP_SOURCEOBS1 = "sourceObservation1";

  private static final String PROP_SOURCEOBS2 = "sourceObservation2";

  private static final String PROP_RANGEMODE1 = "rangeMode1";

  private static final String PROP_RANGEMODE2 = "rangeMode2";

  private static final String PROP_TARGETOBS = "targetObservation";

  private static final String PROP_GML = "gml";

  private static final String PROP_FEATURE = "feature";

  private static final String PROP_TARGETFOLDER = "targetFolder";

  private static final String PROP_STARTSIM = "startsim";

  private static final String PROP_ENDSIM = "endsim";

  private static final String PROP_STARTFORECAST = "startforecast";

  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties,
   *      java.io.BufferedWriter, java.io.BufferedWriter,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected void transformIntern( final Properties properties,
      final BufferedWriter msgWriter, final BufferedWriter logWriter,
      final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "Zeitreihen aufl�sen", 3000 );

    // PROPS parsen
    final String gmlPath = properties.getProperty( PROP_GML, "" );
    final String featureName = properties.getProperty( PROP_FEATURE, "" );
    final String sourceObsName1 = properties.getProperty( PROP_SOURCEOBS1, "" );
    final String sourceObsName2 = properties
        .getProperty( PROP_SOURCEOBS2, null );
    final String targetObsName = properties.getProperty( PROP_TARGETOBS, "" );
    final String targetFolderName = properties.getProperty( PROP_TARGETFOLDER,
        "" );

    final String startsimString = properties.getProperty( PROP_STARTSIM, "" );
    final String endsimString = properties.getProperty( PROP_ENDSIM, "" );
    final String startforecastString = properties.getProperty(
        PROP_STARTFORECAST, "" );

    final String rangeMode1 = properties.getProperty( PROP_RANGEMODE1,
        "start-middle" );
    final String rangeMode2 = properties.getProperty( PROP_RANGEMODE2,
        "middle-stop" );

    try
    {
      final Date start = (Date) Mapper.mapXMLValueToJava( startsimString,
          "java.util.Date" );
      final Date middle = (Date) Mapper.mapXMLValueToJava( startforecastString,
          "java.util.Date" );
      final Date stop = (Date) Mapper.mapXMLValueToJava( endsimString,
          "java.util.Date" );

      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IFolder targetFolder = root
          .getFolder( new Path( targetFolderName ) );
      if( !targetFolder.exists() )
        targetFolder.create( false, true,
            new SubProgressMonitor( monitor, 1000 ) );

      final IProject project = targetFolder.getProject();
      final IFile gmlFile = root.getFile( new Path( gmlPath ) );
      if( gmlFile == null )
        throw new TransformationException( "Datei nicht gefunden: " + gmlPath );

      final URL gmlURL = ResourceUtilities.createURL( gmlFile );

      final UrlResolver resolver = createResolver( project, targetFolder,
          startsimString, startforecastString, endsimString );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL,
          resolver );
      final FeatureType ft = workspace.getFeatureType( featureName );
      if( ft == null )
        throw new TransformationException( "Featurename unbekannt: "
            + featureName );

      final Feature[] features = workspace.getFeatures( ft );

      if( monitor.isCanceled() )
        throw new OperationCanceledException();

      resolveTimeseries( gmlURL, features, sourceObsName1, sourceObsName2,
          targetObsName, targetFolder, start, middle, stop, rangeMode1,
          rangeMode2, new SubProgressMonitor( monitor, 1000 ), msgWriter,
          logWriter );

      monitor.done();
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      throw new TransformationException( e );
    }
  }

  private UrlResolver createResolver( final IProject project,
      final IFolder calcdir, final String startsim, final String startforecast,
      final String endsim )
  {
    final UrlResolver resolver = new UrlResolver();

    resolver.addReplaceToken( "project", "platform:/resource/"
        + project.getName() + "/" );
    resolver.addReplaceToken( "calcdir", "platform:/resource/"
        + calcdir.getFullPath().toString() + "/" );
    resolver.addReplaceToken( "startsim", startsim );
    resolver.addReplaceToken( "startforecast", startforecast );
    resolver.addReplaceToken( "endsim", endsim );

    return resolver;
  }

  /**
   * funktioniert nur, wenn der TimeSeriesLink des Target eine relative URL hat
   * (== relativer Pfad)
   * 
   * @param baseURL
   * @param features
   * @param sourceName1
   * @param sourceName2
   * @param targetName
   * @param targetFolder
   * @param start
   * @param middle
   * @param stop
   * @param rangeMode1
   * @param rangeMode2
   * @param monitor
   * @param msgWriter
   * @param logWriter
   * 
   * @throws TransformationException
   * @throws SensorException
   * @throws MalformedURLException
   */
  private void resolveTimeseries( final URL baseURL, final Feature[] features,
      final String sourceName1, final String sourceName2,
      final String targetName, final IFolder targetFolder, final Date start,
      final Date middle, final Date stop, final String rangeMode1,
      final String rangeMode2, final IProgressMonitor monitor,
      final BufferedWriter msgWriter, final BufferedWriter logWriter )
      throws TransformationException, MalformedURLException, SensorException
  {
    if( features.length == 0 )
      return;

    final FeatureType featureType = features[0].getFeatureType();
    checkColumn( featureType, sourceName1 );
    if( sourceName2 != null )
      checkColumn( featureType, sourceName2 );
    checkColumn( featureType, targetName );

    monitor.beginTask( "Zeitreihen auslesen", features.length * 2 );

    // parse range modi
    // TODO: input validation should be done at the gui level
    // since the user-range from-to for the vorhersage must have 
    // valid time steps.
    final Date from1 = parseRange( start, middle, stop, rangeMode1, false,
        start );
    final Date to1 = parseRange( start, middle, stop, rangeMode1, true, middle );
    final Date from2 = parseRange( start, middle, stop, rangeMode2, false,
        middle );
    final Date to2 = parseRange( start, middle, stop, rangeMode2, true, stop );

    for( int i = 0; i < features.length; i++ )
    {
      if( monitor.isCanceled() )
        throw new OperationCanceledException();

      final Feature feature = features[i];

      final IObservation obs1 = getObservation( feature, sourceName1, from1,
          to1, baseURL );
      final IObservation obs2 = getObservation( feature, sourceName2, from2,
          to2, baseURL );

      final TimeseriesLink targetlink = (TimeseriesLink) feature
          .getProperty( targetName );
      if( obs1 == null || targetlink == null )
        continue;

      try
      {
        final IObservation obs;

        if( obs2 == null )
        {
          // No need for a ForecastFilter since obs2 is null

          obs = obs1;
        }
        else
        {
          // NOTE for ForecastFilter: the order is important:
          // obs( i ) has a higher priority than obs( i + 1 )
          // with 'i' the index in the observations array...

          final ForecastFilter fc = new ForecastFilter();
          fc.initFilter( new IObservation[] { obs1, obs2 }, obs1 );
          obs = fc;
        }

        // set forecast metadata, might be used in diagram for instance
        // to mark the forecast range
        TimeserieUtils.setForecast( obs, from2, to2 );

        // protocol the observations here and inform the user
        KalypsoProcolWriter.analyseValues( obs, obs.getValues( null ),
            msgWriter, logWriter );

        // remove query part if present, href is also used as file name here!
        final String href = ZmlURL.getIdentifierPart( targetlink.getHref() );

        final IFile targetfile = targetFolder.getFile( new Path( href ) );
        FolderUtilities.mkdirs( targetfile.getParent() );

        final SetContentHelper thread = new SetContentHelper()
        {
          protected void write( final Writer w ) throws Throwable
          {
            final ObservationType type = ZmlFactory.createXML( obs, null );
            ZmlFactory.getMarshaller().marshal( type, w );
          }
        };
        thread.setFileContents( targetfile, false, true,
            new NullProgressMonitor() );

        monitor.worked( 1 );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        // TODO report to user!
      }
    }
  }

  private Date parseRange( final Date start, final Date middle,
      final Date stop, String rangeMode1, boolean firstOrLast,
      final Date standard )
  {
    final String[] strings = rangeMode1.split( "-" );
    if( strings == null || strings.length != 2 )
      return standard;

    if( !firstOrLast )
      return mapRange( strings[0], start, middle, stop, standard );

    return mapRange( strings[1], start, middle, stop, standard );
  }

  private Date mapRange( final String string, final Date start,
      final Date middle, final Date stop, final Date standard )
  {
    if( "start".equals( string ) )
      return start;

    if( "middle".equals( string ) )
      return middle;

    if( "stop".equals( string ) )
      return stop;

    return standard;
  }

  private IObservation getObservation( final Feature feature,
      final String sourceProperty, final Date from, final Date to,
      final URL baseURL ) throws MalformedURLException, SensorException
  {
    if( sourceProperty == null )
      return null;
    final TimeseriesLink sourcelink = (TimeseriesLink) feature
        .getProperty( sourceProperty );
    if( sourcelink == null ) // keine Zeitreihe verlink, z.B. kein Pegel am
      // Knoten in KalypsoNA
      return null;
    final String sourceref = ZmlURL.insertDateRange( sourcelink.getHref(),
        new DateRangeArgument( from, to ) );

    final URL sourceURL = new UrlResolver().resolveURL( baseURL, sourceref );

    return ZmlFactory.parseXML( sourceURL, feature.getId() );
  }

  private void checkColumn( final FeatureType ft, final String sourceName )
      throws TransformationException
  {
    final String linkclassname = TimeseriesLink.class.getName();

    final FeatureTypeProperty sourceFTP = ft.getProperty( sourceName );
    if( sourceFTP == null || !sourceFTP.getType().equals( linkclassname ) )
      throw new TransformationException(
          "Spalte existiert nicht oder ist nicht vom Typ " + linkclassname
              + ": " + sourceName );
  }
}