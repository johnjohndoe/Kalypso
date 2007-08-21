/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.awt.Color;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * This runnable will be called while running the 2d-exe and will check for new .2d result files.
 * <p>
 * Every new 2d result file we be processed in order to return it to the kalypso client.
 * </p>
 * TODO: - write System.out in simulation-log
 * 
 * @author Gernot Belger
 */
public class ResultManager implements Runnable
{
  private final static MutexRule m_mutex = new MutexRule();

  private static final FilenameFilter FILTER_2D = new PrefixSuffixFilter( "", ".2d" );

  private static final FilenameFilter FILTER_GMT = new PrefixSuffixFilter( "", ".gmt" );

  /**
   * Time step id for steady calculation
   */
  private static final int PSEUDO_STEADY_TIME_STEP_NR = -1;

  private final List<File> m_found2dFiles = new ArrayList<File>();

  private final List<Job> m_resultJobs = new ArrayList<Job>();

  private final File m_outputDir;

  private final File m_inputDir;

  private final Pattern m_resultFilePattern;

  private final ISimulationDataProvider m_dataProvider;

  private final RMA10Calculation m_calculation;

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

  private final ICalcUnitResultMeta m_calcUnitResultMeta;

  /* just for test purposes */
  private final List<ResultType.TYPE> m_parameters = new ArrayList<ResultType.TYPE>();
  {
    m_parameters.add( ResultType.TYPE.DEPTH );
    m_parameters.add( ResultType.TYPE.WATERLEVEL );
    m_parameters.add( ResultType.TYPE.VELOCITY );
  }

  private final IJobChangeListener m_finishListener = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( final IJobChangeEvent event )
    {
      resultJobDone( event );
    }
  };

  private boolean m_init = false;

  public ResultManager( final File inputDir, final File outputDir, final String resultFilePattern, final ISimulationDataProvider dataProvider, final RMA10Calculation calculation, final Date startTime ) throws InvocationTargetException
  {
    m_inputDir = inputDir;
    m_outputDir = outputDir;
    m_dataProvider = dataProvider;
    m_calculation = calculation;
    m_resultFilePattern = Pattern.compile( resultFilePattern + "(\\d+)" );

    /* GMLWorkspace für Ergebnisse anlegen */
    final GMLWorkspace resultMetaWorkspace = FeatureFactory.createGMLWorkspace( ICalcUnitResultMeta.QNAME, null, null );
    m_calcUnitResultMeta = (ICalcUnitResultMeta) resultMetaWorkspace.getRootFeature().getAdapter( ICalcUnitResultMeta.class );

    final ICalculationUnit calculationUnit = calculation.getCalculationUnit();

    m_calcUnitResultMeta.setCalcStartTime( startTime );
    m_calcUnitResultMeta.setCalcUnit( calculationUnit.getGmlID() );
    m_calcUnitResultMeta.setName( calculationUnit.getName() );
    m_calcUnitResultMeta.setDescription( calculationUnit.getDescription() );
    m_calcUnitResultMeta.setPath( new Path( outputDir.getName() ) );
  }

  /**
   * Call this immediately before calculation starts.
   */
  public void calculationAboutToStart( )
  {
    m_init = true;

    /* Filter existing .2d files at that point (filters out model.2d) */
    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    m_found2dFiles.addAll( Arrays.asList( existing2dFiles ) );
  }

  /**
   * This method should be run often during the real calculation (i.e. execution of the RMA10S.exe).
   * <p>
   * Each time, it is checked if new result files are present and if this is the case they are processed.
   * </p>
   * 
   * @see java.lang.Runnable#run()
   */
  public void run( )
  {
    if( !m_init )
      return;

    final File[] existing2dFiles = m_inputDir.listFiles( FILTER_2D );
    for( final File file : existing2dFiles )
    {
      if( !m_found2dFiles.contains( file ) )
        addResultFile( file );
    }
  }

  private void addResultFile( final File file )
  {
    final String resultFileName = FileUtilities.nameWithoutExtension( file.getName() );

    System.out.println( "Found new 2d-file: " + resultFileName );
    // check for 2d files

    // start a job for each unknown 2d file.
    final Matcher matcher = m_resultFilePattern.matcher( resultFileName );
    final String outDirName;
    final int timeStepNum;
    if( matcher.matches() )
    {
      final String countStr = matcher.group( 1 );
      final int count = Integer.parseInt( countStr ) - 1;
      outDirName = "timestep-" + count;
      timeStepNum = count;
    }
    else
    {
      outDirName = resultFileName;
      timeStepNum = PSEUDO_STEADY_TIME_STEP_NR;
    }

    final File resultOutputDir = new File( m_outputDir, outDirName );
    resultOutputDir.mkdirs();
    final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, resultOutputDir, m_dataProvider, m_calculation, m_parameters, timeStepNum, m_calcUnitResultMeta );
    processResultsJob.addJobChangeListener( m_finishListener );

    m_resultJobs.add( processResultsJob );

    /* Schedule job: wait some time in order to make sure file was written to disk. */
    processResultsJob.setRule( m_mutex );
    processResultsJob.schedule( 1000 );

    m_found2dFiles.add( file );
  }

  public Job[] getResultJobs( )
  {
    return m_resultJobs.toArray( new Job[m_resultJobs.size()] );
  }

  /** After calculation, wait until all result process jobs have finished. */
  private IStatus waitForResultProcessing( )
  {
    // TODO: timeout
    for( int i = 0; i < 1000; i++ )
    {
      try
      {
        final IStatus status = jobsFinished();
        if( status != null )
          return status;

        Thread.sleep( 1000 );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    // Timeout reached, produce error status
    return StatusUtilities.createWarningStatus( "Zeitüberschreitung beim Prozessieren der Ergebnisdateien, möglicherweise können nicht lale Ergebnisdaten übertragen werden." );
  }

  /**
   * Check if all jobs have finished. If this is thre case, return a multi status composed of all job-results.
   * <p>
   * If one or more jobs are not finished, returns <code>null</code>.
   * </p>
   */
  private IStatus jobsFinished( )
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), -1, "Ergebnisse der Ergebnisauswertung:", null );
    for( final Job job : m_resultJobs )
    {
      final IStatus jobResult = job.getResult();
      if( jobResult == null )
        return null;

      status.add( jobResult );
    }

    return status;
  }

  protected void resultJobDone( final IJobChangeEvent event )
  {
    final ProcessResultsJob job = (ProcessResultsJob) event.getJob();
    m_minMaxCatcher.addNodeResultMinMaxCatcher( job.getMinMaxData() );
  }

  public void finish( ) throws SimulationException
  {
    if( m_init )
      return;

    /* Process all remaining .2d files. */
    run();

    /* We need to wait until all result process jobs have finished. */
    final IStatus resultProcessingStatus = waitForResultProcessing();
    if( resultProcessingStatus != null )
      System.out.println( resultProcessingStatus );
    // TODO: evaluate status

    /* other, general post-processing stuff. */
    try
    {
      /* Write template sld into result folder */
      final URL resultStyleURL = (URL) m_dataProvider.getInputForID( "ResultTemplate" );
      ZipUtilities.unzip( resultStyleURL, m_outputDir );

      processVectorStyles();
      processTinStyles();

      // TODO: error handling! handle stati everywhere....

      writeResultMeta( resultProcessingStatus );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei der Ergebnisauswertung", e );
    }
  }

  private void writeResultMeta( final IStatus resultStatus ) throws IOException, GmlSerializeException
  {
    m_calcUnitResultMeta.setCalcEndTime( new Date() );
    m_calcUnitResultMeta.setStatus( resultStatus );

    final GMLWorkspace workspace = m_calcUnitResultMeta.getWrappedFeature().getWorkspace();
    final File metaFile = new File( m_outputDir, "resultMeta.gml" );
    GmlSerializer.serializeWorkspace( metaFile, workspace, "UTF-8" );
  }

  private void processVectorStyles( )
  {
    /* Read sld from template */
    final File styleDir = new File( m_outputDir, "Styles" );
    final File vectorStyleFile = new File( styleDir, "vector.sld" );

    final double maxValue = m_minMaxCatcher.getMaxVelocityAbs();

    processVectorStyle( maxValue, vectorStyleFile, styleDir );
  }

  private void processTinStyles( ) throws IOException, XMLParsingException
  {
    /* Read sld from template */
    final File styleDir = new File( m_outputDir, "Styles" );
    final File tinStyleFile = new File( styleDir, "tinStyles.sld" );
    final StyledLayerDescriptor sld = SLDFactory.createSLD( tinStyleFile );

    // make the isolines-SLD for each defined flow parameter
    for( final ResultType.TYPE parameter : m_parameters )
    {
      final double minValue;
      final double maxValue;
      switch( parameter )
      {
        case WATERLEVEL:
          // get min/max-values of the simulation
          minValue = m_minMaxCatcher.getMinWaterlevel();
          maxValue = m_minMaxCatcher.getMaxWaterlevel();

          break;
        case VELOCITY:
          // get min/max-values of the simulation
          minValue = m_minMaxCatcher.getMinVelocityAbs();
          maxValue = m_minMaxCatcher.getMaxVelocityAbs();
          break;
        case DEPTH:
          // get min/max-values of the simulation
          minValue = m_minMaxCatcher.getMinDepth();
          maxValue = m_minMaxCatcher.getMaxDepth();
          break;

        default:
          // take the waterlevel by default
          minValue = m_minMaxCatcher.getMinWaterlevel();
          maxValue = m_minMaxCatcher.getMaxWaterlevel();
          break;
      }

      processTinStyle( "Line", minValue, maxValue, parameter, sld );
      processTinStyle( "Polygon", minValue, maxValue, parameter, sld );
    }

    /* Write SLD back to file */

    final String sldXML = sld.exportAsXML();
    final String sldXMLwithHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + sldXML;
    FileUtils.writeStringToFile( tinStyleFile, sldXMLwithHeader, "UTF-8" );
  }

  private void processVectorStyle( final double maxValue, final File vectorStyleFile, final File styleDir )
  {
    try
    {
      final String string = FileUtils.readFileToString( vectorStyleFile, "UTF-8" );

      // we assume, that the mean distance of mesh nodes is about 30 m, so that the vectors are expanded by an factor
      // which delivers vector lengths of 30 m as maximum.
      final BigDecimal factorValue;
      if( maxValue > 0 )
        factorValue = new BigDecimal( 30 / maxValue ).setScale( 0, BigDecimal.ROUND_CEILING );
      else
        factorValue = new BigDecimal( 100 ).setScale( 0, BigDecimal.ROUND_CEILING );

      final String factor = factorValue.toString();

      final String replacedString = string.replaceAll( "VECTORFACTOR", factor );
      FileUtils.writeStringToFile( new File( styleDir, "vector.sld" ), replacedString, "UTF-8" );
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private void processTinStyle( final String string, final double minValue, final double maxValue, final ResultType.TYPE parameter, final StyledLayerDescriptor sld )
  {
    final String layerName = "tin" + parameter.name() + string + "Style";
    final String featureTypeStyleName = "tin" + parameter.name() + "FeatureTypeStyle";
    final String ruleName = "tin" + parameter.name() + "Rule";

    final NamedLayer namedLayer = sld.getNamedLayer( "tinStyles" );
    if( namedLayer == null )
      return;

    final Style style = namedLayer.getStyle( layerName );
    if( style instanceof UserStyle )
    {
      final UserStyle userStyle = (UserStyle) style;
      final FeatureTypeStyle featureTypeStyle = userStyle.getFeatureTypeStyle( featureTypeStyleName );
      if( featureTypeStyleName == null )
        return;
      final Rule rule = featureTypeStyle.getRule( ruleName );
      if( rule == null )
        return;
      final Symbolizer[] symbolizers = rule.getSymbolizers();
      for( final Symbolizer symbolizer : symbolizers )
      {
        try
        {
          if( symbolizer instanceof SurfaceLineSymbolizer )
          {
            configureLineSymbolizer( (SurfaceLineSymbolizer) symbolizer, minValue, maxValue );
          }
          else if( symbolizer instanceof SurfacePolygonSymbolizer )
          {
            configurePolygonSymbolizer( (SurfacePolygonSymbolizer) symbolizer, minValue, maxValue );
          }
        }
        catch( final FilterEvaluationException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }
    }
  }

  private void configurePolygonSymbolizer( final SurfacePolygonSymbolizer symbolizer, final double minValue, final double maxValue ) throws FilterEvaluationException
  {
    final PolygonColorMap templateColorMap = symbolizer.getColorMap();
    final PolygonColorMap newColorMap = new PolygonColorMap_Impl();

    // retrieve stuff from template-entries
    final PolygonColorMapEntry fromEntry = templateColorMap.findEntry( "from", null );
    final PolygonColorMapEntry toEntry = templateColorMap.findEntry( "to", null );

    // Fill
    final Color fromPolygonColor = fromEntry.getFill().getFill( null );
    final Color toPolygonColor = toEntry.getFill().getFill( null );
    final double polygonOpacity = fromEntry.getFill().getOpacity( null );

    // Stroke
    final Color fromLineColor = fromEntry.getStroke().getStroke( null );
    final Color toLineColor = toEntry.getStroke().getStroke( null );
    final double lineOpacity = fromEntry.getStroke().getOpacity( null );

    // step width
    final double stepWidth = fromEntry.getTo( null );

    // scale of the step width
    final BigDecimal setScale = new BigDecimal( fromEntry.getFrom( null ) ).setScale( 0, BigDecimal.ROUND_FLOOR );
    final int stepWidthScale = setScale.intValue();

    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isareas by using class steps of 0.1
    // later, the classes will be created by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = new BigDecimal( minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = new BigDecimal( maxValue ).setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal polygonStepWidth = new BigDecimal( stepWidth ).setScale( stepWidthScale, BigDecimal.ROUND_FLOOR );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth )).intValue();

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double fromValue = minDecimal.doubleValue() + currentClass * polygonStepWidth.doubleValue();
      final double toValue = minDecimal.doubleValue() + (currentClass + 1) * polygonStepWidth.doubleValue();

      // Stroke
      Color lineColor;
      if( fromLineColor == toLineColor )
        lineColor = fromLineColor;
      else
        lineColor = interpolateColor( fromLineColor, toLineColor, currentClass, numOfClasses );

      // test
      // lineColor = new Color( 255, 255, 255 );

      // Fill
      final Color polygonColor = interpolateColor( fromPolygonColor, toPolygonColor, currentClass, numOfClasses );
      lineColor = polygonColor;

      final Stroke stroke = StyleFactory.createStroke( lineColor, lineOpacity );
      final Fill fill = StyleFactory.createFill( polygonColor, polygonOpacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isofläche " + currentClass );
      final ParameterValueType from = StyleFactory.createParameterValueType( fromValue );
      final ParameterValueType to = StyleFactory.createParameterValueType( toValue );

      final PolygonColorMapEntry colorMapEntry = new PolygonColorMapEntry_Impl( fill, stroke, label, from, to );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  /**
   * sets the parameters for the colormap of an isoline
   */
  private void configureLineSymbolizer( final SurfaceLineSymbolizer symbolizer, final double minValue, final double maxValue ) throws FilterEvaluationException
  {
    final LineColorMap templateColorMap = symbolizer.getColorMap();
    final LineColorMap newColorMap = new LineColorMap_Impl();

    // retrieve stuff from template-entries
    final LineColorMapEntry fromEntry = templateColorMap.findEntry( "from", null );
    final LineColorMapEntry toEntry = templateColorMap.findEntry( "to", null );
    final LineColorMapEntry fatEntry = templateColorMap.findEntry( "fat", null );

    if( fromEntry == null )
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( IStatus.ERROR, "'from' color-map entry missing in .sld-Template." );
    if( toEntry == null )
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( IStatus.ERROR, "'to' color-map entry missing in .sld-Template." );
    if( fatEntry == null )
      KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( IStatus.ERROR, "'fat' color-map entry missing in .sld-Template." );

    final Color fromColor = fromEntry.getStroke().getStroke( null );
    final Color toColor = toEntry.getStroke().getStroke( null );
    final double opacity = fromEntry.getStroke().getOpacity( null );

    final double normalWidth = fromEntry.getStroke().getWidth( null );
    final double fatWidth = fatEntry.getStroke().getWidth( null );

    // defines which isolines are drawn with a fat line
    final double fatValue = fatEntry.getQuantity( null );
    // TODO: get setep / scale from quantity
    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isolines using class steps of 0.1
    // later, the classes will be done by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = new BigDecimal( minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = new BigDecimal( maxValue ).setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal stepWidth = new BigDecimal( 0.1 ).setScale( 1, BigDecimal.ROUND_HALF_UP );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( stepWidth )).intValue() + 1;

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

      Color lineColor;
      if( fromColor == toColor )
        lineColor = fromColor;
      else
        lineColor = interpolateColor( fromColor, toColor, currentClass, numOfClasses );

      final double strokeWidth;
      if( currentValue % fatValue == 0 )
        strokeWidth = fatWidth;
      else
        strokeWidth = normalWidth;

      final Stroke stroke = StyleFactory.createStroke( lineColor, strokeWidth, opacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isolinie " + currentClass );
      final ParameterValueType quantity = StyleFactory.createParameterValueType( currentValue );

      final LineColorMapEntry colorMapEntry = new LineColorMapEntry_Impl( stroke, label, quantity );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  /**
   * returns the interpolated color of a colormap defined by start and end color.F
   * 
   * @param currentClass
   *            current class
   * @param numOfClasses
   *            number of all classes in which the colormap is divided.
   */
  private Color interpolateColor( final Color minColor, final Color maxColor, final int currentClass, final int numOfClasses )
  {
    // interpolate color
    final float[] minhsb = Color.RGBtoHSB( minColor.getRed(), minColor.getGreen(), minColor.getBlue(), null );
    final float[] maxhsb = Color.RGBtoHSB( maxColor.getRed(), maxColor.getGreen(), maxColor.getBlue(), null );

    final float minHue = minhsb[0];
    final float maxHue = maxhsb[0];

    final float minSat = minhsb[1];
    final float maxSat = maxhsb[1];

    final float minBri = minhsb[2];
    final float maxBri = maxhsb[2];

    final double Hue = minHue + (currentClass * (maxHue - minHue) / (numOfClasses - 1));
    final double Sat = minSat + (currentClass * (maxSat - minSat) / (numOfClasses - 1));
    final double Bri = minBri + (currentClass * (maxBri - minBri) / (numOfClasses - 1));

    final Color hsbColor = Color.getHSBColor( (float) Hue, (float) Sat, (float) Bri );
    final Color rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue() );

    return rgbColor;
  }

}
