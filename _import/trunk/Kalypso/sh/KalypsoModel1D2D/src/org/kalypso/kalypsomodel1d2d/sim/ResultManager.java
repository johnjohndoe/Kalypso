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
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
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
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

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

  private final List<File> m_found2dFiles = new ArrayList<File>();

  private final List<Job> m_resultJobs = new ArrayList<Job>();

  private final File m_outputDir;

  private final File m_inputDir;

  private final Pattern m_resultFilePattern;

  private final ISimulationDataProvider m_dataProvider;

  private final RMA10Calculation m_calculation;

  private final NodeResultMinMaxCatcher m_minMaxCatcher = new NodeResultMinMaxCatcher();

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

  public ResultManager( final File inputDir, final File outputDir, final String resultFilePattern, final ISimulationDataProvider dataProvider, final RMA10Calculation calculation )
  {
    m_inputDir = inputDir;
    m_outputDir = outputDir;
    m_dataProvider = dataProvider;
    m_calculation = calculation;
    m_resultFilePattern = Pattern.compile( resultFilePattern + "(\\d+)" );

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
    if( matcher.matches() )
    {
      final String countStr = matcher.group( 1 );
      final int count = Integer.parseInt( countStr ) - 1;
      outDirName = "timestep-" + count;
    }
    else
      outDirName = resultFileName;

    final File resultOutputDir = new File( m_outputDir, outDirName );
    resultOutputDir.mkdirs();
    final ProcessResultsJob processResultsJob = new ProcessResultsJob( file, resultOutputDir, m_dataProvider, m_calculation );
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
    /* Process all remaining .2d files. */
    run();

    /* We need to wait until all result process jobs have finished. */
    final IStatus resultProcessingStatus = waitForResultProcessing();
    if( resultProcessingStatus != null )
      System.out.println( resultProcessingStatus );
    // TODO: evaluate status

    /* other, generell postprocessing stuff. */
    try
    {
      processTemplates();
      processStyles();
    }
    catch( final SimulationException se )
    {
      throw se;
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei der Ergesbnisauswertung", e );
    }
  }

  private void processTemplates( ) throws SimulationException, IOException, JAXBException
  {
    /* Write template sld into result folder */
    final URL resultStyleURL = (URL) m_dataProvider.getInputForID( "ResultTemplate" );
    ZipUtilities.unzip( resultStyleURL, m_outputDir );

    final File gmtTotalFile = new File( m_outputDir, "Gesamtergebnis.gmt" );

    final Gismapview totalTemplate = GisTemplateHelper.loadGisMapView( gmtTotalFile );

    /* Add sub-map for each step-subdirectory */
    final File[] childFiles = m_outputDir.listFiles();
    for( final File childFile : childFiles )
    {
      if( childFile.isDirectory() )
        addChildMaps( totalTemplate, childFile );
    }

    GisTemplateHelper.saveGisMapView( totalTemplate, gmtTotalFile, "UTF-8" );
  }

  private void addChildMaps( final Gismapview totalTemplate, final File childFile )
  {
    final Layers layers = totalTemplate.getLayers();
    final List<StyledLayerType> layerList = layers.getLayer();

    final File[] gmtFiles = childFile.listFiles( FILTER_GMT );
    for( final File file : gmtFiles )
    {
      final StyledLayerType layer = GisTemplateHelper.OF_TEMPLATE_TYPES.createStyledLayerType();
      layer.setId( "SUB_" + childFile.getName() + "_" + file.getName() );
      layer.setName( childFile.getName() + " - " + file.getName() );
      layer.setHref( childFile.getName() + "/" + file.getName() );
      layer.setVisible( false );
      layer.setLinktype( "gmt" );

      layerList.add( layer );
    }
  }

  private void processStyles( ) throws IOException, XMLParsingException
  {
    // TODO: refactor in order to call same method for several files/parameters

    /* Read sld from template */
    final File styleDir = new File( m_outputDir, "Styles" );
    final File tinStyleFile = new File( styleDir, "tinStyles.sld" );
    final StyledLayerDescriptor sld = SLDFactory.createSLD( tinStyleFile );

    final NamedLayer[] namedLayers = sld.getNamedLayers();
    for( final NamedLayer namedLayer : namedLayers )
    {
      // TODO: move these search functions into helper class? Or into the parent-clas itself?
      if( "tinStyles".equals( namedLayer.getName() ) )
      {
        final Style[] styles = namedLayer.getStyles();
        for( final Style style : styles )
        {
          if( "tinWSPStyle".equals( style.getName() ) && style instanceof UserStyle )
          {
            final UserStyle userStyle = (UserStyle) style;
            final FeatureTypeStyle[] featureTypeStyles = userStyle.getFeatureTypeStyles();
            for( final FeatureTypeStyle featureTypeStyle : featureTypeStyles )
            {
              if( "tinWSPFeatureTypeStyle".equals( featureTypeStyle.getName() ) )
              {
                final Rule[] rules = featureTypeStyle.getRules();
                for( final Rule rule : rules )
                {
                  if( "tinWSPRule".equals( rule.getName() ) )
                  {
                    final Symbolizer[] symbolizers = rule.getSymbolizers();
                    for( final Symbolizer symbolizer : symbolizers )
                    {
                      if( symbolizer instanceof SurfaceLineSymbolizer )
                      {
                        configureLineSymbolizer( (SurfaceLineSymbolizer) symbolizer );
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    /* Write SLD back to file */
    final String sldXML = sld.exportAsXML();
    final String sldXMLwithHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + sldXML;
    FileUtils.writeStringToFile( tinStyleFile, sldXMLwithHeader, "UTF-8" );
  }

  private void configureLineSymbolizer( final SurfaceLineSymbolizer symbolizer )
  {
    final LineColorMap colorMap = symbolizer.getColorMap();

    // TODO: make lots of lines!

    final Stroke stroke = StyleFactory.createStroke( new Color( 0, 1, 2 ) ); // TODO: there are other nice createStroke
    // methods
    final ParameterValueType label = StyleFactory.createParameterValueType( "Hallo Thomas: meine schöne Isolinie" );
    final ParameterValueType quantity = StyleFactory.createParameterValueType( 23.45 );
    final LineColorMapEntry colorMapEntry = new LineColorMapEntry_Impl( stroke, label, quantity );
    colorMap.addColorMapClass( colorMapEntry );

    // TODO Auto-generated method stub

  }
}
