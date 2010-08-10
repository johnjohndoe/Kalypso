/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.optimize;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.binding.NAControl;
import org.kalypso.model.hydrology.internal.binding.NAModellControl;
import org.kalypso.model.hydrology.internal.simulation.NaModelInnerCalcJob;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizeJaxb;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.Parameter;
import org.kalypso.optimizer.Pegel;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.w3c.dom.Document;

/**
 * encapsulates an NAModellCalculation job to optimize it
 * 
 * @author doemming
 */
public class NAOptimizingJob implements IOptimizingJob
{
  private final File m_tmpDir;

  private SortedMap<Date, Double> m_measuredTS;

  private final TimeseriesLinkType m_linkMeasuredTS;

  private final TimeseriesLinkType m_linkCalcedTS;

  private final AutoCalibration m_autoCalibration;

  private final ISimulationDataProvider m_dataProvider;

  private File m_lastOptimizedFile;

  private File m_bestOptimizedFile = null;

  private final ISimulationMonitor m_monitor;

  private File m_lastOptimizeRunDir = null;

  private File m_bestOptimizeRunDir = null;

  private OptimizeCalcResultEater m_lastResultEater = null;

  private OptimizeCalcResultEater m_bestResultEater = null;

  public static final String IN_BestOptimizedRunDir_ID = "BestOptimizedRunDir_so_far"; //$NON-NLS-1$

  private int m_counter = 0;

  private int m_bestNumber = 0;

  private boolean m_lastSucceeded = false;

  private boolean m_bestSucceeded = false;

  public NAOptimizingJob( final File tmpDir, final ISimulationDataProvider dataProvider, final ISimulationMonitor monitor ) throws Exception
  {
    m_tmpDir = tmpDir;
    m_dataProvider = dataProvider;
    m_monitor = monitor;

    // FIXME: already loaded in main job, will also be loaded in inner jobs; can we avoid this?
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID ), null );
    final NAModellControl naControl = (NAModellControl) controlWorkspace.getRootFeature();
    m_linkMeasuredTS = naControl.getPegelZRLink();
    m_linkCalcedTS = naControl.getResultLink();

    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_META_ID ), null );
    final NAControl metaControl = (NAControl) metaWorkspace.getRootFeature();
    final Date measuredStartDate = metaControl.getSimulationStart();
    final Date measuredEndDate = metaControl.getStartForecast();

    final Unmarshaller unmarshaller = OptimizeJaxb.JC.createUnmarshaller();

    m_autoCalibration = (AutoCalibration) unmarshaller.unmarshal( FileUtils.toFile((URL) dataProvider.getInputForID( NaModelConstants.IN_OPTIMIZECONF_ID ) ));

    // correct in intervall autocalibration
    final Pegel pegel = m_autoCalibration.getPegel();

    final Calendar calendarStart = Calendar.getInstance();
    calendarStart.setTime( measuredStartDate );
    pegel.setStartDate( calendarStart );

    final Calendar calendarEnd = Calendar.getInstance();
    calendarEnd.setTime( measuredEndDate );
    pegel.setEndDate( calendarEnd );
  }

  /**
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#calculate()
   */
  @Override
  public void calculate( ) throws MalformedURLException
  {
    m_counter++;
    final File optimizeRunDir = FileUtilities.createNewTempDir( "optimizeRun", m_tmpDir ); //$NON-NLS-1$
    optimizeRunDir.mkdirs();

    final CalcDataProviderDecorater newDataProvider = new CalcDataProviderDecorater( m_dataProvider );
    newDataProvider.addURL( NaModelConstants.IN_CONTROL_ID, m_lastOptimizedFile.toURL() );

    // some generated files from best run can be recycled to increase
    // performance
    if( m_bestOptimizeRunDir != null )
    {
      try
      {
        newDataProvider.addURL( IN_BestOptimizedRunDir_ID, m_bestOptimizeRunDir.toURL() );
      }
      catch( final MalformedURLException e1 )
      {
        // on exception it is simply not used.
      }
    }
    final NaModelInnerCalcJob calcJob = new NaModelInnerCalcJob();
    final OptimizeCalcResultEater optimizeResultEater = new OptimizeCalcResultEater();
    try
    {
      calcJob.run( optimizeRunDir, newDataProvider, optimizeResultEater, m_monitor );
    }
    catch( final SimulationException e )
    {
      e.printStackTrace();
    }
    m_lastOptimizeRunDir = optimizeRunDir;
    m_lastResultEater = optimizeResultEater;
    m_lastSucceeded = calcJob.isSucceeded();
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#setBestEvaluation(boolean)
   */
  @Override
  public void setBestEvaluation( final boolean lastWasBest )
  {
    if( lastWasBest )
    {
      clear( m_bestOptimizeRunDir );
      m_bestOptimizeRunDir = m_lastOptimizeRunDir;
      m_bestResultEater = m_lastResultEater;
      m_bestOptimizedFile = m_lastOptimizedFile;
      m_bestNumber = m_counter;
      m_bestSucceeded = m_lastSucceeded;
    }
    else
    {
      clear( m_lastOptimizeRunDir );
    }
    m_lastOptimizeRunDir = null;
    m_lastResultEater = null;
  }

  private void clear( final File dir )
  {
    if( dir != null )
    {
      // System.out.println( "remove " + dir.toString() );
      try
      {
        FileUtils.deleteDirectory( dir );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#optimize(org.kalypso.optimizer.Parameter[], double[])
   */
  @Override
  public void optimize( final Parameter[] parameterConf, final double values[] ) throws Exception
  {
    final Document dom = XMLHelper.getAsDOM( FileUtils.toFile( (URL) m_dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID )), true );

    final ParameterOptimizeContext[] calcContexts = new ParameterOptimizeContext[parameterConf.length];
    for( int i = 0; i < parameterConf.length; i++ )
      calcContexts[i] = new ParameterOptimizeContext( parameterConf[i] );
    try
    {
      OptimizeModelUtils.transformModel( dom, values, calcContexts );
    }
    catch( final TransformerException e )
    {
      e.printStackTrace();
    }

    final TransformerFactory factory = TransformerFactory.newInstance();
    final Transformer t = factory.newTransformer();

    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
    t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

    final File file = File.createTempFile( "optimizedBean", ".xml", m_tmpDir ); //$NON-NLS-1$//$NON-NLS-2$
    final Writer writer = new FileWriter( file );
    try
    {
      t.transform( new DOMSource( dom ), new StreamResult( writer ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    IOUtils.closeQuietly( writer );

    m_lastOptimizedFile = file;

  }

  /**
   * @throws SensorException
   * @see org.kalypso.optimize.IOptimizingJob#getMeasuredTimeSeries()
   */
  @Override
  public SortedMap<Date, Double> getMeasuredTimeSeries( ) throws SensorException
  {
    if( m_measuredTS == null )
    {
      final SortedMap<Date, Double> result = new TreeMap<Date, Double>();
      URL measuredURL = null;
      try
      {
        measuredURL = new URL( (URL) m_dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID ), m_linkMeasuredTS.getHref() );
      }
      catch( final Exception e )
      {
        // TODO exeption werfen die dem user sagt dass die optimierung nicht
        // möglich ist ohne gemessene zeitreihe
        e.printStackTrace();
      }
      final IObservation observation = ZmlFactory.parseXML( measuredURL ); //$NON-NLS-1$
      final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_DATE );
      final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_RUNOFF );
      final ITupleModel values = observation.getValues( null );
      for( int i = 0; i < values.getCount(); i++ )
      {
        final Date date = (Date) values.getElement( i, dateAxis );
        final Double value = (Double) values.getElement( i, qAxis );
        result.put( date, value );
      }
      m_measuredTS = result;
    }
    return m_measuredTS;
  }

  /**
   * @throws SensorException
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#getCalcedTimeSeries()
   */
  @Override
  public SortedMap<Date, Double> getCalcedTimeSeries( ) throws MalformedURLException, SensorException
  {
    final SortedMap<Date, Double> result = new TreeMap<Date, Double>();
    final File optimizeResultDir = new File( m_lastOptimizeRunDir, NaModelConstants.OUTPUT_DIR_NAME );

    final String calcHref = m_linkCalcedTS.getHref().replaceFirst( "^" + NaModelConstants.OUTPUT_DIR_NAME + ".", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final File tsFile = new File( optimizeResultDir, calcHref );
    final IObservation observation = ZmlFactory.parseXML( tsFile.toURL() ); //$NON-NLS-1$
    final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_DATE );
    final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), ITimeseriesConstants.TYPE_RUNOFF );
    final ITupleModel values = observation.getValues( null );
    for( int i = 0; i < values.getCount(); i++ )
    {
      final Date date = (Date) values.getElement( i, dateAxis );
      final Double value = (Double) values.getElement( i, qAxis );
      result.put( date, value );
    }
    return result;
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.optimize.IOptimizingJob#publishResults(org.kalypso.services.calculation.job.ICalcResultEater)
   */
  @Override
  public void publishResults( final ISimulationResultEater resultEater ) throws SimulationException
  {
    if( m_bestResultEater == null )
      return;
    for( final Object element : m_bestResultEater.keySet() )
    {
      final String id = (String) element;
      resultEater.addResult( id, m_bestResultEater.get( id ) );
    }
    resultEater.addResult( NaModelConstants.OUT_OPTIMIZEFILE, m_bestOptimizedFile );
    System.out.println( "best was #" + m_bestNumber ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#getOptimizeConfiguration()
   */
  @Override
  public AutoCalibration getOptimizeConfiguration( )
  {
    return m_autoCalibration;
  }

  @Override
  public boolean isSucceeded( )
  {
    return m_bestSucceeded;
  }

}