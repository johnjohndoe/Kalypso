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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.XMLHelper;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.ObjectFactory;
import org.kalypso.optimizer.Parameter;
import org.kalypso.optimizer.PegelType;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobHelper;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.w3c.dom.Document;

/**
 * encapsulates an NAModellCalculation job to optimize it
 * 
 * @author doemming
 */
public class NAOptimizingJob implements IOptimizingJob
{
  private final File m_baseInputDir;

  private final CalcJobDataBean[] m_beans;

  private final CalcJobDataBean m_beanToOptimize;

  private CalcJobDataBean m_lastOptimizedBean = null;

  private CalcJobDataBean m_bestOptimizedBean = null;

  private File m_lastOptimizeDir = null;

  private File m_bestOptimizeDir = null;

  private ICalcJob m_lastCalcJob = null;

  private ICalcJob m_bestCalcJob = null;

  private TreeMap m_measuredTS;

  private final TimeseriesLink m_linkMeasuredTS;

  private final TimeseriesLink m_linkCalcedTS;

  private final File m_control;

  private final File m_baseCalcDir;

  private final File m_baseOutputDir;

  private final AutoCalibration m_autoCalibration;

  public NAOptimizingJob( File baseDir, final CalcJobDataBean[] beans ) throws Exception
  {
    m_beans = beans;

    // prepare dirs
    m_baseInputDir = new File( baseDir, ICalcServiceConstants.INPUT_DIR_NAME );
    m_baseCalcDir = new File( baseDir, ICalcServiceConstants.CALC_DIR_NAME );
    m_baseOutputDir = new File( baseDir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    m_baseInputDir.mkdirs();
    m_baseCalcDir.mkdirs();
    m_baseOutputDir.mkdirs();

    // load control to get timeseries to optimize (path of measured and
    // calculated)
    m_beanToOptimize = CalcJobHelper.getBeanForId( NaModelConstants.CONTROL_ID, m_beans );
    m_control = new File( m_baseInputDir, m_beanToOptimize.getPath() );
    final URL schemaURL = getClass().getResource( "schema/nacontrol.xsd" );
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( m_control.toURL(),
        schemaURL );
    final Feature rootFeature = controlWorkspace.getRootFeature();
    m_linkMeasuredTS = (TimeseriesLink)rootFeature.getProperty( "pegelZR" );
    m_linkCalcedTS = (TimeseriesLink)rootFeature.getProperty( "qberechnetZR" );

    // load meta to get measured part of calculation intervall
    final CalcJobDataBean metaBean = CalcJobHelper.getBeanForId( NaModelConstants.META_ID, beans );
    final File metaFile = new File( m_baseInputDir, metaBean.getPath() );
    final URL metaSchemaURL = getClass().getResource( "schema/control.xsd" );
    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( metaFile.toURL(),
        metaSchemaURL );
    final Feature metaFE = metaWorkspace.getRootFeature();
    final Date measuredStartDate = (Date)metaFE.getProperty( "startsimulation" );
    final Date measuredEndDate = (Date)metaFE.getProperty( "startforecast" );

    // optimize configuration
    final CalcJobDataBean optimizeConfBean = CalcJobHelper.getBeanForId(
        NaModelConstants.OPTIMIZECONF_ID, beans );
    final File optimizeFile = new File( m_baseInputDir, optimizeConfBean.getPath() );
    final ObjectFactory fac = new ObjectFactory();
    final Unmarshaller unmarshaller = fac.createUnmarshaller();
    m_autoCalibration = (AutoCalibration)unmarshaller.unmarshal( optimizeFile.toURL() );

    // correct in intervall autocalibration
    final PegelType pegel = m_autoCalibration.getPegel();

    final Calendar calendarStart = Calendar.getInstance();
    calendarStart.setTime( measuredStartDate );
    pegel.setStartDate( calendarStart );

    final Calendar calendarEnd = Calendar.getInstance();
    calendarEnd.setTime( measuredEndDate );
    pegel.setEndDate( calendarEnd );
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#calculate()
   */
  public void calculate()
  {

    final File optimizeBaseDir = FileUtilities.createNewTempDir( "optimizeRun", m_baseCalcDir );
    optimizeBaseDir.mkdirs();

    final File optimizeInputDir = new File( optimizeBaseDir, ICalcServiceConstants.INPUT_DIR_NAME );
    optimizeInputDir.mkdirs();

    final CalcJobDataBean[] optimizedBeans = CalcJobHelper.getMergedBeans( m_beans,
        new CalcJobDataBean[]
        { m_lastOptimizedBean } );

    final CalcJobDataBean[] optimizedBeansII = CalcJobHelper.createBeansForNewBaseDir(
        optimizedBeans, m_baseInputDir, optimizeInputDir );

    final ICalcJob calcJob = new NaModelInnerCalcJob();

    try
    {
      calcJob.run( optimizeBaseDir, optimizedBeansII );
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
    }
    m_lastOptimizeDir = optimizeBaseDir;
    m_lastCalcJob = calcJob;
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#setBestEvaluation(boolean)
   */
  public void setBestEvaluation( boolean lastWasBest )
  {
    if( lastWasBest )
    {
      clear( m_bestCalcJob, m_bestOptimizeDir );
      m_bestOptimizeDir = m_lastOptimizeDir;
      m_bestCalcJob = m_lastCalcJob;
      m_bestOptimizedBean = m_lastOptimizedBean;
    }
    else
    {
      clear( m_lastCalcJob, m_lastOptimizeDir );
    }
    m_lastOptimizeDir = null;
    m_lastCalcJob = null;
    m_lastOptimizedBean = null;
  }

  private void clear( ICalcJob job, File dir )
  {
    if( job != null )
      job.disposeJob();
    if( dir != null )
    {
      System.out.println( "remove " + dir.toString() );
      try
      {
        FileUtils.deleteDirectory( dir );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#optimize(org.kalypso.optimizer.Parameter[],
   *      double[])
   */
  public void optimize( Parameter[] parameterConf, double values[] ) throws Exception
  {
    final File conrolFile = new File( m_baseInputDir, m_beanToOptimize.getPath() );
    final Document dom = XMLHelper.getAsDOM( conrolFile.toURL(), false );
    final ParameterOptimizeContext[] calcContexts = new ParameterOptimizeContext[parameterConf.length];
    for( int i = 0; i < parameterConf.length; i++ )
      calcContexts[i] = new ParameterOptimizeContext( parameterConf[i] );
    try
    {
      OptimizeModelUtils.transformModel( dom, values, calcContexts );
    }
    catch( TransformerException e )
    {
      e.printStackTrace();
    }

    final Transformer t = TransformerFactory.newInstance().newTransformer();

    t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
    t.setOutputProperty( OutputKeys.INDENT, "yes" );

    final File file = File.createTempFile( "optimizedBean", ".xml", m_baseCalcDir );
    Writer writer = new FileWriter( file );
    t.transform( new DOMSource( dom ), new StreamResult( writer ) );
    writer.close();
    String path = FileUtilities.getRelativePathTo( m_baseInputDir, file );
    m_lastOptimizedBean = new CalcJobDataBean( NaModelConstants.CONTROL_ID, "optimized control",
        path );
  }

  /**
   * @throws SensorException
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#getMeasuredTimeSeries()
   */
  public TreeMap getMeasuredTimeSeries() throws MalformedURLException, SensorException
  {
    if( m_measuredTS == null )
    {
      TreeMap result = new TreeMap();
      URL measuredURL = new URL( m_control.toURL(), m_linkMeasuredTS.getHref() );
      //      File tsFile = new File( m_inputDir, m_linkMeasuredTS.getHref() );
      IObservation observation = ZmlFactory.parseXML( measuredURL, "measured" );
      IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(),
          TimeserieConstants.TYPE_DATE );
      IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(),
          TimeserieConstants.TYPE_RUNOFF );
      ITuppleModel values = observation.getValues( null );
      for( int i = 0; i < values.getCount(); i++ )
      {
        Date date = (Date)values.getElement( i, dateAxis );
        Object value = values.getElement( i, qAxis );
        result.put( date, value );
      }
      m_measuredTS = result;
    }
    return m_measuredTS;
  }

  /**
   * @return
   * @throws SensorException
   * @throws MalformedURLException
   * @see org.kalypso.optimize.IOptimizingJob#getCalcedTimeSeries()
   */
  public TreeMap getCalcedTimeSeries() throws MalformedURLException, SensorException
  {
    final TreeMap result = new TreeMap();
    final File optimizeOutDir = new File( m_lastOptimizeDir, ICalcServiceConstants.OUTPUT_DIR_NAME );

    String calcHref = m_linkCalcedTS.getHref().replaceFirst(
        "^" + ICalcServiceConstants.RESULT_DIR_NAME + ".", "" );

    final File tsFile = new File( optimizeOutDir, calcHref );
    final IObservation observation = ZmlFactory.parseXML( tsFile.toURL(), "result" );
    final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(),
        TimeserieConstants.TYPE_DATE );
    final IAxis qAxis = ObservationUtilities.findAxisByType( observation.getAxisList(),
        TimeserieConstants.TYPE_RUNOFF );
    final ITuppleModel values = observation.getValues( null );
    for( int i = 0; i < values.getCount(); i++ )
    {
      Date date = (Date)values.getElement( i, dateAxis );
      Object value = values.getElement( i, qAxis );
      result.put( date, value );
    }
    return result;
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#getResults()
   */
  public CalcJobDataBean[] getResults()
  {
    final List result = new ArrayList();
    final File jobOutputDir = new File( m_bestOptimizeDir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    CalcJobDataBean[] calculatedBeans = m_bestCalcJob.getResults();

    try
    {
      final File from = new File( m_baseInputDir, m_bestOptimizedBean.getPath() );
      final File to = new File( m_baseOutputDir, m_beanToOptimize.getPath() );
      FileUtils.copyFile( from, to );
      result.add( new CalcJobDataBean( NaModelConstants.CONTROL_ID, "optimized control",
          m_beanToOptimize.getPath() ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    for( int i = 0; i < calculatedBeans.length; i++ )
    {
      CalcJobDataBean bean = calculatedBeans[i];
      final File from = new File( jobOutputDir, bean.getPath() );
      final File to = new File( m_baseOutputDir, bean.getPath() );
      try
      {
        FileUtils.copyFile( from, to );
        result.add( bean );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
    return (CalcJobDataBean[])result.toArray( new CalcJobDataBean[result.size()] );
  }

  /**
   * @see org.kalypso.optimize.IOptimizingJob#getOptimizeConfiguration()
   */
  public AutoCalibration getOptimizeConfiguration()
  {
    return m_autoCalibration;
  }
}