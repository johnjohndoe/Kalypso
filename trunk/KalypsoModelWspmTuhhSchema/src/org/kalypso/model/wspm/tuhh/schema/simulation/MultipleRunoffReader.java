/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Comparator;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.lang3.CharEncoding;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureComparator;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class MultipleRunoffReader
{
  private static final String ERGEBNISSE_GMV = "Ergebnisse.gmv"; //$NON-NLS-1$

  private final TuhhCalculation m_calculation;

  private final File m_targetGmlFile;

  private QIntervallResultCollection m_resultCollection;

  private final LogHelper m_log;

  private QIntervalIndex m_intervalIndex;

  private final File m_kmDir;

  public MultipleRunoffReader( final File outputDir, final File kmDir, final TuhhCalculation calculation, final LogHelper log )
  {
    m_kmDir = kmDir;
    m_log = log;
    m_calculation = calculation;
    m_targetGmlFile = new File( outputDir, "qIntervallResults.gml" ); //$NON-NLS-1$
  }

  public void init( ) throws GMLSchemaException, MalformedURLException
  {
    final URL gmlContext = m_targetGmlFile.toURI().toURL();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( QIntervallResultCollection.QNAME_F_QIntervallResultCollection, gmlContext, GmlSerializer.DEFAULT_FACTORY );

    m_resultCollection = (QIntervallResultCollection) workspace.getRootFeature();

    m_intervalIndex = new QIntervalIndex( m_resultCollection.getQIntervalls(), m_calculation.getReach() );
  }

  public void readKM( )
  {
    try
    {
      final FileFilter kmFilter = FileFilterUtils.suffixFileFilter( ".km", IOCase.INSENSITIVE ); //$NON-NLS-1$
      final File[] kmFiles = m_kmDir.listFiles( kmFilter );

      // REMARK: the way we read km/polynomial files it's bit tricky to get the slope
      // However this is not a problem, as we are calculating we a uniform steady slope,
      // which is defined in the calculation
      final BigDecimal startSlope = m_calculation.getStartSlope();
      final BigDecimal slope = startSlope.setScale( 5, RoundingMode.HALF_UP );

      final KMFileReader reader = new KMFileReader( kmFiles, m_log, m_intervalIndex, slope );
      reader.read();
    }
    catch( final Throwable e )
    {
      m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.KMProcessor.0" ), e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }

  public void readPolynomeResults( final File polynomeTmpDir, final File resultDir, final LogHelper log, final ISimulationResultEater resultEater ) throws SimulationException
  {
    try
    {
      final QIntervalReader qIntervalReader = new QIntervalReader( resultDir, m_intervalIndex, log );
      qIntervalReader.execute( new NullProgressMonitor() );
    }
    catch( final Throwable e )
    {
      log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      final ISimulationMonitor monitor = log.getMonitor();
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.15" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
    }

    final File polynomeLogFile = new File( polynomeTmpDir, "Polynome1d.log" ); //$NON-NLS-1$
    if( polynomeLogFile.exists() )
      resultEater.addResult( "polynomeLog", polynomeLogFile ); //$NON-NLS-1$
  }

  public void createResult( final ISimulationResultEater resultEater ) throws SimulationException, IOException, GmlSerializeException
  {
    /* Sort by station before wrinting the result */
    final Comparator<Object> featureComparator = new FeatureComparator( m_resultCollection, QIntervallResult.QNAME_P_QIntervallResult_station );
    Collections.sort( m_resultCollection.getQIntervalls(), featureComparator );

    final GMLWorkspace workspace = m_resultCollection.getWorkspace();
    GmlSerializer.serializeWorkspace( m_targetGmlFile, workspace, CharEncoding.UTF_8 );
    resultEater.addResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT, m_targetGmlFile );

    final File gmvResultFile = new File( m_targetGmlFile.getParentFile(), ERGEBNISSE_GMV );
    final URL ergebnisseGmvLocation = PolynomeProcessor.class.getResource( "resources/" + ERGEBNISSE_GMV ); //$NON-NLS-1$
    FileUtils.copyURLToFile( ergebnisseGmvLocation, gmvResultFile );
    resultEater.addResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT_GMV, gmvResultFile );
  }

  public void createSumComponents( )
  {
    for( final QIntervallResult qInterval : m_resultCollection.getQIntervalls() )
    {
      final IObservation<TupleResult> observation = qInterval.getOrCreatePointsObservation();
      final TupleResult result = observation.getResult();
      final int indexRunoff = result.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
      if( indexRunoff < 0 )
      {
        final IComponent targetComponent = qInterval.createPointsComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
        result.addComponent( targetComponent );
        createSum( result, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_CHANNEL, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_FLOODPLAIN, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );
      }

      final int indexArea = result.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA );
      if( indexArea < 0 )
      {
        final IComponent targetComponent = qInterval.createPointsComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA );
        result.addComponent( targetComponent );
        createSum( result, IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_CHANNEL, IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_FLOODPLAIN, IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA );
      }

      final int indexWidth = result.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH );
      if( indexWidth < 0 )
      {
        final IComponent targetComponent = qInterval.createPointsComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH );
        result.addComponent( targetComponent );
        createSum( result, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_CHANNEL, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_FLOODPLAIN, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH );
      }

      qInterval.setPointsObservation( observation );
    }
  }

  private void createSum( final TupleResult result, final String idOne, final String idTwo, final String idTarget )
  {
    final int indexOne = result.indexOfComponent( idOne );
    final int indexTwo = result.indexOfComponent( idTwo );
    final int indexTarget = result.indexOfComponent( idTarget );

    for( final IRecord record : result )
    {
      final BigDecimal valueOne = (BigDecimal) record.getValue( indexOne );
      final BigDecimal valueTwo = (BigDecimal) record.getValue( indexTwo );
      final BigDecimal valueTarget = valueOne.add( valueTwo );
      record.setValue( indexTarget, valueTarget );
    }
  }
}