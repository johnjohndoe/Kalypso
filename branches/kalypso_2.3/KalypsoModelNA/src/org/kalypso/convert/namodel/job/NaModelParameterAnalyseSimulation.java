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
package org.kalypso.convert.namodel.job;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.convert.namodel.optimize.CalcDataProviderDecorater;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.property.restriction.MaxExclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MaxInclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MinExclusiveRestriction;
import org.kalypso.gmlschema.property.restriction.MinInclusiveRestriction;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.simulation.NaModelCalcJob;
import org.kalypso.model.hydrology.internal.simulation.NaModelInnerCalcJob;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class NaModelParameterAnalyseSimulation implements ISimulation
{
  private ISimulationDataProvider m_inputProvider;

  private File m_analyseDir;

  private ISimulationMonitor m_monitor;

  File m_analyseResultDir;

  private final Logger m_logger;

  protected boolean m_partResult = false;

  public NaModelParameterAnalyseSimulation( final Logger logger )
  {
    m_logger = logger;
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    m_inputProvider = inputProvider;
    m_monitor = monitor;
    m_analyseDir = new File( tmpdir, "analyseDir" ); //$NON-NLS-1$
    m_analyseResultDir = new File( tmpdir, "analyseResultDir" ); //$NON-NLS-1$
    m_analyseDir.mkdirs();
    try
    {
      // check for setanalysis
      // final URL gmlURL = inputProvider.getURLForID( NaModelConstants.IN_CONTROL_ID );
      // final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL );
      // final FeatureXPath path = new FeatureXPath("/bla/bla");
      // final URL modellURL = inputProvider.getURLForID( NaModelConstants.IN_MODELL_ID );
      // final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL );
      final URL analyseXsdURL = (URL) inputProvider.getInputForID( NaModelConstants.IN_ANALYSE_MODELL_XSD_ID );
      final IGMLSchema schema = GMLSchemaFactory.createGMLSchema( null, analyseXsdURL );
      final List<FeaturePropertyToProcess> list = new ArrayList<FeaturePropertyToProcess>();
      final IFeatureType[] featureTypes = schema.getAllFeatureTypes();
      for( final IFeatureType ft : featureTypes )
      {
        final IPropertyType[] properties = ft.getProperties();
        for( final IPropertyType pt : properties )
        {
          if( pt instanceof IValuePropertyType )
          {
            Double min = null;
            Double max = null;
            final IValuePropertyType vpt = (IValuePropertyType) pt;
            final IRestriction[] restrictions = vpt.getRestriction();
            for( final IRestriction restriction : restrictions )
            {
              if( restriction instanceof MinInclusiveRestriction )
                min = ((MinInclusiveRestriction) restriction).getMinInclusive();
              else if( restriction instanceof MinExclusiveRestriction )
                min = ((MinExclusiveRestriction) restriction).getMinExclusive();
              else if( restriction instanceof MaxInclusiveRestriction )
                max = ((MaxInclusiveRestriction) restriction).getMaxInclusive();
              else if( restriction instanceof MaxExclusiveRestriction )
                max = ((MaxExclusiveRestriction) restriction).getMaxExclusive();
            }
            if( min != null && max != null )
              list.add( new FeaturePropertyToProcess( ft, vpt, min, max ) );
          }
        }
      }

      final Iterator<FeaturePropertyToProcess> iterator = list.iterator();
      int count = 0;
      monitor.setProgress( 0 );
      while( iterator.hasNext() )
      {
        m_logger.info( "analyseCountDown: " + (list.size() * 3 - count) ); //$NON-NLS-1$
        final FeaturePropertyToProcess prop = iterator.next();
        final String keyBase = prop.getFeatureType().getQName().getLocalPart() + "_" + prop.getPropertyType().getQName().getLocalPart(); //$NON-NLS-1$
        analyseRun( prop, keyBase + "_MIN", AnalysisFeatureVisitor.MODE_MIN ); //$NON-NLS-1$

        count++;
        monitor.setProgress( (list.size() * 3) * count / 100 );
        if( monitor.isCanceled() )
          return;
        analyseRun( prop, keyBase + "_AVERAGE", AnalysisFeatureVisitor.MODE_AVERAGE ); //$NON-NLS-1$

        count++;
        monitor.setProgress( (list.size() * 3) * count / 100 );
        if( monitor.isCanceled() )
          return;
        analyseRun( prop, keyBase + "_MAX", AnalysisFeatureVisitor.MODE_MAX ); //$NON-NLS-1$

        count++;
        monitor.setProgress( (list.size() * 3) * count / 100 );
        if( monitor.isCanceled() )
          return;
      }

    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString("org.kalypso.convert.namodel.job.NaModelParameterAnalyseSimulation.7"), e ); //$NON-NLS-1$
    }
    resultEater.addResult( NaModelConstants.OUT_ZML, m_analyseResultDir );
  }

  private void analyseRun( final FeaturePropertyToProcess prop, final String key, final int mode ) throws Exception
  {
    m_logger.info( "analyse run: " + key ); //$NON-NLS-1$
    final FeatureVisitor visitor = new AnalysisFeatureVisitor( prop, mode );
    final File baseDir = new File( m_analyseDir, key );
    baseDir.mkdirs();
    final URL modellURL = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL, null );
    modellWorkspace.accept( visitor, modellWorkspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    final File modellFile = new File( baseDir, "modell" + key + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
    FileWriter writer = null;
    try
    {
      writer = new FileWriter( modellFile );
      GmlSerializer.serializeWorkspace( writer, modellWorkspace, "UTF-8" ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

    // decorate original dataprovider
    final CalcDataProviderDecorater provider = new CalcDataProviderDecorater( m_inputProvider );
    provider.addURL( NaModelConstants.IN_MODELL_ID, modellFile.toURL() );

    final NaModelInnerCalcJob job = new NaModelInnerCalcJob();
    final ISimulationResultEater resultEater = new ISimulationResultEater()
    {
      @Override
      public void addResult( final String id, final Object result )
      {
        if( id.equals( NaModelConstants.OUT_ZML ) )
        {
          m_partResult = true;
          System.out.println( Messages.getString("org.kalypso.convert.namodel.job.NaModelParameterAnalyseSimulation.12") ); //$NON-NLS-1$
          final File fromDir = (File) result;
          final File toDir = new File( m_analyseResultDir, key );
          toDir.mkdirs();
          final FileCopyVisitor copyVisitor = new FileCopyVisitor( fromDir, toDir, true );
          try
          {
            FileUtilities.accept( fromDir, copyVisitor, true );
          }
          catch( final IOException e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }
        }
      }
    };

    job.run( baseDir, provider, resultEater, new NullSimulationMonitor( m_monitor ) );
  }

  public boolean isSucceeded( )
  {
    return m_partResult;
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return NaModelCalcJob.class.getResource( NaModelCalcJob.NACALCJOB_SPEC_XML_LOCATION );
  }

  class FeaturePropertyToProcess
  {
    private final IFeatureType m_ft;

    private final IPropertyType m_pt;

    private final double m_min;

    private final double m_max;

    public FeaturePropertyToProcess( final IFeatureType ft, final IPropertyType pt, final Double min, final Double max )
    {
      m_ft = ft;
      m_pt = pt;
      m_min = min;
      m_max = max;
    }

    public IFeatureType getFeatureType( )
    {
      return m_ft;
    }

    public IPropertyType getPropertyType( )
    {
      return m_pt;
    }

    public Double getMin( )
    {
      return m_min;
    }

    public Double getMax( )
    {
      return m_max;
    }
  }

}
