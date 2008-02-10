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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.DefaultSimulationDataProvider;
import org.kalypso.simulation.core.util.DefaultSimulationResultEater;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;

/**
 * @author Gernot Belger
 */
public class FlowRelationshipCalcOperation implements ICoreRunnableWithProgress
{
  private final static class NullSimulationMonitorExtension extends NullSimulationMonitor
  {
    private final SubMonitor m_monitor;

    private int m_status;

    private String m_text;

    public NullSimulationMonitorExtension( final IProgressMonitor monitor )
    {
      m_monitor = SubMonitor.convert( monitor, 100 );
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#isCanceled()
     */
    @Override
    public boolean isCanceled( )
    {
      return m_monitor.isCanceled();
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#setProgress(int)
     */
    @Override
    public void setProgress( final int progress )
    {
      // TODO: implement
      super.setProgress( progress );
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#setFinishInfo(int, java.lang.String)
     */
    @Override
    public void setFinishInfo( final int status, final String text )
    {
      m_status = status;
      m_text = text;
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#getFinishStatus()
     */
    @Override
    public int getFinishStatus( )
    {
      return m_status;
    }

    /**
     * @see org.kalypso.simulation.core.NullSimulationMonitor#getFinishText()
     */
    @Override
    public String getFinishText( )
    {
      return m_text;
    }
  }

  private final IFlowRelation1D[] m_flowRels;

  private final IFEDiscretisationModel1d2d m_discModel;

  private final IFlowRelationshipModel m_flowRelModel;

  private final ISimulationMonitor m_simulationMonitor;

  private boolean m_running;

  private Map<IFlowRelation1D, QIntervallResult> m_results;

  private final TuhhCalculation m_templateCalculation;

  public FlowRelationshipCalcOperation( final TuhhCalculation templateCalculation, final IFlowRelation1D[] flowRels, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel, final ISimulationMonitor simMonitor )
  {
    m_templateCalculation = templateCalculation;
    m_flowRels = flowRels;
    m_flowRelModel = flowModel;
    m_discModel = discModel;
    m_simulationMonitor = simMonitor;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, "Berechne Parameter", m_flowRels.length );

    final String pluginId = PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() );
    final MultiStatus multiStatus = new MultiStatus( pluginId, -1, "Ergebnisse der Paremeterberechnung", null );

    m_results = new HashMap<IFlowRelation1D, QIntervallResult>();
    for( final IFlowRelation1D flowRel : m_flowRels )
    {
      progress.subTask( flowRel.getName() );

      try
      {
        final QIntervallResult result = calculateFlowRel( m_templateCalculation, flowRel, progress.newChild( 1 ) );
        m_results.put( flowRel, result );
        multiStatus.add( StatusUtilities.createStatus( IStatus.OK, "Berechnung erfolgreich an Station km " + flowRel.getStation(), null ) );
      }
      catch( final CoreException e )
      {
        // If canceled, exit
        if( e.getStatus().matches( IStatus.CANCEL ) )
          throw e;

        multiStatus.add( e.getStatus() );
      }
      catch( final Throwable t )
      {
        throw new InvocationTargetException( t );
      }
    }

    /* Create nice message for multi-status */
    final String message;

    if( multiStatus.isOK() || multiStatus.matches( IStatus.INFO ) )
      message = "Alle Parameter wurden erfolgreich berechnet";
    else if( multiStatus.matches( IStatus.WARNING ) )
      message = "Warnung(en) bei der Parameterberechnung";
    else if( multiStatus.matches( IStatus.ERROR ) )
      message = "Fehler bei der Parameterberechnung";
    else
      message = "unbekannter status";

    return new MultiStatus( pluginId, -1, multiStatus.getChildren(), message, null );
  }

  private QIntervallResult calculateFlowRel( final TuhhCalculation calculation, final IFlowRelation1D flowRel, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    if( flowRel instanceof ITeschkeFlowRelation )
    {
      return runCalculation( calculation, flowRel, new IProfil[] { ((ITeschkeFlowRelation) flowRel).getProfile().getProfil() }, monitor );
    }
    else if( flowRel instanceof IBuildingFlowRelation )
    {
      // find element and nodes
      final IBuildingFlowRelation building = (IBuildingFlowRelation) flowRel;

      final IFE1D2DNode upstreamNode = FlowRelationUtilitites.findNeighborNode( building, m_discModel, true );
      final IFE1D2DNode downstreamNode = FlowRelationUtilitites.findNeighborNode( building, m_discModel, false );

      final IFlowRelationship upRel = m_flowRelModel.findFlowrelationship( upstreamNode.getPoint().getPosition(), 0.01 );
      final IFlowRelationship downRel = m_flowRelModel.findFlowrelationship( downstreamNode.getPoint().getPosition(), 0.01 );

      final ITeschkeFlowRelation tUp = (ITeschkeFlowRelation) upRel;
      final ITeschkeFlowRelation tDown = (ITeschkeFlowRelation) downRel;

      // find adjacent teschke parameters
      final IProfil downStreamProfil = tDown.getProfile().getProfil();
      final IProfil buildingProfil = building.getProfile().getProfil();
      final IProfil upStreamProfil = tUp.getProfile().getProfil();

      return runCalculation( calculation, flowRel, new IProfil[] { downStreamProfil, buildingProfil, upStreamProfil }, monitor );
    }

    throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Parameterberechnung nicht möglich für: " + flowRel + " (unbekannter Typ)", null ) );
  }

  private QIntervallResult runCalculation( final TuhhCalculation templateCalculation, final IFlowRelation1D flowRel, final IProfil[] profiles, final IProgressMonitor monitor ) throws InvocationTargetException
  {
    m_running = true;

    File tmpDir = null;
    try
    {
      tmpDir = SimulationUtilitites.createSimulationTmpDir( "" + System.currentTimeMillis() );

      final TuhhCalculation calculation = createCalculation( templateCalculation, profiles, flowRel.getFeature().getWorkspace().getFeatureProviderFactory() );

      // Prepare wspm model
      final File modelFile = new File( tmpDir, "modell.gml" );
      final GMLWorkspace calcWorkspace = calculation.getFeature().getWorkspace();
      GmlSerializer.serializeWorkspace( modelFile, calcWorkspace, Charset.defaultCharset().name() );

      // prepare calcjob
      final PipedInputStream pip = new PipedInputStream();
      final PrintStream outStream = new PrintStream( new PipedOutputStream( pip ) );

      final Thread thread = new Thread()
      {
        /**
         * @see java.lang.Thread#run()
         */
        @Override
        public void run( )
        {
          BufferedReader bis = new BufferedReader( new InputStreamReader( pip ) );

          while( getRunning() )
          {
            try
            {
              String readLine = bis.readLine();
              if( readLine == null )
                break;

              m_simulationMonitor.setMessage( readLine );
            }
            catch( IOException e )
            {
              e.printStackTrace();
            }
          }

          super.run();
        }
      };
      thread.start();

      final WspmTuhhCalcJob wspmTuhhCalcJob = new WspmTuhhCalcJob( outStream );
      final DefaultSimulationDataProvider inputProvider = new DefaultSimulationDataProvider();
      inputProvider.put( WspmTuhhCalcJob.INPUT_MODELL_GML, modelFile.toURI().toURL() );
      inputProvider.put( WspmTuhhCalcJob.INPUT_CALC_PATH, new GMLXPath( calculation.getFeature() ).toString() );
      // eps-thinning is big, as we do not need the tin result and bigger is faster
      inputProvider.put( WspmTuhhCalcJob.INPUT_EPS_THINNING, "100.0" );

      final DefaultSimulationResultEater resultEater = new DefaultSimulationResultEater();
      final NullSimulationMonitorExtension simMonitor = new NullSimulationMonitorExtension( monitor );

      wspmTuhhCalcJob.run( tmpDir, inputProvider, resultEater, simMonitor );

      if( simMonitor.getFinishStatus() != IStatus.OK )
        throw new CoreException( StatusUtilities.createStatus( simMonitor.getFinishStatus(), simMonitor.getFinishText(), null ) );

      // read interval results and remember them
      final File qintervallFile = (File) resultEater.getResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT );
      final GMLWorkspace qresultsWorkspace = GmlSerializer.createGMLWorkspace( qintervallFile, null, true, calcWorkspace.getFeatureProviderFactory() );
      final QIntervallResultCollection qResultCollection = new QIntervallResultCollection( qresultsWorkspace.getRootFeature() );

      final List< ? > resultList = qResultCollection.getQResultFeatures();
      for( final Object o : resultList )
      {
        final QIntervallResult qresult = new QIntervallResult( FeatureHelper.getFeature( qresultsWorkspace, o ) );
        final BigDecimal station = flowRel.getStation();
        if( ObjectUtils.equals( station, qresult.getStation() ) )
          return qresult;
      }

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Kein Ergebnis gefunden", null ) );
    }
    catch( final GMLSchemaException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final GmlSerializeException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final SimulationException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final GMLXPathException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      SimulationUtilitites.clearTmpDir( tmpDir );

      m_running = false;
    }
  }

  protected boolean getRunning( )
  {
    return m_running;
  }

  private TuhhCalculation createCalculation( final TuhhCalculation template, final IProfil[] profiles, final IFeatureProviderFactory factory ) throws GMLSchemaException, InvocationTargetException
  {
    // Create empty project with one calculation and one reach
    final TuhhWspmProject project = TuhhWspmProject.create( null, factory );
    final TuhhCalculation calculation = project.createReibConstCalculation();
    final boolean direction = true; // TODO: depends on building stuff
    final WspmWaterBody waterBody = project.createWaterBody( "someWater", direction );
    project.createNewReach( waterBody.getName(), direction );
    final TuhhReach reach = TuhhWspmProject.createNewReachForWaterBody( waterBody );
    reach.setName( "someReach" );

    // Add Profiles to reach
    double minStation = Double.MAX_VALUE;
    double maxStation = Double.MIN_VALUE;
    for( final IProfil profil : profiles )
    {
      final double station = profil.getStation();

      final WspmProfile profile = waterBody.createNewProfile();
      ProfileFeatureFactory.toFeature( profil, profile.getFeature() );
      reach.createProfileSegment( profile, station );

      minStation = Math.min( minStation, station );
      maxStation = Math.max( maxStation, station );
    }

    // Initialize calculation
    calculation.setCalcCreation( "fe1d2d", new Date() );
    calculation.setDescription( "Recalculation of 1d-parameters" );
    calculation.setName( "1dparameters" );
    calculation.setReachRef( reach );
    calculation.setSubReachDef( minStation - 1, maxStation + 1 );

    /* Copy control parameters from template */
    calculation.setVersion( template.getVersion() );
    calculation.setQRange( template.getMinQ(), template.getMaxQ(), template.getQStep() );
    calculation.setWaterlevelParameters( template.getIterationType(), template.getVerzoegerungsverlust(), template.getReibungsverlust(), true, true );
    calculation.setStartSlope( template.getStartSlope() );

    final PolynomeProperties polynomeProperties = calculation.getPolynomeProperties();
    final PolynomeProperties templateProperties = template.getPolynomeProperties();
    polynomeProperties.setAlphaLimit( templateProperties.getAlphaLimit() );
    polynomeProperties.setAlphaSlope( templateProperties.getAlphaSlope() );
    polynomeProperties.setAreaSlope( templateProperties.getAreaSlope() );
    polynomeProperties.setRunoffSlope( templateProperties.getRunoffSlope() );
    polynomeProperties.setDeegree( templateProperties.getDeegree() );
    polynomeProperties.setIgnoreOutlier( templateProperties.getIgnoreOutlier() );
    polynomeProperties.setTripleForAll( templateProperties.getTripleForAll() );
    polynomeProperties.setTripleMode( templateProperties.getTripleMode() );
    polynomeProperties.setWeightSplinePoint( templateProperties.getWeightSplinePoint() );

    return calculation;
  }

  public void applyResults( ) throws Exception
  {
    for( final Entry<IFlowRelation1D, QIntervallResult> entry : m_results.entrySet() )
    {
      final IFlowRelation1D flowRelation1D = entry.getKey();
      final QIntervallResult qresult = entry.getValue();

      if( flowRelation1D instanceof ITeschkeFlowRelation )
        copyTeschkeData( (ITeschkeFlowRelation) flowRelation1D, qresult );
      else
        copyBuildingData( (IBuildingFlowRelation) flowRelation1D, qresult );
    }
  }

  public static void copyTeschkeData( final ITeschkeFlowRelation flowRel, final QIntervallResult qresult ) throws Exception
  {
    final Feature flowRelParentFeature = flowRel.getFeature();
    final GMLWorkspace flowRelworkspace = flowRelParentFeature.getWorkspace();
    final IFeatureType flowRelFT = flowRelworkspace.getGMLSchema().getFeatureType( ITeschkeFlowRelation.QNAME );
    final IRelationType flowRelObsRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POINTSOBSERVATION );
    final IRelationType flowRelPolynomeRelation = (IRelationType) flowRelFT.getProperty( ITeschkeFlowRelation.QNAME_PROP_POLYNOMES );

    /* clone observation */
    final Feature pointObsFeature = (Feature) qresult.getFeature().getProperty( QIntervallResult.QNAME_P_QIntervallResult_pointsMember );
    if( pointObsFeature != null )
      FeatureHelper.cloneFeature( flowRel.getFeature(), flowRelObsRelation, pointObsFeature );

    /* clone polynomials */
    flowRel.getPolynomials().clear(); // clear polynomials in case, the relation already existed
    final List< ? > polynomeFeatures = qresult.getPolynomialFeatures();
    for( final Object object : polynomeFeatures )
    {
      final GMLWorkspace qresultsWorkspace = qresult.getFeature().getWorkspace();
      final Feature polynomeFeature = FeatureHelper.getFeature( qresultsWorkspace, object );
      if( polynomeFeature != null )
        FeatureHelper.cloneFeature( flowRel.getFeature(), flowRelPolynomeRelation, polynomeFeature );
    }
  }

  public static void copyBuildingData( final IBuildingFlowRelation buildingRelation, final QIntervallResult qresult )
  {
    /* copy building parameter from one observation to the other */
    final IObservation<TupleResult> buildingObservation = buildingRelation.getBuildingObservation();
    final IObservation<TupleResult> qresultBuildingObs = qresult.getBuildingObservation( false );
    final TupleResult qresultResult = qresultBuildingObs.getResult();
    final TupleResult buildingResult = buildingObservation.getResult();
    buildingResult.clear(); // Clear in case if relation already existed
    final Map<String, String> componentMap = new HashMap<String, String>();
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    componentMap.put( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    TupleResultUtilities.copyValues( qresultResult, buildingResult, componentMap );
    buildingRelation.setBuildingObservation( buildingObservation );
  }

}
