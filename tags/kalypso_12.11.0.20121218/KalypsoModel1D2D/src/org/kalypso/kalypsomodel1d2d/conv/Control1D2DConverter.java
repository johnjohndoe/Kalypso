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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Pattern;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation.KIND;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D.KIND2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Gernot Belger
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Control1D2DConverter
{
  /** Directory name for RMA∑Kalypso result files (Output...) files */
  public static final String RESULT_DIR_NAME = "./"; //$NON-NLS-1$

  /** Base filename name for RMA∑Kalypso result files (Output...) files */
  public static final String RESULT_FILE_BASE = "Output"; //$NON-NLS-1$

  private final List<IBoundaryCondition> m_unitBoundaryConditions = new ArrayList<>();

  private final INativeIDProvider m_nativeIDProvider;

  private static IdMap m_staticInnerLinesIDProvider = null;

  private final LinkedHashMap<Integer, IBoundaryCondition> m_WQboundaryConditionsIDProvider = new LinkedHashMap<>();

  private final BuildingIDProvider m_buildingProvider;

  private final IControlModel1D2D m_controlModel;

  private final IRoughnessClsCollection m_roughnessModel;

  private final Map<String, TupleResultIndex> m_BCTupleResultIndexCache = new HashMap<>();

  private boolean m_isBCTupleResultIndexCached = false;

  private final IGeoLog m_log;

  private List<Date> m_listDateSteps = null;

  private final ICalculationUnit m_calculationUnit;

  private boolean m_boolPrintWindLineDone;

  public Control1D2DConverter( final IControlModel1D2D controlModel, final ICalculationUnit calculationUnit, final IFlowRelationshipModel flowModel, final IRoughnessClsCollection roughnessMmodel, final INativeIDProvider idProvider, final BuildingIDProvider buildingProvider, final IGeoLog log )
  {
    m_controlModel = controlModel;
    m_calculationUnit = calculationUnit;
    m_roughnessModel = roughnessMmodel;
    m_nativeIDProvider = idProvider;
    m_buildingProvider = buildingProvider;
    m_log = log;
    m_boolPrintWindLineDone = false;

    // only instantiate this provider once, we require global ids over several runs
    if( m_staticInnerLinesIDProvider == null )
      m_staticInnerLinesIDProvider = new IdMap();

    /* Initialize boundary conditions */
    final String calculationUnitID = m_calculationUnit.getId();
    for( final IFlowRelationship relationship : flowModel.getFlowRelationsShips() )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition)relationship;
        if( boundaryCondition.isMemberOf( calculationUnitID ) )
          m_unitBoundaryConditions.add( boundaryCondition );
      }
    }
    m_listDateSteps = new ArrayList<>();
  }

  public void writeR10File( final File outputFile ) throws CoreException, IOException
  {
    try( OutputStream outputStream = new BufferedOutputStream( new FileOutputStream( outputFile ) ) )
    {
      writeR10File( outputStream );
    }
  }

  private void writeR10File( final OutputStream outputStream ) throws CoreException, IOException
  {
    // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
    // so no local parameter for each format is needed any more .
    try( Formatter formatter = new Formatter( outputStream, Charset.defaultCharset().name(), Locale.US ) )
    {
      writeR10File( formatter );
      FormatterUtils.checkIoException( formatter );
    }
  }

  private void writeR10File( final Formatter formatter ) throws CoreException, IOException
  {
    writeR10ControlDataBlock( formatter );
    writeR10PropertiesDataBlock( formatter );
    writeR10ContinuityLineDataBlock( formatter );
    writeR10TimeStepDataBlock( formatter );
  }

  /**
   * Writes the Control Data Block of the RMA∑Kalypso controlFile (*.R10) into the PrintWriter
   */
  private void writeR10ControlDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    // This value is not used at all during the steady calculation,
    // so if user did not selected unsteady, instead of first time step we will take current date/time (see method
    // getFirstTimeStep())
    final Date firstDate = getFirstTimeStep();
    final Calendar calendarForFirstTimeStep = Calendar.getInstance();
    calendarForFirstTimeStep.setTime( firstDate );

    /* FILES DATA BLOCK */
    formatter.format( "OUTFIL  %s%s%n", RESULT_DIR_NAME, RESULT_FILE_BASE ); //$NON-NLS-1$
    formatter.format( "INKALYPSmodel.2d%n" ); //$NON-NLS-1$
    formatter.format( "CONTROL A %4d 2d 0%n", m_controlModel.getIaccyc() ); //$NON-NLS-1$
    if( m_controlModel.getRestart() )
      formatter.format( "RESTART%n" ); //$NON-NLS-1$

    /* Write W/Q file, even if it is empty. */
    formatter.format( "STFLFIL %s%n", ISimulation1D2DConstants.BC_WQ_File ); //$NON-NLS-1$
    /* We always write a building file, even if it is empty. */
    formatter.format( "INCSTR  %s%n", ISimulation1D2DConstants.BUILDING_File ); //$NON-NLS-1$

    /* We only write a wind field, if we want to consider wind */
    if( m_controlModel.getHasWindDrag() )
    {
      formatter.format( "AWINDIN3%s%n", ISimulation1D2DConstants.WIND_RMA10_File ); //$NON-NLS-1$
      formatter.format( "INSRCORD%s%n", ISimulation1D2DConstants.WIND_RMA10_COORDS_File ); //$NON-NLS-1$
    }

    /* We always write a building file, even if it is empty. */
    //    formatter.format( "INSSTR  %s%n", ISimulation1D2DConstants.SURFACE_TRACTTION_File ); //$NON-NLS-1$

    formatter.format( "ENDFIL%n" ); //$NON-NLS-1$

    // TODO: @Nico -> hard coded defaults could be got from the schema, how?

    // default value: 1500
    if( m_controlModel.getMFW() != 1500 )
      formatter.format( "MAXFRONT %8d%n", m_controlModel.getMFW() ); //$NON-NLS-1$

    // default value: 10.000.000
    if( m_controlModel.getBUFFSIZ() != 20000000 )
      formatter.format( "BUFFSIZL%16d%n", m_controlModel.getBUFFSIZ() ); //$NON-NLS-1$

    /* CONTROL DATA BLOCK */
    final String controlModelName = m_controlModel.getName();
    final String projectName = controlModelName == null ? Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.8" ) : controlModelName; //$NON-NLS-1$
    formatter.format( "TI      %30s%n", projectName ); //$NON-NLS-1$

    // // C0
    Double tbfact = 0.0;
    if( m_controlModel.getIEDSW() == 2 || m_controlModel.getIEDSW() == 13 || m_controlModel.getIEDSW() == 14 )
    {
      tbfact = m_controlModel.getTBFACT();
    }
    else
    {
      tbfact = m_controlModel.getTBFACT_ESCUDIER();
    }
    final Object[] c0Props = new Object[] { 0, m_controlModel.getIDNOPT(), calendarForFirstTimeStep.get( Calendar.YEAR ), calendarForFirstTimeStep.get( Calendar.DAY_OF_YEAR ),
        getTimeInPercentage( calendarForFirstTimeStep ), m_controlModel.getIEDSW(), tbfact, m_controlModel.getTBMIN(), 1 };
    formatter.format( "C0%14d%8d%8d%8d%8.2f%8d%8.3f%8.2f%8d%n", c0Props ); //$NON-NLS-1$

    // C1
    formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 0, 0, 0, 0, 0, 0, 1 ); //$NON-NLS-1$

    // C2
    // TODO: P_BOTTOM still not implemented, ask Nico
    formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%8.2f%n", m_controlModel.getOMEGA(), m_controlModel.getELEV(), 1.0, 1.0, 1.0, 1, m_controlModel.get_P_BOTTOM() ); //$NON-NLS-1$

    // C3
    formatter.format( "C3%14.3f%8.3f%8.3f%8.1f%8.3f%8.3f%8.3f%n", 1.0, 1.0, m_controlModel.getUNOM(), m_controlModel.getUDIR(), m_controlModel.getHMIN(), m_controlModel.getDSET(), m_controlModel.getDSETD() ); //$NON-NLS-1$

    // C4
    final boolean artImpulsstromBeiwert = m_controlModel.getBeient();
    formatter.format( "C4%14.1f%8.1f%8.1f%40d%n", 0.0, 20.0, 0.0, artImpulsstromBeiwert ? 1 : 0 ); //$NON-NLS-1$

    // C5
    formatter.format( "C5%14d%8d%16d%8d%8d%8d%8d%8d%8d%n", m_controlModel.getNITI(), m_controlModel.getNITN(), m_controlModel.getNCYC(), 0, 0, 1, 0, 1, 1 ); //$NON-NLS-1$

    // C6
    if( m_controlModel.getIcpu() != 0 )
      formatter.format( "C6%14d%8d%8d%8d%8.5f%8d%n", 0, 0, 0, m_controlModel.getIcpu(), m_controlModel.getHasWindDrag() ? m_controlModel.getChi() : 0.0, m_controlModel.getMarshFrictionDistr() ); //$NON-NLS-1$
    // C7
    formatter.format( "C7%14d%8d%8d%n", 0, 0, m_controlModel.getPercentCheck() ? 1 : 0 ); //$NON-NLS-1$

    // CV
    formatter.format( "CV%14.2g%8.2g%8.2g%8.2g%8.2g%16d%8.2f%n", m_controlModel.getCONV_1(), m_controlModel.getCONV_2(), m_controlModel.getCONV_3(), 0.05, 0.05, m_controlModel.getIDRPT(), m_controlModel.getDRFACT() ); //$NON-NLS-1$

    // IOP line deleted because Nico said that it is an ugly, baaad line... :)
    // (IOP has something to do with reordering, and this is not working properly with 1D-2D coupling)
    // formatter.format( "IOP%n" );

    // VEGETA
    if( m_controlModel.getVegeta() )
      formatter.format( "VEGETA%n" ); //$NON-NLS-1$

    formatter.format( "ENERGY%n" ); //$NON-NLS-1$
    formatter.format( "KAL_BC%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * writes the Properties Data Block of the RMA∑Kalypso controlFile (*.R10) into the PrintWriter
   */
  private void writeR10PropertiesDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    for( final IRoughnessCls rClass : m_roughnessModel.getRoughnessClasses() )
    {
      final int roughnessAsciiID = m_nativeIDProvider.getConversionID( rClass );

      final int iedsw = m_controlModel.getIEDSW();
      Double[] eddy = rClass.getViscosities( iedsw ); // for empty tbmin* 1000 for all eddies
      eddy = checkViscosities( eddy );
      final Double ks = rClass.getKs();
      final Double axAy = rClass.getAxAy();
      final Double dp = rClass.getDp();

      if( ks == null || ks.isNaN() )
      {
        final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.7", rClass.getName() );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
      }

      final Double axayCorrected = axAy == null ? 0.0 : axAy;
      final Double dpCorrected = dp == null ? 0.0 : dp;

      writeEDBlock( formatter, roughnessAsciiID, eddy, ks, axayCorrected, dpCorrected );
    }

    final Map<Integer, IFlowRelationship> buildingMap = m_buildingProvider.getBuildingData();
    for( final Integer buildingID : buildingMap.keySet() )
      writeEDBlock( formatter, buildingID, 0.0, 0.0, 0.0, 0.0 );
  }

  private Double[] checkViscosities( final Double[] eddy )
  {
    final Double[] eddyRes = new Double[eddy.length];
    final Double lDoubleDefaultViscosity = m_controlModel.getTBMIN();

    for( int i = 0; i < eddy.length; i++ )
    {
      final Double double1 = eddy[i];
      if( double1.equals( 0.0 ) )
      {
        eddyRes[i] = lDoubleDefaultViscosity * 1000;
      }
      else
      {
        eddyRes[i] = double1;
      }
    }
    return eddyRes;
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final Double[] eddy, final Double ks, final Double axayCorrected, final Double dpCorrected ) throws IOException
  {
    if( eddy.length < 4 )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.17" ) ); //$NON-NLS-1$
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, eddy[0], eddy[1], eddy[2], eddy[3], -1.0, 1.0, 1.0 ); //$NON-NLS-1$
    formatter.format( "ED2%21.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, m_controlModel.getMarshFrictionFactor() ); //$NON-NLS-1$
    formatter.format( "ED4%21.5f%8.3f%8.3f%n", ks, axayCorrected, dpCorrected ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final double val, final Double ks, final Double axayCorrected, final Double dpCorrected ) throws IOException
  {
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, val, val, val, val, -1.0, 1.0, 1.0 ); //$NON-NLS-1$
    formatter.format( "ED2%21.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, m_controlModel.getMarshFrictionFactor() ); //$NON-NLS-1$
    formatter.format( "ED4%21.5f%8.3f%8.3f%n", ks, axayCorrected, dpCorrected ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * writes the Continuity Lines Data Block of the RMA∑Kalypso controlFile (*.R10) into the PrintWriter
   */
  private void writeR10ContinuityLineDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    final List<IFELine> continuityLines = m_calculationUnit.getContinuityLines();
    formatter.format( "SCL%9d%n", continuityLines.size() ); //$NON-NLS-1$
    for( final IFELine line : continuityLines )
    {
      final IFE1D2DNode[] nodes = line.getNodes();
      for( int i = 0; i < nodes.length; i++ )
      {
        final IFE1D2DNode node = nodes[i];
        final int nodeID = m_nativeIDProvider.getConversionID( node );
        if( nodeID == 0 )
        {
          final GM_Point position = node.getPoint();
          final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.11" ); //$NON-NLS-1$
          final IGeoStatus status = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, msg, position, null );
          throw new CoreException( status );
        }

        if( i == 0 )
          formatter.format( "CC1 %4d", m_nativeIDProvider.getConversionID( line ) ); //$NON-NLS-1$
        else if( i % 9 == 0 )
        {
          formatter.format( "%nCC2     " ); //$NON-NLS-1$
        }

        formatter.format( "%8d", nodeID ); //$NON-NLS-1$
      }

      // Why this "if" was here?
      // if( nodes.length % 9 != 0 )
      formatter.format( "%n" ); //$NON-NLS-1$

      // check if line is an inner boundary line (only for coupled 1d2d units and enabled coupled simulation)
      // this is the case if it is contained in two 2d calculation units of this coupled unit
      final ICalculationUnit parentCalculationUnit = m_controlModel.getCalculationUnit();
      if( parentCalculationUnit.getType() == TYPE.TYPE1D2D && ((ICalculationUnit1D2D)parentCalculationUnit).isCoupledSimulation() )
      {
        final ICalculationUnit1D2D calculationUnit1D2D = (ICalculationUnit1D2D)parentCalculationUnit;
        final IFeatureBindingCollection<ICalculationUnit> changedSubUnits = calculationUnit1D2D.getSubCalculationUnits();
        int in2DCalcUnitCount = 0;
        final IFE1D2DComplexElement< ? >[] containers = line.getLinkedElements();
        for( final Feature calcUnit : containers )
        {
          if( ((ICalculationUnit)calcUnit).getType() == TYPE.TYPE2D && changedSubUnits.contains( calcUnit ) )
          {
            in2DCalcUnitCount++;
          }
        }
        if( in2DCalcUnitCount == 2 )
          formatter.format( "ICL %4d %4d\n", m_staticInnerLinesIDProvider.getOrAdd( line.getId() ), m_nativeIDProvider.getConversionID( line ) ); //$NON-NLS-1$
      }

      FormatterUtils.checkIoException( formatter );
    }
    formatter.format( "ECL%n" ); //$NON-NLS-1$

    if( m_controlModel.getIDNOPT() != 0 && m_controlModel.getIDNOPT() != -1 )
      formatter.format( "MP %21.3f%8.3f%8.5f%8.2f%n", m_controlModel.getFixedMarshBottom() ? 0.0 : m_controlModel.getAC1(), m_controlModel.getAC2(), m_controlModel.getAC3(), m_controlModel.getFixedMarshBottom() ? m_controlModel.getAC4() : 0.0 ); //$NON-NLS-1$

    formatter.format( "ENDGEO%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * writes the Timestep Data Block of the RMA∑Kalypso controlFile (*.R10) into the PrintWriter
   */
  private void writeR10TimeStepDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    final IObservation<TupleResult> observation = m_controlModel.getTimeSteps();
    final TupleResult result = observation.getResult();
    final IComponent[] components = result.getComponents();

    final IComponent componentTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final IComponent componentRelaxationsFaktor = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR );

    /* Steady state, just one block for the Starting Date. */
    final int niti = m_controlModel.getNITI();

    final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.33" ); //$NON-NLS-1$

    // Double uRValSteady = m_controlModel.get_RelaxationsFactor();
    String uRValSteady = m_controlModel.get_RelaxationsFactor();
    if( uRValSteady == null || uRValSteady == "" ) //$NON-NLS-1$
    {
      if( m_controlModel.isSteadySelected() )
      {
        final String errMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.34" );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, errMsg ) );
      }
      else
        // Not used anyway (1.0 should be default value)
        uRValSteady = "1.0"; //$NON-NLS-1$
    }

    writeTimeStep( formatter, msg, null, null, uRValSteady, niti );

    if( m_controlModel.isUnsteadySelected() )
    {
      /* Sort records by time */
      final TupleResultIndex index = new TupleResultIndex( result, componentTime );
      final Iterator<IRecord> iterator = index.getIterator();
      if( !iterator.hasNext() )
      {
        final String errMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.35" );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, errMsg ) );
      }

      final IRecord firstRecord = iterator.next();

      final int nitn = m_controlModel.getNITN();

      if( nitn > 0 )
      {
        /* Unsteady state: a block for each time step */

        // TODO: check for right time zone
        // // As we are using UTC for all our timeseries, this is the timezone that should be used here
        // final TimeZone DEFAULT_TIMEZONE = TimeZone.getTimeZone( "UTC" );
        final TupleResult owner = firstRecord.getOwner();
        final int indexTime = owner.indexOfComponent( componentTime );
        final int indexRelaxationsFaktor = owner.indexOfComponent( componentRelaxationsFaktor );

        final XMLGregorianCalendar cal = (XMLGregorianCalendar)firstRecord.getValue( indexTime );
        Calendar lastStepCal = cal.toGregorianCalendar();
        m_listDateSteps.add( DateUtilities.toDate( cal ) );

        int stepCount = 1;
        for( ; iterator.hasNext(); stepCount++ )
        {
          final IRecord record = iterator.next();

          // final float uRVal = ((BigDecimal) record.getValue( indexRelaxationsFaktor )).floatValue();
          final String uRVal = ((String)record.getValue( indexRelaxationsFaktor ));
          final XMLGregorianCalendar stepXMLGrCal = (XMLGregorianCalendar)record.getValue( indexTime );
          final Calendar stepCal = stepXMLGrCal.toGregorianCalendar();
          m_listDateSteps.add( DateUtilities.toDate( stepXMLGrCal ) );

          final String unsteadyMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.36", stepCount ); //$NON-NLS-1$
          writeTimeStep( formatter, unsteadyMsg, stepCal, lastStepCal, uRVal, nitn );

          lastStepCal = stepCal;

        }
        if( m_controlModel.getIaccyc() > stepCount )
        {
          final String errMsg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.12", stepCount, m_controlModel.getIaccyc() ); //$NON-NLS-1$
          throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, errMsg ) );
        }
      }
    }
  }

  /**
   * writes out the time steps. <BR>
   * IMPORTANT: we write the dates in the time zone according to the kalypso-preferences!
   */
  private void writeTimeStep( final Formatter formatter, final String message, final Calendar calculationStep, final Calendar lastStepCal, final String uRVal, final int niti ) throws CoreException, IOException
  {
    final String dashes = StringUtils.repeat( "-", message.length() ); //$NON-NLS-1$
    formatter.format( "com %s%n", dashes ); //$NON-NLS-1$
    formatter.format( "com %s%n", message ); //$NON-NLS-1$
    formatter.format( "com %s%n", dashes ); //$NON-NLS-1$

    final long timeStepInterval;
    final double timeStepMinutes;
    final int year;
    final int dayOfYear;
    final double ihre;

    final TimeZone displayTimeZone = KalypsoCorePlugin.getDefault().getTimeZone();
    Calendar kalypsoCalendarStep = Calendar.getInstance( displayTimeZone );

    // unsteady
    if( calculationStep != null )
    {
      // REMARK: we write the date in the kalypso-preferences time zone!
      kalypsoCalendarStep.setTime( calculationStep.getTime() );
      dayOfYear = kalypsoCalendarStep.get( Calendar.DAY_OF_YEAR );
      year = kalypsoCalendarStep.get( Calendar.YEAR );

      // REMARK: we write the date in the kalypso-preferences time zone!
      final Calendar kalypsoCallendarPreviousStep = Calendar.getInstance( displayTimeZone );
      kalypsoCallendarPreviousStep.setTime( lastStepCal.getTime() );

      timeStepInterval = kalypsoCalendarStep.getTimeInMillis() - kalypsoCallendarPreviousStep.getTimeInMillis();

      // FIXME: should never happen, kalypso does not allow summertime timezones any more
      // if timeStepInterval is zero, step will be ignored
      // (this will happen on summertime to wintertime transition)
      if( timeStepInterval == 0 )
      {
        formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.14" ) ); //$NON-NLS-1$
        return;
      }

      timeStepMinutes = (double)timeStepInterval / (60 * 1000);
      ihre = getTimeInPercentage( kalypsoCalendarStep );
    }
    // steady don¥t need startdate
    else
    {
      kalypsoCalendarStep = null;
      dayOfYear = 0;
      year = 0;
      timeStepMinutes = 0;
      ihre = 0;
    }

    formatter.format( "DT MIN  %e%8d%8d%8.2f", timeStepMinutes, year, dayOfYear, ihre ); //$NON-NLS-1$

    // BC lines
    if( niti == 0 )
      formatter.format( "%nBC%n" ); //$NON-NLS-1$
    else
      formatBC( formatter, uRVal, niti );

    // order is important, first QC than HC, SQC and EFE
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

    FormatterUtils.checkIoException( formatter );

    for( final Map.Entry<Integer, IFlowRelationship> buildingData : m_buildingProvider.getBuildingData().entrySet() )
    {
      final Integer buildingID = buildingData.getKey();
      final IFlowRelationship building = buildingData.getValue();
      final int buildingKind = 10;
      int lIntDirection = 0;
      if( building instanceof IBuildingFlowRelation )
      {
        if( ((IBuildingFlowRelation)building).getKind() != KIND.TABULAR )
        {
          final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.43", ((IBuildingFlowRelation)building).getKind() );//$NON-NLS-1$
          throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
        }
        lIntDirection = ((IBuildingFlowRelation)building).getDirection();
      }
      else if( building instanceof IBuildingFlowRelation2D )
      {
        if( ((IBuildingFlowRelation2D)building).getKind() != KIND2D.TABULAR )
        {
          final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.43", ((IBuildingFlowRelation)building).getKind() );//$NON-NLS-1$
          throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
        }
        lIntDirection = ((IBuildingFlowRelation2D)building).getDirection();
      }
      final double direction = Math.toRadians( lIntDirection );
      formatter.format( "FC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", buildingID, buildingKind, 0.0, 0.0, 0.0, 0.0, direction ); //$NON-NLS-1$

      FormatterUtils.checkIoException( formatter );
    }

    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D );

    // add other conti lines types as well (buildings)?
    if( m_controlModel.getHasWindDrag() && !m_boolPrintWindLineDone )
    {
      formatter.format( "WVA          1.0     1.0       1%n" ); //$NON-NLS-1$
      m_boolPrintWindLineDone = true;
    }
    formatter.format( "ENDSTEP %s%n", message ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * For big timeseries, creating TupleResultIndex for every time step is extremely slow. Cacheing those indexes for
   * performances reason is necessary.
   */
  private void cacheBCTupleResultIndexes( )
  {
    m_BCTupleResultIndexCache.clear();
    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_ELEMENT1D2D ) || boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        final IObservation<TupleResult> obs = boundaryCondition.getObservation();
        final TupleResult obsResult = obs.getResult();
        final IComponent timeComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );

        // timeComponent is null for W/Q BCs
        if( timeComponent != null )
          m_BCTupleResultIndexCache.put( boundaryCondition.getId(), new TupleResultIndex( obsResult, timeComponent ) );
      }
    }
    m_isBCTupleResultIndexCached = true;
  }

  /**
   * Formats the lines for one type of boundary condition (QC or HC or ... ).
   */
  private void formatBoundCondLines( final Formatter formatter, final Calendar stepCal, final String bcAbscissaComponentType, final String bcOrdinateComponentType ) throws IOException, CoreException
  {
    if( !m_isBCTupleResultIndexCached )
      cacheBCTupleResultIndexes();

    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_ELEMENT1D2D ) || boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        final int ordinal = m_nativeIDProvider.getConversionID( boundaryCondition.getParentElementID() );
        final IObservation<TupleResult> obs = boundaryCondition.getObservation();
        final TupleResult obsResult = obs.getResult();
        final IComponent abscissaComponent = TupleResultUtilities.findComponentById( obsResult, bcAbscissaComponentType );
        IComponent ordinateComponent = TupleResultUtilities.findComponentById( obsResult, bcOrdinateComponentType );

        // TODO: type of absolute element inflow must also become the type specificDischarge
        if( ordinateComponent == null
            && (bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D ) || bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D ))
            && (boundaryCondition.isAbsolute() != null) )
          ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

        if( abscissaComponent != null && ordinateComponent != null )
        {
          final double stepValue = findStepValue( stepCal, bcAbscissaComponentType, bcOrdinateComponentType, boundaryCondition, ordinateComponent );

          if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE )
              && (boundaryCondition.isAbsolute() == null) )
          {
            // TODO: @Nico This is very ugly, but it works for the moment!! It should be introduced to GMLLoader checks
            final Boolean hasDirectionProperty = boundaryCondition.hasDirection();
            if( hasDirectionProperty == null )
              boundaryCondition.setHasDirection( true );
            else if( hasDirectionProperty == false )
              boundaryCondition.setHasDirection( true );

            final double theta = Math.toRadians( boundaryCondition.getDirection().doubleValue() );
            formatter.format( "QC *%12d%8d%16.3f%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, stepValue, theta, 0.000, 20.000, 0.000 ); //$NON-NLS-1$
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ) )
          {
            // TODO: @Nico This is very ugly, but it works for the moment!! It should be introduced to GMLLoader checks
            final Boolean hasDirectionProperty = boundaryCondition.hasDirection();
            if( hasDirectionProperty == false || hasDirectionProperty == null )
              boundaryCondition.setHasDirection( true );
            final double theta = Math.toRadians( boundaryCondition.getDirection().doubleValue() );
            formatter.format( "SQC%13d%40.4f%8.3f%8.3f%8.3f%n", ordinal, theta, 0.000, 20.000, 0.000 ); //$NON-NLS-1$
            m_WQboundaryConditionsIDProvider.put( ordinal, boundaryCondition );
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) )
          {
            formatter.format( "HC%14d%8d%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, stepValue, 0.0, 20.000, 0.0 ); //$NON-NLS-1$
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D )
              && boundaryCondition.getParentElementID().startsWith( "Element1D" ) ) //$NON-NLS-1$
          {
            // 1D Element
            final Boolean isAbsoluteProperty = boundaryCondition.isAbsolute();
            final Boolean hasDirectionProperty = boundaryCondition.hasDirection();

            // TODO: @Nico This is very ugly, but it works for the moment!! It should be introduced to GMLLoader checks
            if( hasDirectionProperty == null )
            {
              boundaryCondition.setHasDirection( false );
              boundaryCondition.setDirection( new BigInteger( "0" ) ); //$NON-NLS-1$
            }
            if( boundaryCondition.hasDirection() && boundaryCondition.getDirection() == null )
              boundaryCondition.setDirection( new BigInteger( "0" ) ); //$NON-NLS-1$

            final int isAbsolute = (isAbsoluteProperty != null && isAbsoluteProperty.booleanValue()) ? 1 : 0;

            if( boundaryCondition.hasDirection() )
              formatter.format( "EFE%13d%8d%8d%8.3f%8.4f%8.4f%8.4f%8.4f%8.4f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 20.000, 0.0, boundaryCondition.getInflowVelocity(), Math.toRadians( boundaryCondition.getDirection().doubleValue() ) ); //$NON-NLS-1$
            else
              formatter.format( "EFE%13d%8d%8d%8.3f%8.4f%8.4f%8.4f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 20.000, 0.0 ); //$NON-NLS-1$
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D )
              && !boundaryCondition.getParentElementID().startsWith( "Element1D" ) ) //$NON-NLS-1$
          {
            // 2D Element
            final Boolean isAbsoluteProperty = boundaryCondition.isAbsolute();
            final Boolean hasDirectionProperty = boundaryCondition.hasDirection();

            // TODO: @Nico This is very ugly, but it works for the moment!! It should be introduced to GMLLoader checks
            if( hasDirectionProperty == null )
            {
              boundaryCondition.setHasDirection( false );
              boundaryCondition.setDirection( new BigInteger( "0" ) ); //$NON-NLS-1$
            }
            if( boundaryCondition.hasDirection() && boundaryCondition.getDirection() == null )
              boundaryCondition.setDirection( new BigInteger( "0" ) ); //$NON-NLS-1$

            final int isAbsolute = (isAbsoluteProperty != null && isAbsoluteProperty.booleanValue()) ? 1 : 0;
            if( boundaryCondition.hasDirection() )
            {
              double infVel = 0.001;
              try
              {
                infVel = boundaryCondition.getInflowVelocity();
              }
              catch( final Exception e )
              {
                // FIXME: grr, error handling -> we need to stop calculation in such a case!
                m_log.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_PRE, Messages.getString( "Control1D2DConverter.1" ), boundaryCondition.getLocation(), e ); //$NON-NLS-1$
              }
              formatter.format( "EFE%13d%8d%8d%8.3f%8.4f%8.4f%8.4f%8.4f%8.4f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 20.000, 0.0, infVel, Math.toRadians( boundaryCondition.getDirection().doubleValue() ) ); //$NON-NLS-1$
            }
            else
              formatter.format( "EFE%13d%8d%8d%8.3f%8.4f%8.4f%8.4f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 20.000, 0.0 ); //$NON-NLS-1$
          }
        }
      }

      FormatterUtils.checkIoException( formatter );
    }
  }

  private double findStepValue( final Calendar stepCal, final String bcAbscissaComponentType, final String bcOrdinateComponentType, final IBoundaryCondition boundaryCondition, final IComponent ordinateComponent ) throws CoreException
  {
    if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ) )
    {
      // Its a w/Q Boundary Condition, we do not need a step value...
      return Double.NaN;
    }

    if( stepCal != null )
    {
      final TupleResultIndex tupleResultIndex = m_BCTupleResultIndexCache.get( boundaryCondition.getId() );
      final Number result = (Number)tupleResultIndex.getValue( ordinateComponent, stepCal.getTime() );
      if( result == null || Double.isNaN( result.doubleValue() ) )
      {
        final SimpleDateFormat df = new SimpleDateFormat( ISimulation1D2DConstants.TIMESTEP_DISPLAY_FORMAT );
        final String stepText = df.format( stepCal.getTime() );
        final String bcLabel = boundaryCondition.getName();
        final String message = String.format( Messages.getString( "Control1D2DConverter.0" ), stepText, bcLabel ); //$NON-NLS-1$
        final GM_Object location = boundaryCondition.getPosition();
        final IGeoStatus error = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, message, location, null );
        throw new CoreException( error );
      }

      return result.doubleValue();
    }
    else
    {
      try
      {
        return Double.parseDouble( boundaryCondition.getStationaryCondition() );
      }
      catch( final Exception e )
      {
        final String bcLabel = boundaryCondition.getName();
        final String message = String.format( Messages.getString( "Control1D2DConverter.2" ), bcLabel ); //$NON-NLS-1$
        final GM_Object location = boundaryCondition.getPosition();
        final IGeoStatus error = m_log.log( IStatus.ERROR, ISimulation1D2DConstants.CODE_PRE, message, location, null );

        throw new CoreException( error );
      }
    }
  }

  // private void formatBC( final Formatter formatter, final float uRVal, final int nitn ) throws CoreException,
  // IOException
  private void formatBC( final Formatter formatter, final String uRVal, final int nitn ) throws CoreException, IOException
  {
    if( uRVal == null || uRVal == "" ) { //$NON-NLS-1$
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.6" );//$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
    }
    final List<Integer> lListFactors = getListParsedRelaxationFactor( uRVal, nitn );
    // final int buffVal = 10 - (int) (uRVal * 10);
    for( int j = 0; j < nitn; j++ )
    {
      if( j % 9 == 0 )
      {
        formatter.format( "%nBC%6s", " " ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      formatter.format( "%5d010", lListFactors.get( j ) ); //$NON-NLS-1$
    }
    formatter.format( "%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private List<Integer> getListParsedRelaxationFactor( final String pStrVal, final int pIntNumberIterations ) throws CoreException
  {
    final List<Integer> lListFactorsRes = new ArrayList<>();
    final List<Float> lListFactors = new ArrayList<>();
    if( !isLoopFomatedFactor( pStrVal.trim(), lListFactors, pIntNumberIterations ) )
    {
      if( !isSeparatedValuesFormatedFactor( pStrVal.trim(), lListFactors, pIntNumberIterations ) )
      {
        final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.6" );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
      }
    }

    for( int i = 0; i < pIntNumberIterations; ++i )
    {
      final int buffVal = 10 - (int)(lListFactors.get( i ) * 10);
      lListFactorsRes.add( buffVal );
    }

    return lListFactorsRes;
  }

  /**
   * checks if the given string value contains string of form "0.1 to 0.5" and if it is, parses it and expands it into
   * according to amount of steps values and put them into pListFactors as floats
   */
  private boolean isLoopFomatedFactor( final String pStrValue, final List<Float> pListFactors, final int pIntNumberIterations ) throws CoreException
  {
    final String lStrRegex = "[ \\t]*[0-9][.,][0-9][ \\t]*[tT][oO][ \\t]*[0-9][.,][0-9][ \\t]*"; //$NON-NLS-1$
    final String lStrToCheck = pStrValue.trim().toLowerCase();
    if( Pattern.matches( lStrRegex, lStrToCheck ) )
    {
      final int lIntIndexSep = lStrToCheck.indexOf( "to" ); //$NON-NLS-1$
      final float lFloatStart = getFloatFromSimpleFloatString( lStrToCheck.substring( 0, lIntIndexSep ).trim() );
      final float lFloatEnd = getFloatFromSimpleFloatString( lStrToCheck.substring( lIntIndexSep + 2 ).trim() );
      if( lFloatStart < 0.0 || lFloatStart > 1.0 || lFloatEnd < 0.0 || lFloatEnd > 1.0 )
      {
        final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.6" );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
      }
      final float lFloatInc = (float)((lFloatEnd - lFloatStart) / 0.1);
      final int lIntPer = (int)(pIntNumberIterations / (Math.abs( lFloatInc ) + 1));
      int lIntInc = 0;
      final float lFloatSignum = Math.signum( lFloatInc );
      for( int i = 1; i <= pIntNumberIterations; ++i )
      {
        pListFactors.add( (float)(lFloatStart + 0.1 * lIntInc * lFloatSignum) );
        if( (i) % lIntPer == 0 && pIntNumberIterations - i > Math.abs( lIntPer ) && lIntInc < Math.abs( lFloatInc ) )
        {
          lIntInc++;
        }
      }
      return true;
    }

    return false;
  }

  // /**
  // * checks if the given string value contains string of form "0.1 to 0.5" and if it is, parses it and expands it into
  // according to
  // * amount of steps values and put them into pListFactors as floats
  // *
  // * direct implementation of parsing the relaxation factor
  // */
  // private boolean isLoopFomatedFactor( String pStrValue, List<Float> pListFactors, int pIntNumberIterations ) throws
  // CoreException
  // {
  //    final String lStrRegex = "[ \\t]*[0-9][.,][0-9][ \\t]*[tT][oO][ \\t]*[0-9][.,][0-9][ \\t]*"; //$NON-NLS-1$
  // String lStrToCheck = pStrValue.trim().toLowerCase();
  // if( Pattern.matches( lStrRegex, lStrToCheck ) ){
  //      int lIntIndexSep = lStrToCheck.indexOf( "to" ); //$NON-NLS-1$
  // float lFloatStart = getFloatFromSimpleFloatString( lStrToCheck.substring( 0, lIntIndexSep ).trim() );
  // float lFloatEnd = getFloatFromSimpleFloatString( lStrToCheck.substring( lIntIndexSep + 2 ).trim() );
  // if( lFloatStart < 0.0 || lFloatStart > 1.0 || lFloatEnd < 0.0 || lFloatEnd > 1.0 ){
  //        final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.6" );//$NON-NLS-1$
  // throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
  // }
  // int lIntAllInc = Math.abs( ( int )( lFloatEnd * 10 ) - ( int )( lFloatStart * 10 ) );
  // int lIntPer = ( pIntNumberIterations / ( lIntAllInc ) );
  // int lIntIncDone = 0;
  // float lFloatSignum = Math.signum( lFloatEnd - lFloatStart );
  // for( int i = 1; i <= pIntNumberIterations; ++i ){
  // float lFloatRes = (float) ( lFloatStart + 0.1 * lIntIncDone * lFloatSignum );
  // pListFactors.add( lFloatRes ) ;
  // if( (i) % lIntPer == 0 && lIntIncDone < lIntAllInc ){
  // lIntAllInc++;
  // lIntIncDone++;
  // }
  // }
  // return true;
  // }
  //
  // return false;
  // }

  /**
   * checks if the given string value contains semicolon separated list of values and if it is parses it into
   * pListFactors as floats
   */
  private boolean isSeparatedValuesFormatedFactor( final String pStrValue, final List<Float> pListFactors, final int pIntNumberIterations ) throws CoreException
  {
    final StringTokenizer lStringTokenizer = new StringTokenizer( pStrValue, ";" ); //$NON-NLS-1$
    int lIntCounter = 0;
    float lFloatValAct = (float)0.1;
    while( lIntCounter < pIntNumberIterations )
    {
      if( lStringTokenizer.hasMoreTokens() )
      {
        lFloatValAct = getFloatFromSimpleFloatString( lStringTokenizer.nextToken() );
      }
      lIntCounter++;
      pListFactors.add( lFloatValAct );
    }
    return true;
  }

  private float getFloatFromSimpleFloatString( final String pStrFloat ) throws CoreException
  {
    float lFloatValue = (float)0.1;
    try
    {
      lFloatValue = Float.parseFloat( pStrFloat.trim() );
    }
    catch( final Exception e )
    {
      try
      {
        lFloatValue = Float.parseFloat( pStrFloat.trim().replace( ",", "." ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      catch( final Exception e2 )
      {
        final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.6" );//$NON-NLS-1$
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
      }
    }
    return lFloatValue;
  }

  private double getTimeInPercentage( final Calendar cal )
  {
    final int i = cal.get( Calendar.HOUR_OF_DAY );
    final int j2 = cal.get( Calendar.MINUTE );
    final float j = (float)(j2 / 60.0);
    return (double)i + j;
  }

  /**
   * @return If unsteady calculation is selected, returns date/time of the first timestep from the control model.
   *         <p>
   *         If unsteady calculation is not selected, returns current date/time (in the case of steady calculation, this value is not considered by RMA∑Kalypso)
   */
  private Date getFirstTimeStep( ) throws CoreException
  {
    if( m_controlModel.isUnsteadySelected() == false )
    {
      m_listDateSteps.add( new Date() );
      return m_listDateSteps.get( 0 );
    }

    m_controlModel.isUnsteadySelected();
    final IObservation<TupleResult> tupleSet = m_controlModel.getTimeSteps();
    final TupleResult result = tupleSet.getResult();
    final IComponent[] components = result.getComponents();
    final IComponent compTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final TupleResultIndex index = new TupleResultIndex( result, compTime );
    final Iterator<IRecord> iterator = index.getIterator();
    if( !iterator.hasNext() )
    {
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Control1D2DConverter.52" );//$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
    }

    final IRecord firstRecord = iterator.next();

    final TupleResult owner = firstRecord.getOwner();
    final int indexTime = owner.indexOfComponent( compTime );
    return DateUtilities.toDate( (XMLGregorianCalendar)firstRecord.getValue( indexTime ) );
  }

  public LinkedHashMap<Integer, IBoundaryCondition> getBoundaryConditionsIDProvider( )
  {
    return m_WQboundaryConditionsIDProvider;
  }

  public final Date[] getListDateSteps( )
  {
    return m_listDateSteps.toArray( new Date[m_listDateSteps.size()] );
  }
}