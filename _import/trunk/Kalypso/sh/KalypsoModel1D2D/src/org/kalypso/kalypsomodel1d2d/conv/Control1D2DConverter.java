/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
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

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation.KIND;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
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
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Gernot Belger
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Control1D2DConverter
{
  /** Directory name for rma10s result files (Output...) files */
  public static final String RESULT_DIR_NAME = "result";

  /** Base filename name for rma10s result files (Output...) files */
  public static final String RESULT_FILE_BASE = "Output";

  private final List<IBoundaryCondition> m_unitBoundaryConditions = new ArrayList<IBoundaryCondition>();

  private final INativeIDProvider m_nativeIDProvider;

  private final LinkedHashMap<Integer, IBoundaryCondition> m_WQboundaryConditionsIDProvider = new LinkedHashMap<Integer, IBoundaryCondition>();

  private final BuildingIDProvider m_buildingProvider;

  private final IControlModel1D2D m_controlModel;

  private final IRoughnessClsCollection m_roughnessModel;

  private final Map<String, TupleResultIndex> m_BCTupleResultIndexCache = new HashMap<String, TupleResultIndex>();

  private boolean m_isBCTupleResultIndexCached = false;

  public Control1D2DConverter( final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IRoughnessClsCollection roughnessMmodel, final INativeIDProvider idProvider, final BuildingIDProvider buildingProvider )
  {
    m_controlModel = controlModel;
    m_roughnessModel = roughnessMmodel;
    m_nativeIDProvider = idProvider;
    m_buildingProvider = buildingProvider;

    /* Initialize boundary conditions */
    final String calculationUnit = controlModel.getCalculationUnit().getGmlID();
    for( final IFlowRelationship relationship : flowModel )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition) relationship;
        if( boundaryCondition.isMemberOf( calculationUnit ) )
          m_unitBoundaryConditions.add( boundaryCondition );
      }
    }
  }

  public void writeR10File( final File outputFile ) throws CoreException, IOException
  {
    Formatter formatter = null;
    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );
      writeR10File( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    finally
    {
      if( formatter != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        formatter.close();
      }
    }
  }

  public void writeR10File( final Formatter formatter ) throws CoreException, IOException
  {
    writeR10ControlDataBlock( formatter );
    writeR10PropertiesDataBlock( formatter );
    writeR10ContinuityLineDataBlock( formatter );
    writeR10TimeStepDataBlock( formatter );
  }

  /**
   * Writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
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
    formatter.format( "OUTFIL  %s\\%s%n", RESULT_DIR_NAME, RESULT_FILE_BASE ); //$NON-NLS-1$
    formatter.format( "INKALYPSmodel.2d%n" ); //$NON-NLS-1$
    formatter.format( "CONTROL A %4d 2d 0%n", m_controlModel.getIaccyc() ); //$NON-NLS-1$
    if( m_controlModel.getRestart() )
      formatter.format( "RESTART%n" ); //$NON-NLS-1$

    /* Write W/Q file, even if it is empty. */
    formatter.format( "STFLFIL %s%n", ISimulation1D2DConstants.BC_WQ_File ); //$NON-NLS-1$
    /* We always write a building file, even if it is empty. */
    formatter.format( "INCSTR  %s%n", ISimulation1D2DConstants.BUILDING_File ); //$NON-NLS-1$

    formatter.format( "ENDFIL%n" ); //$NON-NLS-1$

    /* CONTROL DATA BLOCK */
    final String controlModelName = m_controlModel.getName();
    final String projectName = controlModelName == null ? Messages.getString( "Control1D2DConverter.8" ) : controlModelName; //$NON-NLS-1$
    formatter.format( "TI      %30s%n", projectName ); //$NON-NLS-1$

    // // C0
    final Object[] c0Props = new Object[] { 0, m_controlModel.getIDNOPT(), calendarForFirstTimeStep.get( Calendar.YEAR ), calendarForFirstTimeStep.get( Calendar.DAY_OF_YEAR ),
        getTimeInPercentage( calendarForFirstTimeStep ), m_controlModel.getIEDSW(), m_controlModel.getTBFACT(), m_controlModel.getTBMIN(), 1 };
    formatter.format( "C0%14d%8d%8d%8d%8.2f%8d%8.3f%8.2f%8d%n", c0Props ); //$NON-NLS-1$

    // C1
    // m_formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 1, 0, 0, 0, 0, 0, 0 ); //$NON-NLS-1$
    formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 0, 0, 0, 0, 0, 0, 0 ); //$NON-NLS-1$

    // C2
    // TODO: P_BOTTOM still not implemented, ask Nico
    formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%8.3f%n", m_controlModel.getOMEGA(), m_controlModel.getELEV(), 1.0, 1.0, 1.0, 1, m_controlModel.get_P_BOTTOM() ); //$NON-NLS-1$
    // formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%n", controlModel.getOMEGA(), controlModel.getELEV(), 1.0, 1.0,
    // 1.0, 1 );

    // C3
    formatter.format( "C3%14.3f%8.3f%8.3f%8.1f%8.3f%8.3f%8.3f%n", 1.0, 1.0, m_controlModel.getUNOM(), m_controlModel.getUDIR(), m_controlModel.getHMIN(), m_controlModel.getDSET(), m_controlModel.getDSETD() ); //$NON-NLS-1$

    // C4
    final boolean artImpulsstromBeiwert = m_controlModel.getBeient();
    formatter.format( "C4%14.1f%8.1f%8.1f%56d%n", 0.0, 20.0, 0.0, artImpulsstromBeiwert ? 1 : 0 ); //$NON-NLS-1$

    // C5
    formatter.format( "C5%14d%8d%16d%8d%8d%8d%8d%8d%8d%n", m_controlModel.getNITI(), m_controlModel.getNITN(), m_controlModel.getNCYC(), 0, 1, 1, 0, 1, 1 ); //$NON-NLS-1$

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
   * writes the Properties Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10PropertiesDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    for( final IRoughnessCls rClass : m_roughnessModel )
    {
      final int roughnessAsciiID = m_nativeIDProvider.getConversionID( rClass );

      final int iedsw = m_controlModel.getIEDSW();
      final Double[] eddy = rClass.getViscosities( iedsw );
      final Double ks = rClass.getKs();
      final Double axAy = rClass.getAxAy();
      final Double dp = rClass.getDp();

      if( ks == null )
      {
        final String msg = Messages.getString( "Control1D2DConverter.7" ) + rClass.getName();//$NON-NLS-1$
        throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
      }

      final Double axayCorrected = axAy == null ? 0.0 : axAy;
      final Double dpCorrected = dp == null ? 0.0 : dp;

      writeEDBlock( formatter, roughnessAsciiID, eddy, ks, axayCorrected, dpCorrected );
    }

    final Map<Integer, IBuildingFlowRelation> buildingMap = m_buildingProvider.getBuildingData();
    for( final Integer buildingID : buildingMap.keySet() )
      writeEDBlock( formatter, buildingID, 0.0, 0.0, 0.0, 0.0 );
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final Double[] eddy, final Double ks, final Double axayCorrected, final Double dpCorrected ) throws IOException
  {
    if( eddy.length < 4 )
      throw new IllegalArgumentException( Messages.getString( "Control1D2DConverter.17" ) ); //$NON-NLS-1$
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, eddy[0], eddy[1], eddy[2], eddy[3], -1.0, 1.0, 1.0 ); //$NON-NLS-1$
    formatter.format( "ED2%21.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, 20.0 ); //$NON-NLS-1$
    formatter.format( "ED4%21.5f%8.1f%8.2f%n", ks, axayCorrected, dpCorrected ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final double val, final Double ks, final Double axayCorrected, final Double dpCorrected ) throws IOException
  {
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, val, val, val, val, -1.0, 1.0, 1.0 ); //$NON-NLS-1$
    formatter.format( "ED2%21.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, 20.0 ); //$NON-NLS-1$
    formatter.format( "ED4%21.5f%8.1f%8.2f%n", ks, axayCorrected, dpCorrected ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private void writeR10ContinuityLineDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    final List<IFELine> continuityLines = m_controlModel.getCalculationUnit().getContinuityLines();
    formatter.format( "SCL%9d%n", continuityLines.size() ); //$NON-NLS-1$
    for( final IFELine line : continuityLines )
    {
      final IFE1D2DNode[] nodes = new IFE1D2DNode[line.getNodes().size()];
      line.getNodes().toArray( nodes );
      for( int i = 0; i < nodes.length; i++ )
      {
        final IFE1D2DNode node = nodes[i];
        final int nodeID = m_nativeIDProvider.getConversionID( node );
        if( nodeID == 0 )
        {
          final String msg = "At least one node contained by boundary line is not included in this calculation unit.";
          throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
        }

        if( i == 0 )
          formatter.format( "CC1 %4d", m_nativeIDProvider.getConversionID( line ) ); //$NON-NLS-1$
        else if( i % 9 == 0 )
          formatter.format( "%nCC2     " ); //$NON-NLS-1$
        formatter.format( "%8d", nodeID ); //$NON-NLS-1$
      }
      if( nodes.length % 9 != 0 )
        formatter.format( "%n" ); //$NON-NLS-1$

      FormatterUtils.checkIoException( formatter );
    }
    formatter.format( "ECL%n" ); //$NON-NLS-1$

    if( m_controlModel.getIDNOPT() != 0 && m_controlModel.getIDNOPT() != -1 )
      formatter.format( "MP%21.2f%8.2f%8.2f%n", m_controlModel.getAC1(), m_controlModel.getAC2(), m_controlModel.getAC3() ); //$NON-NLS-1$

    formatter.format( "ENDGEO%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10TimeStepDataBlock( final Formatter formatter ) throws CoreException, IOException
  {
    final IObservation<TupleResult> observation = m_controlModel.getTimeSteps();
    final TupleResult result = observation.getResult();
    final IComponent[] components = result.getComponents();
    // final IComponent componentOrdinalNumber = ComponentUtilities.findComponentByID( components,
    // Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER );
    final IComponent componentTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final IComponent componentRelaxationsFaktor = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR );

    /* Steady state, just one block for the Starting Date. */
    final int niti = m_controlModel.getNITI();

    final String msg = Messages.getString( "Control1D2DConverter.33" ); //$NON-NLS-1$

    Double uRValSteady = m_controlModel.get_RelaxationsFactor();
    if( uRValSteady == null || uRValSteady.isNaN() || uRValSteady < 0.1 || uRValSteady > 1.0 )
    {
      if( m_controlModel.isSteadySelected() )
      {
        final String errMsg = Messages.getString( "Control1D2DConverter.34" );//$NON-NLS-1$
        throw new CoreException( StatusUtilities.createErrorStatus( errMsg ) );
      }
      else
        // Not used anyway (1.0 should be default value)
        uRValSteady = 1.0;
    }

    writeTimeStep( formatter, msg, null, null, uRValSteady.floatValue(), niti );

    if( m_controlModel.isUnsteadySelected() )
    {
      /* Sort records by time */
      final TupleResultIndex index = new TupleResultIndex( result, componentTime );
      final Iterator<IRecord> iterator = index.getIterator();
      if( !iterator.hasNext() )
      {
        final String errMsg = Messages.getString( "Control1D2DConverter.35" );//$NON-NLS-1$
        throw new CoreException( StatusUtilities.createErrorStatus( errMsg ) );
      }

      final IRecord firstRecord = iterator.next();

      final int nitn = m_controlModel.getNITN();

      if( nitn > 0 )
      {
        /* Unsteady state: a block for each time step */

        // TODO: check for right time zone
        // // As we are using UTC for all our timeseries, this is the timezone that should be used here
        // final TimeZone DEFAULT_TIMEZONE = TimeZone.getTimeZone( "UTC" );
        XMLGregorianCalendar cal = (XMLGregorianCalendar) firstRecord.getValue( componentTime );
        Calendar lastStepCal = cal.toGregorianCalendar();
        // lastStepCal.setTimeZone( DEFAULT_TIMEZONE );
        int stepCount = 1;
        for( ; iterator.hasNext(); stepCount++ )
        {
          final IRecord record = iterator.next();
          final float uRVal = ((BigDecimal) record.getValue( componentRelaxationsFaktor )).floatValue();
          final Calendar stepCal = ((XMLGregorianCalendar) record.getValue( componentTime )).toGregorianCalendar();
          // stepCal.setTimeZone( DEFAULT_TIMEZONE );
          final String unsteadyMsg = String.format( Messages.getString( "Control1D2DConverter.36" ), stepCount ); //$NON-NLS-1$
          writeTimeStep( formatter, unsteadyMsg, stepCal, lastStepCal, uRVal, nitn );

          lastStepCal = stepCal;
        }
        if( m_controlModel.getIaccyc() > stepCount )
        {
          final String errMsg = stepCount + " timesteps defined, but " + m_controlModel.getIaccyc() + " is selected as first restart step.";
          throw new CoreException( StatusUtilities.createErrorStatus( errMsg ) );
        }
      }
    }
  }

  /**
   * writes out the time steps. <BR>
   * IMPORTANT: we write the dates in the time zone according to the kalypso-preferences!
   */
  private void writeTimeStep( final Formatter formatter, final String message, final Calendar calculationStep, final Calendar lastStepCal, final float uRVal, final int niti ) throws CoreException, IOException
  {

    final String dashes = StringUtils.repeat( "-", message.length() ); //$NON-NLS-1$
    formatter.format( "com %s%n", dashes ); //$NON-NLS-1$
    formatter.format( "com %s%n", message ); //$NON-NLS-1$
    formatter.format( "com %s%n", dashes ); //$NON-NLS-1$

    final long timeStepInterval;
    final double timeStepHours;
    final int year;
    final int dayOfYear;
    final double ihre;

    Calendar kalypsoCalendarStep = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );

    // unsteady
    if( calculationStep != null )
    {
      // REMARK: we write the date in the kalypso-preferences time zone!
      kalypsoCalendarStep.setTime( calculationStep.getTime() );
      dayOfYear = kalypsoCalendarStep.get( Calendar.DAY_OF_YEAR );
      year = kalypsoCalendarStep.get( Calendar.YEAR );

      // REMARK: we write the date in the kalypso-preferences time zone!
      final Calendar kalypsoCallendarPreviousStep = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
      kalypsoCallendarPreviousStep.setTime( lastStepCal.getTime() );

      timeStepInterval = kalypsoCalendarStep.getTimeInMillis() - kalypsoCallendarPreviousStep.getTimeInMillis();
      // if timeStepInterval is zero, step will be ignored
      // (this will happen on summertime to wintertime transition)
      if( timeStepInterval == 0 )
      {
        formatter.format( "com Summertime/Wintertime transition; repeated step ignored%n" );
        return;
      }

      timeStepHours = (double) timeStepInterval / (60 * 60 * 1000);
      ihre = getTimeInPercentage( kalypsoCalendarStep );
    }
    // steady don�t need startdate
    else
    {
      kalypsoCalendarStep = null;
      dayOfYear = 0;
      year = 0;
      timeStepHours = 0;
      ihre = 0;
    }

    formatter.format( "DT%14.2f%8d%8d%8.2f", timeStepHours, year, dayOfYear, ihre ); //$NON-NLS-1$

    // BC lines
    if( niti == 0 )
      formatter.format( "%nBC%n" ); //$NON-NLS-1$
    else
      formatBC( formatter, uRVal, niti );

    // order is important, first QC than HC, SQC and EFE
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D );
    formatBoundCondLines( formatter, kalypsoCalendarStep, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D );

    FormatterUtils.checkIoException( formatter );

    for( final Map.Entry<Integer, IBuildingFlowRelation> buildingData : m_buildingProvider.getBuildingData().entrySet() )
    {
      final Integer buildingID = buildingData.getKey();
      final IBuildingFlowRelation building = buildingData.getValue();
      final KIND kind = building.getKind();
      final int buildingKind;
      switch( kind )
      {
        case TABULAR:
          buildingKind = 10;
          break;

        default:
          final String msg = Messages.getString( "Control1D2DConverter.43" ) + kind;//$NON-NLS-1$
          throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
      }

      final double direction = Math.toRadians( building.getDirection() );
      formatter.format( "FC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", buildingID, buildingKind, 0.0, 0.0, 0.0, 0.0, direction ); //$NON-NLS-1$

      FormatterUtils.checkIoException( formatter );
    }

    // add other conti lines types as well (buildings)?
    formatter.format( "ENDSTEP %s%n", message ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  /**
   * For big timeseries, creating TupleResultIndex for every time step is extremely slow. Cacheing those indexes for
   * performances reason is nesessary.
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
        m_BCTupleResultIndexCache.put( boundaryCondition.getGmlID(), new TupleResultIndex( obsResult, timeComponent ) );
      }
    }
    m_isBCTupleResultIndexCached = true;
  }

  /**
   * Formats the lines for one type of boundary condition (QC or HC or ... ).
   */
  private void formatBoundCondLines( final Formatter formatter, final Calendar stepCal, final String bcAbscissaComponentType, final String bcOrdinateComponentType ) throws IOException
  {
    if( !m_isBCTupleResultIndexCached )
      cacheBCTupleResultIndexes();
    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_ELEMENT1D2D ) || boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        final int ordinal = m_nativeIDProvider.getConversionID( boundaryCondition.getParentElementID() );
        final double stepValue;
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
          if( stepCal != null )
          {
            final TupleResultIndex tupleResultIndex = m_BCTupleResultIndexCache.get( boundaryCondition.getGmlID() );
            final Number result = (Number) tupleResultIndex.getValue( ordinateComponent, stepCal.getTime() );
            stepValue = (result == null || Double.isNaN( result.doubleValue() )) ? 0.0 : result.doubleValue();
          }
          else
            stepValue = boundaryCondition.getStationaryCondition();

          if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE )
              && (boundaryCondition.isAbsolute() == null) )
          {
            final double theta = Math.toRadians( boundaryCondition.getDirection().doubleValue() );
            formatter.format( "QC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, stepValue, theta, 0.000, 20.000, 0.000 );
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ) )
          {
            final double theta = Math.toRadians( boundaryCondition.getDirection().doubleValue() );
            formatter.format( "SQC%13d%40.4f%8.3f%8.3f%8.3f%n", ordinal, theta, 0.000, 20.000, 0.000 );
            m_WQboundaryConditionsIDProvider.put( ordinal, boundaryCondition );
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ) )
          {
            formatter.format( "HC%14d%8d%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, stepValue, 0.0, 0.000, 20.000 );
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D )
              && boundaryCondition.getParentElementID().startsWith( "Element1D" ) )
          {
            final Boolean isAbsoluteProperty = boundaryCondition.isAbsolute();
            final int isAbsolute = (isAbsoluteProperty != null && isAbsoluteProperty.booleanValue()) ? 1 : 0;
            formatter.format( "EFE%13d%8d%8d%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 0.0, 20.000 );
          }
          else if( bcAbscissaComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_TIME ) && bcOrdinateComponentType.equals( Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D )
              && !boundaryCondition.getParentElementID().startsWith( "Element1D" ) )
          {
            final Boolean isAbsoluteProperty = boundaryCondition.isAbsolute();
            final int isAbsolute = (isAbsoluteProperty != null && isAbsoluteProperty.booleanValue()) ? 1 : 0;
            formatter.format( "EFE%13d%8d%8d%8.3f%8.3f%8.3f%8.3f%n", ordinal, 0, isAbsolute, stepValue, 0.0, 0.0, 20.000 );
          }
        }
      }

      FormatterUtils.checkIoException( formatter );
    }
  }

  private void formatBC( final Formatter formatter, final float uRVal, final int nitn ) throws CoreException, IOException
  {
    if( uRVal < 0.0 || uRVal > 1.0 )
    {
      final String msg = Messages.getString( "Control1D2DConverter.6" );//$NON-NLS-1$
      throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
    }

    final int buffVal = 10 - (int) (uRVal * 10);
    for( int j = 0; j < nitn; j++ )
    {
      if( j % 9 == 0 )
        formatter.format( "%nBC%6s", " " ); //$NON-NLS-1$

      formatter.format( "%5d010", buffVal ); //$NON-NLS-1$
    }
    formatter.format( "%n" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private double getTimeInPercentage( final Calendar cal )
  {
    final int i = cal.get( Calendar.HOUR_OF_DAY );
    final int j2 = cal.get( Calendar.MINUTE );
    final float j = (float) (j2 / 60.0);
    return (double) i + j;
  }

  /**
   * @return If unsteady calculation is selected, returns date/time of the first timestep from the control model.
   *         <p>
   *         If unsteady calculation is not selected, returns current date/time (in the case of steady calculation, this
   *         value is not considered by RMA10s)
   */
  private Date getFirstTimeStep( ) throws CoreException
  {
    if( m_controlModel.isUnsteadySelected() == false )
      return new Date();

    m_controlModel.isUnsteadySelected();
    final IObservation<TupleResult> tupleSet = m_controlModel.getTimeSteps();
    final TupleResult result = tupleSet.getResult();
    final IComponent[] components = result.getComponents();
    final IComponent compTime = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final TupleResultIndex index = new TupleResultIndex( result, compTime );
    final Iterator<IRecord> iterator = index.getIterator();
    if( !iterator.hasNext() )
    {
      final String msg = Messages.getString( "Control1D2DConverter.52" );//$NON-NLS-1$
      throw new CoreException( StatusUtilities.createErrorStatus( msg ) );
    }

    final IRecord firstRecord = iterator.next();
    return DateUtilities.toDate( (XMLGregorianCalendar) firstRecord.getValue( compTime ) );
  }

  public LinkedHashMap<Integer, IBoundaryCondition> getBoundaryConditionsIDProvider( )
  {
    return m_WQboundaryConditionsIDProvider;
  }
}
