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

import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.StringUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation.KIND;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.kalypsomodel1d2d.sim.RMA10SimModelConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Gernot Belger
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Madanagopal
 */
public class Control1D2DConverter
{
  private final LinkedHashMap<String, Integer> m_nodesIDProvider;

  private final LinkedHashMap<String, Integer> m_roughnessIDProvider;

  private final BuildingIDProvider m_buildingProvider;

  public Control1D2DConverter( final LinkedHashMap<String, Integer> nodesIDProvider, final LinkedHashMap<String, Integer> roughnessIDProvider, final BuildingIDProvider buildingProvider )
  {
    m_nodesIDProvider = nodesIDProvider;
    m_roughnessIDProvider = roughnessIDProvider;
    m_buildingProvider = buildingProvider;
  }

  private int getRoughnessID( final String gmlID )
  {
    return m_roughnessIDProvider.get( gmlID );
  }

  public void writeR10File( final RMA10Calculation calculation, final PrintWriter pw ) throws SimulationException
  {
    final Formatter formatter = new Formatter( pw, Locale.US );

    writeR10ControlDataBlock( calculation, formatter );
    writeR10PropertiesDataBlock( calculation, formatter );
    writeR10ContinuityLineDataBlock( calculation, formatter );
    writeR10TimeStepDataBlock( calculation, formatter );
  }

  /**
   * Writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10ControlDataBlock( final RMA10Calculation calculation, final Formatter formatter ) throws SimulationException
  {
    final IControlModel1D2D controlModel = calculation.getControlModel();

    // TODO: even if only stationary mode is choosen, we get an exception here
    // if no timeserie is given; there should be a fallback in that case to a
    // pseudo start-date; talk to nico about it
    final Date firstDate = getFirstTimeStep( calculation );
    final Calendar calendarForFirstTimeStep = Calendar.getInstance();
    calendarForFirstTimeStep.setTime( firstDate );

    /* FILES DATA BLOCK */
    formatter.format( "OUTFIL  result\\Output%n" );  //$NON-NLS-1$
    formatter.format( "INKALYPSmodel.2d%n" );  //$NON-NLS-1$
    final int restartStep = controlModel.getRestart() ? controlModel.getIaccyc() : 0;
    formatter.format( "CONTROL A %4d 2d 0%n", restartStep );  //$NON-NLS-1$
    if( controlModel.getRestart() )
      formatter.format( "RESTART%n" );  //$NON-NLS-1$

    /* We always write a building file, even if it is empty. */
    formatter.format( "INCSTR  %s%n", RMA10SimModelConstants.BUILDING_File );  //$NON-NLS-1$

    formatter.format( "ENDFIL%n" );  //$NON-NLS-1$

    /* CONTROL DATA BLOCK */
    // TODO: write given name of szenario/projekt
    final String discModelName = calculation.getDiscModel().getName();
    final String projectName = discModelName == null ? Messages.getString("Control1D2DConverter.8") : discModelName; //$NON-NLS-1$
    formatter.format( "TI      %30s%n", projectName ); //$NON-NLS-1$

    // // C0
    final Object[] c0Props = new Object[] { 0, controlModel.getIDNOPT(), calendarForFirstTimeStep.get( Calendar.YEAR ), calendarForFirstTimeStep.get( Calendar.DAY_OF_YEAR ),
        getTimeInPercentage( calendarForFirstTimeStep ), controlModel.getIEDSW(), controlModel.getTBFACT(), controlModel.getTBMIN(), 0 };
    formatter.format( "C0%14d%8d%8d%8d%8.2f%8d%8.3f%8.2f%8d%n", c0Props );  //$NON-NLS-1$

    // C1
    formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 1, 0, 0, 0, 0, 0, 0 );  //$NON-NLS-1$

    // C2
    // TODO: P_BOTTOM still not implemented, ask Nico
    formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%8.3f%n", controlModel.getOMEGA(), controlModel.getELEV(), 1.0, 1.0, 1.0, 1, controlModel.get_P_BOTTOM() );  //$NON-NLS-1$
    // formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%n", controlModel.getOMEGA(), controlModel.getELEV(), 1.0, 1.0,
    // 1.0, 1 );

    // C3
    formatter.format( "C3%14.3f%8.3f%8.3f%8.1f%8.3f%8.3f%8.3f%n", 1.0, 1.0, controlModel.getUNOM(), controlModel.getUDIR(), controlModel.getHMIN(), controlModel.getDSET(), controlModel.getDSETD() );  //$NON-NLS-1$

    // C4
    final int artImpulsstromBeiwert = 0;
    formatter.format( "C4%14.1f%8.1f%8.1f%56d%n", 0.0, 20.0, 0.0, artImpulsstromBeiwert );  //$NON-NLS-1$

    // C5
    formatter.format( "C5%14d%8d%16d%8d%8d%8d%8d%8d%8d%n", controlModel.getNITI(), controlModel.getNITN(), controlModel.getNCYC(), 0, 1, 1, 0, 1, 1 );  //$NON-NLS-1$

    // CV
    formatter.format( "CV%14.2g%8.2g%8.2g%8.2g%8.2g%16d%8.2f%n", controlModel.getCONV_1(), controlModel.getCONV_2(), controlModel.getCONV_3(), 0.05, 0.05, controlModel.getIDRPT(), controlModel.getDRFACT() );  //$NON-NLS-1$

    // IOP line deleted because Nico said that it is an ugly, baaad line... :)
    // (IOP has something to do with reordering, and this is not working properly with 1D-2D copling)
    // formatter.format( "IOP%n" );

    // VEGETA
    if( controlModel.getVegeta() )
      formatter.format( "VEGETA%n" );  //$NON-NLS-1$

    formatter.format( "KAL_BC%n" );  //$NON-NLS-1$
  }

  /**
   * writes the Properties Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10PropertiesDataBlock( final RMA10Calculation calculation, final Formatter formatter ) throws SimulationException
  {
    for( final Object elt : calculation.getRoughnessClassList() )
    {
      final Feature roughnessCL = (Feature) elt;
      final int roughnessAsciiID = getRoughnessID( roughnessCL.getId() );

      final Double[] eddy = calculation.getViskosity( roughnessCL );
      // final double val = 2900.0;

      final Double ks = calculation.getKs( roughnessCL );
      final Double axAy = calculation.getAxAy( roughnessCL );
      final Double dp = calculation.getDp( roughnessCL );

      if( ks == null )
        throw new SimulationException( Messages.getString("Control1D2DConverter.7") + NamedFeatureHelper.getName( roughnessCL ), null ); //$NON-NLS-1$

      final Double axayCorrected = axAy == null ? 0.0 : axAy;
      final Double dpCorrected = dp == null ? 0.0 : dp;

      writeEDBlock( formatter, roughnessAsciiID, eddy, ks, axayCorrected, dpCorrected );
    }

    final Map<Integer, IBuildingFlowRelation> buildingMap = m_buildingProvider.getBuildingData();
    for( final Integer buildingID : buildingMap.keySet() )
      writeEDBlock( formatter, buildingID, 0.0, 0.0, 0.0, 0.0 );
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final Double[] eddy, final Double ks, final Double axayCorrected, final Double dpCorrected )
  {
    if( eddy.length < 4 )
      throw new IllegalArgumentException( Messages.getString("Control1D2DConverter.17") );  //$NON-NLS-1$
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, eddy[0], eddy[1], eddy[2], eddy[3], -1.0, 1.0, 1.0 );  //$NON-NLS-1$
    formatter.format( "ED2%13.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, 20.0 );  //$NON-NLS-1$
    formatter.format( "ED4%21.2f%8.1f%8.2f%n", ks, axayCorrected, dpCorrected );  //$NON-NLS-1$
  }

  private void writeEDBlock( final Formatter formatter, final int roughnessAsciiID, final double val, final Double ks, final Double axayCorrected, final Double dpCorrected )
  {
    formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, val, val, val, val, -1.0, 1.0, 1.0 );  //$NON-NLS-1$
    formatter.format( "ED2%13.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, 20.0 );  //$NON-NLS-1$
    formatter.format( "ED4%21.2f%8.1f%8.2f%n", ks, axayCorrected, dpCorrected );  //$NON-NLS-1$
  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private void writeR10ContinuityLineDataBlock( final RMA10Calculation calculation, final Formatter formatter ) throws SimulationException
  {
    final BoundaryLineInfo[] rawInfos = calculation.getContinuityLineInfo();

    if( rawInfos.length == 0 )
      throw new SimulationException( Messages.getString("Control1D2DConverter.24"), null ); //$NON-NLS-1$

    // Because we might don't have info about all nodes (submodel), we have to filter SCL/CCx lines
    final List<BoundaryLineInfo> filteredInfosList = new ArrayList<BoundaryLineInfo>();
    for( final BoundaryLineInfo info : rawInfos )
    {
      final IFE1D2DNode[] nodes = info.getNodes();
      boolean allNodesPresent = true;
      for( final IFE1D2DNode element : nodes )
      {
        final Integer nodeID = m_nodesIDProvider.get( element.getGmlID() );
        if( nodeID == null )
        {
          allNodesPresent = false;
          break;
        }
      }
      if( allNodesPresent )
        filteredInfosList.add( info );
    }
    final BoundaryLineInfo[] infos = new BoundaryLineInfo[filteredInfosList.size()];
    filteredInfosList.toArray( infos );

    formatter.format( "SCL%9d%n", infos.length );  //$NON-NLS-1$

    int infoCnt = 1;
    for( final BoundaryLineInfo info : infos )
    {
      final IFE1D2DNode[] nodes = info.getNodes();

      for( int i = 0; i < nodes.length; i++ )
      {
        final IFE1D2DNode node = nodes[i];
        final int nodeID = m_nodesIDProvider.get( node.getGmlID() );
        /* Write start stuff */
        if( i == 0 )
// formatter.format( "CC1 %4d", info.getID() );
          formatter.format( "CC1 %4d", infoCnt++ );  //$NON-NLS-1$
        else if( i % 9 == 0 )
          formatter.format( "%nCC2     " );  //$NON-NLS-1$

        formatter.format( "%8d", nodeID );  //$NON-NLS-1$
      }
      if( nodes.length % 9 != 0 )
        formatter.format( "%n" );  //$NON-NLS-1$
    }

    formatter.format( "ECL%n" );  //$NON-NLS-1$

    final IControlModel1D2D controlModel = calculation.getControlModel();
    if( controlModel.getIDNOPT() != 0 && controlModel.getIDNOPT() != -1 ) // Write MP Line only under this conditions
    {
      formatter.format( "MP%21.2f%8.2f%8.2f%n", controlModel.getAC1(), controlModel.getAC2(), controlModel.getAC3() );  //$NON-NLS-1$
    }
    formatter.format( "ENDGEO%n" );  //$NON-NLS-1$
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10TimeStepDataBlock( final RMA10Calculation calculation, final Formatter formatter ) throws SimulationException
  {
    final IControlModel1D2D controlModel = calculation.getControlModel();
    final ITimeStepinfo[] timeStepInfos = calculation.getTimeStepInfos();

    final IObservation<TupleResult> observation = controlModel.getTimeSteps();
    final TupleResult result = observation.getResult();
    // TODO: search components by type!
    final IComponent compTime = result.getComponents()[0];
    final IComponent compUnderRelax = result.getComponents()[1];

    /* Steady state, just one block for the Starting Date. */
    final int niti = controlModel.getNITI();

    final String msg = Messages.getString("Control1D2DConverter.33");  //$NON-NLS-1$

    Double uRValSteady = controlModel.get_steadyBC();
    if( uRValSteady == null || uRValSteady.isNaN() )
      if( controlModel.isSteadySelected() )
        throw new SimulationException( Messages.getString("Control1D2DConverter.34"), null ); //$NON-NLS-1$
      else
        // Not used anyway (1.0 should be default value)
        uRValSteady = 1.0;
    writeTimeStep( formatter, msg, null, null, uRValSteady.floatValue(), niti, timeStepInfos );

    if( controlModel.isUnsteadySelected() )
    {
      /* Sort records by time */
      final TupleResultIndex index = new TupleResultIndex( result, compTime );
      final Iterator<IRecord> iterator = index.getIterator();
      if( !iterator.hasNext() )
        throw new SimulationException( Messages.getString("Control1D2DConverter.35"), null ); //$NON-NLS-1$
      final IRecord firstRecord = iterator.next();

      final int nitn = controlModel.getNITN();

      if( nitn > 0 )
      {
        /* Unsteady state: a block for each time step */
        Calendar lastStepCal = ((XMLGregorianCalendar) firstRecord.getValue( compTime )).toGregorianCalendar();

        for( int stepCount = 1; iterator.hasNext(); stepCount++ )
        {
          final IRecord record = iterator.next();
          final float uRVal = ((BigDecimal) record.getValue( compUnderRelax )).floatValue();

          final Calendar stepCal = ((XMLGregorianCalendar) record.getValue( compTime )).toGregorianCalendar();

          final String unsteadyMsg = String.format( Messages.getString("Control1D2DConverter.36"), stepCount );  //$NON-NLS-1$
          writeTimeStep( formatter, unsteadyMsg, stepCal, lastStepCal, uRVal, nitn, timeStepInfos );

          lastStepCal = stepCal;
        }
      }
    }
  }

  private void writeTimeStep( final Formatter formatter, final String message, final Calendar stepCal, final Calendar lastStepCal, final float uRVal, final int niti, final ITimeStepinfo[] timeStepInfos ) throws SimulationException
  {
    final String dashes = StringUtils.repeat( "-", message.length() );  //$NON-NLS-1$
    formatter.format( "com %s%n", dashes );  //$NON-NLS-1$
    formatter.format( "com %s%n", message );  //$NON-NLS-1$
    formatter.format( "com %s%n", dashes );  //$NON-NLS-1$

    final long timeStepInterval;
    final double timeStepHours;
    final int year;
    final int dayOfYear;
    final double ihre;

    // unsteady
    if( stepCal != null )
    {
      dayOfYear = stepCal.get( Calendar.DAY_OF_YEAR );
      year = stepCal.get( Calendar.YEAR );

      timeStepInterval = stepCal.getTimeInMillis() - lastStepCal.getTimeInMillis();
      timeStepHours = (double) timeStepInterval / (60 * 60 * 1000);
      ihre = getTimeInPercentage( stepCal );
    }
    // steady don¥t need startdate
    else
    {
      dayOfYear = 0;
      year = 0;
      timeStepHours = 0;
      ihre = 0;
    }

    formatter.format( "DT%14.2f%8d%8d%8.2f", timeStepHours, year, dayOfYear, ihre );  //$NON-NLS-1$

    // BC lines
    if( niti == 0 )
      formatter.format( "%nBC%n" );  //$NON-NLS-1$
    else
      formatBC( formatter, uRVal, niti );

    formatBoundCondLines( formatter, stepCal, timeStepInfos, TYPE.CONTI_BC_Q );
    formatBoundCondLines( formatter, stepCal, timeStepInfos, TYPE.CONTI_BC_H );

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
          throw new SimulationException( Messages.getString("Control1D2DConverter.43") + kind, null ); //$NON-NLS-1$
      }

      final double direction = Math.toRadians( building.getDirection() );
      formatter.format( "FC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", buildingID, buildingKind, 0.0, 0.0, 0.0, 0.0, direction );  //$NON-NLS-1$
    }

    // add other conti lines types as well (buildingss)?
    formatter.format( "ENDSTEP %s%n", message );  //$NON-NLS-1$
  }

  /** Formats the lines for one type of boundary condition (QC or HC or ... ). */
  private void formatBoundCondLines( final Formatter formatter, final Calendar stepCal, final ITimeStepinfo[] timeStepInfos, final TYPE contiType ) throws SimulationException
  {
    // Boundary Conditions
    for( final ITimeStepinfo item : timeStepInfos )
    {
      /* Write only considered lines */
      final TYPE type = item.getType();

      if( type != contiType )
        continue;

      final double itemValue = item.getValue( stepCal == null ? null : stepCal.getTime() );

      switch( type )
      {
        case CONTI_BC_Q:
        {
          final double q = itemValue;
          final double theta = item.getTheta();
          formatter.format( "QC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", item.getID(), 0, q, theta, 0.000, 20.000, 0.000 );  //$NON-NLS-1$
        }
          break;

        case CONTI_BC_H:
        {
          final double h = itemValue;
          formatter.format( "HC%14d%8d%8.3f%8.3f%8.3f%8.3f%n", item.getID(), 0, h, 0.0, 20.0, 0.0 );  //$NON-NLS-1$
        }
          break;

        default:
          break;
      }
    }
  }

  private void formatBC( final Formatter formatter, final float uRVal, final int nitn ) throws SimulationException
  {
    if( uRVal < 0.0 || uRVal > 1.0 )
      throw new SimulationException( Messages.getString("Control1D2DConverter.6"), null ); //$NON-NLS-1$

    final int buffVal = 10 - (int) (uRVal * 10);
    /*
     * if( uRVal == (float) 1.0 ) buffVal = 0; else buffVal = 10 - (int) (((uRVal * 10)));
     */
    for( int j = 0; j < nitn; j++ )
    {
      if( j % 9 == 0 )
        formatter.format( "%nBC%5s", " " );  //$NON-NLS-1$

      formatter.format( "%5d010", buffVal );  //$NON-NLS-1$
    }

    formatter.format( "%n" );  //$NON-NLS-1$
  }

  private double getTimeInPercentage( final Calendar cal )
  {
    final int i = cal.get( Calendar.HOUR_OF_DAY );
    final int j2 = cal.get( Calendar.MINUTE );
    final float j = (float) (j2 / 60.0);
    return (double) i + j;
  }

  private Date getFirstTimeStep( final RMA10Calculation calculation ) throws SimulationException
  {
    final IControlModel1D2D controlModel = calculation.getControlModel();
    final IObservation<TupleResult> tupleSet = controlModel.getTimeSteps();
    final TupleResult result = tupleSet.getResult();
    final IComponent compTime = result.getComponents()[0];
    final TupleResultIndex index = new TupleResultIndex( result, compTime );
    // todo check if result is not empty
    final Iterator<IRecord> iterator = index.getIterator();
    if( !iterator.hasNext() )
      throw new SimulationException( Messages.getString("Control1D2DConverter.52"), null ); //$NON-NLS-1$
    final IRecord firstRecord = iterator.next();
    return DateUtilities.toDate( (XMLGregorianCalendar) firstRecord.getValue( compTime ) );
  }
}
