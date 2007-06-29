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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.lang.StringUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
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
  private final LinkedHashMap<String, String> m_nodesIDProvider;

  private final LinkedHashMap<String, String> m_roughnessIDProvider;

  public Control1D2DConverter( final LinkedHashMap<String, String> nodesIDProvider, final LinkedHashMap<String, String> roughnessIDProvider )
  {
    m_nodesIDProvider = nodesIDProvider;
    m_roughnessIDProvider = roughnessIDProvider;
  }

  private int getRoughnessID( final String gmlID )
  {
    return Integer.parseInt( m_roughnessIDProvider.get( gmlID ) );
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

    final Date firstDate = getFirstTimeStep( calculation );
    final Calendar calendarForFirstTimeStep = Calendar.getInstance();
    calendarForFirstTimeStep.setTime( firstDate );

    /* FILES DATA BLOCK */
    formatter.format( "OUTFIL  result\\Output%n" );
    formatter.format( "INKALYPSmodel.2d%n" );
    final int restartStep = controlModel.getRestart() ? controlModel.getIaccyc() : 0;
    formatter.format( "CONTROL A %4d 2d 0%n", restartStep );
    if( controlModel.getRestart() )
      formatter.format( "RESTART%n" );

    formatter.format( "ENDFIL%n" );

    /* CONTROL DATA BLOCK */
    // TODO: write given name of szenario/projekt
    formatter.format( "TI Projekt Name%n" ); // write Project name

    // // C0
    final Object[] c0Props = new Object[] { 0, controlModel.getIDNOPT(), calendarForFirstTimeStep.get( Calendar.YEAR ), calendarForFirstTimeStep.get( Calendar.DAY_OF_YEAR ),
        getTimeInPercentage( calendarForFirstTimeStep ), controlModel.getIEDSW(), controlModel.getTBFACT(), controlModel.getTBMIN(), 0 };
    // TODO: also write the (at the moment) constant values with the corrct format strings (%xxx.yyf),so later we
    // can easily exchange the given values without thinking about format any more (applies to all Cx)
    formatter.format( "C0%14d%8d%8d%8d%8.2f%8d%8.3f%8.2f%8d%n", c0Props );

    // C1
    formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 1, 0, 0, 0, 0, 0, 0 );

    // C2
    formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%n", controlModel.getOMEGA(), controlModel.getELEV(), 1.0, 1.0, 1.0, 1 );

    // C3
    formatter.format( "C3%14.3f%8.3f%8.3f%8.1f%8.3f%8.3f%8.3f%n", 1.0, 1.0, controlModel.getUNOM(), controlModel.getUDIR(), controlModel.getHMIN(), controlModel.getDSET(), controlModel.getDSETD() );

    // C4
    final int artImpulsstromBeiwert = 0;
    formatter.format( "C4%14.1f%8.1f%8.1f%56d%n", 0.0, 20.0, 0.0, artImpulsstromBeiwert );

    // C5
    formatter.format( "C5%14d%8d%16d%8d%8d%8d%8d%8d%8d%n", controlModel.getNITI(), controlModel.getNITN(), controlModel.getNCYC(), 0, 1, 1, 0, 1, 1 );

    // CV
    formatter.format( "CV%14.2g%8.2g%8.2g%8.2g%8.2g%16d%8.2f%n", controlModel.getCONV_1(), controlModel.getCONV_2(), controlModel.getCONV_3(), 0.05, 0.05, controlModel.getIDRPT(), controlModel.getDRFACT() );

    formatter.format( "IOP%n" );

    // VEGETA
    if( controlModel.getVegeta() )
      formatter.format( "VEGETA%n" );

    formatter.format( "KAL_BC%n" );
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

      // Double eddy = calculation.getViskosity( roughnessCL );
      final double val = 2900.0;
      formatter.format( "ED1%13d%8.1f%8.1f%8.1f%8.1f%8.1f%8.3f%8.3f%n", roughnessAsciiID, val, val, val, val, -1.0, 1.0, 1.0 );
      // ED2
      formatter.format( "ED2%13.1f%8.1f%8.3f%16.1f%n", 0.5, 0.5, 0.001, 20.0 );

      // ED4
      final Double ks = calculation.getKs( roughnessCL );
      final Double axAy = calculation.getAxAy( roughnessCL );
      final Double dp = calculation.getDp( roughnessCL );

      if( ks == null )
        throw new SimulationException( "No ks value found for rougness class: " + NamedFeatureHelper.getName( roughnessCL ), null );

      final Double axayCorrected = axAy == null ? 0.0 : axAy;
      final Double dpCorrected = dp == null ? 0.0 : dp;

      formatter.format( "ED4%21.2f%8.1f%8.2f%n", ks, axayCorrected, dpCorrected );
    }
  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private void writeR10ContinuityLineDataBlock( final RMA10Calculation calculation, final Formatter formatter ) throws SimulationException
  {
    final BoundaryLineInfo[] infos = calculation.getContinuityLineInfo();

    if( infos.length == 0 )
      throw new SimulationException( "Keine Randbedingungen definiert, Rechnung wird abgebrochen.", null );

    formatter.format( "SCL%9d%n", infos.length );

    for( final BoundaryLineInfo info : infos )
    {
      final IFE1D2DNode[] nodes = info.getNodes();

      for( int i = 0; i < nodes.length; i++ )
      {
        final IFE1D2DNode node = nodes[i];
        final int nodeID = Integer.parseInt( m_nodesIDProvider.get( node.getGmlID() ) );

        /* Write start stuff */
        if( i == 0 )
          formatter.format( "CC1 %4d", info.getID() );
        else if( i % 9 == 0 )
          formatter.format( "%nCC2     " );

        formatter.format( "%8d", nodeID );
      }
      if( nodes.length % 9 != 0 )
        formatter.format( "%n" );
    }

    formatter.format( "ECL%n" );

    final IControlModel1D2D controlModel = calculation.getControlModel();
    if( controlModel.getIDNOPT() != 0 && controlModel.getIDNOPT() != -1 ) // Write MP Line only under this conditions
    {
      formatter.format( "MP%21.2f%8.2f%8.2f%n", controlModel.getAC1(), controlModel.getAC2(), controlModel.getAC3() );
    }
    formatter.format( "ENDGEO%n" );
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

    /* Sort records by time */
    final TupleResultIndex index = new TupleResultIndex( result, compTime );
    final Iterator<IRecord> iterator = index.getIterator();
    if( !iterator.hasNext() )
      throw new SimulationException( "Zeitschritte leer, keine Rechnung möglich.", null );
    final IRecord firstRecord = iterator.next();

    /* Steady state, just one block for the Starting Date. */
    final int niti = controlModel.getNITI();

    final String msg = "Steady State Input Data";
    final float uRValSteady = ((BigDecimal) firstRecord.getValue( compUnderRelax )).floatValue();
    writeTimeStep( formatter, msg, null, null, uRValSteady, niti, timeStepInfos );

    /* Unsteady state: a block for each time step */
    Calendar lastStepCal = ((XMLGregorianCalendar) firstRecord.getValue( compTime )).toGregorianCalendar();

    for( int stepCount = 1; iterator.hasNext(); stepCount++ )
    {
      final IRecord record = iterator.next();
      final float uRVal = ((BigDecimal) record.getValue( compUnderRelax )).floatValue();
      final int nitn = controlModel.getNITN();

      final Calendar stepCal = ((XMLGregorianCalendar) record.getValue( compTime )).toGregorianCalendar();

      final String unsteadyMsg = String.format( "Unsteady State Input Data; Step: %4d", stepCount );
      writeTimeStep( formatter, unsteadyMsg, stepCal, lastStepCal, uRVal, nitn, timeStepInfos );

      lastStepCal = stepCal;
    }
  }

  private void writeTimeStep( final Formatter formatter, final String message, final Calendar stepCal, final Calendar lastStepCal, final float uRVal, final int nit, final ITimeStepinfo[] timeStepInfos ) throws SimulationException
  {
    final String dashes = StringUtils.repeat( "-", message.length() );
    formatter.format( "com %s%n", dashes );
    formatter.format( "com %s%n", message );
    formatter.format( "com %s%n", dashes );

    final long timeStepInterval;
    final double timeStepHours;
    final int year;
    final int dayOfYear;
    final double ihre;

    if( stepCal != null )
    {
      dayOfYear = stepCal.get( Calendar.DAY_OF_YEAR );
      year = stepCal.get( Calendar.YEAR );

      timeStepInterval = stepCal.getTimeInMillis() - lastStepCal.getTimeInMillis();
      timeStepHours = (double) timeStepInterval / (60 * 60 * 1000);
      ihre = getTimeInPercentage( stepCal );
    }
    else
    {
      dayOfYear = 0;
      year = 0;
      timeStepHours = 0;
      ihre = 0;
    }

    formatter.format( "DT%14.2f%8d%8d%8.2f", timeStepHours, year, dayOfYear, ihre );

    // BC lines
    if( nit == 0 )
      formatter.format( "%nBC%n" );
    else
      formatBC( formatter, uRVal, nit );

    formatBoundCondLines( formatter, stepCal, timeStepInfos, TYPE.CONTI_BC_Q );
    formatBoundCondLines( formatter, stepCal, timeStepInfos, TYPE.CONTI_BC_H );
    // add other conti lines types as well (weirs)?
    formatter.format( "ENDSTEP %s%n", message );
  }

  /** Formats the lines for one type of boundary condition (QC or HC or ... ). */
  private void formatBoundCondLines( final Formatter formatter, final Calendar stepCal, final ITimeStepinfo[] timeStepInfos, final TYPE contiType )
  {
    // Boundary Conditions
    for( final ITimeStepinfo item : timeStepInfos )
    {
      final double itemValue = item.getValue( stepCal == null ? null : stepCal.getTime() );
      final TYPE type = item.getType();

      /* Write only considered lines */
      if( type != contiType )
        continue;

      switch( type )
      {
        case CONTI_BC_Q:
        {
          final double q = itemValue;
          final double theta = item.getTheta();
          formatter.format( "QC%14d%8d%8.3f%8.3f%8.3f%8.3f%8.3f%n", item.getID(), 0, q, theta, 0.000, 20.000, 0.000 );
        }
          break;

        case CONTI_BC_H:
        {
          final double h = itemValue;
          formatter.format( "HC%14d%8d%8.3f%8.3f%8.3f%8.3f%n", item.getID(), 0, h, 0.0, 20.0, 0.0 );
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
      throw new SimulationException( "Wichtungsfaktor muss zwischen 0.0 und 1.0 liegen.", null );

    final int buffVal;
    if( uRVal == (float) 1.0 )
      buffVal = 0;
    else
      buffVal = 10 - (int) (((uRVal * 10)));

    for( int j = 0; j < nitn; j++ )
    {
      if( j % 9 == 0 )
        formatter.format( "%nBC      " );

      formatter.format( "%5d010", buffVal );
    }

    formatter.format( "%n" );
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
    // todo check if result is not empty
    if(result == null || result.size() == 0)
      throw new SimulationException( "Zeitschritte leer, keine Rechnung möglich.", null );
    final IRecord record = result.get( 0 );
    final IComponent res_C_0 = result.getComponents()[0];
    return DateUtilities.toDate( (XMLGregorianCalendar) record.getValue( res_C_0 ) );
  }

}
