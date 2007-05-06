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
import java.util.Locale;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.conv.ITimeStepinfo.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
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

  private long timeStepInterval_;

  private double timeStepInterval;

  private TYPE CONTI_BC_Q;
  private TYPE CONTI_BC_H;
  TYPE _Q = CONTI_BC_Q;
  TYPE _H = CONTI_BC_H;
  //enum TYPE { CONTI, CONTI_BC_Q, CONTI_BC_H, ELE_BC_1D, ELE_BCE_2D, WQ };


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
   //writeR10PropertiesDataBlock( calculation, formatter );

    // TODO: contilines and stuff
    // count contilines and make ids (also for the virtual ones)

    // attach timeseries to ids inside of interpolationhelper classes...
    writeR10ContinuityLineDataBlock( calculation, formatter );
    writeR10TimeStepDataBlock( calculation, formatter );
  }

  /**
   * Writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10ControlDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    Date firstDate = getFirstTimeStep( calculation );
    Calendar calendarForFirstTimeStep = Calendar.getInstance();
    calendarForFirstTimeStep.setTime( firstDate );
    /* FILES DATA BLOCK */
    formatter.format( "OUTFIL  result\\Output%n" );
    formatter.format( "INKALYPSmodel.2d%n" );
    final int restartStep = calculation.getRestart() ? calculation.getIaccyc() : 0;
    formatter.format( "CONTROL A %4d 2d 0%n", restartStep );
    if( calculation.getRestart() )
      formatter.format( "RESTART%n" );

    formatter.format( "ENDFIL%n" );

    /* CONTROL DATA BLOCK */
    // TODO: write given name of szenario/projekt
    formatter.format( "TI Projekt Name%n" ); // write Project name

    // // C0
//    Object[] c0Props = new Object[] { 0, calculation.getIDNOPT(), calculation.getStartYear(), calculation.getStartJulianDay(), calculation.getStartHour(), calculation.getIEDSW(),
//        calculation.getTBFACT(), calculation.getTBMIN(), 0 };
    Object[] c0Props = new Object[] { 0, 
                                        calculation.getIDNOPT(), 
                                        calendarForFirstTimeStep.get( Calendar.YEAR ),
                                        calendarForFirstTimeStep.get( Calendar.DAY_OF_YEAR ),
                                        getTimeInPercentage( firstDate ),
                                        calculation.getIEDSW(),
                                        calculation.getTBFACT(),
                                        calculation.getTBMIN(), 
                                        0 };
    // TODO: also write the (at the moment) constant values with the corrct format strings (%xxx.yyf),so later we
    // can easily exchange the given values without thinking about format any more (applies to all Cx)
    formatter.format( "C0%14d%8d%8d%8d%8.3f%8d%8.3f%8.2f%8d%n", c0Props );

    // C1
    formatter.format( "C1%14d%8d%8d%8d%8d%8d%8d%8d%8d%8d%n", 0, 1, 1, 0, 0, 0, 0, 0, 0, 2 );

    // C2
    formatter.format( "C2%14.2f%8.3f%8.1f%8.1f%8.1f%8d%n", calculation.getOMEGA(), calculation.getELEV(), 1.0, 1.0, 1.0, 1 );

    // C3
    formatter.format( "C3%14.3f%8.3f%8.3f%8.1f%8.3f%8.3f%8.3f%n", 1.0, 1.0, calculation.getUNOM(), calculation.getUDIR(), calculation.getHMIN(), calculation.getDSET(), calculation.getDSETD() );

    // C4
    formatter.format( "C4%14.1f%8.1f%8.1f%n", 0.0, 20.0, 0.0 );

    // C5
    formatter.format( "C5%14d%8d%16d%8d%8d%8d%8d%8d%8d%n", calculation.getNITI(), calculation.getNITN(), calculation.getNCYC(), 0, 1, 1, 0, 1, 1 );

    // CV
    formatter.format( "CV%14.2f%8.2f%8.2f%8.2f%8.2f%16d%8.2f%n", calculation.getCONV_1(), calculation.getCONV_2(), calculation.getCONV_3(), 0.05, 0.05, calculation.getIDRPT(), calculation.getDRFACT() );

    // VEGETA
    if( calculation.getVegeta() )
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
      double val = 2900.0;
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

  private void writeR10ContinuityLineDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    final ContinuityLineInfo[] infos = calculation.getContinuityLineInfo();

    formatter.format( "SCL%9d%n", infos.length );

    for( final ContinuityLineInfo info : infos )
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
          formatter.format( "%nCC2 " );

        formatter.format( "%8d", nodeID );
      }

      if( nodes.length % 9 != 0 )
        formatter.format( "%n" );

    }

    formatter.format( "ECL%n" );

    if( calculation.getIDNOPT() != 0 && calculation.getIDNOPT() != -1 ) // Write MP Line only under this conditions
    {
      formatter.format( "MP%21.2f%8.2f%8.2f%n", calculation.getAC1(), calculation.getAC2(), calculation.getAC3() );
    }
    formatter.format( "ENDGEO%n" );
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10TimeStepDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    formatter.format( "com -----------------------%n" );
    formatter.format( "com steady state input data%n" );
    formatter.format( "com -----------------------%n" );
    formatter.format( "DT        0.0000%n" );
    formatter.format( "BC          8010    8010    8010    7010    6010    5010    4010    2010    0010%n" );
    formatter.format( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n" );
    formatter.format( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n" );
    formatter.format( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n" );
    formatter.format( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n" );
    formatter.format( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n" );
    formatter.format( "QC             1       0   19.65   0.000   0.000  20.000   0.000%n" );
    formatter.format( "QC             2       0   19.20   0.000   0.000  20.000   0.000%n" );
    formatter.format( "HC             3       0    5.69   00.00    20.0     0.0%n" );
    formatter.format( "ENDSTEP  steady%n" );

    Feature controlFeature = calculation.getControlModelFeature();

    IControlModel1D2D controlModel_ = (IControlModel1D2D) controlFeature.getAdapter( IControlModel1D2D.class );
    IObservation<TupleResult> tupleSet = controlModel_.getTimeSteps();

    TupleResult result = tupleSet.getResult();

    Iterator<IRecord> iterator = result.iterator();

    IComponent res_C_0 = result.getComponents()[0];
    IComponent res_C_1 = result.getComponents()[1];
    ArrayList<Date> timeAndDate = new ArrayList<Date>();
    ArrayList<BigDecimal> underRelaxFactors = new ArrayList<BigDecimal>();
    
    // XMLGregorianCalendar
    while( iterator.hasNext() )
    {
      IRecord record = iterator.next();
      timeAndDate.add( DateUtilities.toDate( (XMLGregorianCalendar) record.getValue( res_C_0 ) ) );
      underRelaxFactors.add( (BigDecimal) record.getValue( res_C_1 ) );
    }
    
    final ITimeStepinfo[] timeStepInfos = calculation.getTimeStepInfos();
     

    for( int i = 1; i < timeAndDate.size(); i++ )
    {
      Calendar instance = Calendar.getInstance();
      instance.setTime( timeAndDate.get( i ) );    
      
      Calendar instance_3 = Calendar.getInstance();
      instance_3.setTime( timeAndDate.get( i-1 ) );
      
      timeStepInterval_ =  instance.getTimeInMillis()-instance_3.getTimeInMillis();
           
      timeStepInterval = (double)timeStepInterval_ / (60*60*1000);
           
      formatter.format( "com -----------------------%n" );
      formatter.format( "com unsteady " + i +"%n");
      formatter.format( "com -----------------------%n" );
      //Calendar instance = Calendar.getInstance();
      instance.setTime( timeAndDate.get( i ) );
      //instance.get( Calendar. )
      int dayOfYear = instance.get( Calendar.DAY_OF_YEAR );
      instance.setTime( timeAndDate.get( i ) );
      int _year = instance.get( Calendar.YEAR ); 
      formatter.format( "DT " +
                  " " + timeStepInterval +
                  " " + _year +                    
                  " " + dayOfYear +
                  " " + getTimeInPercentage(timeAndDate.get( i ))+"%n" );      
      BigDecimal uRVal= underRelaxFactors.get( i ); 
      formatter.format("BC          "+uRVal+"010    "+uRVal+"010    "+uRVal+"010    "+uRVal+"010    "+uRVal+"010    "+uRVal+"010    "+uRVal+"010    "+uRVal+"010%n");
      formatter.format("BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n");
      formatter.format("BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n");
      formatter.format("BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n");
      formatter.format("BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n");
      formatter.format("BC          0010    0010    0010    0010    0010    0010    0010    0010    0010%n");                
      for (ITimeStepinfo item : timeStepInfos) {
        TYPE type = item.getType();
        if (type == TYPE.CONTI_BC_Q)
        {     formatter.format(type+
                " "+ item.getID() +
                " "+ item.getValue( timeAndDate.get( i )) +
                "   0.000  20.000   0.000%n"); 
        }
        if (type == TYPE.CONTI_BC_H) {
            formatter.format(type +
                " "+ item.getID() +
                " "+ item.getValue(timeAndDate.get( i )) +
                "    20.0     0.0%n");            
        }
      }      
      formatter.format("ENDSTEP  steady%n");    
    }
  }

  private double getTimeInPercentage(Date _date )
  {
    Calendar instance_ = Calendar.getInstance();
    instance_.setTime( _date );
    int i = instance_.get(Calendar.HOUR_OF_DAY);
    int j2 = instance_.get(Calendar.MINUTE);
    float j = (float) (j2/60.0);
    //return instance_.get(Calendar.HOUR_OF_DAY)+"."+(instance_.get(Calendar.MINUTE)*100)/60;
    return (double)i+j;
  }
  
  private Date getFirstTimeStep(final RMA10Calculation calculation) {
    Feature controlFeature = calculation.getControlModelFeature();
    IControlModel1D2D controlModel_ = (IControlModel1D2D) controlFeature.getAdapter( IControlModel1D2D.class );
    IObservation<TupleResult> tupleSet = controlModel_.getTimeSteps();
    TupleResult result = tupleSet.getResult();
    IRecord record = result.get( 0 );

    IComponent res_C_0 = result.getComponents()[0];     
    return DateUtilities.toDate( (XMLGregorianCalendar) record.getValue( res_C_0 ) );
    
  }

}
