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

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
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

  public void writeR10File( final RMA10Calculation calculation, final PrintWriter pw )
  {
    final Formatter formatter = new Formatter( pw, Locale.US );

    writeR10ControlDataBlock( calculation, formatter );
    writeR10PropertiesDataBlock( calculation, formatter );
    writeR10ContinuityLineDataBlock( calculation, formatter );
    writeR10TimeStepDataBlock( calculation, formatter );
  }

  /**
   * writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10ControlDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
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
    Object[] c0Props = new Object[] { calculation.getIDNOPT(), calculation.getStartYear(), calculation.getStartJulianDay(), calculation.getStartHour(), calculation.getIEDSW(),
        calculation.getTBFACT(), calculation.getTBMIN() };
    // TODO: also write the (at the moment) constant values with the corrct format strings (%xxx.yyf),so later we
    // can easily exchange the given values without thinking about format any more (applies to all Cx)
    formatter.format( "C0             0%8d%8d%8d%8.3f%8d%8.3f%8.2f       0%n", c0Props );

    // C1
    formatter.format( "C1             0       1       1       0       0       0       0       0       0       2%n" ); // fixed

    // C2
    String formatC1 = "C2      %8.2f%8.3f     1.0     1.0     1.0       1%n";
    formatter.format( formatC1 + "\n", calculation.getOMEGA(), calculation.getELEV() );

    // C3
    String formatC3 = "C3         1.000   1.000%8.3f%8.1f%8.3f%8.3f%8.3f%n";
    formatter.format( formatC3, calculation.getUNOM(), calculation.getUDIR(), calculation.getHMIN(), calculation.getDSET(), calculation.getDSETD() );

    // C4
    formatter.format( "C4          00.0    20.0     0.0%n" ); // fixed values

    // C5
    String formatC5 = "C5      %8d%8d        %8d       0       1       1       0       1       1%n";
    formatter.format( formatC5, calculation.getNITI(), calculation.getNITN(), calculation.getNCYC() );

    // CV
    String formatCV = "CV      %8.2f%8.2f%8.2f   0.050   0.050        %8d%8.2f%n";
    formatter.format( formatCV, calculation.getCONV_1(), calculation.getCONV_2(), calculation.getCONV_3(), calculation.getIDRPT(), calculation.getDRFACT() );

    // VEGETA
    if( calculation.getVegeta() )
      formatter.format( "VEGETA%n" );

    formatter.format( "KAL_BC%n" );
  }

  /**
   * writes the Properties Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10PropertiesDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    for( final Object elt : calculation.getRoughnessClassList() )
    {
      final Feature roughnessCL = (Feature) elt;
      final int roughnessAsciiID = getRoughnessID( roughnessCL.getId() );

      String formatED1 = "ED1     %8d%8.1f%8.1f%8.1f%8.1f    -1.0  1.000   1.000%n";
      // Double eddy = calculation.getViskosity( roughnessCL );
      Double val = 2900.;
      formatter.format( formatED1, roughnessAsciiID, val, val, val, val );
      // ED2
      String formatED2 = "ED2     %8.1f%8.1f%8.3f%16.1f%n";
      formatter.format( formatED2, new Double( 0.5 ), new Double( 0.5 ), new Double( 0.001 ), new Double( 20. ) );

      // ED4
      String formatED4 = "ED4             %8.2f%8.1f%8.2f%n";
      formatter.format( formatED4 + "\n", calculation.getKs( roughnessCL ), calculation.getAxAy( roughnessCL ), calculation.getDp( roughnessCL ) );
    }
  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private void writeR10ContinuityLineDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    final List<IFE1D2DContinuityLine> continuityLineList = calculation.getContinuityLineList();
    // TODO: no, this is wrong. There will be additional ('virtual') conti-lines comming from the boundary conditions

    formatter.format( "SCL     %4d", continuityLineList.size() );

    // TODO: consider cased where number of contilines is 0 (line break, ECL, ...)

    int lineID = 1;
    for( final IFE1D2DContinuityLine<IFE1D2DComplexElement, IFE1D2DEdge> line : continuityLineList )
    {
      final List<IFE1D2DNode> nodes = line.getNodes();
      final Iterator<IFE1D2DNode> iterator = nodes.iterator();
      boolean cc1Written = false;
      ArrayList<String> nodeIDs = new ArrayList<String>();
      while( iterator.hasNext() )
      {
        final IFE1D2DNode node = iterator.next();
        nodeIDs.add( m_nodesIDProvider.get( node.getGmlID() ) );
        if( nodeIDs.size() == 9 )
        {
          if( !cc1Written )
          {
            final String formatCC1 = "CC1 %4d%8d%8d%8d%8d%8d%8d%8d%8d%8d";
            Object[] mpProps = new Object[10];
            mpProps[0] = lineID++;
            for( int i = 1; i < 10; i++ )
              mpProps[i] = Integer.parseInt( nodeIDs.get( i - 1 ) );
            formatter.format( formatCC1, mpProps );
            cc1Written = true;
          }
          else
          {
            final String formatCC2 = "CC2 %8d%8d%8d%8d%8d%8d%8d%8d%8d";
            Object[] mpProps = new Object[9];
            for( int i = 0; i < 9; i++ )
              mpProps[i] = Integer.parseInt( nodeIDs.get( i ) );
            formatter.format( formatCC2, mpProps );
          }
          nodeIDs.clear();
        }

        // 01-03 I ID(1:3) "CC1"
        // 05-08 I ID(5:8) Definitionsnummer der Kontinuit‰tslinie
        // 09-80 I LINE(J,K) Liste der ersten bis zu 9 Eckknoten, die die Kontinuit‰tslinie definieren.

        // CC2
        // 01-03 I ID "CC2"
        // 09-80 I LINE(J,K) Liste der folgenden bis zu 9 Eckknoten, Kontinuit‰tslinie definieren.
      }
      if( nodeIDs.size() > 0 )
      {
        String subFormat = "";
        for( int i = 0; i < nodeIDs.size(); i++ )
          subFormat.concat( "%8d" );
        if( !cc1Written )
        {
          final String formatCC1 = "CC1 %4d".concat( subFormat );
          Object[] mpProps = new Object[nodeIDs.size() + 1];
          mpProps[0] = lineID++;
          for( int i = 1; i < nodeIDs.size(); i++ )
            mpProps[i] = Integer.parseInt( nodeIDs.get( i - 1 ) );
          formatter.format( formatCC1, mpProps );
          cc1Written = true;
        }
        else
        {
          final String formatCC2 = "CC2".concat( subFormat );
          Object[] mpProps = new Object[nodeIDs.size()];
          for( int i = 0; i < nodeIDs.size(); i++ )
            mpProps[i] = Integer.parseInt( nodeIDs.get( i ) );
          formatter.format( formatCC2, mpProps );
        }
      }
      formatter.format( "ECL%n" );
    }

    if( calculation.getIDNOPT() != 0 && calculation.getIDNOPT() != -1 ) // Write MP Line only under this conditions
    {
      String formatMP = "MP             %8.2f%8.2f%8.2f%n";
      Object[] mpProps = new Object[] { calculation.getAC1(), calculation.getAC2(), calculation.getAC3() };
      formatter.format( formatMP, mpProps );
    }
    formatter.format( "ENDGEO%n" );
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private void writeR10TimeStepDataBlock( final RMA10Calculation calculation, final Formatter formatter )
  {
    System.out.println( "com -----------------------" );
    System.out.println( "com steady state input data" );
    System.out.println( "com -----------------------" );
    System.out.println( "DT        0.0000" );
    System.out.println( "BC          8010    8010    8010    7010    6010    5010    4010    2010    0010" );
    System.out.println( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010" );
    System.out.println( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010" );
    System.out.println( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010" );
    System.out.println( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010" );
    System.out.println( "BC          0010    0010    0010    0010    0010    0010    0010    0010    0010" );
    System.out.println( "QC             1       0   19.65   0.000   0.000  20.000   0.000" );
    System.out.println( "QC             2       0   19.20   0.000   0.000  20.000   0.000" );
    System.out.println( "HC             3       0    5.69   00.00    20.0     0.0" );
    System.out.println( "ENDSTEP  steady" );

    Feature controlFeature = calculation.getControlModelFeature();

    // IPropertyType property = controlFeature.getFeatureType().getProperty(
    // Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
    // Object property2 = controlFeature.getProperty(
    // Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );

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

    // TODO: chek if timeserie is filled or not...
    long timeStepInterval = timeAndDate.get( 1 ).getTime() - timeAndDate.get( 0 ).getTime();
    timeStepInterval = timeStepInterval / (60 * 60);

    for( int i = 0; i < timeAndDate.size(); i++ )
    {
      System.out.println( "com -----------------------" );
      System.out.println( "com unsteady " + i );
      System.out.println( "com -----------------------" );
      Calendar instance = Calendar.getInstance();
      instance.setTime( timeAndDate.get( i ) );
      int dayOfYear = instance.get( Calendar.DAY_OF_YEAR );
      System.out.println( "DT " + timeStepInterval + " " + timeAndDate.get( i ).getYear() + " " + dayOfYear + " " + (timeAndDate.get( i ).getSeconds() / ((60 * 60) * 100)) );
    }

    // sb.append( "DT " );// add DELTA
    // // TODO ask Nico about BC-Lines (equal values for all Lines???)
    // // TODO add continuity Lines inflow here (QC,HC)
    // sb.append( "ENDSTEP steady" );

  }

}
