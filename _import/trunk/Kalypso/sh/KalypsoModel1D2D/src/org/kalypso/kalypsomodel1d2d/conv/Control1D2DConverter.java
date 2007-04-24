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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.kalypso.kalypsomodel1d2d.sim.RMA10Calculation;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
public class Control1D2DConverter
{

  public Control1D2DConverter( )
  {
    // will not be instantiated
  }

  public static void writeR10File( final RMA10Calculation calculation, final PrintWriter pw )
  {
    writeR10ControlDataBlock( calculation, pw );
   // writeR10PropertiesDataBlock( calculation, pw );
   // writeR10ContinuityLineDataBlock( calculation, pw );
    writeR10TimeStepDataBlock( calculation, pw );

  }

  /**
   * writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  public static void writeR10ControlDataBlock( final RMA10Calculation calculation, final PrintWriter pw )
  {
    Locale l = Locale.US;
    System.out.println( "OUTFIL  result\\Output" );
    // TODO: add hydrograph files if ready and necessary - ask Jessica
    // pw.println( "INELTFL " ); // Inflow Hydrograph Q
    // pw.println( "INELEV " ); // Tidalgraph Data H
    // pw.println( "INHYD " ); // Hydrograph Q
    // pw.println( "INCSTR " ); // Structures (later)
    // pw.println( "INTIMS " ); // Structures time series(later)
    System.out.println( "INKALYPSmodel.2d" );
   /// System.out.println( "CONTROL A " + calculation.getIaccyc() + " 2d 0" );
    if( calculation.getRestart() )
      System.out.println( "RESTART" );

    System.out.println( "ENDFIL" );
    System.out.println( "TI Projekt Name" ); // write Project name

//    // C0
//    String formatC0 = "C0             0%8d%8d%8d%8.3f%8d%8.3f%8.2f       0";
//    Object[] c0Props = new Object[] { calculation.getIDNOPT(), calculation.getStartYear(), calculation.getStartJulianDay(), calculation.getStartHour(), calculation.getIEDSW(),
//        calculation.getTBFACT(), calculation.getTBMIN() };
//    System.out.printf( l, formatC0 + "\n", c0Props );

    // C1
    System.out.println( "C1             0       1       1       0       0       0       0       0       0" ); // fixed

    // C2
    String formatC1 = "C2      %8.2f%8.3f     1.0     1.0     1.0       1";
    Object[] c1Props = new Object[] { calculation.getOMEGA(), calculation.getELEV() };
    System.out.printf( l, formatC1 + "\n", c1Props );

    // C3
    String formatC3 = "C3         1.000   1.000   0.100%8.1f%8.3f%8.3f%8.3f";
    Object[] c3Props = new Object[] { calculation.getUDIR(), calculation.getHMIN(), calculation.getDSET(), calculation.getDSETD() };
    System.out.printf( l, formatC3+ "\n", c3Props );

    // C4
    System.out.println( "C4          00.0    20.0     0.0" ); // fixed values

    // C5
    String formatC5 = "C5      %8d%8d        %8d       0       1       1       0       1       1";
    Object[] c5Props = new Object[] { calculation.getNITI(), calculation.getNITN(), calculation.getNCYC() };
    System.out.printf( l, formatC5+ "\n", c5Props );

    // CV
    String formatCV = "CV      %8.2f%8.2f%8.2f   0.050   0.050        %8d%8.2f";
    Object[] cvProps = new Object[] { calculation.getCONV_1(), calculation.getCONV_2(), calculation.getCONV_3(), calculation.getIDRPT(), calculation.getDRFACT() };
    System.out.printf( l, formatCV+ "\n", cvProps );

    // VEGETA
    if( calculation.getVegeta() )
      System.out.println( "VEGETA" );

    System.out.println( "KAL_BC" );
  }

  /**
   * writes the Properties Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private static void writeR10PropertiesDataBlock( RMA10Calculation calculation, PrintWriter pw )
  {
    Locale l = Locale.US;
    List list = calculation.getRoughnessClassList();
    Iterator iter = list.iterator();
    int counter = 0;
    while( iter.hasNext() )
    {
      final Feature roughnessFE = (Feature) iter.next();
      counter = counter + 1;

      // ED1
      String formatED1 = "ED1     %8d%8.1f%8.1f%8.1f%8.1f    -1.0  1.000   1.000";
      
      Double eddy = calculation.getViskosity( roughnessFE );
      Object[] ed1Props = new Object[] { counter, eddy, eddy, eddy, eddy };
      System.out.printf( l, formatED1+ "\n", ed1Props );

      // ED2
      System.out.println( "ED2                  0.6     0.6   0.001             20.        " );// fixed values

      // ED4
      String formatED4 = "ED4             %8.2f%8.1f%8.2f";
      Object[] ed4Props = new Object[] { calculation.getKs( roughnessFE ), calculation.getAxAy( roughnessFE ), calculation.getDp( roughnessFE ) };
      System.out.printf( l, formatED4+ "\n", ed4Props );
    }

  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private static void writeR10ContinuityLineDataBlock( RMA10Calculation calculation, PrintWriter pw )
  {
    Locale l = Locale.US;
    List list = calculation.getContinuityLineList();
    // SCL
    String formatSCL = "SCL     %4d";
    Object[] sclProps = new Object[] { list.size() };
    System.out.printf( l, formatSCL, sclProps );

    Iterator iter = list.iterator();
    while( iter.hasNext() )
      // CC1
      // CC2
      System.out.println( "ECL" );
    String formatMP = "MP             %8.2f%8.2f%8.2f";
    Object[] mpProps = new Object[] { calculation.getAC1(), calculation.getAC2(), calculation.getAC3() };
    System.out.printf( l, formatMP, mpProps );
    System.out.println( "ENDGEO" );
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private static void writeR10TimeStepDataBlock( RMA10Calculation calculation, PrintWriter pw )
  {
    Locale l = Locale.US;
    System.out.print( "com -----------------------\n" + "com steady state input data\n" + "com -----------------------" );
    
    // sb.append( "DT " );// add DELTA
    // // TODO ask Nico about BC-Lines (equal values for all Lines???)
    // // TODO add continuity Lines inflow here (QC,HC)
    // sb.append( "ENDSTEP steady" );

  }

}
