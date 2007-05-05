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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
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
  private static LinkedHashMap<String, String> m_roughnessIDProvider;

  private static LinkedHashMap<String, String> m_nodesIDProvider;

  public Control1D2DConverter( )
  {
    // will not be instantiated
  }

  public static final void setRoughnessIDProvider( LinkedHashMap<String, String> map )
  {
    m_roughnessIDProvider = map;
  }

  public static final void setNodesIDProvider( LinkedHashMap<String, String> map )
  {
    m_nodesIDProvider = map;
  }

  private static int getRoughnessID( final String gmlID )
  {
    return Integer.parseInt( m_roughnessIDProvider.get( gmlID ) );
  }

  public static void writeR10File( final RMA10Calculation calculation, final PrintWriter pw )
  {
    writeR10ControlDataBlock( calculation, pw );
    writeR10PropertiesDataBlock( calculation, pw );
    writeR10ContinuityLineDataBlock( calculation, pw );
    writeR10TimeStepDataBlock( calculation, pw );

  }

  /**
   * writes the Control Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private static void writeR10ControlDataBlock( final RMA10Calculation calculation, final PrintWriter writer )
  {
    Locale locale = Locale.US;
    writer.println( "OUTFIL  result\\Output" );
    writer.println( "INKALYPSmodel.2d" );
    // / writer.println( "CONTROL A " + calculation.getIaccyc() + " 2d 0" );
    if( calculation.getRestart() )
      writer.println( "RESTART" );

    writer.println( "ENDFIL" );
    writer.println( "TI Projekt Name" ); // write Project name

    // // C0
    String formatC0 = "C0             0%8d%8d%8d%8.3f%8d%8.3f%8.2f       0";
    Object[] c0Props = new Object[] { calculation.getIDNOPT(), calculation.getStartYear(), calculation.getStartJulianDay(), calculation.getStartHour(), calculation.getIEDSW(),
        calculation.getTBFACT(), calculation.getTBMIN() };
    writer.printf( locale, formatC0 + "\n", c0Props );

    // C1
    writer.println( "C1             0       1       1       0       0       0       0       0       0" ); // fixed

    // C2
    String formatC1 = "C2      %8.2f%8.3f     1.0     1.0     1.0       1";
    Object[] c1Props = new Object[] { calculation.getOMEGA(), calculation.getELEV() };
    writer.printf( locale, formatC1 + "\n", c1Props );

    // C3
    String formatC3 = "C3         1.000   1.000%8.3f%8.1f%8.3f%8.3f%8.3f";
    Object[] c3Props = new Object[] { calculation.getUNOM(), calculation.getUDIR(), calculation.getHMIN(), calculation.getDSET(), calculation.getDSETD() };
    writer.printf( locale, formatC3 + "\n", c3Props );

    // C4
    writer.println( "C4          00.0    20.0     0.0" ); // fixed values

    // C5
    String formatC5 = "C5      %8d%8d        %8d       0       1       1       0       1       1";
    Object[] c5Props = new Object[] { calculation.getNITI(), calculation.getNITN(), calculation.getNCYC() };
    writer.printf( locale, formatC5 + "\n", c5Props );

    // CV
    String formatCV = "CV      %8.2f%8.2f%8.2f   0.050   0.050        %8d%8.2f";
    Object[] cvProps = new Object[] { calculation.getCONV_1(), calculation.getCONV_2(), calculation.getCONV_3(), calculation.getIDRPT(), calculation.getDRFACT() };
    writer.printf( locale, formatCV + "\n", cvProps );

    // VEGETA
    if( calculation.getVegeta() )
      writer.println( "VEGETA" );

    writer.println( "KAL_BC" );
  }

  /**
   * writes the Properties Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private static void writeR10PropertiesDataBlock( RMA10Calculation calculation, PrintWriter writer )
  {
    Locale locale = Locale.US;
    List list = calculation.getRoughnessClassList();

    Iterator iterator = list.iterator();
    while( iterator.hasNext() )
    {
      final Feature roughnessCL = (Feature) iterator.next();
      final int roughnessAsciiID = getRoughnessID( roughnessCL.getId() );

      String formatED1 = "ED1     %8d%8.1f%8.1f%8.1f%8.1f    -1.0  1.000   1.000";
      // Double eddy = calculation.getViskosity( roughnessCL );
      Double val = 2900.;
      Object[] ed1Props = new Object[] { roughnessAsciiID, val, val, val, val };
      writer.printf( locale, formatED1 + "\n", ed1Props );
      // ED2
      String formatED2 = "ED2     %8.1f%8.1f%8.3f%16.1f";
      writer.printf( locale, formatED2 + "\n", new Object[] { new Double( 0.5 ), new Double( 0.5 ), new Double( 0.001 ), new Double( 20. ) } );

      // ED4
      String formatED4 = "ED4             %8.2f%8.1f%8.2f";
      Object[] ed4Props = new Object[] { calculation.getKs( roughnessCL ), calculation.getAxAy( roughnessCL ), calculation.getDp( roughnessCL ) };
      writer.printf( locale, formatED4 + "\n", ed4Props );
    }
  }

  /**
   * writes the Continuity Lines Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */

  private static void writeR10ContinuityLineDataBlock( RMA10Calculation calculation, PrintWriter writer )
  {
    Locale locale = Locale.US;
    List<IFE1D2DContinuityLine> continuityLineList = calculation.getContinuityLineList();
    // SCL
    String formatSCL = "SCL     %4d";
    Object[] sclProps = new Object[] { continuityLineList.size() };
    writer.printf( locale, formatSCL, sclProps );

    final Iterator<IFE1D2DContinuityLine> iter = continuityLineList.iterator();
    int lineID = 1;
    while( iter.hasNext() )
    {
      final IFE1D2DContinuityLine line = iter.next();
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
          if(!cc1Written){
            final String formatCC1 = "CC1 %4d%8d%8d%8d%8d%8d%8d%8d%8d%8d";
            Object[] mpProps = new Object[10];
            mpProps[0] = lineID++;
            for(int i=1; i<10;i++) mpProps[i] = Integer.parseInt( nodeIDs.get( i-1 ));
            writer.printf( locale, formatCC1, mpProps );
            cc1Written = true;
          }
          else
          {
            final String formatCC2 = "CC2 %8d%8d%8d%8d%8d%8d%8d%8d%8d";
            Object[] mpProps = new Object[9];
            for(int i=0; i<9;i++) mpProps[i] = Integer.parseInt( nodeIDs.get( i ));
            writer.printf( locale, formatCC2, mpProps );
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
      if(nodeIDs.size() > 0){
        String subFormat = "";
        for(int i=0; i< nodeIDs.size(); i++)
          subFormat.concat( "%8d" );
        if(!cc1Written){
          final String formatCC1 = "CC1 %4d".concat( subFormat );
          Object[] mpProps = new Object[nodeIDs.size() + 1];
          mpProps[0] = lineID++;
          for(int i=1; i<nodeIDs.size();i++) mpProps[i] = Integer.parseInt( nodeIDs.get( i-1 ));
          writer.printf( locale, formatCC1, mpProps );
          cc1Written = true;
        }
        else
        {
          final String formatCC2 = "CC2".concat( subFormat );
          Object[] mpProps = new Object[nodeIDs.size()];
          for(int i=0; i<nodeIDs.size();i++) mpProps[i] = Integer.parseInt( nodeIDs.get( i ));
          writer.printf( locale, formatCC2, mpProps );
        }
      }
      writer.println( "ECL" );
    }
    if( calculation.getIDNOPT() != 0 && calculation.getIDNOPT() != -1 ) // Write MP Line only under this conditions
    {
      String formatMP = "MP             %8.2f%8.2f%8.2f";
      Object[] mpProps = new Object[] { calculation.getAC1(), calculation.getAC2(), calculation.getAC3() };
      writer.printf( locale, formatMP, mpProps );
    }
    writer.println( "ENDGEO" );
  }

  /**
   * writes the Timestep Data Block of the RMA10 controlFile (*.R10) into the PrintWriter
   */
  private static void writeR10TimeStepDataBlock( RMA10Calculation calculation, PrintWriter writer )
  {
    // Locale locale = Locale.US;
    System.out.println( "com -----------------------" );
    System.out.println( "com steady state input data" );
    System.out.println( "com -----------------------" );
    Feature controlFeature = calculation.getControlModelFeature();
    // IControlModel1D2D controlModel = new ControlModel1D2D(controlFeature);
    IPropertyType property = controlFeature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
    Object property2 = controlFeature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );

//    IPropertyType property = controlFeature.getFeatureType().getProperty(
//        Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
//    Object property2 = controlFeature.getProperty( 
//        Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
    
    IControlModel1D2D controlModel_ = (IControlModel1D2D) controlFeature.getAdapter( IControlModel1D2D.class );
    IObservation<TupleResult> tupleSet = controlModel_.getTimeSteps();
    System.out.println( "Res :" + tupleSet.getResult() );
    TupleResult result = tupleSet.getResult();
    
    Iterator<IRecord> iterator = result.iterator();
    
    IComponent res_C_0 = result.getComponents()[0];
    IComponent res_C_1 = result.getComponents()[1];
    //IComponent res_C_2 = result.getComponents()[2];
    while (iterator.hasNext()) {
     IRecord record = iterator.next();
     System.out.print("Time :"+record.getValue( res_C_0 ));
     System.out.print("  UnderRelax :"+record.getValue( res_C_1 ));
     //System.out.println("  Q :"+record.getValue( res_C_2 ));
    }
    
    // sb.append( "DT " );// add DELTA
    // // TODO ask Nico about BC-Lines (equal values for all Lines???)
    // // TODO add continuity Lines inflow here (QC,HC)
    // sb.append( "ENDSTEP steady" );

  }

}
