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
import java.io.StringWriter;
import java.util.Locale;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
public class Control1D2DConverter
{
  final private GMLWorkspace m_controlWS;

  public Control1D2DConverter( final GMLWorkspace controlWS)
  {
    m_controlWS = controlWS;
    
  }

  public StringBuffer writeR10ControlData( )
  {
    final Feature controlRootFE = m_controlWS.getRootFeature();
    StringBuffer sb = new StringBuffer();
    StringWriter stringWriter = new StringWriter();
    PrintWriter printWriter = new PrintWriter( stringWriter );

    // ControlDataBlock
    sb.append( "OUTFIL  result\\Output\n" );
    sb.append( "INKALYPSmodel.2d\n" );
    sb.append( "CONTROL A 1 2d 0\n" );
    sb.append( "ENDFIL\n" );
    sb.append( "TI" ); // write Project name
    // C0
    String formatC0 = "C0      %8d%8d%8d%8d%8.6f%8d%8.2f%8.2f%8d";
    XMLGregorianCalendar calendar = (XMLGregorianCalendar) controlRootFE.getProperty( "startsim" );
    int year = calendar.getYear();
    // TODO:calc day of the year!
    int month = calendar.getMonth();
    int day = calendar.getDay();
    int hour = calendar.getHour();
    Integer property = (Integer) controlRootFE.getProperty( "IOPTZD" );
    Object[] c0Props = new Object[] { (Integer) controlRootFE.getProperty( "IOPTZD" ), (Integer) controlRootFE.getProperty( "IDNOPT" ), year, day, hour,
        (Integer) controlRootFE.getProperty( "IEDSW" ), (Double) controlRootFE.getProperty( "TBFACT" ), (Double) controlRootFE.getProperty( "TBMIN" ), (Integer) controlRootFE.getProperty( "IPROJ" ) };
    printWriter.printf( Locale.US, formatC0 + "\n", c0Props );
    sb.append( stringWriter.getBuffer() );
    // C1
    sb.append( "C1             0       1       1       0       0       0       0       0       0\n" ); // fixed
    // Line
    sb.append( "C2" ); // add OMEGA, ELEV
    sb.append( "1.0     1.0     1.0       1\n" ); // fixed Values
    sb.append( "C3         1.000   1.000   0.100\n" ); // add UDIR,HMIN,DSET,DSETD
    sb.append( "C4" ); // add SALI,TEMPI,SEDI,UINP,VINP,PRCNT,DMIX,
    sb.append( "C5" ); // add NITI,NITN,TSTART as fixed value-notused,NCYC
    sb.append( "       0       1       1       0       1       1\n" ); // fixed values
    sb.append( "CV" ); // add CONV_1,CONV_2,CONV_3
    sb.append( "0.050   0.050               1    0.05\n" ); // fixed values
    sb.append( "VEGETA\n" ); // for roughness with vegetation

    // Properties DataBlock
    // TODO add ED-Lines from roughnessDB
    // TODO add SCL and CCL-Lines and ECL from Continuity Lines nodes info - simulation model
    // what is about MP-Line - ask Nico
    sb.append( "ENDGEO\n" );

    // Time step DataBlock
    sb.append( "com -----------------------\n" + "com steady state input data\n" + "com -----------------------" );
    sb.append( "DT        " );// add DELTA
    // TODO ask Nico about BC-Lines (equal values for all Lines???)
    // TODO add continuity Lines inflow here (QC,HC)
    sb.append( "ENDSTEP  steady" );
    printWriter.close();
    return sb;
  }
}
