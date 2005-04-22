/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.test;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.service.CalcJobClientBean;

/**
 * @author doemming
 *  
 */
public class NaModelCalcJobTest extends TestCase
{

  public void testRun() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );
    registry.registerTypeHandler( new DiagramTypeHandler() );
    final File baseDir = new File( "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST" );
    final File simDir = new File( baseDir, "sim" );
    final File ergDir = new File( baseDir, "output" );
    if( simDir.exists() )
      FileUtils.cleanDirectory( simDir );
    if( ergDir.exists() )
      FileUtils.cleanDirectory( ergDir );
 

    final CalcJobClientBean[] beans = new CalcJobClientBean[]
    {
        new CalcJobClientBean( NaModelConstants.MODELL_ID, "Modelldaten", "calc/calcCase.gml" ),

//        new CalcJobClientBean( NaModelConstants.CONTROL_ID, "Steuerdaten", "calc/.nacontrol.gml" ),
        new CalcJobClientBean( NaModelConstants.CONTROL_ID, "Steuerdaten", "calc/expertControl.gml" ),

        new CalcJobClientBean( NaModelConstants.OPTIMIZECONF_ID, "optimizefile", "calc/.sce.xml" ),     
        new CalcJobClientBean( NaModelConstants.META_ID, "MetaSteuerdaten", "calc/.calculation" ),
        new CalcJobClientBean( "NiederschlagDir", "niederschlag", "calc/Niederschlag/" ),
        new CalcJobClientBean( "ZuflussDir", "zufluesse", "calc/Zufluss/" ),
        new CalcJobClientBean( "PegelDir", "pegel", "calc/Pegel/" ), };
    try
    {
      final NaModelCalcJob job = new NaModelCalcJob();
      job.run( baseDir, beans );
      if(job.isSucceeded())
        System.out.println("berechnung ohne Fehler beendet :-)");
      else
        System.out.println(":-( fehler irgendwo");
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}