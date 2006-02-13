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

package org.kalypso.floodrisk.damageAnalysis.test;

import junit.framework.TestCase;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.floodrisk.damageAnalysis.CalculateDamageJob;
import org.kalypso.floodrisk.process.impl.ProcessDataProvider;
import org.kalypso.floodrisk.process.impl.ProcessResultEater;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.ogc.gml.typehandler.ResourceFileTypeHandler;
import org.kalypso.simulation.core.ISimulationConstants;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationInfo;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;

public class CalculateDamageJobTest extends TestCase
{

  public void testRun()
  {
    //  initialize schemaCatalog
    final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new DeegreeUrlCatalog(),
        new UrlCatalogFloodRisk() } );
    GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );

    //  register typeHandler
    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    try
    {
      registry.registerTypeHandler( new RangeSetTypeHandler() );
      registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
      registry.registerTypeHandler( new ResourceFileTypeHandler() );
    }
    catch( TypeRegistryException e )
    {
      e.printStackTrace();
    }

    CalculateDamageJob job = new CalculateDamageJob();
    testCalculateDamage( job );
    testGetSpezifikation( job );
  }

  private void testCalculateDamage( CalculateDamageJob job )
  {
    String base = "D://Nadja//eclipse//runtime-workspace//Test_Risikoanalyse//";
    //Input
    int numInputBeans = 3;
    SimulationDataPath[] input = new SimulationDataPath[numInputBeans];
    SimulationDataPath input1 = new SimulationDataPath( CalculateDamageJob.LanduseRasterDataID, base
        + "Landuse//landuseData.gml" );
    input[0] = input1;
    SimulationDataPath input2 = new SimulationDataPath( CalculateDamageJob.WaterlevelDataID, base
        + "Control//waterlevelData.gml" );
    input[1] = input2;
    SimulationDataPath input3 = new SimulationDataPath( CalculateDamageJob.ContextModelID, base
        + "Control//contextModell.gml" );
    input[2] = input3;
    ProcessDataProvider inputProvider = new ProcessDataProvider( input );
    //Output
    int numOutputBeans = 2;
    SimulationDataPath[] output = new SimulationDataPath[numOutputBeans];
    SimulationDataPath output1 = new SimulationDataPath( CalculateDamageJob.DamageDirectoryID, base + "Damage" );
    output[0] = output1;
    SimulationDataPath output2 = new SimulationDataPath( CalculateDamageJob.AnnualDamageRasterDataID, base
        + "Damage//annualDamage.gml" );
    output[1] = output2;
    ProcessResultEater resultEater = new ProcessResultEater( output );

    SimulationInfo jobBean = new SimulationInfo( "", "", "CalculateDamageJob", ISimulationConstants.STATE.RUNNING, -1, "" );

    try
    {
      job.run( null, inputProvider, resultEater, jobBean );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
    }
  }

  public void testGetSpezifikation( CalculateDamageJob job )
  {
    System.out.println( "Spezifikation: " + job.getSpezifikation() );
  }

}