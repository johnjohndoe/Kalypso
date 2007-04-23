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
package org.kalypso.kalypsomodel1d2d.sim;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;
import junit.framework.TestCase;

/**
 * @author madanago
 *
 */
public class TestRMA10Calculation extends TestCase
{

  TestRMA10Calculation(){
    
  }
  
  TestRMA10Calculation(String name){
    super(name);    
  }
  private static final IFEDiscretisationModel1d2d createTestRMA100Calculation()
  {
    File tmpFile=null;
    GMLWorkspace workspace =null;
    InputStream aggerStream=null;
    
    try
    {
      String tempFileName = 
        "test_discretisation_model"+System.currentTimeMillis();
      tmpFile=File.createTempFile( tempFileName, "gml" );
    }
    catch( IOException e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
    
    try
    {
      workspace =
          FeatureFactory.createGMLWorkspace(
              Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel, 
              tmpFile.toURL(), 
              GmlSerializer.DEFAULT_FACTORY);
      
    }
    catch( Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ));
    }
    
    
    
    FE1D2DDiscretisationModel targetModel=
        new FE1D2DDiscretisationModel(workspace.getRootFeature());
    return targetModel;
  }
}
