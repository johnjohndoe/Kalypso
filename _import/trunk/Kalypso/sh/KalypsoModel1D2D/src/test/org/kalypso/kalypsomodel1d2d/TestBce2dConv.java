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
package test.org.kalypso.kalypsomodel1d2d;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.TypeIdAppendIdProvider;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

import junit.framework.TestCase;

/**
 * Test bce2d conversion
 * 
 * @author Patrice Congo
 *
 */
public class TestBce2dConv extends TestCase
{
  public void testConvertAgger()
  {
    File tmpFile=null;
    GMLWorkspace workspace =null;
    InputStream aggerStream=null;
    
    try
    {
      tmpFile=File.createTempFile( "test_convert_agger_gml", "gml" );
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
    
    try
    {
      aggerStream=
        TestWorkspaces.URL_AGGER_2D.openStream();
    }
    catch( IOException e )
    {
      fail(TestUtils.getStackTraceAsString( e ));
    }
    try
    {
      RMA10S2GmlConv.toDiscretisationModel( 
                              aggerStream, 
                              targetModel,
                              TestWorkspaces.getGaussKrueger(),
                              new TypeIdAppendIdProvider() );
    }
    catch(Throwable th)
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
//    try
//    {
//      File gmlFile= new File("C:\\temp\\agger_modell.gml");
//      OutputStreamWriter writer= 
//            new OutputStreamWriter(
//                new FileOutputStream(gmlFile));
//      
//      GmlSerializer.serializeWorkspace( writer,workspace );
//    }
//    catch(Throwable th)
//    {
//      fail( TestUtils.getStackTraceAsString( th ) );
//    }
  }
}
