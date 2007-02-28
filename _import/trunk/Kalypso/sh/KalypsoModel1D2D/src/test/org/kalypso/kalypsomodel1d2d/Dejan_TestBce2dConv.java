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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.TypeIdAppendIdProvider;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

public class Dejan_TestBce2dConv extends TestCase
{
  private QName test_discretisationModelQName;

  private URL test_RMA10sModelURL;

  private URL test_discretisationModelURL;

  private final CS_CoordinateSystem test_CoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( TestWorkspaces.CS_KEY_GAUSS_KRUEGER );

  private void init( ) throws MalformedURLException
  {
    test_discretisationModelQName = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "DiscretisationModel" );
    test_RMA10sModelURL = new URL( "file:/D:/working/modell1D.2d" );
    test_discretisationModelURL = new URL( "file:/D:/working/modell1D.gml" );
  }

  public void testConvertAgger( ) throws MalformedURLException
  {
    init();
    File tmpFile = null;
    GMLWorkspace workspace = null;
    InputStream inputStream = null;

    try
    {
      tmpFile = File.createTempFile( "test_convert_temporary_gml", "gml" );
    }
    catch( IOException e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }

    try
    {
      workspace = FeatureFactory.createGMLWorkspace( test_discretisationModelQName, tmpFile.toURL(), GmlSerializer.DEFAULT_FACTORY );

    }
    catch( Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }

    FE1D2DDiscretisationModel targetModel = new FE1D2DDiscretisationModel( workspace.getRootFeature() );

    try
    {
      inputStream = test_RMA10sModelURL.openStream();
    }
    catch( IOException e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
    try
    {
      IPositionProvider positionProvider = new XYZOffsetPositionProvider( test_CoordinateSystem, 35 * 100000, 35 * 100000, 0 );
      RMA10S2GmlConv.toDiscretisationModel( inputStream, targetModel, positionProvider, new TypeIdAppendIdProvider() );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
    try
    {
      File file = new File(test_discretisationModelURL.getPath());
      OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream(file) );
      GmlSerializer.serializeWorkspace( writer, workspace );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
  }
}
