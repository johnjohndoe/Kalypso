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

import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.kalypsomodel1d2d.conv.IPositionProvider;
import org.kalypso.kalypsomodel1d2d.conv.XYZOffsetPositionProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * @author antanas
 * 
 */
public class Dejan_Test_Gml2RMA10s extends TestCase
{
  private URL test_RMA10sModelURL;

  private URL test_discretisationModelURL;

  private final String test_CoordinateSystem = TestWorkspaces.CS_KEY_GAUSS_KRUEGER;

  private void init( ) throws MalformedURLException
  {
    test_RMA10sModelURL = new URL( "file:/F:/_ECLIPSE/modell1D_OUTPUT.2d" );
    // test_discretisationModelURL = new URL( "file:/D:/working/discretisation.gml" );
    test_discretisationModelURL = new URL( "file:/F:/_ECLIPSE/discretisation.gml" );
  }

  /**
   * Test method for
   * {@link org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv#Gml2RMA10SConv(org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d, java.io.OutputStream, org.kalypso.kalypsomodel1d2d.conv.IPositionProvider, org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)}.
   */
  public final void testGml2RMA10SConv( ) throws MalformedURLException
  {
    init();
    GMLWorkspace workspace = null;

    try
    {
      workspace = GmlSerializer.createGMLWorkspace( test_discretisationModelURL, null );
      // workspace = TestWorkspaces.loadGMLWorkspace( test_discretisationModelURL, test_schemaLocation );
    }
    catch( Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }

    FE1D2DDiscretisationModel sourceModel = new FE1D2DDiscretisationModel( workspace.getRootFeature() );

    try
    {
      IPositionProvider positionProvider = new XYZOffsetPositionProvider( test_CoordinateSystem, 35 * 100000, 35 * 100000, 0 );
      // Gml2RMA10SConv converter = new Gml2RMA10SConv(sourceModel, test_RMA10sModelURL, positionProvider);
      // converter.toRMA10sModel();
      // converter.sysout();
      // converter.write();
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
  }
}
