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
package test.org.kalypso.kalypsosimulationmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author madanago
 * 
 */
public class TestRoughnessClsCollection_2 extends TestCase
{
  public void testWorkspaceLoad( ) throws MalformedURLException, Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( new URL( "data/roughness.gml" ), null ); //$NON-NLS-1$

    final Feature root = workspace.getRootFeature();
    final RoughnessClsCollection roughnessClsList = (RoughnessClsCollection) root;
    final Iterator<IRoughnessCls> itr = roughnessClsList.getRoughnessClasses().iterator();

    while( itr.hasNext() )
    {
      final IRoughnessCls ir = itr.next();
      System.out.println( ir.getId() );
    }

    // assertEquals( "ColName1", rcc.getName() );
    // assertEquals("r2", rcc.getRoughnessByURI("uri_r2").getName());
    // assertEquals("uri_r2", rcc.getRoughnessByURI("uri_r2").getURI());
    /*
     * List<IRoughnessCls> rList = rcc.selectRoughnessByName( "Klass 2" ); assertEquals( 1, rList.size() );
     */
    // System.out.println("LIST="+rList);
    // assertEquals("uri_r2", rList.get(0).getURI());

  }
}
