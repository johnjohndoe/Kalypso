/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.updatemodel.weisseelster.inum2name;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * 
 * @author doemming
 */
public class Num2nameTest extends TestCase
{

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    KalypsoTest.init();
  }

  public void testUpdate() throws Exception
  {
    final URL modellURL = getClass().getResource( "resource/modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL );
    updateName( workspace, "inum", workspace.getFeatureType( "Catchment" ) );
    updateName( workspace, "inum", workspace.getFeatureType( "VirtualChannel" ) );
    updateName( workspace, "inum", workspace.getFeatureType( "KMChannel" ) );
    updateName( workspace, "inum", workspace.getFeatureType( "StorageChannel" ) );
    updateName( workspace, "num", workspace.getFeatureType( "Node" ) );
    final Writer writer = new FileWriter( new File( "C:\\TMP\\modell_out.gml" ) );
    GmlSerializer.serializeWorkspace( writer, workspace, "UTF-8" );
    IOUtils.closeQuietly( writer );
  }

  /**
   * 
   * @param workspace
   * @param propName
   * @param featureType
   */
  private void updateName( GMLWorkspace workspace, String propName, FeatureType featureType )
  {
    final Feature[] features = workspace.getFeatures( featureType );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      final Object value = feature.getProperty( propName );
      if( value != null )
      {
        final String orgName = (String)feature.getProperty( "name" );
        if( orgName == null || orgName.length() == 0 )
        {
          final FeatureProperty property = FeatureFactory.createFeatureProperty( "name", value );
          feature.setProperty( property );
        }
      }
    }
  }
}
