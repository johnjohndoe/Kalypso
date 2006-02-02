package org.kalypso.ui.wizard.wfs.test;

import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

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

public class WFSTest extends TestCase
{

  final String[] layers = { "hq10_gesamt", "hq50_gesamt", "hq100_gesamt", "Haltungen" };

  final String base = "http://134.28.87.71:8080/deegreewms/wfs";

  public void testLayers( ) throws MalformedURLException
  {
    for( int i = 0; i < layers.length; i++ )
      testLayer( layers[i] );
  }

  public void testLayer( String layer ) throws MalformedURLException
  {
    URL url = new URL( base + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + layer );
    GMLSchema schema = null;
    try
    {
      schema = new GMLSchema( url );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    FeatureType[] featureTypes = schema.getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      FeatureType type = featureTypes[i];
      System.out.println( type.getNamespace() + ":" + type.getName() );
      System.out.println( type.getNamespace() + ":" + type.getName() );

    }
  }
}