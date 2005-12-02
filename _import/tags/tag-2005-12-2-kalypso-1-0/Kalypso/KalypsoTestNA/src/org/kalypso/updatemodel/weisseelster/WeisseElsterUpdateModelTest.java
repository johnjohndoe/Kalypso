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
package org.kalypso.updatemodel.weisseelster;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 * 
 * here are moethodes used for preparing the modell
 */
public class WeisseElsterUpdateModelTest extends TestCase
{

  private ObjectFactory m_zmlLinkFac;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    KalypsoTest.init();
    m_zmlLinkFac = new ObjectFactory();
  }

  public void testUpdateGMLTemperaturLinks() throws Exception
  {
    final URL inputModel = getClass().getResource( "resources/modell.gml" );
    final File outputFile = new File( "C:\\TMP\\modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputModel );
    final FeatureType catchmentFT = workspace.getFeatureType( "Catchment" );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      final TimeseriesLink link = m_zmlLinkFac.createTimeseriesLink();
      link.setHref( "Temperatur/Temperatur_" + feature.getId() + ".zml" );
      final FeatureProperty property = FeatureFactory.createFeatureProperty( "temperaturZR", link );
      feature.setProperty( property );
    }
    updateGMLTemperaturMapping( workspace );
    final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( outputFile ), "UTF-8" );
    GmlSerializer.serializeWorkspace( writer, workspace );
    IOUtils.closeQuietly( writer );
    System.out.println( "wrote new model to " + outputFile.toString() );
  }

  public void updateGMLTemperaturMapping( GMLWorkspace modelWorkspace ) throws Exception
  {
    final URL inputMappingURL = getClass().getResource( "resources/ObsTGebMapping.gml" );
    final File outputFile = new File( "C:\\TMP\\ObsTGebMapping.gml" );
    final GMLWorkspace mapWorkspace = GmlSerializer.createGMLWorkspace( inputMappingURL );
    final FeatureType mapFT = mapWorkspace.getFeatureType( "MappingObservation" );
    final Feature mapRootFeature = mapWorkspace.getRootFeature();
    final FeatureType catchmentFT = modelWorkspace.getFeatureType( "Catchment" );
    final Feature[] features = modelWorkspace.getFeatures( catchmentFT );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature modelFeature = features[i];
      // srcProp
      final TimeseriesLink srcLink = m_zmlLinkFac.createTimeseriesLink();
      srcLink.setHref( "Ombrometer/T_virtuell.zml" );
      final FeatureProperty srcProperty = FeatureFactory.createFeatureProperty( "inObservationLink", srcLink );
      // targetProp
      final Object targetLink = modelFeature.getProperty( "temperaturZR" );
      final FeatureProperty targetProperty = FeatureFactory.createFeatureProperty( "outObservationLink", targetLink );
      // nameProp
      final Object nameValue = modelFeature.getProperty( "inum" );
      final FeatureProperty nameProp = FeatureFactory.createFeatureProperty( "name", nameValue );
      // geoProp
      final Object geoValue = modelFeature.getProperty( "Ort" );
      final FeatureProperty geoProp = FeatureFactory.createFeatureProperty( "polygon", geoValue );

      // createFeature
      final Feature mapFeature = mapWorkspace.createFeature( mapFT );
      mapWorkspace.addFeatureAsComposition(mapRootFeature, "mappingMember",0, mapFeature);
      //set props
      mapFeature.setProperty( nameProp );
      mapFeature.setProperty( geoProp );
      mapFeature.setProperty( srcProperty );
      mapFeature.setProperty( targetProperty );
    }

    final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( outputFile ), "UTF-8" );
    GmlSerializer.serializeWorkspace( writer, mapWorkspace );
    IOUtils.closeQuietly( writer );
    System.out.println( "wrote new temperaturmapping to " + outputFile.toString() );
  }
}