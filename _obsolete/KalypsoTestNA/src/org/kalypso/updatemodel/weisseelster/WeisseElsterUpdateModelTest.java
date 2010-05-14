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
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming here are moethodes used for preparing the modell
 */
public class WeisseElsterUpdateModelTest extends TestCase
{

  private ObjectFactory m_zmlLinkFac;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    m_zmlLinkFac = new ObjectFactory();
  }

  public void testUpdateGMLTemperaturLinks( ) throws Exception
  {
    final URL inputModel = getClass().getResource( "resources/modell.gml" );
    final File outputFile = new File( "C:\\TMP\\modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputModel, null );
    final IFeatureType catchmentFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final Feature[] features = workspace.getFeatures( catchmentFT );
    for( final Feature feature : features )
    {
      final TimeseriesLinkType link = m_zmlLinkFac.createTimeseriesLinkType();
      link.setHref( "Temperatur/Temperatur_" + feature.getId() + ".zml" );
      feature.setProperty( NaModelConstants.CATCHMENT_PROP_ZR_TEMPERATUR, link );
    }
    updateGMLTemperaturMapping( workspace );
    final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( outputFile ), "UTF-8" );
    GmlSerializer.serializeWorkspace( writer, workspace );
    IOUtils.closeQuietly( writer );
    System.out.println( "wrote new model to " + outputFile.toString() );
  }

  public void updateGMLTemperaturMapping( final GMLWorkspace modelWorkspace ) throws Exception
  {
    final URL inputMappingURL = getClass().getResource( "resources/ObsTGebMapping.gml" );
    final File outputFile = new File( "C:\\TMP\\ObsTGebMapping.gml" );
    final GMLWorkspace mapWorkspace = GmlSerializer.createGMLWorkspace( inputMappingURL, null );
    final IFeatureType mapFT = mapWorkspace.getFeatureType( "MappingObservation" );
    final Feature mapRootFeature = mapWorkspace.getRootFeature();
    final IFeatureType catchmentFT = modelWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final Feature[] features = modelWorkspace.getFeatures( catchmentFT );
    for( final Feature modelFeature : features )
    {
      // srcProp
      final TimeseriesLinkType srcLink = m_zmlLinkFac.createTimeseriesLinkType();
      srcLink.setHref( "Ombrometer/T_virtuell.zml" );
      // targetProp
      final Object targetLink = modelFeature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_TEMPERATUR );
      // nameProp
      final Object nameValue = modelFeature.getProperty( "inum" );
      // geoProp
      final Object geoValue = modelFeature.getProperty( NaModelConstants.CATCHMENT_GEOM_PROP );

      // createFeature
      final IRelationType linkFT = (IRelationType) mapRootFeature.getFeatureType().getProperty( "mappingMember" );
      final Feature mapFeature = mapWorkspace.createFeature( mapRootFeature, linkFT, mapFT );
      mapWorkspace.addFeatureAsComposition( mapRootFeature, linkFT, 0, mapFeature );
      // set props
      mapFeature.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, nameValue );
      mapFeature.setProperty( "polygon", geoValue );
      mapFeature.setProperty( "inObservationLink", srcLink );
      mapFeature.setProperty( "outObservationLink", targetLink );
    }

    final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( outputFile ), "UTF-8" );
    GmlSerializer.serializeWorkspace( writer, mapWorkspace );
    IOUtils.closeQuietly( writer );
    System.out.println( "wrote new temperaturmapping to " + outputFile.toString() );
  }
}