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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class SwaleAndTranchManager extends AbstractManager
{
  private final IFeatureType m_swaleAndTrenchFT;

//  private final IFeatureType m_grundwasserabflussFT;

  private final NAConfiguration m_conf;


  private static final HashMap<String, String> m_fileMap = new HashMap<String, String>();

  public SwaleAndTranchManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getSwaleAndTrenchFormatURL() );
    m_conf = conf;
    m_swaleAndTrenchFT = schema.getFeatureType( "SwaleAndTrench" );

//    final IRelationType ftp2 = (IRelationType) m_swaleAndTrenchFT.getProperty( "grundwasserabflussMember" );
//    m_grundwasserabflussFT = ftp2.getTargetFeatureType();
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    List<Feature> result = new ArrayList<Feature>();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    return null;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace ) throws Exception
  {
    Feature rootFeature = workspace.getRootFeature();
    Feature col = (Feature) rootFeature.getProperty( "SwaleAndTrenchCollectionMember" );
    List list = (List) col.getProperty( "swaleTrenchMember" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature swaleTrenchFE = (Feature) iter.next();
      if( asciiBuffer.writeFeature( swaleTrenchFE ) )
        writeFeature( asciiBuffer, workspace, swaleTrenchFE );
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace workSpace, final Feature feature ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();
    int asciiID = idManager.getAsciiID( feature );
    
    asciiBuffer.getSwaleTrenchBuffer().append( "           " );
  }

  /**
   * @see org.kalypso.convert.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }
}