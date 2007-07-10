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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RestartEater
{

  private FeatureList m_nodes;

  private double m_distance = 0.5;

  private boolean m_any_results = false;

  public final static String GAUS_KRUEGER = "EPSG:31467";

  private static final CS_CoordinateSystem CS = ConvenienceCSFactory.getInstance().getOGCCSByName( GAUS_KRUEGER );

  public void addResultFile( final File file ) throws Exception
  {
    if(file == null)
      throw new SimulationException("Restart file(s) not defined.".concat( file.toString() ), new FileNotFoundException());
    if(!file.exists())
      throw new SimulationException("Restart file(s) does not exists: ".concat( file.toString() ), new FileNotFoundException());
    final GMLWorkspace resultWorkspace = GmlSerializer.createGMLWorkspace( file.toURL(), null );
//    final GMLWorkspace resultWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResultCollection" ), file.toURL(), GmlSerializer.DEFAULT_FACTORY );
    final Feature rootFeature = resultWorkspace.getRootFeature();
    FeatureList nodeList = (FeatureList) rootFeature.getProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "nodeResultMember" ) );
    addNodes( nodeList );
  }

  /**
   * Set search distance for finding nodes, default is 0.5
   */
  public void setSearchDistance( double distance )
  {
    m_distance = distance;
  }

  /**
   * Returns parameters for node at certain position, or nearest (depends on search distance)
   */
  public INodeResult getNodeResultAtPosition( double x, double y )
  {
    final GM_Point point = GeometryFactory.createGM_Point( x, y, CS );
    return getNodeResult( point );
  }

  private void addNodes( List<INodeResult> nodes )
  {
    if( m_any_results )
      m_nodes.addAll( nodes );
    else
      m_nodes = (FeatureList) nodes;
    m_any_results = true;
  }

  private INodeResult getNodeResult( final GM_Point point )
  {
    final Feature feature = GeometryUtilities.findNearestFeature( point, m_distance, m_nodes, GMLNodeResult.QNAME_PROP_LOCATION );
    return new GMLNodeResult(feature);
  }

}
