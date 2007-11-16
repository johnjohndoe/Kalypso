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
package org.kalypso.model.flood.util;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.bce.gis.io.hmo.HMOReader;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.io.ParseException;

/**
 * @author Thomas Jung
 * 
 * 
 */
public class HmoToGM_SurfaceConverter
{
  private final URL m_inputUrl;

  private final IFloodModel m_model;

  private final IFeatureWrapperCollection<ITinReference> m_tins;

  private TriangulatedSurfaceTriangleEater eater = null;

  public HmoToGM_SurfaceConverter( final URL hmoFileUrl, final IFloodModel model, final IFeatureWrapperCollection<ITinReference> tins, CS_CoordinateSystem crs )
  {
    m_inputUrl = hmoFileUrl;
    m_model = model;
    m_tins = tins;

    // open file
    if( m_inputUrl == null || m_model == null )
      return;

    try
    {
      eater = new TriangulatedSurfaceTriangleEater( crs );
      processInputFile( eater );
    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( ParseException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    // write into model
  }

  private void processInputFile( TriangulatedSurfaceTriangleEater eater ) throws IOException, ParseException, GM_Exception
  {
    // parse file via HMOReader
    final HMOReader hmoReader = new HMOReader( new GeometryFactory() );
    final Reader r = new InputStreamReader( m_inputUrl.openStream() );
    final LinearRing[] rings = hmoReader.read( r );

    final List<GM_Point> pointList = new LinkedList<GM_Point>();

    for( LinearRing ring : rings )
    {
      for( int i = 0; i < rings.length; i++ )
      {
        GM_Object object = JTSAdapter.wrap( ring.getPointN( i ) );

        pointList.add( (GM_Point) object );
      }
      eater.add( pointList );
    }

    final GM_TriangulatedSurface surface = eater.getSurface();
  }

}
