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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.refinement.RefinementUtils;

/**
 * @author Thomas Jung
 */
public class Sms2dmFEModelElementHandler implements IFEModelElementHandler
{
  private class SmsElementData
  {
    private final Integer[] m_nodeIds;

    public SmsElementData( final Integer[] nodeIds )
    {
      m_nodeIds = nodeIds;
    }

    public GM_Surface< ? > getSurface( final String crs ) throws GM_Exception
    {
      final ArrayList<GM_Position> posList = new ArrayList<GM_Position>();

      for( final Integer m_nodeId : m_nodeIds )
      {
        posList.add( m_nodeMap.get( m_nodeId ) );
      }
      posList.add( m_nodeMap.get( m_nodeIds[0] ) );

      final GM_Position[] poses = posList.toArray( new GM_Position[posList.size()] );
      return RefinementUtils.getSurface( poses, crs );
    }
  }

  final List<IDiscModelImporter> m_importerList = new ArrayList<IDiscModelImporter>();

  private final List<SmsElementData> m_elementList = new ArrayList<SmsElementData>();

  final HashMap<Integer, GM_Position> m_nodeMap = new HashMap<Integer, GM_Position>();

  // TODO: get from outside!
  private String m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#end()
   */
  @Override
  public void end( )
  {
    try
    {
      for( final SmsElementData element : m_elementList )
      {
        final GM_Surface< ? > surface = element.getSurface( m_crs );
        for( final IDiscModelImporter importer : m_importerList )
          importer.addElement( surface );
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    for( final IDiscModelImporter importer : m_importerList )
      importer.finish();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#getCreatedFeatures()
   */
  @Override
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#handleElement(java.lang.String, int,
   *      java.lang.Integer[], int)
   */
  @Override
  public void handleElement( final String lineString, final int id, final Integer[] nodeIds, final int roughnessClassID )
  {
    final SmsElementData data = new SmsElementData( nodeIds );
    m_elementList.add( data );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#handleError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  @Override
  public void handleNode( final String lineString, final int id, final double easting, final double northing, final double elevation )
  {
    final GM_Position position = GeometryFactory.createGM_Position( easting, northing, elevation );
    m_nodeMap.put( id, position );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  @Override
  public void handlerUnIdentifyable( final String lineString )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2dmModelElementHandler#start()
   */
  @Override
  public void start( )
  {
    // TODO Auto-generated method stub

  }

  public void setCrs( final String crs )
  {
    m_crs = crs;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IFEModelElementHandler#addImporter(org.kalypso.kalypsomodel1d2d.conv.IDiscModelImporter)
   */
  @Override
  public void addImporter( final IDiscModelImporter importer )
  {
    m_importerList.add( importer );
  }

}
