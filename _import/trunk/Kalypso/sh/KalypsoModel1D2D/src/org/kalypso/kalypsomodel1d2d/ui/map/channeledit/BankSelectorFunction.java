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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.MapfunctionHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Thomas Jung
 */
public class BankSelectorFunction implements IRectangleMapFunction
{
  private final CreateChannelData m_data;

  private final CreateChannelData.SIDE m_side;

  public BankSelectorFunction( final CreateChannelData data, final CreateChannelData.SIDE side )
  {
    m_data = data;
    m_side = side;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction#execute(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Rectangle)
   */
  @SuppressWarnings("unchecked")
  public void execute( MapPanel mapPanel, Rectangle rectangle )
  {
    final GM_Envelope envelope = MapfunctionHelper.rectangleToEnvelope( mapPanel.getProjection(), rectangle );
    IKalypsoFeatureTheme bankTheme = null;
    if( m_side == CreateChannelData.SIDE.LEFT )
    {
      bankTheme = m_data.getBankTheme1();
    }
    else if( m_side == CreateChannelData.SIDE.RIGHT )
    {
      bankTheme = m_data.getBankTheme2();
    }
    if( bankTheme == null )
      return;

    // search bank within rectangle
    final FeatureList featureList = bankTheme.getFeatureList();
    final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();

    final Feature[] selectedBanks = m_data.getSelectedBanks( m_side );
    final Set<Feature> selectedBankSet = new HashSet<Feature>( Arrays.asList( selectedBanks ) );
    final List list = featureList.query( envelope, null );
    final Polygon rectanglePoly = JTSUtilities.convertGMEnvelopeToPolygon( envelope, new GeometryFactory() );

    for( final Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final Object o = iter.next();
      final Feature feature = FeatureHelper.getFeature( workspace, o );

      final GM_Curve line = (GM_Curve) feature.getDefaultGeometryProperty();
      try
      {
        final LineString jtsLine = (LineString) JTSAdapter.export( line );
        if( !jtsLine.intersects( rectanglePoly ) )
          iter.remove();
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
    }

    if( list.size() == 0 )
    {
      // empty selection: remove selection
      m_data.removeSelectedBanks( selectedBanks );
    }
    else
    {
      for( int i = 0; i < list.size(); i++ )
      {
        final Object o = list.get( i );
        final Feature feature = FeatureHelper.getFeature( workspace, o );

        if( selectedBankSet.contains( feature ) )
          m_data.removeSelectedBanks( new Feature[] { feature } );
        else
          m_data.addSelectedBanks( new Feature[] { feature }, m_side );
      }
    }
  }
}
