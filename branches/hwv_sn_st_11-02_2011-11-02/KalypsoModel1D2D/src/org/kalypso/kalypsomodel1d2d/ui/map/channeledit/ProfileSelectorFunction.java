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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
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
 * Manages the (de-)selection of several profiles on the map view by drwaing a rectangle
 * 
 * @author Thomas Jung
 */
public class ProfileSelectorFunction implements IRectangleMapFunction
{
  private final CreateChannelData m_data;

  public ProfileSelectorFunction( final CreateChannelData data )
  {
    m_data = data;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction#execute(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Rectangle)
   */
  @Override
  @SuppressWarnings("unchecked")
  public void execute( final IMapPanel mapPanel, final Rectangle rectangle )
  {

    if( m_data.getMeshStatus() == true )
    {
      if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ProfileSelectorFunction.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ProfileSelectorFunction.1" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
        return;
    }

    final GM_Envelope envelope = MapfunctionHelper.rectangleToEnvelope( mapPanel.getProjection(), rectangle );

    final IKalypsoFeatureTheme profileTheme = m_data.getProfileTheme();
    if( profileTheme == null )
      return;

    // search profile within rectangle
    final FeatureList featureList = profileTheme.getFeatureList();
    final GMLWorkspace workspace = featureList.getParentFeature().getWorkspace();

    final IProfileFeature[] selectedProfiles = m_data.getSelectedProfiles();
    final Set<IProfileFeature> selectedProfileSet = new HashSet<IProfileFeature>( Arrays.asList( selectedProfiles ) );

    final List list = featureList.query( envelope, null );

    final Polygon rectanglePoly = JTSUtilities.convertGMEnvelopeToPolygon( envelope, new GeometryFactory() );
    for( final Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final Object o = iter.next();
      final Feature feature = FeatureHelper.getFeature( workspace, o );

      if( feature instanceof ProfileFeatureBinding )
      {
        final ProfileFeatureBinding profile = (ProfileFeatureBinding) feature;
        final GM_Curve line = profile.getLine();
        if( line == null )
          return;
        try
        {
          final LineString jtsLine = (LineString) JTSAdapter.export( line );
          if( !jtsLine.intersects( rectanglePoly ) )
            iter.remove();
        }
        catch( final GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    if( list.size() == 0 )
    {
      // empty selection: remove selection
      m_data.changeSelectedProfiles( selectedProfiles, new IProfileFeature[0] );
    }
    else
    {
      final List<IProfileFeature> featureToAdd = new ArrayList<IProfileFeature>();
      final List<IProfileFeature> featureToRemove = new ArrayList<IProfileFeature>();

      for( int i = 0; i < list.size(); i++ )
      {
        final Object o = list.get( i );
        final IProfileFeature feature = (IProfileFeature) FeatureHelper.getFeature( workspace, o );

        if( selectedProfileSet.contains( feature ) )
          featureToRemove.add( feature );
        else
          featureToAdd.add( feature );
      }
      m_data.changeSelectedProfiles( featureToRemove.toArray( new IProfileFeature[featureToRemove.size()] ), featureToAdd.toArray( new IProfileFeature[featureToAdd.size()] ) );
    }
  }

}
