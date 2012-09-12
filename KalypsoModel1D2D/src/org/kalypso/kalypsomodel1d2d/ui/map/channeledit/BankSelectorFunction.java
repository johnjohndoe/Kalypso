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

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
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
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Thomas Jung
 */
class BankSelectorFunction implements IRectangleMapFunction
{
  private final ChannelEditData m_data;

  private final IObservableValue m_themeValue;

  private final SIDE m_side;

  public BankSelectorFunction( final ChannelEditData data, final SIDE side, final IObservableValue themeValue )
  {
    m_data = data;
    m_side = side;
    m_themeValue = themeValue;
  }

  @Override
  public void execute( final IMapPanel mapPanel, final Rectangle rectangle )
  {
    final IKalypsoFeatureTheme bankTheme = getBankTheme();
    if( bankTheme == null )
      return;

    final GM_Envelope envelope = MapfunctionHelper.rectangleToEnvelope( mapPanel.getProjection(), rectangle );

    // search bank within rectangle
    final FeatureList featureList = bankTheme.getFeatureList();
    if( featureList == null )
      return;

    final GMLWorkspace workspace = featureList.getOwner().getWorkspace();

    @SuppressWarnings( "unchecked" )
    final List< ? > list = featureList.query( envelope, null );

    final Polygon rectanglePoly = JTSUtilities.convertGMEnvelopeToPolygon( envelope, new GeometryFactory() );

    for( final Iterator< ? > iter = list.iterator(); iter.hasNext(); )
    {
      GM_Curve line = null;

      final Object o = iter.next();
      final Feature feature = FeatureHelper.getFeature( workspace, o );

      final GM_Object geometry = feature.getDefaultGeometryPropertyValue();

      if( geometry instanceof GM_MultiCurve )
      {
        final GM_MultiCurve multiline = (GM_MultiCurve) geometry;
        if( multiline.getSize() > 1 )
        {
          SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.BankSelectorFunction.4" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.BankSelectorFunction.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return;
        }

        line = multiline.getCurveAt( 0 );
      }
      else if( geometry instanceof GM_Curve )
      {
        final GM_Curve curve = (GM_Curve) geometry;
        line = curve;
      }
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

    final GM_Curve bankline = m_data.getBanklineForSide( m_side );
    final Set<GM_Curve> selectedBankSet = new HashSet<>( Arrays.asList( bankline ) );

    if( list.size() == 0 )
    {
      // empty selection: remove selection
      m_data.removeBank( bankline );
    }
    else
    {
      for( int i = 0; i < list.size(); i++ )
      {
        final Object o = list.get( i );
        final Feature feature = FeatureHelper.getFeature( workspace, o );

        final GM_Curve bankCurve = getCurveFromBanklineFeature( feature );
        if( bankCurve == null )
          return;

        if( selectedBankSet.contains( bankCurve ) )
          m_data.removeBank( bankCurve );
        else
          m_data.setBankline( bankCurve, m_side );
      }
    }
  }

  private IKalypsoFeatureTheme getBankTheme( )
  {
    final IKalypsoFeatureTheme[] result = new IKalypsoFeatureTheme[] { null };

    final IObservableValue themeValue = m_themeValue;

    // must be fetched in swt thread
    final Runnable runnable = new Runnable()
    {
      @Override
      public void run( )
      {
        result[0] = (IKalypsoFeatureTheme) themeValue.getValue();
      }
    };
    PlatformUI.getWorkbench().getDisplay().syncExec( runnable );

    return result[0];
  }

  private GM_Curve getCurveFromBanklineFeature( final Feature feature )
  {
    final GM_Object geometry = feature.getDefaultGeometryPropertyValue();

    GM_Curve bankCurve = null;

    if( geometry instanceof GM_MultiCurve )
    {
      final GM_MultiCurve multiline = (GM_MultiCurve) geometry;
      if( multiline.getSize() > 1 )
        return null;
      bankCurve = multiline.getCurveAt( 0 );
    }
    else if( geometry instanceof GM_Curve )
    {
      bankCurve = (GM_Curve) geometry;
    }
    return bankCurve;
  }
}
