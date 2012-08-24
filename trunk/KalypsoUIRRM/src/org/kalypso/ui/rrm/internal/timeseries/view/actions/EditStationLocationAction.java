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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.ogc.gml.featureview.dialog.PointDialog;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class EditStationLocationAction extends Action
{
  private final FeatureBean<IStation> m_bean;

  public EditStationLocationAction( final FeatureBean<IStation> bean )
  {
    m_bean = bean;

    setText( Messages.getString("EditStationLocationAction_0") ); //$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final String kalypsoCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final GM_Point point = (GM_Point) m_bean.getProperty( IStation.PROPERTY_LOCATION );

    final double[] values = point == null ? new double[3] : point.getAsArray();
    final String crs = point == null ? kalypsoCRS : point.getCoordinateSystem();

    final PointDialog dialog = new PointDialog( shell, values, crs );
    if( dialog.open() != Window.OK )
      return;

    /* Create new point */
    final String newCrs = dialog.getCS();
    final double[] newValues = dialog.getValues();
    final GM_Position newPos = GeometryFactory.createGM_Position( newValues );

    try
    {
      final IGeoTransformer geoTransformer = GeoTransformerFactory.getGeoTransformer( kalypsoCRS );
      // REMARK: geometries MUST always be created in the current Kalypso coordinate system
      final GM_Position newPosTransformed = geoTransformer.transform( newPos, newCrs );
      final GM_Point newPoint = GeometryFactory.createGM_Point( newPosTransformed, kalypsoCRS );

      /* Set to bean */
      m_bean.setProperty( IStation.PROPERTY_LOCATION, newPoint );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "", e ); //$NON-NLS-1$
      StatusDialog.open( shell, status, getText() );
    }
  }
}