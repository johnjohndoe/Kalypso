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
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class opens a point editing dialog.
 * 
 * @author Holger Albert
 */
public class PointFeatureDialog implements IFeatureDialog
{
  private FeatureChange m_change = null;

  private final Feature m_feature;

  private final IValuePropertyType m_ftp;

  public PointFeatureDialog( final Feature feature, final IValuePropertyType ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( Shell shell )
  {
    GM_Point point = (GM_Point) m_feature.getProperty( m_ftp );

    PointDialog dialog = null;

    if( point != null )
      dialog = new PointDialog( shell, point.getAsArray(), point.getCoordinateSystem() );
    else
      dialog = new PointDialog( shell, new double[] { 0.0, 0.0 }, KalypsoCorePlugin.getDefault().getCoordinatesSystem() );

    final int open = dialog.open();

    if( open == Window.OK )
    {
      double[] values = dialog.getValues();

      GM_Position pos = GeometryFactory.createGM_Position( values );

      String crs = dialog.getCS_CoordinateSystem();

      point = GeometryFactory.createGM_Point( pos, crs );

      m_change = new FeatureChange( m_feature, m_ftp, point );
    }

    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( Collection<FeatureChange> c )
  {
    if( c != null && m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel( )
  {
    return Messages.getString( "org.kalypso.ogc.gml.featureview.dialog.PointFeatureDialog.values" ); //$NON-NLS-1$
  }
}