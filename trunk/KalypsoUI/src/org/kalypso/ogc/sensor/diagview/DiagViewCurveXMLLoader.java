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
package org.kalypso.ogc.sensor.diagview;

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.kalypso.java.awt.ColorUtilities;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.template.PooledObsProvider;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * DefaultTableViewTheme
 * 
 * @author schlienger
 */
public class DiagViewCurveXMLLoader extends PoolableObjectWaiter
{
  public DiagViewCurveXMLLoader( final DiagView view, final TypeObservation xmlObs, final URL context,
      final boolean synchron )
  {
    super( new PoolableObjectType( xmlObs.getLinktype(), xmlObs.getHref(), context ), new Object[]
    {
        view,
        xmlObs }, synchron );
  }

  /**
   * @see org.kalypso.util.pool.PoolableObjectWaiter#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  protected void objectLoaded( final IPoolableObjectType key, final Object newValue )
  {
    final IObservation obs = (IObservation)newValue;

    final TypeObservation xmlObs = (TypeObservation)m_data[1];
    final DiagView view = (DiagView)m_data[0];

    for( final Iterator it = xmlObs.getCurve().iterator(); it.hasNext(); )
    {
      final TypeCurve tcurve = (TypeCurve)it.next();

      final List tmaps = tcurve.getMapping();
      final List mappings = new ArrayList( tmaps.size() );

      for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
      {
        final TypeAxisMapping tmap = (TypeAxisMapping)itm.next();

        IAxis obsAxis;
        try
        {
          obsAxis = ObservationUtilities.findAxisByName( obs.getAxisList(), tmap.getObservationAxis() );
        }
        catch( final NoSuchElementException nse )
        {
          // If name doesn't match, we try to find it by type
          obsAxis = ObservationUtilities.findAxisByType( obs.getAxisList(), tmap.getObservationAxis() );
        }

        final DiagramAxis diagAxis = view.getDiagramAxis( tmap.getDiagramAxis() );

        mappings.add( new AxisMapping( obsAxis, diagAxis ) );
      }

      final String strc = tcurve.getColor();
      Color color = null;
      if( strc != null )
      {
        try
        {
          color = StringUtilities.stringToColor( strc );
        }
        catch( final IllegalArgumentException e )
        {
          e.printStackTrace();
        }
      }

      if( color == null )
        color = ColorUtilities.random();

      final String curveName = tcurve.getName();

      // each curve gets its own provider as the curve disposes the provider,
      // when disposed
      final IObsProvider provider = key == null ? (IObsProvider)new PlainObsProvider( obs, null )
          : new PooledObsProvider( key, null );
      final DiagViewCurve curve = new DiagViewCurve( view, provider, curveName, color, (AxisMapping[])mappings
          .toArray( new AxisMapping[0] ) );
      curve.setShown( tcurve.isShown() );

      view.addItem( curve );
    }
  }
}