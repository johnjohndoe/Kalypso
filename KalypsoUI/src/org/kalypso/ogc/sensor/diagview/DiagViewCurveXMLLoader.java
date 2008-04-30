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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Stroke;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Logger;

import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.template.PooledObsProvider;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.TypeCurve.StrokeType;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * Waits for the observation to be loaded and creates a diagram-curve using the xml-template information.
 * 
 * @see org.kalypso.util.pool.PoolableObjectWaiter
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

    final List ignoreTypes = view.getIgnoreTypesAsList();

    for( final Iterator it = xmlObs.getCurve().iterator(); it.hasNext(); )
    {
      final TypeCurve tcurve = (TypeCurve)it.next();

      final List tmaps = tcurve.getMapping();
      final List mappings = new ArrayList( tmaps.size() );

      boolean useThisCurve = true;

      for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
      {
        final TypeAxisMapping tmap = (TypeAxisMapping)itm.next();

        try
        {
          final IAxis obsAxis = ObservationUtilities.findAxisByNameThenByType( obs.getAxisList(), tmap
              .getObservationAxis() );

          if( ignoreTypes.contains( obsAxis.getType() ) )
          {
            useThisCurve = false;
            break;
          }

          final DiagramAxis diagAxis = view.getDiagramAxis( tmap.getDiagramAxis() );

          mappings.add( new AxisMapping( obsAxis, diagAxis ) );
        }
        catch( final NoSuchElementException e )
        {
          Logger.getLogger( getClass().getName() ).warning(
              "Kann DiagViewCurve nicht laden, Ursache: " + e.getLocalizedMessage() );

          useThisCurve = false;
          break;
        }
      }

      if( useThisCurve )
      {
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
        {
          final IAxis axis = DiagViewUtils.getValueAxis( (AxisMapping[])mappings.toArray( new AxisMapping[mappings
              .size()] ) );

          if( axis == null )
            color = ColorUtilities.random();
          else
            color = TimeserieUtils.getColorsFor( axis.getType() )[0];
        }

        final String curveName = ObsViewUtils.replaceTokens( tcurve.getName(), obs, null );

        final Stroke stroke;
        final StrokeType xmlStroke = tcurve.getStroke();
        if( xmlStroke == null )
          stroke = null;
        else
        {
          final List dashList = xmlStroke.getDash();
          if( dashList == null || dashList.size() == 0 )
            stroke = new BasicStroke( xmlStroke.getWidth(), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f );
          else
          {
            final float[] dashArray = new float[dashList.size()];
            for( int i = 0; i < dashList.size(); i++ )
              dashArray[i] = ( (Float)dashList.get( i ) ).floatValue();

            stroke = new BasicStroke( xmlStroke.getWidth(), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 1.0f,
                dashArray, 0.0f );
          }
        }

        // each curve gets its own provider since the curve disposes its provider, when it get disposed
        final IObsProvider provider = isSynchron() ? (IObsProvider)new PlainObsProvider( obs, null )
            : new PooledObsProvider( key, null );

        final DiagViewCurve curve = new DiagViewCurve( view, provider, curveName, color, stroke,
            (AxisMapping[])mappings.toArray( new AxisMapping[0] ) );
        curve.setShown( tcurve.isShown() );

        view.addItem( curve );
      }
    }
  }
}