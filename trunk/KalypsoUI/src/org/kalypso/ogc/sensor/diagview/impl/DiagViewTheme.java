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
package org.kalypso.ogc.sensor.diagview.impl;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.java.awt.ColorUtilities;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateUtils;
import org.kalypso.ogc.sensor.proxy.IProxyFactory;
import org.kalypso.ogc.sensor.template.AbstractObservationTheme;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * DefaultTableViewTheme
 * 
 * @author schlienger
 */
public class DiagViewTheme extends AbstractObservationTheme
{
  /** list of curves */
  private final List m_curves = new ArrayList();

  /** the base xml if defined (can be null) */
  private final TypeObservation m_xmlObs;

  private final DiagViewTemplate m_template;

  public DiagViewTheme( DiagViewTemplate template, String name,
      IVariableArguments args )
  {
    super( name, args );

    m_template = template;
    m_xmlObs = null;
  }

  public DiagViewTheme( DiagViewTemplate template, String name,
      TypeObservation xmlObs )
  {
    super( name, null );

    m_template = template;
    m_xmlObs = xmlObs;
  }

  private void addCurve( final DiagViewCurve curve )
  {
    //    if( isNameDefined() )
    //      curve.setName( getName() + " (" + curve.getName() + ")" );

    m_curves.add( curve );
  }

  public void dispose( )
  {
    super.dispose();

    m_curves.clear();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.AbstractObservationTheme#beforeObservationSet(org.kalypso.ogc.sensor.IObservation)
   */
  protected IObservation beforeObservationSet( final IObservation observation )
  {
    final IProxyFactory pf = m_template.getProxyFactory();
    
    if( pf != null )
    {
      try
      {
        return pf.proxyObservation( observation );
      }
      catch( SensorException e )
      {
        e.printStackTrace();
      }
    }
    
    return observation;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.AbstractObservationTheme#afterObservationSet()
   */
  protected void afterObservationSet( )
  {
    synchronized( m_curves )
    {
      // reset curves (clearing them will force refresh once getCurves is
      // called)
      m_curves.clear();
    }
  }

  /**
   * Build the columns as defined in the xml template (if existing)
   */
  private void buildXmlCurves( )
  {
    final IObservation obs = getObservation();

    if( obs != null && m_xmlObs != null )
    {
      for( final Iterator it = m_xmlObs.getCurve().iterator(); it.hasNext(); )
      {
        final TypeCurve tcurve = (TypeCurve) it.next();

        final List tmaps = tcurve.getMapping();
        final List mappings = new ArrayList( tmaps.size() );

        for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
        {
          final TypeAxisMapping tmap = (TypeAxisMapping) itm.next();

          final IAxis obsAxis = ObservationUtilities.findAxisByName( obs
              .getAxisList(), tmap.getObservationAxis() );
          final DiagramAxis diagAxis = m_template.getDiagramAxis( tmap
              .getDiagramAxis() );

          mappings.add( new AxisMapping( obsAxis, diagAxis ) );
        }

        final String strc = tcurve.getColor();
        Color color;
        if( strc != null )
        {
          try
          {
            color = StringUtilities.stringToColor( strc );
          }
          catch( IllegalArgumentException e )
          {
            e.printStackTrace();
            color = ColorUtilities.random();
          }
        }
        else
          color = ColorUtilities.random();

        // create curve and add it to theme
        final DiagViewCurve curve = new DiagViewCurve( tcurve.getName(), color,
            this, (AxisMapping[]) mappings.toArray( new AxisMapping[0] ),
            m_template );

        addCurve( curve );
      }
    }
  }

  /**
   * Build the curves according to the observation, doing our best
   */
  private void buildDefaultCurves( )
  {
    final IObservation obs = getObservation();

    if( obs != null && isUseDefault() )
    {
      final IAxis[] valueAxis = ObservationUtilities.findAxisByClass( obs
          .getAxisList(), Number.class, true );
      final IAxis[] keyAxes = ObservationUtilities.findAxisByKey( obs
          .getAxisList() );

      if( keyAxes.length == 0 )
        return;

      final IAxis dateAxis = keyAxes[0];

      for( int i = 0; i < valueAxis.length; i++ )
      {
        final String type = valueAxis[i].getType();
        if( !type.equals( getIgnoreType() ) )
        {
          final AxisMapping[] mappings = new AxisMapping[2];

          // look for a date diagram axis
          DiagramAxis daDate = m_template.getDiagramAxis( dateAxis.getType() );
          if( daDate == null )
          {
            daDate = DiagramTemplateUtils.createAxisFor( dateAxis );
            m_template.addAxis( daDate );
          }
          mappings[0] = new AxisMapping( dateAxis, daDate );

          // look for a value diagram axis
          DiagramAxis daValue = m_template.getDiagramAxis( type );
          if( daValue == null )
          {
            daValue = DiagramTemplateUtils.createAxisFor( valueAxis[i] );
            m_template.addAxis( daValue );
          }
          mappings[1] = new AxisMapping( valueAxis[i], daValue );

          final DiagViewCurve curve = new DiagViewCurve( getName() + " ("
              + valueAxis[i].getName() + ")", TimeserieUtils
              .getColorFor( valueAxis[i].getType() ), this, mappings,
              m_template );

          addCurve( curve );
        }
      }
    }
  }

  /**
   * @return all curves
   */
  public List getCurves( )
  {
    synchronized( m_curves )
    {
      // still empty columns? look if we got some
      if( m_curves.size() == 0 )
      {
        buildXmlCurves();
        buildDefaultCurves();
      }

      return m_curves;
    }
  }
}