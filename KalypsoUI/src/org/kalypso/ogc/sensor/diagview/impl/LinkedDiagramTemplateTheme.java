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
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;

/**
 * LinkedDiagramTemplateTheme
 * 
 * @author schlienger
 */
public class LinkedDiagramTemplateTheme extends DefaultDiagramTemplateTheme
{
  private final TypeObservation m_tobs;

  private final ObservationDiagramTemplate m_template;

  /**
   * Constructor.
   * 
   * @param template
   * @param tobs
   */
  public LinkedDiagramTemplateTheme( final ObservationDiagramTemplate template,
      final TypeObservation tobs )
  {
    m_tobs = tobs;
    m_template = template;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplateTheme#setObservation(org.kalypso.ogc.sensor.IObservation)
   */
  public void setObservation( IObservation obs )
  {
    super.setObservation( obs );

    for( final Iterator it = m_tobs.getCurve().iterator(); it.hasNext(); )
    {
      final TypeCurve tcurve = (TypeCurve) it.next();

      final List tmaps = tcurve.getMapping();
      final List mappings = new ArrayList( tmaps.size() );

      for( final Iterator itm = tmaps.iterator(); itm.hasNext(); )
      {
        final TypeAxisMapping tmap = (TypeAxisMapping) itm.next();

        final IAxis obsAxis = ObservationUtilities.findAxisByName( obs
            .getAxisList(), tmap.getObservationAxis() );
        final IDiagramAxis diagAxis = m_template.getDiagramAxis( tmap
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
      final DiagramCurve curve = new DiagramCurve( tcurve.getName(),
          color, this, (IAxisMapping[]) mappings
              .toArray( new IAxisMapping[0] ), m_template );

      addCurve( curve );
    }
  }
}