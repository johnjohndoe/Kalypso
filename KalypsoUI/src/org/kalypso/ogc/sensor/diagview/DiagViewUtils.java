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

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.ObsdiagviewType.LegendType;

/**
 * Observation Diagramm Template Handling made easy.
 * 
 * @author schlienger
 */
public class DiagViewUtils
{
  public final static String ODT_FILE_EXTENSION = "odt";

  private final static ObjectFactory ODT_OF = new ObjectFactory();

  /**
   * Not to be instanciated
   */
  private DiagViewUtils( )
  {
    // empty
  }

  /**
   * Saves the given template (binding). Closes the stream.
   * 
   * @param xml
   * @param outs
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType xml,
      final OutputStream outs ) throws JAXBException
  {
    try
    {
      final Marshaller m = ODT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( xml, outs );
    }
    finally
    {
      IOUtils.closeQuietly( outs );
    }
  }

  /**
   * Saves the given template (binding). Closes the writer.
   * 
   * @param tpl
   * @param writer
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType tpl,
      final Writer writer ) throws JAXBException
  {
    try
    {
      final Marshaller m = ODT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( tpl, writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Loads a binding template. Closes the stream.
   * 
   * @param ins
   * @return diagram template object parsed from the file
   * @throws JAXBException
   */
  public static ObsdiagviewType loadDiagramTemplateXML( final InputStream ins )
      throws JAXBException
  {
    try
    {
      final ObsdiagviewType baseTemplate = (ObsdiagviewType) ODT_OF
          .createUnmarshaller().unmarshal( ins );

      return baseTemplate;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Builds the xml binding object using the given diagram view template
   * 
   * @param template
   * @return xml binding object (ready for marshalling for instance)
   * @throws JAXBException
   */
  public static ObsdiagviewType buildDiagramTemplateXML(
      final DiagViewTemplate template ) throws JAXBException
  {
    final ObsdiagviewType xmlTemplate = ODT_OF.createObsdiagview();

    final LegendType xmlLegend = ODT_OF.createObsdiagviewTypeLegendType();
    xmlLegend.setTitle( template.getLegendName() );
    xmlLegend.setVisible( template.isShowLegend() );

    xmlTemplate.setLegend( xmlLegend );
    xmlTemplate.setTitle( template.getTitle() );

    final List xmlAxes = xmlTemplate.getAxis();
    final Iterator itAxes = template.getDiagramAxes().iterator();
    while( itAxes.hasNext() )
    {
      final DiagramAxis axis = (DiagramAxis) itAxes.next();

      final TypeAxis xmlAxis = ODT_OF.createTypeAxis();
      xmlAxis.setDatatype( axis.getDataType() );
      xmlAxis.setDirection( axis.getDirection() );
      xmlAxis.setId( axis.getIdentifier() );
      xmlAxis.setInverted( axis.isInverted() );
      xmlAxis.setLabel( axis.getLabel() );
      xmlAxis.setPosition( axis.getPosition() );
      xmlAxis.setUnit( axis.getUnit() );

      xmlAxes.add( xmlAxis );
    }

    int ixCurve = 1;

    final List xmlThemes = xmlTemplate.getObservation();
    final Iterator itThemes = template.getThemes().iterator();
    while( itThemes.hasNext() )
    {
      final DiagViewTheme theme = (DiagViewTheme) itThemes
          .next();

      final IObservation obs = theme.getObservation();

      final TypeObservation xmlTheme = ODT_OF.createTypeObservation();
      xmlTheme.setLinktype( "zml" );
      xmlTheme.setHref( obs.getHref() );

      final List xmlCurves = xmlTheme.getCurve();

      final Iterator itCurves = theme.getCurves().iterator();
      while( itCurves.hasNext() )
      {
        final DiagViewCurve curve = (DiagViewCurve) itCurves.next();

        final TypeCurve xmlCurve = ODT_OF.createTypeCurve();
        xmlCurve.setId( "C" + ixCurve++ );
        xmlCurve.setName( curve.getName() );
        xmlCurve.setColor( StringUtilities.colorToString( curve.getColor() ) );
        xmlCurve.setShown( curve.isShown() );
        
        final List xmlMappings = xmlCurve.getMapping();

        final AxisMapping[] mappings = curve.getMappings();
        for( int i = 0; i < mappings.length; i++ )
        {
          final TypeAxisMapping xmlMapping = ODT_OF.createTypeAxisMapping();
          xmlMapping.setDiagramAxis( mappings[i].getDiagramAxis()
              .getIdentifier() );
          xmlMapping.setObservationAxis( mappings[i].getObservationAxis()
              .getName() );

          xmlMappings.add( xmlMapping );
        }

        xmlCurves.add( xmlCurve );
      }

      xmlThemes.add( xmlTheme );
    }

    return xmlTemplate;
  }

  /**
   * Creates a diagram axis according to the given IObservation axis
   * 
   * @param axis
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final IAxis axis )
  {
    return createAxisFor( axis.getType(), axis.getName(), axis.getUnit() );
  }
  
  /**
   * Creates a diagram axis according to the given IObservation axis
   * 
   * @param axisType
   * @param label
   * @param unit
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final String axisType,
      final String label, final String unit )
  {
    if( axisType.equals( TimeserieConstants.TYPE_DATE ) )
      return new DiagramAxis( axisType, "date", label, unit,
          DiagramAxis.DIRECTION_HORIZONTAL, DiagramAxis.POSITION_BOTTOM,
          false );

    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          DiagramAxis.DIRECTION_VERTICAL, DiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return new DiagramAxis( axisType, "double", label, unit,
          DiagramAxis.DIRECTION_VERTICAL, DiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          DiagramAxis.DIRECTION_VERTICAL, DiagramAxis.POSITION_RIGHT, true, null, new Double(0.8) );

    if( axisType.equals( TimeserieConstants.TYPE_TEMPERATURE ) )
      return new DiagramAxis( axisType, "double", label, unit,
          DiagramAxis.DIRECTION_VERTICAL, DiagramAxis.POSITION_RIGHT, false );

    // default axis
    return new DiagramAxis( axisType, "double", label, unit,
        DiagramAxis.DIRECTION_VERTICAL, DiagramAxis.POSITION_LEFT, false );
  }
}