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
import java.awt.Stroke;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeDirection;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.TypePosition;
import org.kalypso.template.obsdiagview.Obsdiagview.Legend;
import org.xml.sax.InputSource;

/**
 * Observation Diagramm Template Handling made easy.
 * 
 * @author schlienger
 */
public class DiagViewUtils
{
  public final static String ODT_FILE_EXTENSION = "odt"; //$NON-NLS-1$

  private final static ObjectFactory ODT_OF = new ObjectFactory();

  private final static JAXBContext ODT_JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  /**
   * Not to be instanciated
   */
  private DiagViewUtils( )
  {
    // empty
  }

  /**
   * Saves the given template (binding). Closes the stream.
   */
  public static void saveDiagramTemplateXML( final Obsdiagview xml, final OutputStream outs ) throws JAXBException
  {
    try
    {
      final Marshaller m = JaxbUtilities.createMarshaller( ODT_JC );
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
   */
  public static void saveDiagramTemplateXML( final Obsdiagview tpl, final Writer writer ) throws JAXBException
  {
    try
    {
      final Marshaller m = JaxbUtilities.createMarshaller( ODT_JC );
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
   * @return diagram template object parsed from the file
   */
  public static Obsdiagview loadDiagramTemplateXML( final InputStream ins ) throws JAXBException
  {
    try
    {
      return loadDiagramTemplateXML( new InputSource( ins ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Loads a binding template. Closes the stream.
   * 
   * @return diagram template object parsed from the file
   */
  public static Obsdiagview loadDiagramTemplateXML( final Reader reader ) throws JAXBException
  {
    try
    {
      return loadDiagramTemplateXML( new InputSource( reader ) );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * Loads a binding template. Closes the stream.
   * 
   * @return diagram template object parsed from the file
   */
  private static Obsdiagview loadDiagramTemplateXML( final InputSource ins ) throws JAXBException
  {
    final Obsdiagview baseTemplate = (Obsdiagview) ODT_JC.createUnmarshaller().unmarshal( ins );

    return baseTemplate;
  }

  /**
   * Builds the xml binding object using the given diagram view template
   * 
   * @return xml binding object (ready for marshalling for instance)
   */
  public static Obsdiagview buildDiagramTemplateXML( final DiagView view )
  {
    final Obsdiagview xmlTemplate = ODT_OF.createObsdiagview();

    final Legend xmlLegend = ODT_OF.createObsdiagviewLegend();
    xmlLegend.setTitle( view.getLegendName() );
    xmlLegend.setVisible( view.isShowLegend() );

    xmlTemplate.setLegend( xmlLegend );
    xmlTemplate.setTitle( view.getTitle() );

    // only set timezone if not default one
    if( !view.getTimezone().getID().equals( TimeZone.getDefault().getID() ) )
      xmlTemplate.setTimezone( view.getTimezone().getID() );

    final List<TypeAxis> xmlAxes = xmlTemplate.getAxis();

    final DiagramAxis[] diagramAxes = view.getDiagramAxes();
    for( int i = 0; i < diagramAxes.length; i++ )
    {
      final DiagramAxis axis = diagramAxes[i];

      final TypeAxis xmlAxis = ODT_OF.createTypeAxis();
      xmlAxis.setDatatype( axis.getDataType() );
      xmlAxis.setDirection( TypeDirection.fromValue( axis.getDirection() ) );
      xmlAxis.setId( axis.getIdentifier() );
      xmlAxis.setInverted( axis.isInverted() );
      xmlAxis.setLabel( axis.getLabel() );
      xmlAxis.setPosition( TypePosition.fromValue( axis.getPosition() ) );
      xmlAxis.setUnit( axis.getUnit() );

      xmlAxes.add( xmlAxis );
    }

    xmlTemplate.setFeatures( StringUtils.join( view.getEnabledFeatures(), ';' ) );

    int ixCurve = 1;

    final List<TypeObservation> xmlThemes = xmlTemplate.getObservation();
    final Map<IObservation, ArrayList<ObsViewItem>> map = ObsView.mapItems( view.getItems() );
    for( final Iterator<Map.Entry<IObservation, ArrayList<ObsViewItem>>> itThemes = map.entrySet().iterator(); itThemes.hasNext(); )
    {
      final Map.Entry<IObservation, ArrayList<ObsViewItem>> entry = itThemes.next();
      final IObservation obs = entry.getKey();
      if( obs == null )
        continue;

      final TypeObservation xmlTheme = ODT_OF.createTypeObservation();
      xmlTheme.setLinktype( "zml" ); //$NON-NLS-1$
      xmlTheme.setHref( obs.getHref() );

      final List<TypeCurve> xmlCurves = xmlTheme.getCurve();

      final Iterator<ObsViewItem> itCurves = ((List<ObsViewItem>) entry.getValue()).iterator();
      while( itCurves.hasNext() )
      {
        final DiagViewCurve curve = (DiagViewCurve) itCurves.next();

        final TypeCurve xmlCurve = ODT_OF.createTypeCurve();
        xmlCurve.setId( "C" + ixCurve++ ); //$NON-NLS-1$
        xmlCurve.setName( curve.getName() );
        xmlCurve.setColor( StringUtilities.colorToString( curve.getColor() ) );
        xmlCurve.setShown( curve.isShown() );
        Stroke stroke = curve.getStroke();
        if( stroke instanceof BasicStroke )
        {
          final BasicStroke bs = (BasicStroke)stroke;
          final TypeCurve.Stroke strokeType = ODT_OF.createTypeCurveStroke();
          xmlCurve.setStroke( strokeType );
          strokeType.setWidth( bs.getLineWidth() );
          final float[] dashArray = bs.getDashArray();
          if( dashArray != null )
          {
            final List<Float> dashList = strokeType.getDash();
            for( int i = 0; i < dashArray.length; i++ )
              dashList.add( new Float( dashArray[i] ) );
          }
        }

        final List<TypeAxisMapping> xmlMappings = xmlCurve.getMapping();

        final AxisMapping[] mappings = curve.getMappings();
        for( int i = 0; i < mappings.length; i++ )
        {
          final TypeAxisMapping xmlMapping = ODT_OF.createTypeAxisMapping();
          xmlMapping.setDiagramAxis( mappings[i].getDiagramAxis().getIdentifier() );
          xmlMapping.setObservationAxis( mappings[i].getObservationAxis().getName() );

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
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final IAxis axis )
  {
    return createAxisFor( axis.getType(), axis.getName(), axis.getUnit(), axis.isKey() );
  }

  /**
   * Creates a diagram axis according to the given IObservation axis
   * 
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final String axisType, final String label, final String unit, final boolean isKey )
  {
    final String direction = isKey == true ? DiagramAxis.DIRECTION_HORIZONTAL : DiagramAxis.DIRECTION_VERTICAL;
    String position = isKey == true ? DiagramAxis.POSITION_BOTTOM : DiagramAxis.POSITION_LEFT;

    // TODO: move this stuff into config.properties as everything else

    if( axisType.equals( TimeserieConstants.TYPE_DATE ) )
      return new DiagramAxis( axisType, "date", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_HOURS ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_NORMNULL ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_VOLUME ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_NORM ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_POLDER_CONTROL ) )
      return new DiagramAxis( axisType, "boolean", label, unit, direction, position, false );
    
    position = isKey == true ? DiagramAxis.POSITION_BOTTOM : DiagramAxis.POSITION_RIGHT;

    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, true, null, TimeserieUtils
          .getTopMargin( axisType ) ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_TEMPERATURE ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    if( axisType.equals( TimeserieConstants.TYPE_EVAPORATION ) )
      return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$

    position = isKey == true ? DiagramAxis.POSITION_BOTTOM : DiagramAxis.POSITION_LEFT;

    // default axis
    return new DiagramAxis( axisType, "double", label, unit, direction, position, false ); //$NON-NLS-1$
  }

  /**
   * Apply the given xml-template representation to the diagview.
   * 
   * @param ignoreHref
   *            [optional] tricky, used in the context of the wizard where token replace takes place. If a href could
   *            not be replaced, it is set to a specific tag-value and the ignoreHref parameter if specified denotes is
   *            compared to each href found in the template. If it is found, then the href is ignored and the
   *            corresponding observation isn't loaded.
   */
  public static IStatus applyXMLTemplate( final DiagView view, final Obsdiagview xml, final URL context, final boolean synchron, final String ignoreHref )
  {
    view.removeAllItems();

    view.setTitle( xml.getTitle() );
    view.setLegendName( xml.getLegend() == null ? "" : xml.getLegend().getTitle() ); //$NON-NLS-1$
    view.setShowLegend( xml.getLegend() == null ? false : xml.getLegend().isVisible() );

    // features-list is optional
    if( xml.getFeatures() != null )
    {
      // features list specified, so clear before enabling the ones sepcified
      view.clearFeatures();

      final String[] featureNames = xml.getFeatures().split( ";" ); //$NON-NLS-1$
      for( int i = 0; i < featureNames.length; i++ )
        view.setFeatureEnabled( featureNames[i], true );
    }

    // timezone is optional
    if( xml.getTimezone() != null && xml.getTimezone().length() > 0 )
    {
      final TimeZone timeZone = TimeZone.getTimeZone( xml.getTimezone() );
      view.setTimezone( timeZone );
    }

    // axes spec is optional
    if( xml.getAxis() != null )
    {
      for( final Iterator<TypeAxis> it = xml.getAxis().iterator(); it.hasNext(); )
      {
        final TypeAxis baseAxis = it.next();
        view.addAxis( new DiagramAxis( baseAxis ) );
      }
    }

    final List<IStatus> stati = new ArrayList<IStatus>();

    final List<TypeObservation> list = xml.getObservation();
    for( final Iterator<TypeObservation> it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = it.next();

      // check, if href is ok
      final String href = tobs.getHref();

      // Hack: elemente, die durch token-replace nicht richtig aufgel�st werden einfach �bergehen
      if( ignoreHref != null && href.indexOf( ignoreHref ) != -1 )
      {
        Logger.getLogger( DiagViewUtils.class.getName() ).warning( Messages.getString( "org.kalypso.ogc.sensor.diagview.DiagViewUtils.16" ) + href ); //$NON-NLS-1$
        continue;
      }

      final DiagViewCurveXMLLoader loader = new DiagViewCurveXMLLoader( view, tobs, context, synchron );
      stati.add( loader.getResult() );
    }

    return StatusUtilities.createStatus( stati, Messages.getString( "org.kalypso.ogc.sensor.diagview.DiagViewUtils.17" ) ); //$NON-NLS-1$
  }

  /**
   * Return the first axis of the mappings list which is not a key axis.
   * 
   * @param mappings
   *            array of obs-diag axes mappings
   * @return obs axis (not a key axis) or null if not found
   */
  public static IAxis getValueAxis( final AxisMapping[] mappings )
  {
    for( int i = 0; i < mappings.length; i++ )
    {
      if( !mappings[i].getObservationAxis().isKey() )
        return mappings[i].getObservationAxis();
    }

    return null;
  }
}