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
import org.kalypso.ogc.sensor.diagview.impl.DiagramAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.ObsdiagviewType.LegendType;

/**
 * Observation template handling made easy.
 * 
 * @author schlienger
 */
public class DiagramTemplateUtils
{
  public final static String ODT_FILE_EXTENSION = "odt";

  private final static ObjectFactory ODT_OF = new ObjectFactory();

  private DiagramTemplateUtils( )
  {
    // not to be instanciated
  }

  /**
   * Saves the given template (binding). Closes the stream.
   * 
   * @param tpl
   * @param out
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType tpl,
      final OutputStream out ) throws JAXBException
  {
    try
    {
      final Marshaller m = ODT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( tpl, out );
    }
    finally
    {
      IOUtils.closeQuietly( out );
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
   * Builds the template binding type using the given template.
   * 
   * @param template
   * @return binding type, ready for marshalling
   * @throws JAXBException
   */
  public static ObsdiagviewType buildDiagramTemplateXML(
      final IDiagramTemplate template ) throws JAXBException
  {
    final ObsdiagviewType bdgTemplate = ODT_OF.createObsdiagview();

    final LegendType bdgLegend = ODT_OF.createObsdiagviewTypeLegendType();
    bdgLegend.setTitle( template.getLegendName() );
    bdgLegend.setVisible( template.isShowLegend() );

    bdgTemplate.setLegend( bdgLegend );
    bdgTemplate.setTitle( template.getTitle() );

    final List bdgAxes = bdgTemplate.getAxis();
    final Iterator itAxes = template.getDiagramAxes().iterator();
    while( itAxes.hasNext() )
    {
      final IDiagramAxis axis = (IDiagramAxis) itAxes.next();

      final TypeAxis bdgAxis = ODT_OF.createTypeAxis();
      bdgAxis.setDatatype( axis.getDataType() );
      bdgAxis.setDirection( axis.getDirection() );
      bdgAxis.setId( axis.getIdentifier() );
      bdgAxis.setInverted( axis.isInverted() );
      bdgAxis.setLabel( axis.getLabel() );
      bdgAxis.setPosition( axis.getPosition() );
      bdgAxis.setUnit( axis.getUnit() );

      bdgAxes.add( bdgAxis );
    }

    int ixCurve = 1;

    final List bdgThemes = bdgTemplate.getObservation();
    final Iterator itThemes = template.getThemes().iterator();
    while( itThemes.hasNext() )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) itThemes
          .next();

      final IObservation obs = theme.getObservation();

      final TypeObservation bdgTheme = ODT_OF.createTypeObservation();
      bdgTheme.setLinktype( "zml" );
      bdgTheme.setHref( obs.getHref() );

      final List bdgCurves = bdgTheme.getCurve();

      final Iterator itCurves = theme.getCurves().iterator();
      while( itCurves.hasNext() )
      {
        final IDiagramCurve curve = (IDiagramCurve) itCurves.next();

        final TypeCurve bdgCurve = ODT_OF.createTypeCurve();
        bdgCurve.setId( "C" + ixCurve++ );
        bdgCurve.setName( curve.getName() );
        bdgCurve.setColor( StringUtilities.colorToString( curve.getColor() ) );

        final List bdgMappings = bdgCurve.getMapping();

        final IAxisMapping[] mappings = curve.getMappings();
        for( int i = 0; i < mappings.length; i++ )
        {
          final TypeAxisMapping bdgMapping = ODT_OF.createTypeAxisMapping();
          bdgMapping.setDiagramAxis( mappings[i].getDiagramAxis()
              .getIdentifier() );
          bdgMapping.setObservationAxis( mappings[i].getObservationAxis()
              .getName() );

          bdgMappings.add( bdgMapping );
        }

        bdgCurves.add( bdgCurve );
      }

      bdgThemes.add( bdgTheme );
    }

    return bdgTemplate;
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
          IDiagramAxis.DIRECTION_HORIZONTAL, IDiagramAxis.POSITION_BOTTOM,
          false );

    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_RIGHT, true, null, new Double(0.8) );

    if( axisType.equals( TimeserieConstants.TYPE_TEMPERATURE ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_RIGHT, false );

    // default axis
    return new DiagramAxis( axisType, "double", label, unit,
        IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );
  }
}