/**
 *
 */
package org.kalypso.google.earth.export.utils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.apache.commons.lang.ArrayUtils;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;

import com.google.earth.kml._2.DocumentType;
import com.google.earth.kml._2.LineStyleType;
import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PolyStyleType;
import com.google.earth.kml._2.StyleSelectorType;
import com.google.earth.kml._2.StyleType;

/**
 * @author kuch
 */
public class StyleTypeFactory
{
  private static StyleTypeFactory m_styleFactory = null;

  private final ObjectFactory kmlFactory;

  private StyleType m_labelStyleType = null;

  private final List<StyleType> m_polyStyles = new ArrayList<StyleType>();

  private final List<StyleType> m_lineStyles = new ArrayList<StyleType>();

  /**
   * @param factory
   */
  public StyleTypeFactory( final ObjectFactory factory )
  {
    kmlFactory = factory;
  }

  /**
   * @param feature
   * @param symbolizer
   */
  public StyleType getPointSymbolizer( final PointSymbolizer symbolizer )
  {
    // TODO implement some point styletype which will render an image and generates an IconStyleType
    if( m_labelStyleType == null )
    {
      m_labelStyleType = kmlFactory.createStyleType();
      m_labelStyleType.setLabelStyle( kmlFactory.createLabelStyleType() );
      m_labelStyleType.setId( Integer.toString( m_labelStyleType.hashCode() ) );
    }

    return m_labelStyleType;
  }

  /**
   * @param feature
   * @param symbolizer
   * @return
   * @throws FilterEvaluationException
   */
  public StyleType getPolygonSymbolizer( final PolygonSymbolizer symbolizer ) throws FilterEvaluationException
  {
    final Stroke stroke = symbolizer.getStroke();
    final Color color = stroke.getStroke( null );
    final double opacity = stroke.getOpacity( null );

    return getPolygonSymbolizer( color, opacity );
  }

  /**
   * @param color
   * @param opacity
   * @return
   */
  private StyleType getPolygonSymbolizer( final Color color, final double opacity )
  {
    final byte[] myColor = getKmlColor( color, opacity );

    for( final StyleType style : m_polyStyles )
    {
      final PolyStyleType poly = style.getPolyStyle();

      final byte[] polyColor = poly.getColor();
      if( ArrayUtils.isEquals( myColor, polyColor ) )
        return style;
    }

    final StyleType style = kmlFactory.createStyleType();

    final PolyStyleType poly = kmlFactory.createPolyStyleType();
    poly.setColor( myColor );
    style.setPolyStyle( poly );

    style.setId( Integer.toString( style.hashCode() ) );

    m_polyStyles.add( style );

    return style;
  }

  /**
   * @param color
   * @param opacity
   * @return
   */
  private byte[] getKmlColor( final Color color, final double opacity )
  {
    if( opacity > 1.0 )
      throw (new IllegalStateException( "opacity must be a value in range of 0.0 - 1.0!" ));

    final int kmlOpacity = (int) opacity * 255;
    final String kmlColor = String.format( "%02x%02x%02x%02x", kmlOpacity, color.getRed(), color.getGreen(), color.getBlue() );
    final byte[] myColor = new byte[kmlColor.length()];

    int pos = 0;
    for( final char c : kmlColor.toCharArray() )
      myColor[pos++] = (byte) c;

    return myColor;
  }

  /**
   * @return
   */
  public static StyleTypeFactory getStyleFactory( final ObjectFactory factory )
  {
    if( StyleTypeFactory.m_styleFactory == null )
      StyleTypeFactory.m_styleFactory = new StyleTypeFactory( factory );

    return StyleTypeFactory.m_styleFactory;
  }

  /**
   * @param symbolizer
   * @return
   * @throws FilterEvaluationException
   */
  public StyleType getLineSymbolizer( final LineSymbolizer symbolizer ) throws FilterEvaluationException
  {
    final Stroke stroke = symbolizer.getStroke();
    final Color color = stroke.getStroke( null );
    final double opacity = stroke.getOpacity( null );

    return getLineSymbolizer( color, opacity );
  }

  /**
   * @param color
   * @param opacity
   * @return
   */
  private StyleType getLineSymbolizer( final Color color, final double opacity )
  {
    final byte[] myColor = getKmlColor( color, opacity );

    for( final StyleType style : m_lineStyles )
    {
      final LineStyleType lineStyle = style.getLineStyle();

      final byte[] polyColor = lineStyle.getColor();
      if( ArrayUtils.isEquals( myColor, polyColor ) )
        return style;
    }

    final StyleType style = kmlFactory.createStyleType();

    final LineStyleType lineStyle = kmlFactory.createLineStyleType();
    lineStyle.setColor( myColor );

    style.setLineStyle( lineStyle );
    style.setId( Integer.toString( style.hashCode() ) );

    m_lineStyles.add( style );

    return style;
  }

  /**
   * adding all styles to kml root document!
   * 
   * @param documentType
   */
  public void addStylesToDocument( final DocumentType documentType )
  {
    final List<JAXBElement< ? extends StyleSelectorType>> styles = documentType.getStyleSelector();

    for( final StyleType style : m_polyStyles )
      styles.add( kmlFactory.createStyle( style ) );

    for( final StyleType style : m_lineStyles )
      styles.add( kmlFactory.createStyle( style ) );

  }

}
