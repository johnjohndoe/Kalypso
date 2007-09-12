/**
 *
 */
package org.kalypso.google.earth.export.utils;

import java.awt.Color;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.bind.JAXBElement;

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

  private final Map<String, StyleType> m_polyStyles = new HashMap<String, StyleType>();

  private final Map<String, StyleType> m_lineStyles = new HashMap<String, StyleType>();

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
      m_labelStyleType.setId( "id" + Integer.toString( m_labelStyleType.hashCode() ) );

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
    final int[] intColor = getColor( color, opacity );
    final byte[] kmlColor = getKmlColor( intColor );
    final String idColor = getColorId( intColor );

    StyleType styleType = m_polyStyles.get( idColor );
    if( styleType == null )
    {
      styleType = kmlFactory.createStyleType();

      final PolyStyleType poly = kmlFactory.createPolyStyleType();
      poly.setColor( kmlColor );
      styleType.setPolyStyle( poly );
      styleType.setId( "poly" + idColor );

      m_polyStyles.put( idColor, styleType );
    }

    return styleType;
  }

  /**
   * @param myColor
   * @return
   */
  private String getColorId( final int[] color )
  {
    final StringBuffer buffer = new StringBuffer();
    for( final int i : color )
      buffer.append( String.format( "%02x", i ) );

    return buffer.toString();
  }

  private int[] getColor( final Color color, final double opacity )
  {
    if( opacity > 1.0 )
      throw (new IllegalStateException( "opacity must be a value in range of 0.0 - 1.0!" ));

    final int kmlOpacity = (int) opacity * 255;
    // alpha=0x7f, blue=0xff, green=0x00, and red=0x00.

    final int[] bs = new int[4];
    bs[0] = kmlOpacity;
    bs[1] = color.getBlue();
    bs[2] = color.getGreen();
    bs[3] = color.getRed();

    return bs;
  }

  /**
   * @param color
   * @param opacity
   * @return
   */
  private byte[] getKmlColor( final int[] color )
  {
    final byte[] bs = new byte[color.length];
    for( int i = 0; i < color.length; i++ )
      bs[i] = (byte) color[i];

    return bs;
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
    final int[] intColor = getColor( color, opacity );
    final byte[] kmlColor = getKmlColor( intColor );
    final String idColor = getColorId( intColor );

    StyleType styleType = m_lineStyles.get( idColor );
    if( styleType == null )
    {
      styleType = kmlFactory.createStyleType();

      final LineStyleType lineStyle = kmlFactory.createLineStyleType();
      lineStyle.setColor( kmlColor );

      styleType.setLineStyle( lineStyle );
      styleType.setId( "line" + idColor );

      m_lineStyles.put( idColor, styleType );
    }

    return styleType;
  }

  /**
   * adding all styles to kml root document!
   * 
   * @param documentType
   */
  public void addStylesToDocument( final DocumentType documentType )
  {
    final List<JAXBElement< ? extends StyleSelectorType>> styles = documentType.getStyleSelector();

    styles.add( kmlFactory.createStyle( m_labelStyleType ) );

    Set<Entry<String, StyleType>> set = m_polyStyles.entrySet();
    for( final Entry<String, StyleType> entry : set )
      styles.add( kmlFactory.createStyle( entry.getValue() ) );

    set = m_lineStyles.entrySet();
    for( final Entry<String, StyleType> entry : set )
      styles.add( kmlFactory.createStyle( entry.getValue() ) );
  }
}
