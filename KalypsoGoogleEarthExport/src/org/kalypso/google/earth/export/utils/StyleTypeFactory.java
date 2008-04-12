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

import org.kalypso.google.earth.export.Messages;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.CssParameter;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;

import com.google.earth.kml.DocumentType;
import com.google.earth.kml.IconStyleIconType;
import com.google.earth.kml.IconStyleType;
import com.google.earth.kml.LineStyleType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PolyStyleType;
import com.google.earth.kml.StyleSelectorType;
import com.google.earth.kml.StyleType;

/**
 * @author kuch
 */
public class StyleTypeFactory
{
  private static StyleTypeFactory m_styleFactory = null;

  /**
   * @return
   */
  public static StyleTypeFactory getStyleFactory( final ObjectFactory factory )
  {
    if( StyleTypeFactory.m_styleFactory == null )
      StyleTypeFactory.m_styleFactory = new StyleTypeFactory( factory );

    return StyleTypeFactory.m_styleFactory;
  }

  private final ObjectFactory kmlFactory;

  private StyleType m_labelStyleType = null;

  private final Map<String, StyleType> m_polyStyles = new HashMap<String, StyleType>();

  private final Map<String, StyleType> m_lineStyles = new HashMap<String, StyleType>();

  private final Map<String, StyleType> m_iconStyles = new HashMap<String, StyleType>();

  /**
   * @param factory
   */
  public StyleTypeFactory( final ObjectFactory factory )
  {
    kmlFactory = factory;
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

    set = m_iconStyles.entrySet();
    for( final Entry<String, StyleType> entry : set )
      styles.add( kmlFactory.createStyle( entry.getValue() ) );
  }

  private int[] getColor( final Color color, final double opacity )
  {
    if( opacity > 1.0 )
      throw (new IllegalStateException( Messages.StyleTypeFactory_0 ));

    final Double kmlOpacity = opacity * 255 + 40;
    // alpha=0x7f, blue=0xff, green=0x00, and red=0x00.

    // FIXME - adjust opacity
    final int[] bs = new int[4];

    bs[0] = Math.min( kmlOpacity.intValue(), 230 );
    bs[1] = color.getBlue();
    bs[2] = color.getGreen();
    bs[3] = color.getRed();

    return bs;
  }

  /**
   * @param myColor
   * @return
   */
  private String getColorId( final int[] color )
  {
    final StringBuffer buffer = new StringBuffer();
    for( final int i : color )
      buffer.append( String.format( "%02x", i ) ); //$NON-NLS-1$

    return buffer.toString();
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
      styleType.setId( "line" + idColor ); //$NON-NLS-1$

      m_lineStyles.put( idColor, styleType );
    }

    return styleType;
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
      m_labelStyleType.setId( "id" + Integer.toString( m_labelStyleType.hashCode() ) ); //$NON-NLS-1$

    }

    return m_labelStyleType;
  }

  /**
   * @param color
   * @param opacity
   * @param polyOpacity
   * @param polyColor
   * @return
   */
  private StyleType getPolygonSymbolizer( final Color lineColor, final double lineOpacity, final Color polyColor, final double polyOpacity )
  {
    final int[] polyIntColor = getColor( polyColor, polyOpacity );
    final byte[] polyKmlColor = getKmlColor( polyIntColor );
    final String polyColorId = getColorId( polyIntColor );

    final int[] lineIntColor = getColor( lineColor, lineOpacity );
    final byte[] lineKmlColor = getKmlColor( lineIntColor );

    StyleType styleType = m_polyStyles.get( polyColorId );
    if( styleType == null )
    {
      styleType = kmlFactory.createStyleType();

      final LineStyleType line = kmlFactory.createLineStyleType();
      line.setColor( lineKmlColor );
      styleType.setLineStyle( line );

      final PolyStyleType poly = kmlFactory.createPolyStyleType();
      poly.setColor( polyKmlColor );
      styleType.setPolyStyle( poly );
      styleType.setId( "poly" + polyColorId ); //$NON-NLS-1$

      m_polyStyles.put( polyColorId, styleType );
    }

    return styleType;
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
    final Color lineColor = stroke.getStroke( null );
    final double lineOpacity = stroke.getOpacity( null );

    final Fill fill = symbolizer.getFill();

    final CssParameter parameter = (CssParameter) fill.getCssParameters().get( "fill" ); //$NON-NLS-1$
    final ParameterValueType value = parameter.getValue();
    final Object[] components = value.getComponents();
    final Color polyColor;
    if( components.length == 1 )
    {
      final String color = (String) components[0];

      final String red = color.substring( 1, 3 );
      final String green = color.substring( 3, 5 );
      final String blue = color.substring( 5, 7 );

      final Integer r = Integer.decode( "0x" + red ); //$NON-NLS-1$
      final Integer g = Integer.decode( "0x" + green ); //$NON-NLS-1$
      final Integer b = Integer.decode( "0x" + blue ); //$NON-NLS-1$

      polyColor = new Color( r, g, b );
    }
    else
      polyColor = lineColor;

    final double polyOpacity = fill.getOpacity( null );

    return getPolygonSymbolizer( lineColor, lineOpacity, polyColor, polyOpacity );
  }

  public StyleType createIconStyle( final String href )
  {

    StyleType styleType = m_iconStyles.get( href );
    if( styleType == null )
    {
      styleType = kmlFactory.createStyleType();
      styleType.setId( "icon" + Integer.valueOf( href.hashCode() ).toString() );

      final IconStyleType iconStyleType = kmlFactory.createIconStyleType();
      iconStyleType.setId( "iconStyleType" + Integer.valueOf( styleType.hashCode() ).toString() );

      final IconStyleIconType icon = kmlFactory.createIconStyleIconType();
      icon.setId( "iconStyleIconType" + Integer.valueOf( iconStyleType.hashCode() ).toString() );
      icon.setHref( href );

      iconStyleType.setIcon( icon );
      styleType.setIconStyle( iconStyleType );

      m_iconStyles.put( href, styleType );
    }

    return styleType;
  }

  public void dispose( )
  {
    m_iconStyles.clear();
    m_lineStyles.clear();
    m_polyStyles.clear();

  }
}
