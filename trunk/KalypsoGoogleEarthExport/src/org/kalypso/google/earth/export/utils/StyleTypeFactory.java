/**
 *
 */
package org.kalypso.google.earth.export.utils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;

import com.google.earth.kml._2.ColorStyleType;
import com.google.earth.kml._2.LabelStyleType;
import com.google.earth.kml._2.LineStyleType;
import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PolyStyleType;

/**
 * @author kuch
 */
public class StyleTypeFactory
{
  private static StyleTypeFactory m_styleFactory = null;

  private final ObjectFactory kmlFactory;

  private LabelStyleType m_labelStyleType = null;

  private final List<PolyStyleType> m_polyStyles = new ArrayList<PolyStyleType>();

  private final List<LineStyleType> m_lineStyles = new ArrayList<LineStyleType>();

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
  public LabelStyleType getPointSymbolizer( final PointSymbolizer symbolizer )
  {
    // TODO implement some point styletype which will render an image and generates an IconStyleType
    if( m_labelStyleType == null )
      m_labelStyleType = kmlFactory.createLabelStyleType();

    return m_labelStyleType;
  }

  /**
   * @param feature
   * @param symbolizer
   * @return
   * @throws FilterEvaluationException
   */
  public PolyStyleType getPolygonSymbolizer( final PolygonSymbolizer symbolizer ) throws FilterEvaluationException
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
  private PolyStyleType getPolygonSymbolizer( final Color color, final double opacity )
  {
    final byte[] myColor = getKmlColor( color, opacity );

    for( final PolyStyleType poly : m_polyStyles )
    {
      final byte[] polyColor = poly.getColor();
      if( ArrayUtils.isEquals( myColor, polyColor ) )
        return poly;
    }

    final PolyStyleType styleType = kmlFactory.createPolyStyleType();
    styleType.setColor( myColor );

    m_polyStyles.add( styleType );

    return styleType;
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
  public ColorStyleType getLineSymbolizer( final LineSymbolizer symbolizer ) throws FilterEvaluationException
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
  private ColorStyleType getLineSymbolizer( final Color color, final double opacity )
  {
    final byte[] myColor = getKmlColor( color, opacity );

    for( final LineStyleType line : m_lineStyles )
    {
      final byte[] polyColor = line.getColor();
      if( ArrayUtils.isEquals( myColor, polyColor ) )
        return line;
    }

    final LineStyleType styleType = kmlFactory.createLineStyleType();
    styleType.setColor( myColor );

    m_lineStyles.add( styleType );

    return styleType;
  }

}
