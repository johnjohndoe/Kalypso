/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree_impl.graphics.displayelements.ElevationColorEntry;
import org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.intervalrtree.SortedPackedIntervalRTree;

/**
 * @author Patrice Congo
 */
public class SimpleElevationColorModel implements IElevationColorModel
{
  private final List<ElevationColorEntry> m_entries = new ArrayList<>();

  private final SortedPackedIntervalRTree m_entryIndex = new SortedPackedIntervalRTree();

  public static final double DEEPEST_POINT_ON_EARTH = -10924;

  public static final double HIGHEST_POINT_ON_EARTH = 8850;

  private double m_minElevation;

  private double m_maxElevation;

  private final double m_minHue;

  private final double m_maxHue;

  private final double m_minSat;

  private final double m_maxSat;

  private float[] noElevationColorHSB;

  private final int m_transparency;

  private final boolean m_goDarkerFromMinToMax;

  private final Color m_minColor;

  private final Color m_maxColor;

  private final float[] m_minhsb;

  private final float[] m_maxhsb;

  private final double m_minBri;

  private final double m_maxBri;

  private final List<Color> m_colorList = new ArrayList<>();

  private final int m_numOfClasses;

  private final Color m_noElevationColor;

  private GeoTransform m_projection;

  public SimpleElevationColorModel( final double minElevation, final double maxElevation, final Color minColor, final Color maxColor, final Color noElevationColor, final double transparency, final int numOfClasses, final boolean goDarkerFromMinToMax )
  {
    m_minElevation = minElevation;
    m_maxElevation = maxElevation;

    m_minColor = minColor;
    m_maxColor = maxColor;

    m_minhsb = Color.RGBtoHSB( m_minColor.getRed(), m_minColor.getGreen(), m_minColor.getBlue(), null );
    m_maxhsb = Color.RGBtoHSB( m_maxColor.getRed(), m_maxColor.getGreen(), m_maxColor.getBlue(), null );

    m_minHue = m_minhsb[0];
    m_maxHue = m_maxhsb[0];

    m_minSat = m_minhsb[1];
    m_maxSat = m_maxhsb[1];

    m_minBri = m_minhsb[2];
    m_maxBri = m_maxhsb[2];

    m_numOfClasses = numOfClasses;
    m_transparency = (255 - (int)(transparency * 255.0 / 100.0));
    m_goDarkerFromMinToMax = goDarkerFromMinToMax;

    m_noElevationColor = new Color( noElevationColor.getRed(), noElevationColor.getGreen(), noElevationColor.getBlue(), m_transparency );

    try
    {
      fillColorList();
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * fills the color list with colors for each class by interpolation between two chosen colors
   */
  private void fillColorList( ) throws FilterEvaluationException
  {
    /* min Color */
    Color hsbColor = Color.getHSBColor( (float)m_minHue, (float)m_minSat, (float)m_minBri );
    Color rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue(), m_transparency );
    m_colorList.add( rgbColor );

    final ElevationColorEntry minEntry = createPainterEntry( 0, rgbColor );
    addEntry( minEntry );

    for( int i = 1; i < m_numOfClasses - 1; i++ )
    {
      final double Hue = m_minHue + (i * (m_maxHue - m_minHue) / (m_numOfClasses - 1));
      final double Sat = m_minSat + (i * (m_maxSat - m_minSat) / (m_numOfClasses - 1));
      final double Bri = m_minBri + (i * (m_maxBri - m_minBri) / (m_numOfClasses - 1));

      hsbColor = Color.getHSBColor( (float)Hue, (float)Sat, (float)Bri );
      rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue(), m_transparency );
      m_colorList.add( rgbColor );

      final ElevationColorEntry entry = createPainterEntry( i, rgbColor );
      addEntry( entry );
    }

    /* max Color */
    hsbColor = Color.getHSBColor( (float)m_maxHue, (float)m_maxSat, (float)m_maxBri );
    rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue(), m_transparency );
    m_colorList.add( rgbColor );

    final ElevationColorEntry maxEntry = createPainterEntry( m_numOfClasses, rgbColor );
    addEntry( maxEntry );
  }

  private void addEntry( final ElevationColorEntry entry )
  {
    m_entries.add( entry );
    m_entryIndex.insert( entry.getFrom(), entry.getTo(), entry );
  }

  private ElevationColorEntry createPainterEntry( final int i, final Color rgbColor ) throws FilterEvaluationException
  {
    final int alpha = rgbColor.getAlpha();
    final double opacity = alpha / 255.0;
    final Fill fill = StyleFactory.createFill( rgbColor, opacity );

    // REMARK: NOT using a stroke for two reasons:
    // - paint speed-up by ~40%
    // - looks better in most cases (before, always some white pixels where visible)
    // final Stroke stroke = null;
    final Stroke stroke = StyleFactory.createStroke( rgbColor, 1, opacity );

    final String label = ""; //$NON-NLS-1$
    final double interval = Math.abs( (m_maxElevation - m_minElevation) ) / m_numOfClasses;
    final double from = m_minElevation + i * interval;
    final double to = m_minElevation + (i + 1) * interval;

    return new ElevationColorEntry( fill, stroke, null, null, m_projection, label, from, to );
  }

  @Override
  public Color getColor( final double elevation )
  {
    return interpolateColor( elevation );
  }

  /**
   * gets the corresponding color class for the given elevation
   * 
   * @param elevation
   *          given elevation
   */
  private final Color interpolateColor( final double elevation )
  {
    final int colorClass = (int)((elevation - m_minElevation) / (m_maxElevation - m_minElevation) * m_numOfClasses);
    int red = 0;
    int green = 0;
    int blue = 0;

    if( Double.isNaN( elevation ) )
    {
      return m_noElevationColor;
    }
    else if( elevation > m_minElevation && elevation < m_maxElevation )
    {
      if( !m_goDarkerFromMinToMax )
      {
        red = m_colorList.get( colorClass ).getRed();
        green = m_colorList.get( colorClass ).getGreen();
        blue = m_colorList.get( colorClass ).getBlue();
      }
      else
      {
        red = m_colorList.get( (m_numOfClasses - 1) - colorClass ).getRed();
        green = m_colorList.get( (m_numOfClasses - 1) - colorClass ).getGreen();
        blue = m_colorList.get( (m_numOfClasses - 1) - colorClass ).getBlue();
      }

    }
    else
    {
      /*
       * elevation lies outside the specific range => this can be caused by rounding errors. check, if the elevation is
       * greater than the specified maximum
       */
      if( elevation >= m_maxElevation )
      {
        if( !m_goDarkerFromMinToMax )
        {
          red = m_colorList.get( (m_numOfClasses - 1) ).getRed();
          green = m_colorList.get( (m_numOfClasses - 1) ).getGreen();
          blue = m_colorList.get( (m_numOfClasses - 1) ).getBlue();
        }
        else
        {
          red = m_colorList.get( 0 ).getRed();
          green = m_colorList.get( 0 ).getGreen();
          blue = m_colorList.get( 0 ).getBlue();
        }
        final Color rgbColor = new Color( red, green, blue, elevation == m_maxElevation ? m_transparency : 0 );
        return rgbColor;
      }

      /* check, if the elevation is less than the specified minimum */
      if( elevation <= m_minElevation )
      {
        if( !m_goDarkerFromMinToMax )
        {
          red = m_colorList.get( 0 ).getRed();
          green = m_colorList.get( 0 ).getGreen();
          blue = m_colorList.get( 0 ).getBlue();
        }
        else
        {
          red = m_colorList.get( (m_numOfClasses - 1) ).getRed();
          green = m_colorList.get( (m_numOfClasses - 1) ).getGreen();
          blue = m_colorList.get( (m_numOfClasses - 1) ).getBlue();
        }
        final Color rgbColor = new Color( red, green, blue, elevation == m_minElevation ? m_transparency : 0 );

        return rgbColor;
      }

    }
    final Color rgbColor = new Color( red, green, blue, m_transparency );

    return rgbColor;
  }

  @Override
  public void setElevationMinMax( final double minElevation, final double maxElevation )
  {
    m_minElevation = minElevation;
    m_maxElevation = maxElevation;
    try
    {
      fillColorList();
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public float[] getHSB( final double elevation )
  {
    if( Double.isNaN( elevation ) )
    {
      return new float[] { noElevationColorHSB[0], noElevationColorHSB[1], noElevationColorHSB[2] };
    }
    else if( elevation >= m_minElevation && elevation <= m_maxElevation )
    {
      final double hue = m_minHue + elevation * (m_maxHue - m_minHue) / (m_maxElevation - m_minElevation);
      final double sat = m_minSat + elevation * (m_maxSat - m_minSat) / (m_maxElevation - m_minElevation);
      final double bri = m_minBri + elevation * (m_maxBri - m_minBri) / (m_maxElevation - m_minElevation);
      return new float[] { (float)hue, (float)sat, (float)bri };
    }
    else
    {
      // or return a translucent color
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SimpleElevationColorModel.0" ) + "\n\tminElevation=" + m_minElevation + "\n\tmaxElevation=" + m_maxElevation + "\n\tcurrentElevation=" + elevation ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
  }

  public float[] getRealRGB( final double elevation )
  {
    final float[] val = getHSB( elevation );
    return new float[] { Color.getHSBColor( val[0], val[1], val[2] ).getRed(), Color.getHSBColor( val[0], val[1], val[2] ).getGreen(), Color.getHSBColor( val[0], val[1], val[2] ).getBlue() };
  }

  @Override
  public double[] getElevationMinMax( )
  {
    final double[] values = new double[2];
    values[0] = m_minElevation;
    values[1] = m_maxElevation;
    return values;
  }

  // @Override
  // public int getNumOfClasses( )
  // {
  // return m_numOfClasses;
  // }

  // @Override
  // public double getDiscretisationInterval( )
  // {
  // return Math.abs( (m_maxElevation - m_minElevation) ) / m_numOfClasses;
  // }

  // @Override
  // public double getFrom( final int currentClass )
  // {
  // return m_minElevation + currentClass * getDiscretisationInterval();
  // }
  //
  // @Override
  // public double getTo( final int currentClass )
  // {
  // return m_minElevation + (currentClass + 1) * getDiscretisationInterval();
  // }
  //
  // @Override
  // public FillPainter getFillPolygonPainter( final int currentClass )
  // {
  // return m_lister.get( currentClass ).getPolygonPainter();
  // }
  //
  // @Override
  // public StrokePainter getLinePainter( final int currentClass )
  // {
  // return m_lister.get( currentClass ).getLinePainter();
  // }

  @Override
  public void setProjection( final GeoTransform projection )
  {
    m_projection = projection;
    try
    {
      m_colorList.clear();
      m_entries.clear();

      fillColorList();
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Override
  public ElevationColorEntry getColorEntry( final double elevation )
  {
    // REMARK: necessary! query to empty index leads to endless loop.
    if( m_entries.size() == 0 )
      return null;

    final ElevationColorEntry[] result = new ElevationColorEntry[1];

    final ItemVisitor visitor = new ItemVisitor()
    {
      @Override
      public void visitItem( final Object item )
      {
        result[0] = (ElevationColorEntry)item;
      }
    };
    m_entryIndex.query( elevation, elevation, visitor );

    return result[0];
  }

  @Override
  public ElevationColorEntry[] getColorEntries( )
  {
    return m_entries.toArray( new ElevationColorEntry[m_entries.size()] );
  }
}
