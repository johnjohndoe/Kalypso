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
package org.kalypso.ui.wizards.results;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author Thomas Jung
 * 
 */

public class PolygonColorMapEditorComposite extends Composite
{
  private final PolygonColorMap m_colorMap;

  private PolygonColorMapEntry m_toEntry;

  private PolygonColorMapEntry m_fromEntry;

  private char[] m_stepWidth;

  private char[] m_minValue;

  private char[] m_maxValue;

  public PolygonColorMapEditorComposite( final Composite parent, final int style, final PolygonColorMap colorMap )
  {
    super( parent, style );
    m_colorMap = colorMap;

    createControl();

  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    Group fromColorMapGroup = new Group( this, SWT.NONE );
    fromColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fromColorMapGroup.setLayout( new GridLayout( 1, true ) );
    fromColorMapGroup.setText( "Startfarbe" );

    Group toColorMapGroup = new Group( this, SWT.NONE );
    toColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toColorMapGroup.setLayout( new GridLayout( 1, true ) );
    toColorMapGroup.setText( "Endfarbe" );

    m_fromEntry = m_colorMap.getColorMap()[0];
    final PolygonColorMapEntryEditorComposite fromEntryComposite = new PolygonColorMapEntryEditorComposite( fromColorMapGroup, SWT.NONE, m_fromEntry );
    fromEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_toEntry = m_colorMap.getColorMap()[m_colorMap.getColorMap().length - 1];
    final PolygonColorMapEntryEditorComposite toEntryComposite = new PolygonColorMapEntryEditorComposite( toColorMapGroup, SWT.NONE, m_toEntry );
    toEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

  }

  protected void updateColorMap( ) throws FilterEvaluationException
  {

    // Fill
    final Color fromPolygonColor = m_fromEntry.getFill().getFill( null );
    final Color toPolygonColor = m_toEntry.getFill().getFill( null );
    final double polygonOpacity = m_fromEntry.getFill().getOpacity( null );

    // Stroke
    final Color fromLineColor = m_fromEntry.getStroke().getStroke( null );
    final Color toLineColor = m_toEntry.getStroke().getStroke( null );
    final double lineOpacity = m_fromEntry.getStroke().getOpacity( null );

    int stepWidthScale = 2;

    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isareas by using class steps of 0.1
    // later, the classes will be created by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = new BigDecimal( m_minValue ).setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = new BigDecimal( m_maxValue ).setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal polygonStepWidth = new BigDecimal( m_stepWidth ).setScale( stepWidthScale, BigDecimal.ROUND_FLOOR );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth )).intValue();

    final List<PolygonColorMapEntry> colorMapList = new LinkedList<PolygonColorMapEntry>();

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double fromValue = minDecimal.doubleValue() + currentClass * polygonStepWidth.doubleValue();
      final double toValue = minDecimal.doubleValue() + (currentClass + 1) * polygonStepWidth.doubleValue();

      // Stroke

      Color lineColor;
      if( fromLineColor == toLineColor )
        lineColor = fromLineColor;
      else
        lineColor = ColorMapHelper.interpolateColor( fromLineColor, toLineColor, currentClass, numOfClasses );

      // Fill
      final Color polygonColor = ColorMapHelper.interpolateColor( fromPolygonColor, toPolygonColor, currentClass, numOfClasses );
      lineColor = polygonColor;

      final Stroke stroke = StyleFactory.createStroke( lineColor, lineOpacity );
      final Fill fill = StyleFactory.createFill( polygonColor, polygonOpacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isofl‰che " + currentClass );
      final ParameterValueType from = StyleFactory.createParameterValueType( fromValue );
      final ParameterValueType to = StyleFactory.createParameterValueType( toValue );

      final PolygonColorMapEntry colorMapEntry = new PolygonColorMapEntry_Impl( fill, stroke, label, from, to );
      colorMapList.add( colorMapEntry );
    }
    if( colorMapList.size() > 0 )
      m_colorMap.setColorMap( colorMapList );
  }

}
