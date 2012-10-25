/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.gml.classifications.IPartType;
import org.kalypso.model.wspm.core.gml.classifications.IStyleDefinition;
import org.kalypso.model.wspm.core.gml.classifications.IStyleParameterConstants;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IMarker;
import de.openali.odysseus.chart.framework.model.style.IStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.OvalMarker;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;

/**
 * Gives access to the part type of a given object part type.
 * 
 * @author Gernot Belger
 */
public class PartTypeAccessor
{
  private final IProfile m_profil;

  private final String m_type;

  public PartTypeAccessor( final IProfile profil, final String type )
  {
    m_profil = profil;
    m_type = type;
  }

  public String getTypeLabel( )
  {
    final IPartType partType = getPartType();
    if( partType == null )
      return "<unkown>";

    return partType.getDescription();
  }

  private IPartType getPartType( )
  {
    final IWspmClassification classification = WspmClassifications.getClassification( m_profil );
    if( classification == null )
      return null;

    return classification.findPartType( m_type );
  }

  public IStyle[] getStyles( )
  {
    final IPartType partType = getPartType();
    if( partType == null )
      return new IStyle[] {};

    final IStyleDefinition definition = partType.getStyleDefinition();
    if( definition == null )
      return new IStyle[] {};

    final Collection<IStyle> styles = new ArrayList<>();

    final IFeatureBindingCollection<org.kalypso.model.wspm.core.gml.classifications.IStyle> styleCollection = definition.getStyleCollection();
    for( final org.kalypso.model.wspm.core.gml.classifications.IStyle wspmStyle : styleCollection )
    {
      final IStyle style = convertStyle( wspmStyle );
      if( style != null )
        styles.add( style );
    }

    return styles.toArray( new IStyle[styles.size()] );
  }

  private IStyle convertStyle( final org.kalypso.model.wspm.core.gml.classifications.IStyle wspmStyle )
  {
    final IStyleParameterConstants.TYPE styleType = wspmStyle.getType();
    switch( styleType )
    {
      case point:
        return convertPointStyle( wspmStyle );

      case line:
        return convertLineStyle( wspmStyle );
    }

    return null;
  }

  private IStyle convertPointStyle( final org.kalypso.model.wspm.core.gml.classifications.IStyle wspmStyle )
  {
    final int lineWidth = wspmStyle.getStrokeWidth();
    final RGB lineColor = wspmStyle.getStrokeColor();
    final ILineStyle outline = new LineStyle( lineWidth, lineColor, 255, 0, null, LINEJOIN.ROUND, LINECAP.ROUND, 0, true );

    final int width = wspmStyle.getMarkerWidth();
    final int height = wspmStyle.getMarkerHeight();
    final int alpha = 255;
    final RGB inlineColor = wspmStyle.getFillColor();
    final boolean isFillVisible = true;
    final IMarker marker = new OvalMarker();

    return new PointStyle( outline, width, height, alpha, inlineColor, isFillVisible, marker, true );
  }

  private IStyle convertLineStyle( final org.kalypso.model.wspm.core.gml.classifications.IStyle wspmStyle )
  {
    final int width = wspmStyle.getStrokeWidth();
    final RGB color = wspmStyle.getStrokeColor();
    final int alpha = 255;
    final float dashOffset = 0;
    final float[] dashArray = new float[] { 10, 10 };
    final LINEJOIN lineJoin = LINEJOIN.ROUND;
    final LINECAP lineCap = LINECAP.ROUND;
    final int miterLimit = 0;

    return new LineStyle( width, color, alpha, dashOffset, dashArray, lineJoin, lineCap, miterLimit, true );
  }
}